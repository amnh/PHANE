{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedSums #-}

{- |
The 'Evaluation' type's monad transformer definition and types.
-}
module Control.Evaluation (
    Evaluation (),

    -- * Run computation
    runEvaluation,

    -- * Logging operations
    LogConfiguration (),
    initializeLogging,
    setVerbositySTDERR,
    setVerbositySTDOUT,
    setVerbosityFileLog,

    -- * Parallel operations
    getParallelChunkMap,
    getParallelChunkTraverse,

    -- * Randomness operations
    RandomSeed (),
    initializeRandomSeed,
    setRandomSeed,

    -- * Other
    failWithPhase,
    mapEvaluation,
) where

import Control.Applicative (Alternative (..))
import Control.DeepSeq
import Control.Evaluation.Logging.Class
import Control.Evaluation.Logging.Configuration
import Control.Evaluation.Result
import Control.Evaluation.Verbosity
import Control.Exception
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Random.Strict
import Control.Monad.Reader (MonadReader (..), ReaderT (..), withReaderT)
import Control.Monad.Zip (MonadZip (..))
import Control.Parallel.Strategies
import Data.Bimap qualified as BM
import Data.Bits (xor)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (sconcat)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Conc (getNumCapabilities)
import GHC.Generics
import System.CPUTime (cpuTimePrecision, getCPUTime)
import System.ErrorPhase
import System.Exit
import System.Random.Stateful
import Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..))
import Test.QuickCheck.Gen (Gen (..), variant)
import UnliftIO.Async (pooledMapConcurrently)


{- |
A computational "evaluation."

An evaluation has a /read-only/ global environment @env@, accessible to it's computation.

An evaluation can be in one of two states, "successful" or "failure".
Use 'pure' to place a value inside a successful computational context.
Use 'fail' to indicate a computational failure.

A computational evaluation short-circuits at the first failure encountered.
The semigroup operator '(<>)' reflects this.
The alternative operator '(<|>)' inverts this logic, short-circuiting at the first success.
The following should hold:

> foldr1 (<>)  [fail x, fail y, pure z] === fail x
> foldr1 (<|>) [fail x, fail y, pure z] === pure z


  *  __Computation:__

     Use 'runEvaluation' to get the result of the computation.

  *  __Logging:__

     Use the 'Logger' type-class method 'logWith' to log output.

  *  __Randomness:__

     Use the 'MonadRandom' type-class methods to generate random values.

  *  __Parallelism:__

     Use the 'getParallelChunkMap' to compute a pure function in parallel, equal sized-chunks of the list.

     Use the 'getParallelChunkTraverse' to compute an effectful function in parallel, equal sized-chunks of the list.
-}
newtype Evaluation env a = Evaluation
    --    { unwrapEvaluation ∷ RWST env Void (ImplicitEnvironment) IO (EvaluationResult a)
    { unwrapEvaluation ∷ ReaderT (ImplicitEnvironment env) IO (EvaluationResult a)
    -- ^ Run the 'Evaluation' monad transformer
    }
    deriving stock (Generic)


type role Evaluation representational nominal


data ImplicitEnvironment env = ImplicitEnvironment
    { implicitBucketNum ∷ {-# UNPACK #-} ParallelBucketCount
    , implicitLogConfig ∷ {-# UNPACK #-} LogConfiguration
    , implicitRandomGen ∷ {-# UNPACK #-} (AtomicGenM StdGen)
    , explicitReaderVal ∷ env
    }


type role ImplicitEnvironment representational


newtype ParallelBucketCount = MaxPar Word
    deriving newtype (Eq, Enum, Integral, Num, Ord, Real, Show)


{- |
A seed from which a /(practically infinite)/ stream of pseudorandomness can be generated.
-}
newtype RandomSeed = RandomSeed Int
    deriving newtype (Eq, Enum, Integral, Num, Ord, Real, Show)


instance Alternative (Evaluation env) where
    {-# INLINEABLE (<|>) #-}
    (<|>) x y = Evaluation . ReaderT $ \store → do
        res ← runReaderT (unwrapEvaluation x) store
        case runEvaluationResult res of
            Left _ → runReaderT (unwrapEvaluation y) store
            Right _ → pure res


    empty = fail "Alternative identity"


instance Applicative (Evaluation env) where
    {-# INLINEABLE (<*>) #-}
    {-# INLINE (*>) #-}
    {-# INLINE pure #-}
    pure = Evaluation . pure . pure


    (<*>) = apply


    (*>) = propagate


instance (Arbitrary a, CoArbitrary env) ⇒ Arbitrary (Evaluation env a) where
    arbitrary = do
        fun ← (arbitrary ∷ Gen (ImplicitEnvironment env → EvaluationResult a))
        pure . Evaluation . ReaderT $ pure . fun


instance (CoArbitrary env) ⇒ CoArbitrary (ImplicitEnvironment env) where
    coarbitrary store =
        let x = implicitBucketNum store
            y = implicitLogConfig store
            z = explicitReaderVal store
        in  coarbitrary z . coarbitrary y . variant x


deriving stock instance Functor ImplicitEnvironment


instance Functor (Evaluation env) where
    {-# INLINEABLE fmap #-}
    fmap f x = Evaluation . fmap (fmap f) $ unwrapEvaluation x


instance Logger (Evaluation env) where
    {-# INLINEABLE logWith #-}
    logWith level str = Evaluation $ do
        impEnv ← ask
        let logConfig = implicitLogConfig impEnv
        liftIO . fmap pure . processMessage logConfig level $ logToken str


instance (NFData a) ⇒ NFData (Evaluation env a) where
    {-# INLINE rnf #-}
    rnf (Evaluation x) = (rnf <$> x) `seq` ()


instance Monad (Evaluation env) where
    {-# INLINEABLE (>>=) #-}
    {-# INLINE (>>) #-}
    {-# INLINE return #-}


    (>>=) = bind


    (>>) = (*>)


    return = pure


instance MonadFail (Evaluation env) where
    {-# INLINE fail #-}
    fail = Evaluation . pure . fail


instance MonadFix (Evaluation env) where
    mfix f = let a = a >>= f in a


instance MonadInterleave (Evaluation env) where
    interleave action = Evaluation . ReaderT $ \store → do
        let gen = implicitRandomGen store
        gen' ← applyAtomicGen split gen >>= newAtomicGenM
        let store' = store{implicitRandomGen = gen'}
        pure <$> executeEvaluation store' action


instance MonadIO (Evaluation env) where
    {-# INLINE liftIO #-}
    liftIO = Evaluation . fmap pure . liftIO


instance MonadRandom (Evaluation env) where
    getRandomR range =
        Evaluation $
            reader implicitRandomGen >>= liftIO . fmap pure . randomRM range


    getRandom =
        Evaluation $
            reader implicitRandomGen >>= liftIO . fmap pure . randomM


    getRandomRs range = Evaluation $ do
        ref ← reader implicitRandomGen
        gen ← splitGenM ref
        pure . pure $ randomRs range gen


    getRandoms = Evaluation $ do
        ref ← reader implicitRandomGen
        gen ← splitGenM ref
        pure . pure $ randoms gen


instance MonadReader env (Evaluation env) where
    {-# INLINEABLE local #-}
    {-# INLINE ask #-}
    ask = Evaluation $ do
        store ← ask
        pure . pure $ explicitReaderVal store


    local f = Evaluation . local (fmap f) . unwrapEvaluation


instance MonadThrow (Evaluation env) where
    throwM e = Evaluation $ throwM e


instance MonadUnliftIO (Evaluation env) where
    {-# INLINE withRunInIO #-}
    -- f :: (forall a. Evaluation env a -> IO a) -> IO b
    withRunInIO f =
        {-# SCC withRunInIO_Evaluation #-}
        Evaluation . ReaderT $ \env →
            {-# SCC withRunInIO_ReaderT_f #-}
            withRunInIO $
                {-# SCC withRunInIO_WITH_run #-}
                \run →
                    pure <$> f (run . executeEvaluation env)


instance MonadZip (Evaluation env) where
    {-# INLINEABLE mzip #-}
    {-# INLINEABLE munzip #-}
    {-# INLINE mzipWith #-}


    mzip = liftA2 (,)


    mzipWith = liftA2


    munzip !x = (fst <$> x, snd <$> x)


instance PrimMonad (Evaluation env) where
    type PrimState (Evaluation env) = PrimState IO
    primitive = Evaluation . ReaderT . const . fmap pure . primitive


instance Semigroup (Evaluation env a) where
    {-# INLINE (<>) #-}
    lhs <> rhs = Evaluation . ReaderT $ \store → do
        x ← runReaderT (unwrapEvaluation lhs) store
        case runEvaluationResult x of
            Left s → pure . EU $ Left s
            _ → runReaderT (unwrapEvaluation rhs) store


{- |
Run the 'Evaluation' computation.

Initial randomness seed and configuration for logging outputs required to initiate the computation.
-}
runEvaluation ∷ (MonadIO m) ⇒ LogConfiguration → RandomSeed → env → Evaluation env a → m a
runEvaluation logConfig randomSeed environ eval = do
    randomRef ← newAtomicGenM . mkStdGen $ fromEnum randomSeed
    maxBuckets ← liftIO $ toEnum . max 1 . pred <$> getNumCapabilities
    let implicit =
            ImplicitEnvironment
                { implicitBucketNum = maxBuckets
                , implicitLogConfig = logConfig
                , implicitRandomGen = randomRef
                , explicitReaderVal = environ
                }
    executeEvaluation implicit eval


{- |
Set the verbosity level of logs streamed to @STDERR@ for the sub-'Evaluation'.
-}
setVerbositySTDERR ∷ Verbosity → Evaluation env () → Evaluation env ()
setVerbositySTDERR = setVerbosityOf modConfigSTDERR


{- |
Set the verbosity level of logs streamed to @STDOUT@ for the sub-'Evaluation'.
-}
setVerbositySTDOUT ∷ Verbosity → Evaluation env () → Evaluation env ()
setVerbositySTDOUT = setVerbosityOf modConfigSTDOUT


{- |
Set the verbosity level of log data streamed to the log file (if any) for the sub-'Evaluation'.
-}
setVerbosityFileLog ∷ Verbosity → Evaluation env () → Evaluation env ()
setVerbosityFileLog =
    let setVerbosityOf'
            ∷ ((LogFeed → LogFeed) → LogConfiguration → LogConfiguration)
            → Verbosity
            → Evaluation env a
            → Evaluation env a
        setVerbosityOf' f v =
            let transformation = modImplicitLogConfiguration (f (setFeedLevel v))
            in  Evaluation . withReaderT transformation . unwrapEvaluation
    in  setVerbosityOf' modConfigStream


{- |
/Note:/ Does not work on infinite lists!

Get a parallel mapping function which evenly distributes elements of the list
across available threads. The number of threads available on they system is
queried and memoized at the start of the 'Evaluation'. The length of the supplied
list is calculated, and the list is split into sub-lists of equal length (± 1).
Each sub list is given to a thread and fully evaluated.
-}
getParallelChunkMap ∷ ∀ a b env. (NFData b) ⇒ Evaluation env ((a → b) → [a] → [b])
getParallelChunkMap =
    let construct ∷ Word → (a → b) → [a] → [b]
        construct = \case
            0 → fmap
            1 → fmap
            n → \f → \case
                [] → []
                x : xs →
                    let !maxBuckets = fromIntegral n
                        len = length xs
                        num = case len `quotRem` maxBuckets of
                            (q, 0) → q
                            (q, _) → q + 1
                        y :| ys = withStrategy (parListChunk' num rdeepseq) $ f <$> x :| xs
                    in  y : ys
    in  Evaluation $ reader (pure . construct . fromIntegral . implicitBucketNum)


{- |
/Note:/ Does not work on infinite lists!

Like getParallelChunkMap, but performs monadic actions over the list in parallel.
Each thread will have a different, /uncorrelated/ random number generator.
-}
getParallelChunkTraverse
    ∷ ∀ a b env t
     . (NFData b, Traversable t)
    ⇒ Evaluation env ((a → Evaluation env b) → t a → Evaluation env (t b))
getParallelChunkTraverse = pure parallelTraverseEvaluation


{- |
Generate a 'RandomSeed' to initialize an 'Evaluation' by using system entropy.
-}
initializeRandomSeed ∷ IO RandomSeed
initializeRandomSeed = do
    now ← getPOSIXTime
    let timebits = truncate now ∷ Int
    cpu ← getCPUTime
    let cpubits = fromIntegral (cpu `div` cpuTimePrecision) ∷ Int
    pure . RandomSeed $ timebits `xor` cpubits


{- |
Set the random seed for the sub-'Evaluation'.
-}
setRandomSeed ∷ (Enum i) ⇒ i → Evaluation env () → Evaluation env ()
setRandomSeed seed eval =
    let gen = mkStdGen $ fromEnum seed
    in  withRandomGenerator gen eval


{- |
Lift one 'Evaluation' environment to another.
-}
mapEvaluation ∷ (outer → inner) → Evaluation inner a → Evaluation outer a
mapEvaluation f = Evaluation . withReaderT (fmap f) . unwrapEvaluation


{- |
Fail and indicate the phase in which the failure occurred.
-}
failWithPhase ∷ (Loggable s) ⇒ ErrorPhase → s → Evaluation env a
failWithPhase p message = do
    logWith LogFail message
    Evaluation . ReaderT . const . pure $ evalUnitWithPhase p message


executeEvaluation ∷ (MonadIO m) ⇒ ImplicitEnvironment env → Evaluation env a → m a
executeEvaluation implicitEnv (Evaluation (ReaderT f)) =
    let logConfig = implicitLogConfig implicitEnv
    in  {-
                flushLogBuffers ∷ IO ()
                flushLogBuffers =
                    let flushBufferOf ∷ (LogConfiguration → LogFeed) → IO ()
                        flushBufferOf g = hFlush . feedSpout $ g logConfig
                    in  do
                            flushBufferOf configSTDERR
                            flushBufferOf configSTDOUT
                            traverse_ (hFlush . feedSpout) $ configStream logConfig
        -}
        liftIO . flip finally (flushLogs logConfig) $ do
            res ← f implicitEnv
            case runEvaluationResult res of
                Right value → pure value
                Left (phase, txt) →
                    let exitCode = errorPhaseToExitCode BM.! phase
                    in  processMessage logConfig LogFail txt *> exitWith exitCode


parallelTraverseEvaluation
    ∷ ∀ a b env t
     . (NFData b, Traversable t)
    ⇒ (a → Evaluation env b)
    → t a
    → Evaluation env (t b)
parallelTraverseEvaluation f = pooledMapConcurrently (interleave . fmap force . f)


setVerbosityOf
    ∷ ((LogFeed → LogFeed) → LogConfiguration → LogConfiguration)
    → Verbosity
    → Evaluation env a
    → Evaluation env a
setVerbosityOf f v =
    let transformation = modImplicitLogConfiguration (f (setFeedLevel v))
    in  Evaluation . withReaderT transformation . unwrapEvaluation


{-
modConfigSTDERR ∷ (LogFeed → LogFeed) → LogConfiguration → LogConfiguration
modConfigSTDERR f x = x{configSTDERR = f $ configSTDERR x}

modConfigSTDOUT ∷ (LogFeed → LogFeed) → LogConfiguration → LogConfiguration
modConfigSTDOUT f x = x{configSTDOUT = f $ configSTDOUT x}

modConfigStream ∷ (Maybe LogFeed → Maybe LogFeed) → LogConfiguration → LogConfiguration
modConfigStream f x = x{configStream = f $ configStream x}

setFeedLevel ∷ Verbosity → LogFeed → LogFeed
setFeedLevel v x = x{feedLevel = v}
-}

modImplicitLogConfiguration
    ∷ (LogConfiguration → LogConfiguration)
    → ImplicitEnvironment env
    → ImplicitEnvironment env
modImplicitLogConfiguration f x = x{implicitLogConfig = f $ implicitLogConfig x}


withRandomGenerator ∷ StdGen → Evaluation env a → Evaluation env a
withRandomGenerator gen eval =
    let transformation ∷ AtomicGenM StdGen → ImplicitEnvironment env → ImplicitEnvironment env
        transformation val store = store{implicitRandomGen = val}
    in  liftIO (newAtomicGenM gen) >>= \ref →
            Evaluation . local (transformation ref) $ unwrapEvaluation eval


bind ∷ Evaluation env a → (a → Evaluation env b) → Evaluation env b
bind x f = Evaluation . ReaderT $ \store → do
    y ← runReaderT (unwrapEvaluation x) store
    case runEvaluationResult y of
        Left s → pure . EU $ Left s
        Right v → runReaderT (unwrapEvaluation (f v)) store


apply ∷ Evaluation env (t → a) → Evaluation env t → Evaluation env a
apply lhs rhs = Evaluation . ReaderT $ \store → do
    x ← runReaderT (unwrapEvaluation lhs) store
    case runEvaluationResult x of
        Left s → pure . EU $ Left s
        Right f → do
            y ← runReaderT (unwrapEvaluation rhs) store
            pure . EU $ case runEvaluationResult y of
                Left s → Left s
                Right v → Right $ f v


propagate ∷ Evaluation env a → Evaluation env b → Evaluation env b
propagate lhs rhs = Evaluation . ReaderT $ \store → do
    x ← runReaderT (unwrapEvaluation lhs) store
    case runEvaluationResult x of
        Left s → pure . EU $ Left s
        _ → runReaderT (unwrapEvaluation rhs) store


{-
doLogCs ∷ (MonadIO m) ⇒ LogConfiguration → LogLevel → CallStack → LogStr → m ()
doLogCs config level cs txt =
    let loc = case GHC.getCallStack cs of
            ((_, l) : _) → GHC.srcLocFile l <> ":" <> show (GHC.srcLocStartLine l)
            _ → "unknown"
    in  doLog config level (toLogStr loc) txt

doLog ∷ (MonadIO m) ⇒ LogConfiguration → LogLevel → LogStr → LogStr → m ()
doLog config level loc txt =
    let renderedLevelFull ∷ LogStr
        renderedLevelFull = case level of
            LogFail → "[FAIL]"
            LogWarn → "[WARN]"
            LogDone → "[DONE]"
            LogInfo → "[INFO]"
            LogMore → "[MORE]"
            LogTech → "[TECH]"
            LogDump → "[DUMP]"

        renderedLevelNice ∷ LogStr
        renderedLevelNice = case level of
            LogFail → "[FAIL]"
            LogWarn → "[WARN]"
            LogTech → "[TECH]"
            LogDump → "[DUMP]"
            _ → ""

        renderedLoc ∷ LogStr
        renderedLoc =
            let fullTrace = " <" <> toLogStr loc <> ">"
            in  case level of
                    LogTech → fullTrace
                    LogDump → fullTrace
                    _ → mempty

        renderedTime ∷ FormattedTime → LogStr
        renderedTime t = toLogStr t <> " @ "

        resetColor = "\o33[0;0m"

        (setColorPrefix, setColorSuffix) =
            let mkColorNum ∷ Word → Word → LogStr
                mkColorNum m n = fromString . show $ m + n

                mkColorPref ∷ Word → LogStr
                mkColorPref n = "\o33[0;" <> mkColorNum 90 n <> "m"

                mkColorSuff ∷ Word → LogStr
                mkColorSuff n = "\o33[0;" <> mkColorNum 30 n <> "m"

                mkColor ∷ Word → (LogStr, LogStr)
                mkColor x = (mkColorPref x, mkColorSuff x)
            in  case level of
                    LogFail → mkColor 1
                    LogWarn → mkColor 3
                    LogDone → mkColor 2
                    LogInfo → mkColor 7
                    LogMore → mkColor 6
                    LogTech → mkColor 5
                    LogDump → mkColor 5

        printer ∷ LoggerSet → LogStr → IO ()
        printer = pushLogStr

        outputLogFor ∷ LogFeed → Maybe FormattedTime → IO ()
        outputLogFor feed timeStamp =
            let prefix ∷ LogStr → LogStr
                prefix x =
                    let opening = x <> renderedLoc
                        seperatorOf
                            | opening == "" = id
                            | otherwise = (<> " ")
                    in  seperatorOf opening

                logger = feedSpout feed
            in  case timeStamp of
                    Just ts → printer logger $ prefix renderedLevelFull <> renderedTime ts <> txt
                    Nothing →
                        let coloredOutput ∷ LogStr
                            coloredOutput =
                                fold
                                    [ setColorPrefix
                                    , prefix renderedLevelNice
                                    , setColorSuffix
                                    , txt
                                    , resetColor
                                    ]
                        in  printer logger coloredOutput

        errFeed = configSTDERR config
        outFeed = configSTDOUT config
        errShow = optShow errFeed
        outShow = optShow outFeed && not errShow
        optShow x = case verbosityToLogLevel $ feedLevel x of
            Just spec | spec <= level → True
            _ → False
    in  liftIO $
            let
            in  do
                    when errShow $ outputLogFor errFeed Nothing
                    when outShow $ outputLogFor outFeed Nothing
                    case configStream config of
                        Nothing → pure ()
                        Just feed →
                            when (optShow feed) $
                                configTiming config >>= outputLogFor feed . Just
-}

{- |
Divides a list into chunks, and applies the strategy
@'evalList' s@ to each chunk in parallel.

It is expected that this function will be replaced by a more
generic clustering infrastructure in the future.

If the chunk size is 1 or less, 'parListChunk' is equivalent to
'parList'
-}
parListChunk' ∷ Int → Strategy a → Strategy (NonEmpty a)
parListChunk' n s xs = sconcat `fmap` parTraversable (evalTraversable s) (chunk' n xs)


chunk' ∷ Int → NonEmpty a → NonEmpty (NonEmpty a)
chunk' n (x :| xs) =
    let (as, bs) = splitAt (n - 1) xs
    in  (x :| as) :| case bs of
            [] → []
            y : ys → toList . chunk' n $ y :| ys

{-
chunkEvenlyBy ∷ Word → [a] → [[a]]
chunkEvenlyBy n xs =
    let num = fromEnum n
        len = length xs
        (q, r) = len `quotRem` num
        sizes = replicate r (q + 1) <> replicate (num - r) q
    in  chunkInto sizes xs

chunkInto ∷ [Int] → [a] → [[a]]
chunkInto (n : ns) jobs@(_ : _) = as : chunkInto ns bs where (as, bs) = splitAt n jobs
chunkInto _ _ = []

splitGenInto ∷ (RandomGenM (AtomicGenM StdGen) StdGen m) ⇒ Word → AtomicGenM StdGen → m [StdGen]
splitGenInto n = fmap force . replicateM (fromEnum n) . splitGenM
-}
