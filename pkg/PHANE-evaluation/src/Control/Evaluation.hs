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
    LogConfig (),
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
import Control.Arrow ((&&&))
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Evaluation.Result
import Control.Exception
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Random.Strict
import Control.Monad.Reader (MonadReader (..), ReaderT (..), withReaderT)
import Control.Monad.Zip (MonadZip (..))
import Control.Parallel.Strategies
import Data.Bimap qualified as BM
import Data.Bits (xor)
import Data.Foldable (fold, toList, traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (sconcat)
import Data.String
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Conc (getNumCapabilities)
import GHC.Generics
import GHC.Stack (CallStack)
import GHC.Stack qualified as GHC
import System.CPUTime (cpuTimePrecision, getCPUTime)
import System.ErrorPhase
import System.Exit
import System.Log.FastLogger hiding (check)
import System.Random.Stateful
import Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..), coarbitraryEnum)
-- import Test.QuickCheck.Function (Fun, Function (..), applyFun)
import Test.QuickCheck.Gen (Gen (..), variant)


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


{-
instance Show (Evaluation env a) where

    show (Evaluation (ReaderT f)) =
-}

type role Evaluation representational nominal


data ImplicitEnvironment env = ImplicitEnvironment
    { implicitBucketNum ∷ {-# UNPACK #-} ParallelBucketCount
    , implicitLogConfig ∷ {-# UNPACK #-} LogConfig
    , implicitRandomGen ∷ {-# UNPACK #-} (AtomicGenM StdGen)
    , explicitReader ∷ env
    }


type role ImplicitEnvironment representational


data LoggerFeed = LoggerFeed
    { feedLevel ∷ Verbosity
    , feedLogger ∷ LoggerSet
    }


newtype ParallelBucketCount = MaxPar Word
    deriving newtype (Eq, Enum, Integral, Num, Ord, Real, Show)


{- |
Configuration specifying how log messages are to be handled.
-}
data LogConfig = LogConfig
    { configSTDERR ∷ {-# UNPACK #-} LoggerFeed
    , configSTDOUT ∷ {-# UNPACK #-} LoggerFeed
    , configStream ∷ {-# UNPACK #-} Maybe LoggerFeed
    , configTiming ∷ IO FormattedTime
    }


{- |
A seed from which a /(practically infinite)/ stream of pseudorandomness can be generated.
-}
newtype RandomSeed = RandomSeed Int
    deriving newtype (Eq, Enum, Integral, Num, Ord, Real, Show)


instance Applicative (Evaluation env) where
    {-# INLINEABLE (<*>) #-}
    {-# INLINE (*>) #-}
    {-# INLINE pure #-}
    pure = Evaluation . pure . pure


    (<*>) = apply


    (*>) = propagate


instance Alternative (Evaluation env) where
    {-# INLINEABLE (<|>) #-}
    (<|>) x y = Evaluation . ReaderT $ \store → do
        res ← runReaderT (unwrapEvaluation x) store
        case runEvaluationResult res of
            Left _ → runReaderT (unwrapEvaluation y) store
            Right _ → pure res


    empty = fail "Alternative identity"


instance (Arbitrary a, CoArbitrary env) ⇒ Arbitrary (Evaluation env a) where
    arbitrary = do
        fun ← (arbitrary ∷ Gen (ImplicitEnvironment env → EvaluationResult a))
        pure . Evaluation . ReaderT $ pure . fun


instance (CoArbitrary env) ⇒ CoArbitrary (ImplicitEnvironment env) where
    coarbitrary store =
        let x = implicitBucketNum store
            y = implicitLogConfig store
            z = explicitReader store
        in  coarbitrary z . coarbitrary y . variant x


instance CoArbitrary LogConfig where
    coarbitrary config =
        let x = configSTDERR config
            y = configSTDOUT config
            z = configStream config
        in  coarbitrary z . coarbitrary y . coarbitrary x


instance CoArbitrary LoggerFeed where
    coarbitrary (LoggerFeed v _) = coarbitraryEnum v


deriving stock instance Functor ImplicitEnvironment


instance Functor (Evaluation env) where
    {-# INLINEABLE fmap #-}
    fmap f x = Evaluation . fmap (fmap f) $ unwrapEvaluation x


instance Logger (Evaluation env) where
    {-# INLINEABLE logWith #-}
    logWith level str = Evaluation $ do
        impEnv ← ask
        let logConfig = implicitLogConfig impEnv
        liftIO . fmap pure . doLogCs logConfig level ?callStack $ toLogStr str


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
        pure . pure $ explicitReader store


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
    x <> y =
        let xReader = unwrapEvaluation x
            yReader = unwrapEvaluation y
        in  Evaluation $ liftA2 (<>) xReader yReader


{- |
Run the 'Evaluation' computation.

Initial randomness seed and configuration for logging outputs required to initiate the computation.
-}
runEvaluation ∷ (MonadIO m) ⇒ LogConfig → RandomSeed → env → Evaluation env a → m a
runEvaluation logConfig randomSeed environ eval = do
    randomRef ← newAtomicGenM . mkStdGen $ fromEnum randomSeed
    maxBuckets ← liftIO $ toEnum . max 1 . pred <$> getNumCapabilities
    let implicit =
            ImplicitEnvironment
                { implicitBucketNum = maxBuckets
                , implicitLogConfig = logConfig
                , implicitRandomGen = randomRef
                , explicitReader = environ
                }

    executeEvaluation implicit eval


{- |
Create configuration for logging output stream to initialize an 'Evaluation'.
-}
initializeLogging
    ∷ Verbosity
    -- ^ Verbosity level for STDOUT
    → Verbosity
    -- ^ Verbosity level for STDERR
    → Maybe (Verbosity, FilePath)
    -- ^ optional verbosity level for a file stream
    → IO LogConfig
initializeLogging vOut vErr vFile =
    let builderFilePath ∷ Maybe (Verbosity, FilePath) → IO (Maybe LoggerFeed)
        builderFilePath fileMay =
            let mkFeed ∷ Maybe (Verbosity, LoggerSet) → Maybe LoggerFeed
                mkFeed = fmap (uncurry LoggerFeed)

                create = traverse (traverse initLoggerSetStream)

                blankFile ∷ (Verbosity, FilePath) → Maybe (Verbosity, FilePath)
                blankFile = \case
                    (_, []) → Nothing
                    x → Just x
            in  fmap mkFeed . create $ fileMay >>= blankFile

        builderStandard ∷ Verbosity → IO LoggerSet → IO LoggerFeed
        builderStandard level = fmap (LoggerFeed level)

        timeFormat ∷ TimeFormat
        timeFormat = "%Y-%m-%d %T %z"
    in  LogConfig
            <$> builderStandard vErr initLoggerSetSTDERR
            <*> builderStandard vOut initLoggerSetSTDOUT
            <*> builderFilePath vFile
            <*> newTimeCache timeFormat


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
            ∷ ((Maybe LoggerFeed → Maybe LoggerFeed) → LogConfig → LogConfig)
            → Verbosity
            → Evaluation env a
            → Evaluation env a
        setVerbosityOf' f v =
            let transformation = modImplicitLogConfig (f (fmap (setFeedLevel v)))
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
getParallelChunkTraverse ∷ ∀ a b env m. (MonadIO m, NFData b) ⇒ Evaluation env ((a → m b) → [a] → m [b])
getParallelChunkTraverse =
    let construct ∷ AtomicGenM StdGen → Word → (a → m b) → [a] → m [b]
        construct randomRef = \case
            n | n <= 1 → traverse
            maxBuckets → flip $ \case
                [] → const $ pure []
                xs → \f →
                    let jobCount ∷ Word
                        jobCount = toEnum . force $ length xs

                        allotBuckets ∷ [a] → m [(StdGen, [a])]
                        allotBuckets ys =
                            flip zip (chunkEvenlyBy maxBuckets ys) <$> splitGenInto maxBuckets randomRef

                        -- For MonadInterleave we use the type: (RandT StdGen IO a)
                        evalBucket ∷ (StdGen, [a]) → IO (m [b])
                        evalBucket (gen, jobs) = flip evalRandT gen $ do
                            liftIO . pure $ force <$> traverse f jobs

                        evalJob ∷ (StdGen, a) → IO (m b)
                        evalJob (gen, job) = flip evalRandT gen $ do
                            liftIO . pure $ force <$> f job

                        -- For when the number of jobs do not exceed the maximum parallel threads
                        parallelLess' ∷ m [b]
                        parallelLess' = do
                            (jobs ∷ [(StdGen, a)]) ← flip zip xs <$> splitGenInto jobCount randomRef
                            fmap force . join . liftIO $ sequenceA <$> mapConcurrently evalJob jobs

                        -- If the number of jobs exceed the maximum parallel threads,
                        -- we evenly distribute the jobs into "buckets" and then
                        -- give each thread a bucket of jobs to complete concurrently.
                        parallelMore' ∷ m [b]
                        parallelMore' = do
                            buckets ← allotBuckets xs
                            fmap (force . fold) . join . liftIO $ sequenceA <$> mapConcurrently evalBucket buckets
                    in  force
                            <$> if jobCount <= maxBuckets
                                then parallelLess'
                                else parallelMore'
    in  Evaluation $ reader (pure . uncurry construct . (implicitRandomGen &&& fromIntegral . implicitBucketNum))


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
failWithPhase ∷ (ToLogStr s) ⇒ ErrorPhase → s → Evaluation env a
failWithPhase p message = do
    logWith LogFail message
    Evaluation . ReaderT . const . pure $ evalUnitWithPhase p message


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


executeEvaluation ∷ (MonadIO m) ⇒ ImplicitEnvironment env → Evaluation env a → m a
executeEvaluation implicitEnv (Evaluation (ReaderT f)) =
    let logConfig = implicitLogConfig implicitEnv

        flushLogBuffers ∷ IO ()
        flushLogBuffers =
            let flushBufferOf ∷ (LogConfig → LoggerFeed) → IO ()
                flushBufferOf g = flushLogStr . feedLogger $ g logConfig
            in  do
                    flushBufferOf configSTDERR
                    flushBufferOf configSTDOUT
                    traverse_ (flushLogStr . feedLogger) $ configStream logConfig
    in  liftIO . flip finally flushLogBuffers $ do
            res ← f implicitEnv
            case runEvaluationResult res of
                Right value → pure value
                Left (phase, txt) →
                    let exitCode = errorPhaseToExitCode BM.! phase
                    in  doLogCs logConfig LogFail ?callStack txt *> exitWith exitCode


setVerbosityOf
    ∷ ((LoggerFeed → LoggerFeed) → LogConfig → LogConfig)
    → Verbosity
    → Evaluation env a
    → Evaluation env a
setVerbosityOf f v =
    let transformation = modImplicitLogConfig (f (setFeedLevel v))
    in  Evaluation . withReaderT transformation . unwrapEvaluation


modConfigSTDERR ∷ (LoggerFeed → LoggerFeed) → LogConfig → LogConfig
modConfigSTDERR f x = x{configSTDERR = f $ configSTDERR x}


modConfigSTDOUT ∷ (LoggerFeed → LoggerFeed) → LogConfig → LogConfig
modConfigSTDOUT f x = x{configSTDOUT = f $ configSTDOUT x}


modConfigStream ∷ (Maybe LoggerFeed → Maybe LoggerFeed) → LogConfig → LogConfig
modConfigStream f x = x{configStream = f $ configStream x}


setFeedLevel ∷ Verbosity → LoggerFeed → LoggerFeed
setFeedLevel v x = x{feedLevel = v}


modImplicitLogConfig
    ∷ (LogConfig → LogConfig)
    → ImplicitEnvironment env
    → ImplicitEnvironment env
modImplicitLogConfig f x = x{implicitLogConfig = f $ implicitLogConfig x}


bufferSize ∷ BufSize
bufferSize = 4096


initLoggerSetSTDERR ∷ IO LoggerSet
initLoggerSetSTDERR = newStderrLoggerSet bufferSize


initLoggerSetSTDOUT ∷ IO LoggerSet
initLoggerSetSTDOUT = newStdoutLoggerSet bufferSize


initLoggerSetStream ∷ FilePath → IO LoggerSet
initLoggerSetStream = newFileLoggerSet bufferSize


withRandomGenerator ∷ StdGen → Evaluation env a → Evaluation env a
withRandomGenerator gen eval =
    let transformation ∷ AtomicGenM StdGen → ImplicitEnvironment env → ImplicitEnvironment env
        transformation val store = store{implicitRandomGen = val}
    in  liftIO (newAtomicGenM gen) >>= \ref →
            Evaluation . local (transformation ref) $ unwrapEvaluation eval


bind ∷ Evaluation env a → (a → Evaluation env b) → Evaluation env b
bind x f = Evaluation $ do
    y ← unwrapEvaluation x
    case runEvaluationResult y of
        Left s → pure . EU $ Left s
        Right v → unwrapEvaluation (f v)


apply ∷ Evaluation env (t → a) → Evaluation env t → Evaluation env a
apply lhs rhs = Evaluation $ do
    x ← unwrapEvaluation lhs
    case runEvaluationResult x of
        Left s → pure . EU $ Left s
        Right f → do
            y ← unwrapEvaluation rhs
            pure . EU $ case runEvaluationResult y of
                Left s → Left s
                Right v → Right $ f v


propagate ∷ Evaluation env a → Evaluation env b → Evaluation env b
propagate lhs rhs = Evaluation $ do
    x ← unwrapEvaluation lhs
    case runEvaluationResult x of
        Left s → pure . EU $ Left s
        Right _ → unwrapEvaluation rhs


doLogCs ∷ (MonadIO m) ⇒ LogConfig → LogLevel → CallStack → LogStr → m ()
doLogCs config level cs txt =
    let loc = case GHC.getCallStack cs of
            ((_, l) : _) → GHC.srcLocFile l <> ":" <> show (GHC.srcLocStartLine l)
            _ → "unknown"
    in  doLog config level (toLogStr loc) txt


doLog ∷ (MonadIO m) ⇒ LogConfig → LogLevel → LogStr → LogStr → m ()
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

        outputLogFor ∷ LoggerFeed → Maybe FormattedTime → IO ()
        outputLogFor feed timeStamp =
            let prefix ∷ LogStr → LogStr
                prefix x =
                    let opening = x <> renderedLoc
                        seperatorOf
                            | opening == "" = id
                            | otherwise = (<> " ")
                    in  seperatorOf opening

                logger = feedLogger feed
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
