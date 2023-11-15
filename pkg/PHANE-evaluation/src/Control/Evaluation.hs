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
    --    getParallelChunkTraverse,
    getParallelChunkTraverse2,
    getParallelChunkTraverse3,

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
-- For the implementation of Conc below, we do not want any of the
-- smart async exception handling logic from UnliftIO.Exception, since
-- (eg) we're low-level enough to need to explicit be throwing async
-- exceptions synchronously.

import Control.Concurrent (threadDelay)
import Control.Concurrent qualified as C
import Control.Concurrent.Async
import Control.Concurrent.Async (Async, mapConcurrently)
import Control.Concurrent.Async qualified as A
import Control.Concurrent.MSem
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Evaluation.Result
import Control.Exception
import Control.Exception (Exception, SomeException)
import Control.Exception qualified as E
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Random.Strict
import Control.Monad.Reader (MonadReader (..), ReaderT (..), withReaderT)
import Control.Monad.Trans.Control (MonadBaseControl (..), defaultLiftBaseWith, defaultRestoreM, liftWith)
import Control.Monad.Zip (MonadZip (..))
import Control.Parallel.Strategies
import Data.Bimap qualified as BM
import Data.Bits (xor)
import Data.Foldable (fold, for_, toList, traverse_)
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (sconcat)
import Data.String
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Traversable
import Data.Traversable (for)
import GHC.Conc (getNumCapabilities, numCapabilities)
import GHC.Generics
import GHC.Stack (CallStack)
import GHC.Stack qualified as GHC
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream.Prelude (MonadAsync)
import Streamly.Data.Stream.Prelude qualified as Stream
import Streamly.Internal.Control.Concurrent qualified as Stream (RunInIO (..), askRunInIO)
import Streamly.Internal.Data.Stream.Concurrent qualified as Stream
import System.CPUTime (cpuTimePrecision, getCPUTime)
import System.ErrorPhase
import System.Exit
import System.Log.FastLogger hiding (check)
import System.Random.Stateful
import UnliftIO.Async (pooledMapConcurrently, pooledMapConcurrentlyN)


{- |
A computational "evaluation."

An evaluation has a /read-only/ global enviroment @env@, accessible to it's computation.

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


instance MonadBase IO (Evaluation env) where
    liftBase = liftIO


instance MonadBaseControl IO (Evaluation env) where
    type StM (Evaluation env) a = StM IO a


    -- type RunInBase m b = forall a. m a -> b (StM m a)
    -- liftBaseWith :: (RunInBase m b -> b a) -> m a
    -- liftBaseWith :: (RunInBase (Evaluation env) IO -> IO a) -> (Evaluation env) a
    -- liftBaseWith :: ((forall a. Evaluation env a -> IO (StM (Evaluation env) a)) -> IO a) -> Evaluation env a
    -- liftBaseWith :: ((forall a. Evaluation env a -> IO (StM IO a)) -> IO a) -> Evaluation env a
    -- liftBaseWith :: ((forall a. Evaluation env a -> IO a) -> IO a) -> Evaluation env a

    -- f :: ((forall a. Evaluation env a -> IO a) -> IO a)
    liftBaseWith f = Evaluation . ReaderT $ \env →
        fmap pure $ f (executeEvaluation env)


    {-
    --    liftBaseWith :: (RunInBase m b -> b a) -> m a
    --    liftBaseWith :: ((forall a. m a -> b (StM m a)) -> b a) -> m a
        liftBaseWith = \f -> -- ((forall a. m a -> b (StM m a)) -> b a)
            liftWith $ \run -> -- (forall a. m a -> b (StM m a)
                liftBaseWith $ \runInBase ->
                    f $ runInBase . fmap pure . run

        -- liftWith :: Monad m => (Run t -> m a) -> t m a
        -- liftWith :: Monad m => (forall n b. Monad n => t n b -> n (StT t b) -> m a) -> t m a
    -}
    --    restoreM :: StM (Evaluation env) a -> (Evaluation env) a
    --    restoreM :: StM IO a -> (Evaluation env) a
    restoreM ∷ a → (Evaluation env) a
    restoreM = Evaluation . ReaderT . const . pure . pure
    {-# INLINEABLE liftBaseWith #-}
    {-# INLINEABLE restoreM #-}


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


{-
    withRunInIO f = Evaluation . ReaderT $ \env →
        pure <$> f (executeEvaluation env)
-}

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
accross available threads. The number of threads available on they system is
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
                val@(x : xs) →
                    let !maxBuckets = fromIntegral n
                        len = length val
                        num = case len `quotRem` maxBuckets of
                            (q, 0) → q
                            (q, _) → q + 1
                        y : ys = withStrategy (parListChunk num rdeepseq) $ f <$> val
                    in  y : ys
    in  Evaluation $ reader (pure . construct . fromIntegral . implicitBucketNum)


{- | Divides a list into chunks, and applies the strategy
@'evalList' strat@ to each chunk in parallel.

It is expected that this function will be replaced by a more
generic clustering infrastructure in the future.

If the chunk size is 1 or less, 'parListChunk' is equivalent to
'parList'
-}
parListChunk' ∷ Int → Strategy a → Strategy (NonEmpty a)
parListChunk' n strat xs = sconcat `fmap` parTraversable (evalTraversable strat) (chunk' n xs)


chunk' ∷ Int → NonEmpty a → NonEmpty (NonEmpty a)
chunk' n (x :| xs) =
    let (as, bs) = splitAt (n - 1) xs
    in  (x :| as) :| case bs of
            [] → []
            y : ys → toList . chunk' n $ y :| ys


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
                    let openning = x <> renderedLoc
                        seperatorOf
                            | openning == "" = id
                            | otherwise = (<> " ")
                    in  seperatorOf openning

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


getParallelChunkTraverse3 ∷ ∀ a b env m. (MonadIO m, NFData b) ⇒ Evaluation env ((a → m b) → [a] → m [b])
getParallelChunkTraverse3 =
    let construct ∷ (Word, AtomicGenM StdGen) → (a → m b) → [a] → m [b]
        construct (maxBuckets, randomRef)
            | maxBuckets <= 1 = traverse
            | otherwise = \f → \case
                [] → pure []
                xs →
                    let jobCount ∷ Word
                        jobCount = force . toEnum $ length xs

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
                        parallelLess ∷ m [b]
                        parallelLess = do
                            jobs ← flip zip xs <$> splitGenInto jobCount randomRef
                            fmap force . join . liftIO $ sequenceA <$> mapConcurrently evalJob jobs

                        -- If the number of jobs exceed the maximum parallel threads,
                        -- we evenly distribute the jobs into "buckets" and then
                        -- give each thread a bucket of jobs to complete concurrently.
                        parallelMore ∷ m [b]
                        parallelMore = do
                            buckets ← allotBuckets xs
                            fmap (force . fold) . join . liftIO $ sequenceA <$> mapConcurrently evalBucket buckets
                    in  force
                            <$> if jobCount <= maxBuckets
                                then parallelLess
                                else parallelMore
    in  Evaluation $ reader (pure . construct . (fromIntegral . implicitBucketNum &&& implicitRandomGen))


{- |
/Note:/ Does not work on infinite lists!

Like 'getParallelChunkMap', but performs monadic actions over the list in parallel.
Each thread will have a different /uncorrolated/ random numer generator.
-}
getParallelChunkTraverse2 ∷ ∀ a b env. (NFData b) ⇒ Evaluation env ((a → Evaluation env b) → [a] → Evaluation env [b])
getParallelChunkTraverse2 = do
    compute ← askRunInIO
    Evaluation $ do
        buckets ← reader (fromIntegral . implicitBucketNum)
        liftIO $ print buckets
        pure . pure $ \f → liftIO . mapPool buckets (compute . f)


mapPool ∷ (Traversable t) ⇒ Int → (a → IO b) → t a → IO (t b)
mapPool max f xs = do
    sem ← new max
    mapConcurrently (with sem . f) xs

-- A little test:
-- main = mapPool 10 (\x -> threadDelay 1000000 >> print x) [1..100]

{-
{- |
/Note:/ Does not work on infinite lists!

Like 'getParallelChunkMap', but performs monadic actions over the list in parallel.
Each thread will have a different /uncorrolated/ random numer generator.
-}
getParallelChunkTraverse ∷ ∀ a b env. (NFData b) ⇒ Evaluation env ((a → Evaluation env b) → [a] → Evaluation env [b])
getParallelChunkTraverse =
    let allotBuckets ∷ Word → AtomicGenM StdGen → [a] → m [(StdGen, [a])]
        allotBuckets n gen ys = flip zip (chunkEvenlyBy n ys) <$> splitGenInto n gen

--        -- For MonadInterleave we use the type: (RandT StdGen IO a)
--        evalBucket ∷ (a → m b) -> (StdGen, [a]) → IO (m [b])
--        evalBucket f (gen, jobs) = flip evalRandT gen $ do
--            liftIO . pure $ force <$> traverse f jobs

        modifyStreamConfig ∷ Word → Stream.Config → Stream.Config
        modifyStreamConfig n = Stream.eager True . Stream.ordered True . Stream.maxThreads (fromEnum n)

{--}
        evalBucket ∷ (a → m b) → [a] → m [b]
        evalBucket f jobs =
            -- force <$> traverse f jobs
            traverse f jobs

        construct2 ∷ Word → (a → m b) → [a] → m [b]
        construct2 maxBuckets = \f →
            Stream.fold Fold.toList . Stream.parMapM (modifyStreamConfig maxBuckets) f . Stream.fromList

        -- !!! TODO: Add SCC annotations
{-
        construct ∷ Word → (a → m b) → [a] → m [b]
        construct maxBuckets = \f xs → do
            let buckets = chunkEvenlyBy maxBuckets xs
            --                buckets ← allotBuckets maxBuckets randomRef xs
            fold <$> pooledMapConcurrentlyN (fromEnum maxBuckets) (evalBucket f) buckets
-}
{--}
        construct ∷ (Word, AtomicGenM StdGen) → (a → Evaluation env b) → [a] → Evaluation env [b]
        construct (maxBuckets, randomRef) = case maxBuckets of
            0 → traverse
            1 → traverse
            n → flip $ \case
                        [] → const $ pure []
                        xs → case toEnum $ length xs of
                            -- For when the number of jobs do not exceed the maximum parallel threads
                            jobCount | jobCount <= maxBuckets ->
                                let evalJob ∷ (a → m b) → (StdGen, a) → IO (m b)
                                    evalJob f (gen, job) = flip evalRandT gen $ do
                                        liftIO . pure $ force <$> f job

                                    inParallel ∷ (MonadUnliftIO f) ⇒ (x → f y) → [x] → f [y]
                                    inParallel = pooledMapConcurrentlyN $ fromEnum jobCount

                                in  do  -- (jobs ∷ [(StdGen, a)]) ← flip zip xs <$>
                                        gens <- splitGenInto jobCount randomRef
                                        let jobs = gens `seq` flip zip xs gens
                                        \f -> fmap force . join . liftIO $ sequenceA <$> inParallel (evalJob f) jobs

                            -- If the number of jobs exceed the maximum parallel threads,
                            -- we evenly distribute the jobs into "buckets" and then
                            -- give each thread a bucket of jobs to complete concurrently.
                            jobCount -> \f ->
                                let
                                    -- For MonadInterleave we use the type: (RandT StdGen IO a)
                                    evalBucket ∷ (StdGen, [a]) → IO (m [b])
                                    evalBucket (gen, jobs) = flip evalRandT gen $ do
                                        liftIO . pure $ force <$> traverse f jobs

                                    inParallel ∷ (MonadUnliftIO f) ⇒ (x → f y) → [x] → f [y]
                                    inParallel = pooledMapConcurrentlyN $ fromEnum jobCount

                                in  do  buckets ← allotBuckets maxBuckets randomRef xs
                                        fmap (force . fold) . join . liftIO $ sequenceA <$> pooledMapConcurrently evalBucket buckets
            in  Evaluation $ reader (pure . construct . (fromIntegral . implicitBucketNum &&& implicitRandomGen))
{--}
{-
        do  compute <- askRunInIO
            Evaluation $ do
                buckets <- reader (fromIntegral . implicitBucketNum)
                liftIO $ print buckets
                let mod :: Stream.Config → Stream.Config
                    mod = Stream.eager True . Stream.ordered True . Stream.maxThreads (fromEnum buckets)
                pure . pure $
                    let g :: (a → Evaluation env b) → [a] → Evaluation env [b]
                        g f = liftIO . Stream.fold Fold.toList . Stream.parMapM mod (fmap force . compute . f) . Stream.fromList
                    in  g
-}
-}

{-
pooledMapConcurrentlyN'
    ∷ (MonadUnliftIO m, Traversable t)
    ⇒ Int
    -- ^ Max. number of threads. Should not be less than 1.
    → (a → m b)
    → t a
    → m (t b)
pooledMapConcurrentlyN' numProcs f xs =
    withRunInIO $ \run → pooledMapConcurrentlyIO'' numProcs (run . f) xs

pooledMapConcurrentlyIO'' ∷ (Traversable t) ⇒ Int → (a → IO b) → t a → IO (t b)
pooledMapConcurrentlyIO'' numProcs f xs =
    if (numProcs < 1)
        then error "pooledMapconcurrentlyIO: number of threads < 1"
        else pooledMapConcurrentlyIO''' numProcs f xs

pooledMapConcurrentlyIO'''
    ∷ (Traversable t)
    ⇒ Int
    -- ^ Max. number of threads. Should not be less than 1.
    → (a → IO b)
    → t a
    → IO (t b)
pooledMapConcurrentlyIO''' numProcs f xs = do
    -- prepare one IORef per result...
    jobs ∷ t (a, IORef b) ←
        for xs (\x → (x,) <$> newIORef (error "pooledMapConcurrentlyIO''': empty IORef"))
    -- ...put all the inputs in a queue..
    jobsVar ∷ IORef [(a, IORef b)] ← newIORef (toList jobs)
    -- ...run `numProcs` threads in parallel, each
    -- of them consuming the queue and filling in
    -- the respective IORefs.
    pooledConcurrently numProcs jobsVar $ \(x, outRef) → f x >>= atomicWriteIORef outRef -- Read all the IORefs
    for jobs (\(_, outputRef) → readIORef outputRef)

{- | Performs the actual pooling for the tasks. This function will
continue execution until the task queue becomes empty. When one of
the pooled thread finishes it's task, it will pickup the next task
from the queue if an job is available.
-}
pooledConcurrently
    ∷ Int
    -- ^ Max. number of threads. Should not be less than 1.
    → IORef [a]
    -- ^ Task queue. These are required as inputs for the jobs.
    → (a → IO ())
    -- ^ The task which will be run concurrently (but
    -- will be pooled properly).
    → IO ()
pooledConcurrently numProcs jobsVar f = do
    replicateConcurrently_ numProcs $ do
        let loop = do
                mbJob ∷ Maybe a ← atomicModifyIORef' jobsVar $ \x → case x of
                    [] → ([], Nothing)
                    var : vars → (vars, Just var)
                case mbJob of
                    Nothing → return ()
                    Just x → do
                        f x
                        loop
         in loop

{-# INLINE replicateConcurrently_ #-}
replicateConcurrently_ cnt m =
    case compare cnt 1 of
        LT → pure ()
        EQ → void m
        GT → mapConcurrently_ id (replicate cnt m)

{- | Executes a 'Traversable' container of items concurrently, it uses the 'Flat'
type internally. This function ignores the results.

@since 0.1.0.0
-}
mapConcurrently_ ∷ (MonadUnliftIO m) ⇒ (Foldable f) ⇒ (a → m b) → f a → m ()
mapConcurrently_ f t = withRunInIO $ \run →
    runFlat $
        traverse_
            (FlatApp . FlatAction . run . f)
            t
{-# INLINE mapConcurrently_ #-}

-------------------------
-- Conc implementation --
-------------------------

-- Data types for flattening out the original @Conc@ into a simplified
-- view. Goals:
--

-- * We want to get rid of the Empty data constructor. We don't want

--   it anyway, it's only there because of the Alternative typeclass.
--

-- * We want to ensure that there is no nesting of Alt data

--   constructors. There is a bookkeeping overhead to each time we
--   need to track raced threads, and we want to minimize that
--   bookkeeping.
--

-- * We want to ensure that, when racing, we're always racing at least

--   two threads.
--

-- * We want to simplify down to IO.

-- | Flattened structure, either Applicative or Alternative
data Flat a
    = FlatApp !(FlatApp a)
    | -- | Flattened Alternative. Has at least 2 entries, which must be
      -- FlatApp (no nesting of FlatAlts).
      FlatAlt !(FlatApp a) !(FlatApp a) ![FlatApp a]

deriving instance Functor Flat

instance Applicative Flat where
    pure = FlatApp . pure
    (<*>) f a = FlatApp (FlatLiftA2 id f a)
    liftA2 f a b = FlatApp (FlatLiftA2 f a b)

{- | Flattened Applicative. No Alternative stuff directly in here, but may be in
the children. Notice this type doesn't have a type parameter for monadic
contexts, it hardwires the base monad to IO given concurrency relies
eventually on that.

@since 0.2.9.0
-}
data FlatApp a where
    FlatPure ∷ a → FlatApp a
    FlatAction ∷ IO a → FlatApp a
    FlatApply ∷ Flat (v → a) → Flat v → FlatApp a
    FlatLiftA2 ∷ (x → y → a) → Flat x → Flat y → FlatApp a

deriving instance Functor FlatApp

instance Applicative FlatApp where
    pure = FlatPure
    (<*>) mf ma = FlatApply (FlatApp mf) (FlatApp ma)
    liftA2 f a b = FlatLiftA2 f (FlatApp a) (FlatApp b)

-- | Simple difference list, for nicer types below
type DList a = [a] → [a]

dlistConcat ∷ DList a → DList a → DList a
dlistConcat = (.)
{-# INLINE dlistConcat #-}

dlistCons ∷ a → DList a → DList a
dlistCons a as = dlistSingleton a `dlistConcat` as
{-# INLINE dlistCons #-}

dlistConcatAll ∷ [DList a] → DList a
dlistConcatAll = foldr (.) id
{-# INLINE dlistConcatAll #-}

dlistToList ∷ DList a → [a]
dlistToList = ($ [])
{-# INLINE dlistToList #-}

dlistSingleton ∷ a → DList a
dlistSingleton a = (a :)
{-# INLINE dlistSingleton #-}

dlistEmpty ∷ DList a
dlistEmpty = id
{-# INLINE dlistEmpty #-}

-- | Run a @Flat a@ on multiple threads.
runFlat ∷ Flat a → IO a
-- Silly, simple optimizations
runFlat (FlatApp (FlatAction io)) = io
runFlat (FlatApp (FlatPure x)) = pure x
-- Start off with all exceptions masked so we can install proper cleanup.
runFlat f0 = E.uninterruptibleMask $ \restore → do
    -- How many threads have been spawned and finished their task? We need to
    -- ensure we kill all child threads and wait for them to die.
    resultCountVar ← newTVarIO 0

    -- Forks off as many threads as necessary to run the given Flat a,
    -- and returns:
    --
    -- + An STM action that will block until completion and return the
    --   result.
    --
    -- + The IDs of all forked threads. These need to be tracked so they
    --   can be killed (either when an exception is thrown, or when one
    --   of the alternatives completes first).
    --
    -- It would be nice to have the returned STM action return an Either
    -- and keep the SomeException values somewhat explicit, but in all
    -- my testing this absolutely kills performance. Instead, we're
    -- going to use a hack of providing a TMVar to fill up with a
    -- SomeException when things fail.
    --
    -- TODO: Investigate why performance degradation on Either
    let go
            ∷ ∀ a
             . TMVar E.SomeException
            → Flat a
            → IO (STM a, DList C.ThreadId)
        go _excVar (FlatApp (FlatPure x)) = pure (pure x, dlistEmpty)
        go excVar (FlatApp (FlatAction io)) = do
            resVar ← newEmptyTMVarIO
            tid ← C.forkIOWithUnmask $ \restore1 → do
                res ← E.try $ restore1 io
                atomically $ do
                    modifyTVar' resultCountVar (+ 1)
                    case res of
                        Left e → void $ tryPutTMVar excVar e
                        Right x → putTMVar resVar x
            pure (readTMVar resVar, dlistSingleton tid)
        go excVar (FlatApp (FlatApply cf ca)) = do
            (f, tidsf) ← go excVar cf
            (a, tidsa) ← go excVar ca
            pure (f <*> a, tidsf `dlistConcat` tidsa)
        go excVar (FlatApp (FlatLiftA2 f a b)) = do
            (a', tidsa) ← go excVar a
            (b', tidsb) ← go excVar b
            pure (liftA2 f a' b', tidsa `dlistConcat` tidsb)
        go excVar0 (FlatAlt x y z) = do
            -- As soon as one of the children finishes, we need to kill the siblings,
            -- we're going to create our own excVar here to pass to the children, so
            -- we can prevent the ThreadKilled exceptions we throw to the children
            -- here from propagating and taking down the whole system.
            excVar ← newEmptyTMVarIO
            resVar ← newEmptyTMVarIO
            pairs ← traverse (go excVar . FlatApp) (x : y : z)
            let (blockers, workerTids) = unzip pairs

            -- Fork a helper thread to wait for the first child to
            -- complete, or for one of them to die with an exception so we
            -- can propagate it to excVar0.
            helperTid ← C.forkIOWithUnmask $ \restore1 → do
                eres ←
                    E.try $
                        restore1 $
                            atomically $
                                foldr
                                    (\blocker rest → (Right <$> blocker) <|> rest)
                                    (Left <$> readTMVar excVar)
                                    blockers
                atomically $ do
                    modifyTVar' resultCountVar (+ 1)
                    case eres of
                        -- NOTE: The child threads are spawned from @traverse go@ call above, they
                        -- are _not_ children of this helper thread, and helper thread doesn't throw
                        -- synchronous exceptions, so, any exception that the try above would catch
                        -- must be an async exception.
                        -- We were killed by an async exception, do nothing.
                        Left (_ ∷ E.SomeException) → pure ()
                        -- Child thread died, propagate it
                        Right (Left e) → void $ tryPutTMVar excVar0 e
                        -- Successful result from one of the children
                        Right (Right res) → putTMVar resVar res

                -- And kill all of the threads
                for_ workerTids $ \tids' →
                    -- NOTE: Replacing A.AsyncCancelled with KillThread as the
                    -- 'A.AsyncCancelled' constructor is not exported in older versions
                    -- of the async package
                    -- for_ (tids' []) $ \workerTid -> E.throwTo workerTid A.AsyncCancelled
                    for_ (dlistToList tids') $ \workerTid → C.killThread workerTid

            pure
                ( readTMVar resVar
                , helperTid `dlistCons` dlistConcatAll workerTids
                )

    excVar ← newEmptyTMVarIO
    (getRes, tids0) ← go excVar f0
    let tids = dlistToList tids0
        tidCount = length tids
        allDone count =
            if count > tidCount
                then
                    error
                        ( "allDone: count ("
                            <> show count
                            <> ") should never be greater than tidCount ("
                            <> show tidCount
                            <> ")"
                        )
                else count == tidCount

    -- Automatically retry if we get killed by a
    -- BlockedIndefinitelyOnSTM. For more information, see:
    --
    -- + https:\/\/github.com\/simonmar\/async\/issues\/14
    -- + https:\/\/github.com\/simonmar\/async\/pull\/15
    --
    let autoRetry action =
            action
                `E.catch` \E.BlockedIndefinitelyOnSTM → autoRetry action

    -- Restore the original masking state while blocking and catch
    -- exceptions to allow the parent thread to be killed early.
    res ←
        E.try $
            restore $
                autoRetry $
                    atomically $
                        (Left <$> readTMVar excVar)
                            <|> (Right <$> getRes)

    count0 ← atomically $ readTVar resultCountVar
    unless (allDone count0) $ do
        -- Kill all of the threads
        -- NOTE: Replacing A.AsyncCancelled with KillThread as the
        -- 'A.AsyncCancelled' constructor is not exported in older versions
        -- of the async package
        -- for_ tids $ \tid -> E.throwTo tid A.AsyncCancelled
        for_ tids $ \tid → C.killThread tid

        -- Wait for all of the threads to die. We're going to restore the original
        -- masking state here, just in case there's a bug in the cleanup code of a
        -- child thread, so that we can be killed by an async exception. We decided
        -- this is a better behavior than hanging indefinitely and wait for a SIGKILL.
        restore $ atomically $ do
            count ← readTVar resultCountVar
            -- retries until resultCountVar has increased to the threadId count returned by go
            check $ allDone count

    -- Return the result or throw an exception. Yes, we could use
    -- either or join, but explicit pattern matching is nicer here.
    case res of
        -- Parent thread was killed with an async exception
        Left e → E.throwIO (e ∷ E.SomeException)
        -- Some child thread died
        Right (Left e) → E.throwIO e
        -- Everything worked!
        Right (Right x) → pure x
{-# INLINEABLE runFlat #-}
-}
