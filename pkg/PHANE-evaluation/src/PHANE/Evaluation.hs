{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

{- |
The 'PHANE.Evaluation.Evaluation' type's monad transformer definition and types.
-}
module PHANE.Evaluation (
    Evaluation (),

    -- * Run computation
    runEvaluation,

    -- * Specialized functions
    failWithPhase,
    mapEvaluation,

    -- * Operations

    -- ** Environment
    alterEnvironment,

    -- ** Logging
    LogConfiguration (),
    initializeLogging,
    setVerbositySTDERR,
    setVerbositySTDOUT,
    setVerbosityFileLog,

    -- ** Parallelism

    -- *** With Laziness
    getParallelChunkMapBy,
    getParallelChunkTraverseBy,

    -- *** Full Strictness
    getParallelChunkMap,
    getParallelChunkTraverse,

    -- ** Randomness
    RandomSeed (),
    initializeRandomSeed,
    setRandomSeed,
    shuffleList,
) where

import Control.Applicative (Alternative (..))
-- import Control.Concurrent.Async (mapConcurrently)
import Control.DeepSeq
import Control.Exception
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Random.Strict
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Zip (MonadZip (..))
import Control.Parallel.Strategies
import Data.Bimap qualified as BM
import Data.Bits (xor)
import Data.Foldable (toList)
import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (sconcat)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Conc (getNumCapabilities)
import GHC.Exts (IsList (Item))
import GHC.Exts qualified as List
import GHC.Generics
import PHANE.Evaluation.ErrorPhase
import PHANE.Evaluation.Logging
import PHANE.Evaluation.Logging.Configuration
import PHANE.Evaluation.Result
import PHANE.Evaluation.Verbosity
import QuickCheck.GenT (GenT, arbitrary', runGenT)
import System.CPUTime (cpuTimePrecision, getCPUTime)
import System.Exit
import System.Random.Shuffle (shuffleM)
import System.Random.Stateful
import Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..))
import Test.QuickCheck.Gen (variant)
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
    -- ^ Run the 'PHANE.Evaluation.Evaluation' monad transformer
    }


type role Evaluation representational nominal


data ImplicitEnvironment env = ImplicitEnvironment
    { implicitBucketNum ∷ {-# UNPACK #-} !ParallelBucketCount
    , implicitLogConfig ∷ {-# UNPACK #-} !(IORef LogConfiguration)
    , implicitRandomGen ∷ {-# UNPACK #-} !(IOGenM StdGen)
    , explicitVariables ∷ {-# UNPACK #-} !(IORef env)
    }


type role ImplicitEnvironment representational


newtype ParallelBucketCount = MaxPar Word


{- |
A seed from which a /(practically infinite)/ stream of pseudorandomness can be generated.
-}
newtype RandomSeed = RandomSeed Int


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


instance (Arbitrary a, Arbitrary env, CoArbitrary env) ⇒ Arbitrary (Evaluation env a) where
    arbitrary = do
        getEnv ←
            runGenT $
                ImplicitEnvironment
                    <$> (MaxPar <$> arbitrary')
                    <*> (arbitrary' >>= liftIO . newIORef)
                    <*> (arbitrary' >>= liftIO . newIOGenM . mkStdGen)
                    <*> (arbitrary' >>= liftIO . newIORef)
        getRun ← runGenT (arbitrary' ∷ GenT IO (ImplicitEnvironment env → EvaluationResult a))

        pure . Evaluation . ReaderT $ \_ → do
            env ← getEnv
            run ← getRun
            pure $ run env


instance CoArbitrary (ImplicitEnvironment env) where
    coarbitrary = variant . implicitBucketNum


deriving newtype instance Eq ParallelBucketCount


deriving newtype instance Eq RandomSeed


deriving newtype instance Enum ParallelBucketCount


deriving newtype instance Enum RandomSeed


instance Functor (Evaluation env) where
    {-# INLINEABLE fmap #-}
    fmap f x = Evaluation . fmap (fmap f) $ unwrapEvaluation x


deriving stock instance Generic (Evaluation env a)


instance Logger (Evaluation env) where
    {-# INLINEABLE logWith #-}
    logWith level str = Evaluation $ do
        logRef ← reader implicitLogConfig
        liftIO . fmap pure . processMessage logRef level $ logToken str


deriving newtype instance Integral ParallelBucketCount


deriving newtype instance Integral RandomSeed


instance (NFData a) ⇒ NFData (Evaluation env a) where
    {-# INLINE rnf #-}
    rnf (Evaluation x) = (rnf <$> x) `seq` ()


deriving newtype instance Num ParallelBucketCount


deriving newtype instance Num RandomSeed


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
        gen' ← applyIOGen split gen >>= newIOGenM
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
    ask =
        Evaluation . ReaderT $
            fmap pure . readIORef . explicitVariables


    local f (Evaluation (ReaderT inner)) = Evaluation . ReaderT $ \store →
        adjustVariables f store *> inner store


instance MonadThrow (Evaluation env) where
    throwM e = Evaluation $ throwM e


instance MonadUnliftIO (Evaluation env) where
    {-# INLINE withRunInIO #-}
    -- f :: (forall a. Evaluation env a -> IO a) -> IO b
    withRunInIO f =
        {-# SCC "withRunInIO_Evaluation" #-}
        Evaluation . ReaderT $ \env →
            {-# SCC "withRunInIO_ReaderT_f" #-}
            withRunInIO $
                {-# SCC "withRunInIO_WITH_run" #-}
                \run →
                    pure <$> f (run . executeEvaluation env)


instance MonadZip (Evaluation env) where
    {-# INLINEABLE mzip #-}
    {-# INLINEABLE munzip #-}
    {-# INLINE mzipWith #-}


    mzip = liftA2 (,)


    mzipWith = liftA2


    munzip !x = (fst <$> x, snd <$> x)


deriving newtype instance Ord ParallelBucketCount


deriving newtype instance Ord RandomSeed


instance PrimMonad (Evaluation env) where
    type PrimState (Evaluation env) = PrimState IO
    primitive = Evaluation . ReaderT . const . fmap pure . primitive


deriving newtype instance Real ParallelBucketCount


deriving newtype instance Real RandomSeed


instance Semigroup (Evaluation env a) where
    {-# INLINE (<>) #-}
    lhs <> rhs = Evaluation . ReaderT $ \store → do
        x ← runReaderT (unwrapEvaluation lhs) store
        case runEvaluationResult x of
            Left s → pure . EU $ Left s
            _ → runReaderT (unwrapEvaluation rhs) store


deriving newtype instance Show ParallelBucketCount


deriving newtype instance Show RandomSeed


{- |
__Time:__ \( \mathcal{O}\left( \mathord{?} \right) \)

Run the 'PHANE.Evaluation.Evaluation' computation.

Initial randomness seed and configuration for logging outputs required to initiate the computation.
-}
runEvaluation ∷ (MonadIO m) ⇒ LogConfiguration → RandomSeed → env → Evaluation env a → m a
runEvaluation logConfig randomSeed environ eval = do
    maxChunks ← liftIO $ toEnum . max 1 . pred <$> getNumCapabilities
    loggerRef ← liftIO $ newIORef logConfig
    randomRef ← newIOGenM . mkStdGen $ fromEnum randomSeed
    valuesRef ← liftIO $ newIORef environ
    let implicit =
            ImplicitEnvironment
                { implicitBucketNum = maxChunks
                , implicitLogConfig = loggerRef
                , implicitRandomGen = randomRef
                , explicitVariables = valuesRef
                }
    executeEvaluation implicit eval


{- |
__Time:__ \( \mathcal{O}\left( 1 \right) \)

Change the polymorphic environment of the 'PHANE.Evaluation.Evaluation' /in place/ as a "mutable" global update.

If this is undeisreable, consider 'Control.Monad.Reader.Class.local' for a localized change to the environemnt.
-}
alterEnvironment ∷ (env → env) → Evaluation env ()
alterEnvironment f = Evaluation . ReaderT $ \store →
    let envRef = explicitVariables store
        make x = (f x, pure ())
    in  atomicModifyIORef' envRef make


{- |
__Time:__ \( \mathcal{O}\left( 1 \right) \)

Set the verbosity level of logs streamed to @STDERR@.
-}
setVerbositySTDERR ∷ Verbosity → Evaluation env ()
setVerbositySTDERR = setVerbosityOf modConfigSTDERR


{- |
__Time:__ \( \mathcal{O}\left( 1 \right) \)

Set the verbosity level of logs streamed to @STDOUT@.
-}
setVerbositySTDOUT ∷ Verbosity → Evaluation env ()
setVerbositySTDOUT = setVerbosityOf modConfigSTDOUT


{- |
__Time:__ \( \mathcal{O}\left( 1 \right) \)

Set the verbosity level of log data streamed to the log file (if any) for the sub-'PHANE.Evaluation.Evaluation'.
-}
setVerbosityFileLog ∷ Verbosity → Evaluation env ()
setVerbosityFileLog = setVerbosityOf modConfigStream


{- |
__Time:__ \( \mathcal{O}\left( 1 \right) \)

/Note:/ Does not work on infinite lists!

Get a parallel mapping function which produces results evaluated to /normal form/.

This is a specialization of 'getParallelChunkMapBy'.
The following equivalence holds:

> getParallelChunkMap ≡ getParallelChunkMapBy id

See the documentation of 'getParallelChunkMapBy' for more details on operational semantics.
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
__Time:__ \( \mathcal{O}\left( 1 \right) \)

/Note:/ Does not work on infinite lists!

Get a parallel mapping function, with customizable laziness.

Laziness is customizable via the "selector" argument @(b -> c)@.
The parallel map evaluates each @b@ value to /weak head normal form/.
Additionally, by applying the "selector" @(b -> c)@, the sub-structure @c@ will
be fully evaluated to /normal form/, along with "the spine" connecting @b@ to @c@.

The parallel map will evenly distributes elements of the list across available
threads. The number of threads available on they system is queried and memoized
at the start of the 'PHANE.Evaluation.Evaluation'. The length of the supplied list is calculated,
and the list is split into sub-lists of equal length (± 1).

__Examples__

This example shows the semantic of the selected laziness.
The "selector" 'snd' specifies the second argument of the tuple to be fully evaluated.
The first argument of the tuple is not evaluated due to laziness.

> xs <- getParallelChunkMapBy snd >>= \parMap ->
>     (\x -> (undefined, x * x)) `parMap` [ 1 .. 10 ]
> liftIO . print $ snd <$> xs
> >>> [1,4,9,16,25,36,49,64,81,100]
-}
getParallelChunkMapBy ∷ ∀ a b c env. (NFData c) ⇒ (b → c) → Evaluation env ((a → b) → [a] → [b])
getParallelChunkMapBy g =
    let h ∷ (a → b) → a → b
        h f x = let v = f x in force (g v) `seq` v

        construct ∷ Word → (a → b) → [a] → [b]
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
                        y :| ys = withStrategy (parListChunk' num rseq) $ h f <$> x :| xs
                    in  y : ys
    in  Evaluation $ reader (pure . construct . fromIntegral . implicitBucketNum)


{- |
__Time:__ \( \mathcal{O}\left( 1 \right) \)

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
__Time:__ \( \mathcal{O}\left( 1 \right) \)

/Note:/ Does not work on infinite lists!

Like getParallelChunkMapBy, but performs monadic actions over the list in parallel,
with customizable laziness.
Each thread will have a different, /uncorrelated/ random number generator.

See the documentation of 'getParallelChunkMapBy' for more details on laziness semantics.
-}
getParallelChunkTraverseBy
    ∷ ∀ a b c env t
     . (NFData c, Traversable t)
    ⇒ (b → c)
    -- ^ The sub-structure of the element to be forced.
    → Evaluation env ((a → Evaluation env b) → t a → Evaluation env (t b))
getParallelChunkTraverseBy selector = pure (parallelTraverseEvaluationBy selector)


{- |
__Time:__ \( \mathcal{O}\left( 1 \right) \)

Generate a 'PHANE.Evaluation.RandomSeed' to initialize an 'PHANE.Evaluation.Evaluation' by using system entropy.
-}
initializeRandomSeed ∷ IO RandomSeed
initializeRandomSeed = do
    now ← getPOSIXTime
    let timebits = truncate now ∷ Int
    cpu ← getCPUTime
    let cpubits = fromIntegral (cpu `div` cpuTimePrecision) ∷ Int
    pure . RandomSeed $ timebits `xor` cpubits


{- |
__Time:__ \( \mathcal{O}\left( 1 \right) \)

Set the random seed for the 'PHANE.Evaluation.Evaluation'.
-}
setRandomSeed ∷ (Enum i) ⇒ i → Evaluation env ()
setRandomSeed seed = Evaluation . ReaderT $ \store →
    let genRef = implicitRandomGen store
        genNew = mkStdGen $ fromEnum seed
        update = const (pure (), genNew)
    in  liftIO $ applyIOGen update genRef


{- |
__Time:__ \( \mathcal{O}\left( n \right) \)

Randomly permute the elements of the abstract list.
-}
shuffleList ∷ (IsList (f a), Item (f a) ~ a) ⇒ f a → Evaluation env (f a)
shuffleList = fmap List.fromList . shuffleM . List.toList


{- |
__Time:__ \( \mathcal{O}\left( \mathord{?} \right) \)

Lift one 'PHANE.Evaluation.Evaluation' environment to another.
-}
mapEvaluation ∷ (outer → inner) → Evaluation inner a → Evaluation outer a
mapEvaluation f eval =
    Evaluation . ReaderT $
        changeVariables f >=> advanceEvaluation eval


{-
mapEvaluation f (Evaluation (ReaderT inner)) = Evaluation . ReaderT $ \store -> do
    old <- readIORef $ explicitVariables store
    inner <- store
    ref <- newIORef $ f old
    inner
    let new = store{ explicitVariables = f old }
    adjustVariables f store *> inner store
-}

{- |
__Time:__ \( \mathcal{O}\left( 1 \right) \)

Fail and indicate the phase in which the failure occurred.
-}
failWithPhase ∷ (Loggable s) ⇒ ErrorPhase → s → Evaluation env a
failWithPhase p message = do
    logWith LogFail $ logToken message
    Evaluation . ReaderT . const . pure $ evalUnitWithPhase p message


adjustVariables ∷ (env → env) → ImplicitEnvironment env → IO ()
adjustVariables f = flip modifyIORef' f . explicitVariables


changeVariables ∷ ∀ env env'. (env → env') → ImplicitEnvironment env → IO (ImplicitEnvironment env')
changeVariables f store = do
    old ← readIORef $ explicitVariables store
    ref ← newIORef $ f old
    let new = store{explicitVariables = ref}
    pure new


advanceEvaluation ∷ Evaluation env a → ImplicitEnvironment env → IO (EvaluationResult a)
advanceEvaluation (Evaluation (ReaderT f)) implicitEnv = do
    logConfig ← readIORef $ implicitLogConfig implicitEnv
    flip finally (flushLogs logConfig) $ f implicitEnv


executeEvaluation ∷ (MonadIO m) ⇒ ImplicitEnvironment env → Evaluation env a → m a
executeEvaluation implicitEnv (Evaluation (ReaderT f)) =
    let logRef = implicitLogConfig implicitEnv
    in  liftIO . flip finally (readIORef logRef >>= flushLogs) $ do
            res ← f implicitEnv
            case runEvaluationResult res of
                Right value → pure value
                Left (phase, txt) →
                    let exitCode = errorPhaseToExitCode BM.! phase
                    in  processMessage logRef LogFail txt *> exitWith exitCode


parallelTraverseEvaluation
    ∷ ∀ a b env t
     . (NFData b, Traversable t)
    ⇒ (a → Evaluation env b)
    → t a
    → Evaluation env (t b)
parallelTraverseEvaluation f = pooledMapConcurrently (fmap force . f)


{-
parallelTraverseEvaluation f xs = withRunInIO $ \runner →
    mapConcurrently (runner . fmap force . f) xs
-}

parallelTraverseEvaluationBy
    ∷ ∀ a b c env t
     . (NFData c, Traversable t)
    ⇒ (b → c)
    → (a → Evaluation env b)
    → t a
    → Evaluation env (t b)
parallelTraverseEvaluationBy g f =
    let h x = f x >>= \v → force (g v) `seq` pure v
    in  pooledMapConcurrently h


setVerbosityOf
    ∷ ((LogFeed → LogFeed) → LogConfiguration → LogConfiguration)
    → Verbosity
    → Evaluation env ()
setVerbosityOf f v =
    Evaluation . ReaderT $
        fmap pure . modImplicitLogConfiguration (f (setFromVerbosity v))


modImplicitLogConfiguration
    ∷ (LogConfiguration → LogConfiguration)
    → ImplicitEnvironment env
    → IO ()
modImplicitLogConfiguration f x = do
    modifyIORef' (implicitLogConfig x) f


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

splitGenInto ∷ (RandomGenM (IOGenM StdGen) StdGen m) ⇒ Word → IOGenM StdGen → m [StdGen]
splitGenInto n = fmap force . replicateM (fromEnum n) . splitGenM
-}
