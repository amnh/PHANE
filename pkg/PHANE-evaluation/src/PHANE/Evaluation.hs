{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

{- |
The 'Evaluation' type's monad transformer definition and types.
-}
module PHANE.Evaluation (
    Evaluation (),

    -- * Run computation
    runEvaluation,

    -- * Specialized functions
    failWithPhase,

    -- * Operations

    -- ** Logging
    LogConfiguration (),
    initializeLogging,
    setVerbositySTDERR,
    setVerbositySTDOUT,
    setVerbosityFileLog,

    -- ** Parallelism
    getParallelChunkMap,
    getParallelChunkTraverse,

    -- ** Randomness
    RandomSeed (),
    initializeRandomSeed,
    setRandomSeed,
) where

import Control.Applicative (Alternative (..))
import Control.Concurrent.Async (mapConcurrently)
import Control.DeepSeq
import Control.Exception
import Control.Monad ((<=<))
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.RWS.Strict
import Control.Monad.Random.Strict
import Control.Monad.Reader (MonadReader (..), ReaderT (..), withReaderT)
import Control.Monad.Zip (MonadZip (..))
import Control.Parallel.Strategies
import Data.Bimap qualified as BM
import Data.Bits (xor)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (sconcat)
import Data.String
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Conc (getNumCapabilities)
import GHC.Generics
import PHANE.Evaluation.ErrorPhase
import PHANE.Evaluation.Logging
import PHANE.Evaluation.Logging.Configuration
import PHANE.Evaluation.Result
import PHANE.Evaluation.Verbosity
import System.CPUTime (cpuTimePrecision, getCPUTime)
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
    { unwrapEvaluation ∷ RWST ImplicitEnvironment () () IO (EvaluationResult a)
    -- ^ Run the 'Evaluation' monad transformer
    }


--    { unwrapEvaluation ∷ ReaderT (ImplicitEnvironment env) IO (EvaluationResult a)

type role Evaluation nominal nominal


type role ImplicitVariables representational


-- type ImplicitVariables = ()

data ImplicitVariables var = ImplicitVariables
    { implicitLogging ∷ {-# UNPACK #-} !LogConfiguration
    , explicitStorage ∷ !var
    }


data ImplicitEnvironment = ImplicitEnvironment
    { implicitBucketNum ∷ {-# UNPACK #-} !ParallelBucketCount
    , implicitLogConfig ∷ {-# UNPACK #-} !LogConfiguration
    , implicitRandomGen ∷ {-# UNPACK #-} !(AtomicGenM StdGen)
    --    , explicitReader ∷ !env
    }


newtype ParallelBucketCount = MaxPar Word


{- |
A seed from which a /(practically infinite)/ stream of pseudorandomness can be generated.
-}
newtype RandomSeed = RandomSeed Int


instance Alternative (Evaluation env) where
    {-# INLINEABLE (<|>) #-}
    (<|>) (Evaluation x) (Evaluation y) = Evaluation . RWST $ \vals vars → do
        tup@(res, vals', _) ← runRWST x vals vars
        case runEvaluationResult res of
            Left _ → runRWST y vals vars
            Right _ → pure tup


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
        fun ← (arbitrary ∷ Gen (ImplicitEnvironment → ImplicitVariables env → (EvaluationResult a, ImplicitVariables env, ())))
        pure . Evaluation . RWST $ \vals vars → pure (fun vals vars)


instance (Arbitrary env) ⇒ Arbitrary (ImplicitVariables env) where
    arbitrary = ImplicitVariables <$> arbitrary <*> arbitrary


instance CoArbitrary ImplicitEnvironment where
    coarbitrary store =
        let x = implicitBucketNum store
            y = implicitLogConfig store
        in  coarbitrary y . variant x


instance (CoArbitrary env) ⇒ CoArbitrary (ImplicitVariables env) where
    coarbitrary store =
        let x = implicitLogging store
            y = explicitStorage store
        in  coarbitrary y . coarbitrary x


deriving newtype instance Eq ParallelBucketCount


deriving newtype instance Eq RandomSeed


deriving newtype instance Enum ParallelBucketCount


deriving newtype instance Enum RandomSeed


deriving stock instance Functor ImplicitVariables


instance Functor (Evaluation env) where
    {-# INLINEABLE fmap #-}
    fmap f x = Evaluation . fmap (fmap f) $ unwrapEvaluation x


deriving stock instance Generic (Evaluation env a)


instance Logger (Evaluation env) where
    {-# INLINEABLE logWith #-}
    logWith level str = Evaluation $ do
        impEnv ← ask
        let logConfig = implicitLogConfig impEnv
        liftIO . fmap pure . processMessage logConfig level $ logToken str


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
    interleave action = Evaluation . RWST $ \vals vars → do
        let gen = implicitRandomGen vals
        gen' ← applyAtomicGen split gen >>= newAtomicGenM
        let vals' = vals{implicitRandomGen = gen'}
        executeEvaluation vals' vars action >>= include vars . pure


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


{-
instance MonadReader env (Evaluation env) where
    {-# INLINEABLE local #-}
    {-# INLINE ask #-}
    ask = Evaluation $ do
        store ← ask
        pure . pure $ explicitReader store

    local f = Evaluation . local (fmap f) . unwrapEvaluation
-}

instance MonadThrow (Evaluation env) where
    throwM e = Evaluation $ throwM e


{-
instance MonadUnliftIO (Evaluation env) where
    {-# INLINE withRunInIO #-}
    -- f :: (forall a. Evaluation env a -> IO a) -> IO b
    withRunInIO f =
        {-# SCC withRunInIO_Evaluation #-}
        Evaluation . RWST $ \vals vars →
            {-# SCC withRunInIO_RWST_inner #-}
            withRunInIO $ -- ((forall a. IO a -> IO a) -> IO b) -> IO b
                {-# SCC withRunInIO_WITH_run #-}
                \run → -- (forall a. IO a -> IO a) -> IO b
                    pure <$> f (run . executeEvaluation vals vars)
-}

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
    primitive token = Evaluation . RWST $ \vals vars →
        primitive token >>= include vars . pure


deriving newtype instance Real ParallelBucketCount


deriving newtype instance Real RandomSeed


instance Semigroup (Evaluation env a) where
    {-# INLINE (<>) #-}
    lhs <> rhs = Evaluation . RWST $ \vals vars → do
        (x, vars', _) ← runRWST (unwrapEvaluation lhs) vals vars
        case runEvaluationResult x of
            Left s → include vars . EU $ Left s
            _ → runRWST (unwrapEvaluation rhs) vals vars


deriving newtype instance Show ParallelBucketCount


deriving newtype instance Show RandomSeed


{- |
Run the 'Evaluation' computation.

Initial randomness seed and configuration for logging outputs required to initiate the computation.
-}
runEvaluation ∷ (MonadIO m) ⇒ LogConfiguration → RandomSeed → env → x → Evaluation env a → m a
runEvaluation logConfig randomSeed environ x eval = do
    randomRef ← newAtomicGenM . mkStdGen $ fromEnum randomSeed
    maxBuckets ← liftIO $ toEnum . max 1 . pred <$> getNumCapabilities
    let staticValues =
            ImplicitEnvironment
                { implicitBucketNum = maxBuckets
                , implicitLogConfig = logConfig
                , implicitRandomGen = randomRef
                --                , explicitReader = environ
                }

    let implicitState =
            ImplicitVariables
                { implicitLogging = logConfig
                , explicitStorage = environ
                }

    executeEvaluation staticValues implicitState eval


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
            in  Evaluation . withRWST transformation . unwrapEvaluation
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
getParallelChunkTraverse = Evaluation . RWST $ \vals vars → include vars . pure $ parallelTraverseEvaluation vals vars


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
Fail and indicate the phase in which the failure occurred.
-}
failWithPhase ∷ (Loggable s) ⇒ ErrorPhase → s → Evaluation env a
failWithPhase p message = do
    logWith LogFail $ fromString "\n" <> logToken message
    Evaluation . RWST $ \vals vars →
        include vars (evalUnitWithPhase p message)


discard ∷ (a, b, c) → a
discard (a, _, _) = a


include
    ∷ ImplicitVariables env
    → a
    → IO (a, ImplicitVariables env, ())
include a b = pure (b, a, mempty)


executeEvaluation ∷ (MonadIO m) ⇒ ImplicitEnvironment → ImplicitVariables env → Evaluation env a → m a
executeEvaluation implicitEnv implicitVars eval =
    let logConfig = implicitLogConfig implicitEnv
    in  liftIO $ do
            (res, _, _) ← encapsulatedEvaluation implicitEnv implicitVars eval
            case runEvaluationResult res of
                Right value → pure value
                Left (phase, txt) →
                    let exitCode = errorPhaseToExitCode BM.! phase
                    in  processMessage logConfig LogFail txt *> exitWith exitCode


encapsulatedEvaluation
    ∷ (MonadIO m)
    ⇒ ImplicitEnvironment
    → ImplicitVariables env
    → Evaluation env a
    → m (EvaluationResult a, ImplicitVariables env, ())
encapsulatedEvaluation implicitEnv implicitVars (Evaluation (RWST f)) =
    let logConfig = implicitLogConfig implicitEnv
    in  liftIO . flip finally (flushLogs logConfig) $ f implicitEnv implicitVars


parallelTraverseEvaluation
    ∷ ∀ a b env t
     . (NFData b, Traversable t)
    ⇒ ImplicitEnvironment
    → ImplicitVariables env
    → (a → Evaluation env b)
    → t a
    → Evaluation env (t b)
parallelTraverseEvaluation vals vars f =
    let transform = executeEvaluation vals vars . interleave . fmap force
    in  liftIO . mapConcurrently (transform . f)


mkUnliftIO
    ∷ ImplicitEnvironment
    → ImplicitVariables env
    → UnliftIO (RWST ImplicitEnvironment () (ImplicitVariables env) IO)
mkUnliftIO vals vars = UnliftIO $ \x → fst <$> evalRWST x vals vars


unliftEvaluation
    ∷ ImplicitEnvironment
    → ImplicitVariables env
    → UnliftIO (Evaluation env)
unliftEvaluation vals vars = UnliftIO $ executeEvaluation vals vars


setVerbosityOf
    ∷ ((LogFeed → LogFeed) → LogConfiguration → LogConfiguration)
    → Verbosity
    → Evaluation env a
    → Evaluation env a
setVerbosityOf f v =
    let transformation = modImplicitLogConfiguration (f (setFeedLevel v))
    in  Evaluation . withRWST transformation . unwrapEvaluation


modImplicitLogConfiguration
    ∷ (LogConfiguration → LogConfiguration)
    → ImplicitEnvironment
    → ImplicitVariables env
    → (ImplicitEnvironment, ImplicitVariables env)
modImplicitLogConfiguration f x y = (x{implicitLogConfig = f $ implicitLogConfig x}, y)


withRandomGenerator ∷ StdGen → Evaluation env a → Evaluation env a
withRandomGenerator gen eval =
    let transformation ∷ AtomicGenM StdGen → ImplicitEnvironment → ImplicitEnvironment
        transformation val store = store{implicitRandomGen = val}
    in  liftIO (newAtomicGenM gen) >>= \ref →
            Evaluation . local (transformation ref) $ unwrapEvaluation eval


bind ∷ Evaluation env a → (a → Evaluation env b) → Evaluation env b
bind (Evaluation x) f = Evaluation . RWST $ \vals vars → do
    tup@(y, vars', _) ← runRWST x vals vars
    case runEvaluationResult y of
        Left s → include vars' . EU $ Left s
        Right v → runRWST (unwrapEvaluation (f v)) vals vars'


apply ∷ Evaluation env (t → a) → Evaluation env t → Evaluation env a
apply (Evaluation lhs) rhs = Evaluation . RWST $ \vals vars → do
    (x, vars', _) ← runRWST lhs vals vars
    case runEvaluationResult x of
        Left s → include vars' . EU $ Left s
        Right f → do
            (y, vars'', _) ← runRWST (unwrapEvaluation rhs) vals vars'
            include vars'' . EU $ case runEvaluationResult y of
                Left s → Left s
                Right v → Right $ f v


propagate ∷ Evaluation env a → Evaluation env b → Evaluation env b
propagate (Evaluation lhs) rhs = Evaluation . RWST $ \vals vars → do
    (x, vars', _) ← runRWST lhs vals vars
    case runEvaluationResult x of
        Left s → include vars' . EU $ Left s
        _ → runRWST (unwrapEvaluation rhs) vals vars'


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
