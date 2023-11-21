module Main (main) where

import Control.Evaluation
import Control.Evaluation.Logging.Class (LogLevel (..), Logger (..))
import Control.Evaluation.Verbosity
import Control.Monad (replicateM, void, when)
import Control.Monad.IO.Class
import Control.Monad.Random.Class (MonadRandom (..))
import Data.Functor (($>))
import Data.Ratio
import Numeric.Natural
import System.IO


{- |
Main entry point
-}
main ∷ IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    logConfig ← initializeLogging Info Warn Nothing
    firstSeed ← initializeRandomSeed

    when True $ do
        print "Test harness: Randomness (isolated)"
        runEvaluation logConfig firstSeed () harnessRanomness

    when False $ do
        print "Test harness: Paralelism (isolated)"
        void . runEvaluation logConfig firstSeed () $ runningInParallel True

    when True $ do
        print "Test harness: Randomness (parallel)"
        void $ runEvaluation logConfig firstSeed () runningInParallelRandom

    print "Done!"


runningInParallel ∷ Bool → Evaluation () [Natural]
runningInParallel False = runningInParallelPure
runningInParallel True = runningInParallelEffect


runningInParallelPure ∷ Evaluation () [Natural]
runningInParallelPure = do
    inParallel ← getParallelChunkMap
    pure $ ackermann 10 `inParallel` [1 .. 16]


runningInParallelEffect ∷ Evaluation () [Natural]
runningInParallelEffect = do
    parTraverse ← getParallelChunkTraverse
    (ackermannEffect . Just) `parTraverse` [1 .. 16]


runningInParallelRandom ∷ Evaluation () [Natural]
runningInParallelRandom = do
    parTraverse ← getParallelChunkTraverse
    ackermannEffect `parTraverse` replicate 16 Nothing


harnessRanomness ∷ Evaluation () ()
harnessRanomness =
    let n ∷ Int
        n = 100000

        mean ∷ [Integer] → Rational
        mean x = sum x % toInteger n
    in  do
            observations ← replicateM n $ getRandomR (1, 100) ∷ Evaluation () [Integer]
            liftIO . putStrLn . display 3 $ mean observations


ackermann ∷ Natural → Natural → Natural
ackermann 0 0 = 0
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann m (ackermann (m - 1) (n - 1))


ackermannEffect ∷ Maybe Natural → Evaluation () Natural
ackermannEffect x =
    let randNat ∷ Evaluation () Natural
        randNat = fromIntegral <$> (getRandomR (1, 5) ∷ Evaluation () Word)
    in  do
            n ← maybe randNat pure x
            logWith LogInfo $ "Start:\t" <> show n
            let v = ackermann 10 n
            logWith LogInfo ("Value:\t" <> show v) $> v


display ∷ Int → Rational → String
display len rat =
    let (d', next') = abs num `quotRem` den
        num = numerator rat
        den = denominator rat

        go 0 = ""
        go x =
            let (d, next) = (10 * x) `quotRem` den
            in  shows d (go next)

        prefix
            | num < 0 = "-"
            | otherwise = ""
    in  prefix <> shows d' ("." <> take len (go next'))
