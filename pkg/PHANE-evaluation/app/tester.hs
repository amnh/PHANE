module Main (main) where

import Control.Concurrent (threadDelay, yield)
import Control.Evaluation
import Control.Monad (replicateM)
import Control.Monad.IO.Class
import Control.Monad.Logger
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

    runEvaluation logConfig firstSeed () harnessRanomness

    result ← runEvaluation logConfig firstSeed () $ runningInParallel True
    print result


runningInParallel ∷ Bool → Evaluation () [Natural]
runningInParallel False = runningInParallelPure
runningInParallel True = runningInParallelEffect


runningInParallelPure ∷ Evaluation () [Natural]
runningInParallelPure = do
    inParallel ← getParallelChunkMap
    pure $ ackermann 10 `inParallel` [1 .. 16]


runningInParallelEffect ∷ Evaluation () [Natural]
runningInParallelEffect =
    --    let expensiveOp x = (\n → ackermann (fromIntegral n) x) <$> (getRandomR (5, 10) ∷ Evaluation () Word)
    let expensiveOp x = do
            logWith LogInfo $ "Start:\t" <> show x
            let v = ackermann x x `seq` 24
            pure v <* logWith LogInfo ("Close:\t" <> show x)
    in  do
            parTraverse ← getParallelChunkTraverse
            expensiveOp `parTraverse` [1 .. 16]


harnessRanomness ∷ Evaluation () ()
harnessRanomness =
    let n ∷ Int
        n = 100000

        mean ∷ [Integer] → Rational
        mean x = sum x % toInteger n

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
    in  do
            observations ← replicateM n $ getRandomR (1, 100) ∷ Evaluation () [Integer]
            liftIO . putStrLn . display 3 $ mean observations


ackermann ∷ Natural → Natural → Natural
ackermann 0 0 = 0
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann m (ackermann (m - 1) (n - 1))
