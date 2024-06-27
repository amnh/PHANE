{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedSums #-}

{- |
The type of metric possesed by a diagnosed matrix.
-}
module TransitionMatrix.Metricity (
    -- * Diagnosable Metrics
    Metricity (..),
    SpecializableMetric (..),

    -- * Diagnoses
    metricityOfDistance,
) where

import Control.DeepSeq
import Data.Bits
import Data.Data
import Data.Foldable
import Data.Ix
import GHC.Generics
import Layout.Special.Type
import Measure.Distance
import Measure.Unit.SymbolCount


{- |
There is a heirachichal nature if a Symbol Distance Matrix's metricity.

@
      Non-metric

          |

        Metric

        /    \\

Ultrametric  L1 Norm

     |

Discrete metric ⨉ Gap

     |

Discrete metric

@

Each SDM structure has certain properties that must hold and allows for space
& time optimizations.

  * Non-metric: Generate a warning for the user!

  * Metric:

      * /σ(i,j) = 0 iff i = j/

      * /σ(i,j) = σ(j,i)/

      * /σ(i,k) ≤ σ(i,j) + σ(j,k)/

 * Ultrametric: Allows for some runtime optimizations

      * /σ(i,j) = 0 iff i = j/

      * /σ(i,j) = σ(j,i)/

      * /σ(i,k) ≤ max { σ(i,j),  σ(j,k) }/

  * Discrete metric ⨉ Gap: Allows for /no/ space allocation and runtime optimizations

      * /σ(i,j) = 0 iff i = j/

      * /σ(0,j) = c iff i ≠ j/

      * /σ(i,0) = c iff i ≠ j/

      * /σ(i,j) = 1 iff i ≠ j, i > 0, j > 0/

  * Discrete metric: Allows for /no/ space allocation and runtime optimizations

      * /σ(i,j) = 0 iff i = j/

      * /σ(i,j) = 1 iff i ≠ j/

  * L1 Norm: Allows for /no/ space allocation and runtime optimizations

      * /σ(i,j) = max(i,j) - min(i,j)/
-}
data Metricity
    = NonMetric
    | Metric
    | Special SpecializableMetric
    deriving stock (Data, Eq, Generic, Show)
    deriving anyclass (NFData)


{- |
__Time:__ \( \mathcal{O} \left( n^3 \right) \) & \( \Omega \left( n^2 \right) \)
where \( n \) is the magnitude of the supplied interval.

Computes the metricity of ther supplied 'Distance' measure over the specified (inclusive) interval.
-}
{-# INLINEABLE metricityOfDistance #-}
metricityOfDistance
    ∷ ( Integral d
      , Ix i
      )
    ⇒ (i, i)
    -- ^ Contiguous, discrete, inclusive bounds within which metricity is observed
    → Distance d i
    -- ^ Distance measure to observe
    → Metricity
metricityOfDistance dim δ =
    let line = range dim
    in  if nonZeroDiagonal δ line
            then NonMetric
            else case pass2D dim δ of
                Just m → m
                -- We know that the matrix is symetric,
                -- With zeroes along the diagonal,
                -- but it is not the Distcrete metric,
                -- nor the Discrete metric plus Gap
                -- nor the 1st linear norm.
                --
                -- Hence we check for metricity ancd ultrametricity
                Nothing →
                    let points =
                            [ (δ i k, δ i j, δ j k)
                            | i ← line
                            , j ← line
                            , i < j
                            , k ← line
                            , j <= k
                            ]

                        triangleInequality ∷ (Ord a, Num a) ⇒ (a, a, a) → Bool
                        triangleInequality ~(x, y, z) = x <= y + z
                    in  if all triangleInequality points
                            then Metric
                            else NonMetric


{- |
An internal helper function used in in determining Metric & Ultra Metric cases.
-}
nonZeroDiagonal ∷ (Foldable f, Integral d) ⇒ Distance d i → f i → Bool
nonZeroDiagonal δ = any (\i → δ i i /= 0)


{- |
Just one pass over the upper triangluar of the matrix
-}
pass2D
    ∷ ∀ i d
     . ( Integral d
       , Ix i
       )
    ⇒ (i, i)
    → Distance d i
    → Maybe Metricity
pass2D dim δ
    | resultBits `testBit` discrete = Just . Special $ DiscreteMetric count
    | resultBits `testBit` subindel = Just . Special $ gapSubMetric
    | resultBits `testBit` linear1N = Just . Special $ L1Norm count
    | resultBits `testBit` symetric = Nothing
    | otherwise = Just NonMetric
    where
        line = range dim
        resultBits = foldr check initialBits pairs

        count = SymbolCount . toEnum $ rangeSize dim

        gapSubMetric = DiscreteCrossGap count (fromIntegral gDist) $ fromIntegral sDist

        discrete = 0 ∷ Int
        subindel = 1
        linear1N = 2
        symetric = 3

        isDiscrete ∷ (Eq b, Num b) ⇒ (a, a, b, c) → Bool
        isDiscrete (_, _, x, _) = x == 1

        isInDelSub ∷ ∀ c. (i, i, d, c) → Bool
        isInDelSub (i, j, x, _) = x == if i == fst dim || j == fst dim then gDist else sDist

        isLinear1N ∷ (Eq b, Num b, Ix a) ⇒ (a, a, b, c) → Bool
        isLinear1N (i, j, x, _) = x == fromIntegral (rangeSize (max i j, min i j))

        bitSet ∷ [(Int, (i, i, d, c) → Bool)]
        bitSet =
            [ (discrete, isDiscrete)
            , (subindel, isInDelSub)
            , (linear1N, isLinear1N)
            , (symetric, const True)
            ]

        gDist = uncurry δ dim
        sDist = case line of
            _ : y : z : _ → δ y z
            _ → gDist

        check ∷ (Bits a) ⇒ (i, i, d, d) → a → a
        check p@(_, _, x, y) bits
            | x /= y = zeroBits
            | otherwise =
                let f ∷ (Bits a) ⇒ a → (Int, (i, i, d, d) → Bool) → a
                    f b (i, g)
                        | b `testBit` i && not (g p) = b `clearBit` i
                        | otherwise = b
                in  foldl' f bits $ init bitSet

        initialBits ∷ Word
        initialBits =
            let b = foldl' setBit zeroBits $ fst <$> bitSet
            in  if rangeSize dim > 2
                    then b
                    else b `clearBit` subindel

        pairs = [(i, j, δ i j, δ j i) | i ← toList line, j ← toList line, i < j]
