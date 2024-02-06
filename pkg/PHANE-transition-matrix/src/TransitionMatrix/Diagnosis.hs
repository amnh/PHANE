{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedSums #-}

{- |
Smart constructor methods for producing a tranistion cost matrix.
The 'Diagnosis' provides a metadata describing the structure of the tranistion cost matrix.
-}
module TransitionMatrix.Diagnosis (
    -- * Specialized Representation
    TransitionMatrix (),
    TransitionMeasureDiagnosis (..),

    -- * Special Constructors
    discreteCrossGap,
    discreteMetric,
    linearNorm,

    -- * General Constructors
    fromList,
    fromColumns,
    fromRows,
    generate,
) where

import Control.DeepSeq
import Data.Bits
import Data.Coerce
import Data.Foldable
import Data.Hashable
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (sortBy, transpose, unfoldr)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord
import Data.Ratio
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word
import GHC.Exts qualified as GHC (fromList)
import GHC.Generics
import Layout.Compact.States (StateTransitionsCompact)
import Layout.Compact.Symbols
import Layout.Compact.Symbols.Triangular (lowerTriangleOfSquare)
import Layout.Compact.Symbols.Unsafe (unsafeCompactStateFromSDMS, unsafeFromVectorSquare)
import Layout.Memoize.States (initialize)
import Layout.Special.Discrete qualified as Dis
import Layout.Special.DiscreteCrossGap qualified as Gap
import Layout.Special.L1Norm qualified as L1N
import Layout.Special.Type
import Measure.Distance
import Measure.Range
import Measure.Transition
import Measure.Unit.SymbolChangeCost
import Measure.Unit.SymbolCount
import Measure.Unit.SymbolIndex
import Numeric
import Numeric.Natural
import TransitionMatrix.Diagnosis.Error
import TransitionMatrix.Metricity
import TransitionMatrix.Representation


{- |
The result of intelligently encoding a "Transition Measure."
-}
data TransitionMeasureDiagnosis a = TransitionMeasureDiagnosis
    { factoredCoefficient ∷ Rational
    -- ^ The multiplicative constant factor of a symbol change matrix.
    --  Minimum value of the multiplicative identity /one/.
    , transitionMetricity ∷ Metricity
    -- ^ The most restrictive metric classifcation of the 'TransitionMatrix'.
    , transitionMatrix ∷ TransitionMatrix a
    -- ^ The most compact encoding of the "Transition Measure."
    }
    deriving stock (Eq, Generic)
    deriving anyclass (NFData)


type role TransitionMeasureDiagnosis representational


instance Show (TransitionMeasureDiagnosis a) where
    show d =
        let display ∷ Int → Rational → String
            display n x = (showFFloat (Just n) $ fromRat x) ""
        in  unwords
                [ "TransitionMeasureDiagnosis"
                , "("
                , display 4 $ factoredCoefficient d
                , show $ transitionMetricity d
                , ")"
                ]


{- |
__Time:__ \( \mathcal{O} \left( 1 \right) \)

Nullary constructor for the <https://en.wikipedia.org/wiki/Discrete_space discrete metric>.
-}
discreteMetric ∷ SymbolCount → TransitionMatrix a
discreteMetric dim@(SymbolCount n) =
    let m = DiscreteMetric dim

        f ∷ StateTransitionsCompact → TransitionMatrix a
        f = IotaMatrix $ Just m
    in  case n of
            2 → f Dis.tcmρ2
            3 → f Dis.tcmρ3
            4 → f Dis.tcmρ4
            5 → f Dis.tcmρ5
            6 → f Dis.tcmρ6
            7 → f Dis.tcmρ7
            8 → f Dis.tcmρ8
            _ → VastSpecialized m


{- |
__Time:__ \( \mathcal{O} \left( 1 \right) \)

Nullary constructor for the <https://en.wikipedia.org/wiki/Discrete_space discrete metric>.
-}
discreteCrossGap ∷ SymbolCount → SymbolChangeCost → SymbolChangeCost → TransitionMatrix a
discreteCrossGap dim@(SymbolCount n) 1 2 =
    let m = DiscreteCrossGap dim 1 2

        f ∷ StateTransitionsCompact → TransitionMatrix a
        f = IotaMatrix $ Just m
    in  case n of
            2 → f Gap.tcm12ρ2
            3 → f Gap.tcm12ρ3
            4 → f Gap.tcm12ρ4
            5 → f Gap.tcm12ρ5
            6 → f Gap.tcm12ρ6
            7 → f Gap.tcm12ρ7
            8 → f Gap.tcm12ρ8
            _ → VastSpecialized m
discreteCrossGap n sub gap =
    let m = DiscreteCrossGap n sub gap
    in  case n of
            v | iota v → IotaMatrix (Just m) $ Gap.tcmρ n gap sub
            _ → VastSpecialized m


{- |
__Time:__ \( \mathcal{O} \left( 1 \right) \)

Nullary constructor for the <https://en.wikipedia.org/wiki/Lp_space 1st linear norm>.
-}
linearNorm ∷ SymbolCount → TransitionMatrix a
linearNorm dim@(SymbolCount n) =
    let m = L1Norm dim

        f ∷ StateTransitionsCompact → TransitionMatrix a
        f = IotaMatrix $ Just m
    in  case n of
            2 → f L1N.tcmρ2
            3 → f L1N.tcmρ3
            4 → f L1N.tcmρ4
            5 → f L1N.tcmρ5
            6 → f L1N.tcmρ6
            7 → f L1N.tcmρ7
            8 → f L1N.tcmρ8
            _ → VastSpecialized m


{- |
__Time:__ \( \mathcal{O} \left( n^3 \right) \) & \( \Omega \left( n^2 \right) \)
where \( n \) is the number of symbols for the transition measure.

Construct a 'TransitionMatrix' from a list of elements in row major order.

==== __Examples__

>>> fromList [1..9]
SDM: 3 x 3
  1 2 3
  4 5 6
  7 8 9

>>> fromList []
*** Exception: fromList: An empty structure was supplied. Cannot construct an empty SDM!

>>> fromList [42]
*** Exception: fromList: A singleton structure was supplied. Cannot construct a SDM with size of 1, must have size of 2 or greater.

>>> fromList [1..12]
*** Exception: fromList: The number of element (12) is not a square number. Cannot construct an non-square SDM! The number of elements (12) lies between the valid square numbers (9) and (16).
-}
{-# INLINEABLE fromList #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Double → Either (DiagnosisFailure Double) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Double → Either (DiagnosisFailure Double) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Double → Either (DiagnosisFailure Double) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Double → Either (DiagnosisFailure Double) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Int → Either (DiagnosisFailure Int) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Int → Either (DiagnosisFailure Int) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Int → Either (DiagnosisFailure Int) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Int → Either (DiagnosisFailure Int) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Integer → Either (DiagnosisFailure Integer) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Integer → Either (DiagnosisFailure Integer) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Integer → Either (DiagnosisFailure Integer) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Integer → Either (DiagnosisFailure Integer) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Natural → Either (DiagnosisFailure Natural) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Natural → Either (DiagnosisFailure Natural) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Natural → Either (DiagnosisFailure Natural) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Natural → Either (DiagnosisFailure Natural) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Rational → Either (DiagnosisFailure Rational) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Rational → Either (DiagnosisFailure Rational) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Rational → Either (DiagnosisFailure Rational) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Rational → Either (DiagnosisFailure Rational) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Word → Either (DiagnosisFailure Word) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Word → Either (DiagnosisFailure Word) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Word → Either (DiagnosisFailure Word) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE fromList ∷
    (Foldable f) ⇒ f Word → Either (DiagnosisFailure Word) (TransitionMeasureDiagnosis Word64)
    #-}
fromList
    ∷ ( FiniteBits b
      , Foldable t
      , Hashable b
      , NFData b
      , Real a
      )
    ⇒ t a
    → Either (DiagnosisFailure a) (TransitionMeasureDiagnosis b)
fromList xs =
    let chunks ∷ Int → [a] → [[a]]
        chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

        inputLen = length xs
        size = floor $ sqrt (fromIntegral inputLen ∷ Double)
        isSquare = inputLen == size * size
        gridForm = chunks size $ toList xs
        squareCheck
            | isSquare = mempty
            | otherwise = singletonFailure . NotSquareList $ toEnum inputLen
    in  completeDiagnosis <$> checkInputValidity gridForm squareCheck


{- |
__Time:__ \( \mathcal{O} \left( n^3 \right) \) & \( \Omega \left( n^2 \right) \)
where \( n \) is the number of symbols for the transition measure.

Construct a 'TransitionMatrix' from an ordered collection of columns.

==== __Examples__

>>> fromColumns [[1,2,3],[4,5,6],[7,8,9]]
SDM: 3 x 3
  1 4 7
  2 5 8
  3 6 9
-}
{-# INLINEABLE fromColumns #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Double) → Either (DiagnosisFailure Double) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Double) → Either (DiagnosisFailure Double) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Double) → Either (DiagnosisFailure Double) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Double) → Either (DiagnosisFailure Double) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Int) → Either (DiagnosisFailure Int) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Int) → Either (DiagnosisFailure Int) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Int) → Either (DiagnosisFailure Int) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Int) → Either (DiagnosisFailure Int) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Integer) → Either (DiagnosisFailure Integer) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Integer) → Either (DiagnosisFailure Integer) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Integer) → Either (DiagnosisFailure Integer) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Integer) → Either (DiagnosisFailure Integer) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Natural) → Either (DiagnosisFailure Natural) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Natural) → Either (DiagnosisFailure Natural) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Natural) → Either (DiagnosisFailure Natural) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Natural) → Either (DiagnosisFailure Natural) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Rational) → Either (DiagnosisFailure Rational) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Rational) → Either (DiagnosisFailure Rational) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Rational) → Either (DiagnosisFailure Rational) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Rational) → Either (DiagnosisFailure Rational) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Word) → Either (DiagnosisFailure Word) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Word) → Either (DiagnosisFailure Word) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Word) → Either (DiagnosisFailure Word) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE fromColumns ∷
    (Foldable t, Foldable t') ⇒ t (t' Word) → Either (DiagnosisFailure Word) (TransitionMeasureDiagnosis Word64)
    #-}
fromColumns
    ∷ ( FiniteBits b
      , Foldable t
      , Foldable t'
      , Hashable b
      , NFData b
      , Real a
      )
    ⇒ t (t' a)
    → Either (DiagnosisFailure a) (TransitionMeasureDiagnosis b)
fromColumns xs =
    let gridForm = transpose . fmap toList $ toList xs
        (height, out) = modeAndOutlierLengths xs
        jaggedCheck
            | IS.null out = mempty
            | otherwise = singletonFailure $ JaggedColumns height out
    in  completeDiagnosis <$> checkInputValidity gridForm jaggedCheck


{- |
__Time:__ \( \mathcal{O} \left( n^3 \right) \) & \( \Omega \left( n^2 \right) \)
where \( n \) is the number of symbols for the transition measure.

Construct a 'TransitionMatrix' from an ordered collection of rows.

==== __Examples__

>>> fromRows [[1,2,3],[4,5,6],[7,8,9]]
SDM: 3 x 3
  1 2 3
  4 5 6
  7 8 9
-}
{-# INLINEABLE fromRows #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Double) → Either (DiagnosisFailure Double) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Double) → Either (DiagnosisFailure Double) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Double) → Either (DiagnosisFailure Double) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Double) → Either (DiagnosisFailure Double) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Int) → Either (DiagnosisFailure Int) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Int) → Either (DiagnosisFailure Int) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Int) → Either (DiagnosisFailure Int) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Int) → Either (DiagnosisFailure Int) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Integer) → Either (DiagnosisFailure Integer) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Integer) → Either (DiagnosisFailure Integer) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Integer) → Either (DiagnosisFailure Integer) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Integer) → Either (DiagnosisFailure Integer) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Natural) → Either (DiagnosisFailure Natural) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Natural) → Either (DiagnosisFailure Natural) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Natural) → Either (DiagnosisFailure Natural) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Natural) → Either (DiagnosisFailure Natural) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Rational) → Either (DiagnosisFailure Rational) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Rational) → Either (DiagnosisFailure Rational) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Rational) → Either (DiagnosisFailure Rational) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Rational) → Either (DiagnosisFailure Rational) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Word) → Either (DiagnosisFailure Word) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Word) → Either (DiagnosisFailure Word) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Word) → Either (DiagnosisFailure Word) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE fromRows ∷
    (Foldable t, Foldable t') ⇒ t (t' Word) → Either (DiagnosisFailure Word) (TransitionMeasureDiagnosis Word64)
    #-}
fromRows
    ∷ ( FiniteBits b
      , Foldable t
      , Foldable t'
      , Hashable b
      , NFData b
      , Real a
      )
    ⇒ t (t' a)
    → Either (DiagnosisFailure a) (TransitionMeasureDiagnosis b)
fromRows xs =
    let (width, out) = modeAndOutlierLengths xs
        jaggedCheck
            | IS.null out = mempty
            | otherwise = singletonFailure $ JaggedRows width out
    in  completeDiagnosis <$> checkInputValidity xs jaggedCheck


{- |
__Time:__ \( \mathcal{O} \left( n^3 \right) \) & \( \Omega \left( n^2 \right) \)
where \( n \) is the number of symbols for the transition measure.

A generating function for a 'TransitionMatrix'. Efficiently constructs a
'TransitionMatrix' of the specified size with each value defined by the result
of the supplied function.

==== __Examples__

>>> generate 5 $ const 5
SDM: 5 x 5
  5 5 5 5 5
  5 5 5 5 5
  5 5 5 5 5
  5 5 5 5 5
  5 5 5 5 5

>>> generate 4 $ \(i,j) -> abs (i - j)
SDM: 4 x 4
  0 1 2 3
  1 0 1 2
  2 1 0 1
  3 2 1 0

>>> generate 8 $ \(i,j) -> if i == j || i + j == 6 then 0 else 1
SDM: 8 x 8
  0 1 1 1 1 1 0 1
  1 0 1 1 1 0 1 1
  1 1 0 1 0 1 1 1
  1 1 1 0 1 1 1 1
  1 1 0 1 0 1 1 1
  1 0 1 1 1 0 1 1
  0 1 1 1 1 1 0 1
  1 1 1 1 1 1 1 0
-}
{-# INLINEABLE generate #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Double SymbolIndex → Either (DiagnosisFailure Double) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Double SymbolIndex → Either (DiagnosisFailure Double) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Double SymbolIndex → Either (DiagnosisFailure Double) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Double SymbolIndex → Either (DiagnosisFailure Double) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Int SymbolIndex → Either (DiagnosisFailure Int) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Int SymbolIndex → Either (DiagnosisFailure Int) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Int SymbolIndex → Either (DiagnosisFailure Int) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Int SymbolIndex → Either (DiagnosisFailure Int) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Integer SymbolIndex → Either (DiagnosisFailure Integer) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Integer SymbolIndex → Either (DiagnosisFailure Integer) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Integer SymbolIndex → Either (DiagnosisFailure Integer) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Integer SymbolIndex → Either (DiagnosisFailure Integer) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Natural SymbolIndex → Either (DiagnosisFailure Natural) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Natural SymbolIndex → Either (DiagnosisFailure Natural) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Natural SymbolIndex → Either (DiagnosisFailure Natural) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Natural SymbolIndex → Either (DiagnosisFailure Natural) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Rational SymbolIndex → Either (DiagnosisFailure Rational) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Rational SymbolIndex → Either (DiagnosisFailure Rational) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Rational SymbolIndex → Either (DiagnosisFailure Rational) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Rational SymbolIndex → Either (DiagnosisFailure Rational) (TransitionMeasureDiagnosis Word64)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Word SymbolIndex → Either (DiagnosisFailure Word) (TransitionMeasureDiagnosis Word8)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Word SymbolIndex → Either (DiagnosisFailure Word) (TransitionMeasureDiagnosis Word16)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Word SymbolIndex → Either (DiagnosisFailure Word) (TransitionMeasureDiagnosis Word32)
    #-}
{-# SPECIALIZE INLINE generate ∷
    SymbolCount → Distance Word SymbolIndex → Either (DiagnosisFailure Word) (TransitionMeasureDiagnosis Word64)
    #-}
generate
    ∷ ( FiniteBits b
      , Hashable b
      , NFData b
      , Real a
      )
    ⇒ SymbolCount
    -- ^ Dimension of the transition measure.
    → Distance a SymbolIndex
    -- ^ Function to determine the value of a given index.
    → Either (DiagnosisFailure a) (TransitionMeasureDiagnosis b)
generate (SymbolCount n) f =
    let dim = fromEnum n
        len = dim * dim
        g i = let (q, r) = toEnum i `divMod` n in f (coerce q) (coerce r)
        vec = V.generate len g
        shapeCheck =
            case n of
                0 → singletonFailure MatrixDimension0
                1 → singletonFailure MatrixDimension1
                _ → mempty
    in  completeDiagnosis <$> mergeInputFailures (shapeCheck, checkValueRange n vec)


-- Un-exported Functionality

completeDiagnosis
    ∷ ( FiniteBits a
      , Hashable a
      , NFData a
      )
    ⇒ (Rational, SymbolDistanceMatrixSquare)
    → TransitionMeasureDiagnosis a
completeDiagnosis (coefficient, sdms) =
    let (metricity, measure) = meaureWithMetricity sdms
    in  TransitionMeasureDiagnosis
            { factoredCoefficient = coefficient
            , transitionMetricity = metricity
            , transitionMatrix = measure
            }


meaureWithMetricity
    ∷ ( FiniteBits a
      , Hashable a
      , NFData a
      )
    ⇒ SymbolDistanceMatrixSquare
    → (Metricity, TransitionMatrix a)
meaureWithMetricity sdms =
    let dim = symbolCount sdms
        mkρ = iota dim
        met = metricityOfDistance (measureRange sdms) $ symbolDistances sdms
        stm = case met of
            NonMetric | mkρ → IotaMatrix Nothing $ unsafeCompactStateFromSDMS 0 sdms
            Metric | mkρ → IotaMatrix Nothing $ unsafeCompactStateFromSDMS 0 sdms
            NonMetric → VastMatrix . initialize $ Right sdms
            Metric → VastMatrix . initialize . Left $ lowerTriangleOfSquare sdms
            Special metric →
                case metric of
                    DiscreteCrossGap n sub gap → discreteCrossGap n sub gap
                    DiscreteMetric n → discreteMetric n
                    L1Norm n → linearNorm n
    in  (met, stm)


checkInputValidity
    ∷ ( Foldable t
      , Foldable t'
      , Real a
      )
    ⇒ t (t' a)
    → Maybe (DiagnosisFailure a)
    → Either (DiagnosisFailure a) (Rational, SymbolDistanceMatrixSquare)
checkInputValidity grid precheckedError =
    let dimension ∷ Word
        dimension = toEnum . length $ grid
        vectorLen ∷ Int
        vectorLen = fromEnum $ dimension * dimension
        linearize ∷ (Foldable t, Foldable t') ⇒ t (t' a1) → V.Vector a1
        linearize = V.fromListN vectorLen . foldMap toList . toList
        shapeCheck = liftA2 (<>) precheckedError $ checkGridValidity grid
        valueCheck = checkValueRange dimension $ linearize grid
    in  mergeInputFailures (shapeCheck, valueCheck)


mergeInputFailures
    ∷ (Real a)
    ⇒ ( Maybe (DiagnosisFailure a)
      , Either (DiagnosisFailure a) (Rational, Word, VS.Vector Word16)
      )
    → Either (DiagnosisFailure a) (Rational, SymbolDistanceMatrixSquare)
mergeInputFailures =
    \case
        (Just shapeError, Left valueErrors) → Left $ shapeError <> valueErrors
        (Just shapeError, Right _) → Left shapeError
        (Nothing, Left valueErrors) → Left valueErrors
        (Nothing, Right (c, n, v)) →
            let dim = SymbolCount n
                sdms = unsafeFromVectorSquare dim v
            in  Right (c, sdms)


{- |
Checks for the following errors:

  * MatrixDimension0
  * MatrixDimension1
  * NotSquareGrid
-}
checkGridValidity
    ∷ ( Foldable t
      , Foldable t'
      , Real a
      )
    ⇒ t (t' a)
    → Maybe (DiagnosisFailure a)
checkGridValidity grid =
    case toList grid of
        [] → singletonFailure MatrixDimension0
        [_] → singletonFailure MatrixDimension1
        x : _ →
            let rowCount = toEnum $ length grid
                colCount = toEnum $ length x
            in  if rowCount /= colCount
                    then singletonFailure $ NotSquareGrid rowCount colCount
                    else mempty


{- |
Checks for the following errors:

  * ValueNegative
  * ValueOverflow
-}
checkValueRange
    ∷ (Real a)
    ⇒ Word
    → V.Vector a
    → Either (DiagnosisFailure a) (Rational, Word, VS.Vector Word16)
checkValueRange dimension originalValues =
    let overflow ∷ (a, Rational) → Bool
        overflow = let limit = toRational (maxBound ∷ Word16) in \(_, y) → y > limit
        negative ∷ (Ord b, Num b) ⇒ (a, b) → Bool
        negative (_, y) = y < 0
        rationalValues = toRational <$> originalValues
        coefficient = foldl1 lcm $ abs . denominator <$> rationalValues
        prospectiveValues = ((coefficient % 1) *) <$> rationalValues
        negativeValues = fmap fst . V.filter negative $ V.zip originalValues prospectiveValues
        overflowValues = fmap fst . V.filter overflow $ V.zip originalValues prospectiveValues
        coercedVector = VS.convert $ toEnum . fromEnum <$> prospectiveValues

        failure ∷ (Ord a) ⇒ NonEmpty (DiagnosisError a) → Either (DiagnosisFailure a) b
        failure = Left . makeDiagnosisFailure
        vectorToSet = GHC.fromList . toList
    in  case (V.null negativeValues, V.null overflowValues) of
            (True, True) → Right (1 % coefficient, dimension, coercedVector)
            (False, True) → failure . pure $ ValueNegative (vectorToSet negativeValues)
            (True, False) → failure . pure $ ValueOverflow (vectorToSet overflowValues)
            (False, False) →
                failure $
                    ValueNegative (vectorToSet negativeValues)
                        :| [ValueOverflow (vectorToSet overflowValues)]


{- |
\( \mathcal{O} \left( n * \log_2 n \right) \)

Determines the mode length and the other lengths of a nested foldable
structure.
-}
modeAndOutlierLengths ∷ (Foldable f, Foldable t) ⇒ t (f a) → (Word, IntSet)
modeAndOutlierLengths =
    let buildOccuranceMap ∷ (Foldable f, Foldable t) ⇒ t (f a) → Map Int Word
        buildOccuranceMap =
            let occurrence ∷ (Foldable t) ⇒ t a → Map Int Word → Map Int Word
                occurrence e = Map.insertWith (const succ) (length e) (1 ∷ Word)
            in  foldr occurrence mempty

        collateOccuranceMap ∷ (Ord v) ⇒ Map k v → [k]
        collateOccuranceMap =
            let comparator ∷ (Ord v) ⇒ (k, v) → (k, v) → Ordering
                comparator = comparing (Down . snd)
            in  fmap fst . sortBy comparator . Map.assocs

        getResults =
            \case
                [] → error "modeAndOutlierLengths: Empty structure!"
                x : xs → (toEnum x, IS.fromList xs)
    in  getResults . collateOccuranceMap . buildOccuranceMap


singletonFailure ∷ (Applicative f, Ord a) ⇒ DiagnosisError a → f (DiagnosisFailure a)
singletonFailure = pure . makeDiagnosisFailure . pure
