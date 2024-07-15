{- |
Unit-- and property-tests for the 'TransitionMeasureDiagnosis' data-type.
-}
module TestDiscretization (
    testSuite,
) where

import Control.Monad (replicateM)
import Data.Either (isRight)
import Data.Foldable (fold)
import Data.List (intercalate)
import Data.List qualified as L
import Data.Scientific
import Data.Semigroup (Sum (..))
import Measure.Transition (SymbolDistanceλ, symbolDistances)
import Measure.Unit.SymbolCount (symbolBounds)
import Measure.Unit.SymbolIndex (SymbolIndex)
import Test.QuickCheck
import Test.Tasty
-- import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC
import TransitionMatrix.Diagnosis (TransitionMatrix, TransitionMeasureDiagnosis (..), discretizationErrrorTotal, fromRows)
import TransitionMatrix.Diagnosis.Error (DiagnosisFailure)


newtype IncoherentMatrix = IncoherentMatrix [[Scientific]]


instance Arbitrary IncoherentMatrix where
    arbitrary ∷ Gen IncoherentMatrix
    arbitrary = do
        Positive symbols ← succ <$> arbitrary
        mat ← replicateM symbols (vectorOf symbols (arbitrary ∷ Gen (NonNegative Double)))
        pure . IncoherentMatrix $ fmap (fromFloatDigits . getNonNegative) <$> mat


instance Show IncoherentMatrix where
    show (IncoherentMatrix rows) =
        let showRow = ('[' :) . (<> "]") . intercalate ", " . fmap show
        in  intercalate "\n" $ showRow <$> rows


{- |
The test-suite for the 'Alphabet' data type.
-}
testSuite ∷ TestTree
testSuite = testGroup "Matrix Discretization Tests" [testPropertyCases, testExampleCases]


testPropertyCases ∷ TestTree
testPropertyCases =
    testGroup
        "Invariant properties"
        [ fromRowsDiscretization
            fromRowsCorrectApproximation
        ]


fromRowsDiscretization ∷ TestTree
fromRowsDiscretization =
    let fromRows' ∷ IncoherentMatrix → Bool
        fromRows' (IncoherentMatrix rows) =
            isRight
                (fromRows rows ∷ Either (DiagnosisFailure Scientific) (TransitionMeasureDiagnosis Word))
    in  QC.testProperty
            "Ensure 'fromRows' does not fail to discretize"
            fromRows'


fromRowsCorrectApproximation ∷ TestTree
fromRowsCorrectApproximation =
    let fromRowsCorrectApprox' ∷ IncoherentMatrix → Bool
        fromRowsCorrectApprox' (IncoherentMatrix rows) =
            let result ∷ Either (DiagnosisFailure Scientific) (TransitionMeasureDiagnosis Word)
                result = fromRows rows
            in  case result of
                    Left{} → False
                    Right diagnosis →
                        let c = factoredCoefficient diagnosis
                            m = transitionMatrix diagnosis
                            err = maybe 0 discretizationErrrorTotal $ measuredErrorFromDiscretization diagnosis
                            sdm = getSDM m
                            reapply = fmap (fmap ((c *) . toRational))
                            rescaledRows = reapply sdm
                            originalRows = fmap toRational <$> rows

                            diffError =
                                let diffVal ∷ (Num b) ⇒ b → b → Sum b
                                    diffVal x y = Sum . abs $ x - y

                                    contrast ∷ (Num b) ⇒ [b] → [b] → Sum b
                                    contrast xs = fold . zipWith diffVal xs
                                in  getSum . fold $ zipWith contrast originalRows rescaledRows
                        in  diffError == err
    in  QC.testProperty
            "Ensure 'fromRows' correctly approximates values"
            fromRowsCorrectApprox'


getSDM ∷ TransitionMatrix Word → [[Word]]
getSDM transMatrix =
    let start, final ∷ SymbolIndex
        (start, final) = symbolBounds transMatrix

        sdm ∷ SymbolDistanceλ
        sdm = symbolDistances transMatrix

        row ∷ SymbolIndex → Maybe ([Word], SymbolIndex)
        row i
            | i <= final = Just (L.unfoldr (col i) start, succ i)
            | otherwise = Nothing

        col ∷ SymbolIndex → SymbolIndex → Maybe (Word, SymbolIndex)
        col i j
            | j <= final = Just (toEnum . fromEnum $ sdm i j, succ j)
            | otherwise = Nothing
    in  L.unfoldr row start


testExampleCases ∷ TestTree
testExampleCases =
    testGroup
        "Example cases for Matrix Discretization"
        []
