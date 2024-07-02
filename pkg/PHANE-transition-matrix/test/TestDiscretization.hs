{- |
Unit-- and property-tests for the 'Alphabet' data-ype.
-}
module TestDiscretization (
    testSuite,
) where

import Control.Monad (replicateM)
import Data.Either (isRight)
import Data.List (intercalate)
import Data.Scientific
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC
import TransitionMatrix.Diagnosis (TransitionMeasureDiagnosis, fromRows)
import TransitionMatrix.Diagnosis.Error (DiagnosisFailure)


newtype IncoherentMatrix = IncoherentMatrix [[Scientific]]


instance Arbitrary IncoherentMatrix where
    arbitrary ∷ Gen IncoherentMatrix
    arbitrary = do
        Positive symbols ← arbitrary
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


testExampleCases ∷ TestTree
testExampleCases =
    testGroup
        "Example cases for Matrix Discretization"
        []
