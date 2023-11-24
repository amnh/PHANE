{- |
Tests for the evaluation sub-library of the PHANE Project.
-}
module Main (main) where

import PHANE.Evaluation.ErrorPhase.Test qualified as ErrorPhase
import PHANE.Evaluation.Test qualified as Evaluation
import Test.Tasty
import Test.Tasty.Ingredients.Rerun (rerunningTests)


{- |
Executable entry point for the evaluation sub-library's test suite.
-}
main ∷ IO ()
main = testSuite >>= defaultMainWithIngredients [rerunningTests defaultIngredients]


{- |
Test suite for the evaluation sub-library.
-}
testSuite ∷ IO TestTree
testSuite =
    Evaluation.testSuite >>= \e →
        pure $ testGroup "Evaluation Tests" [ErrorPhase.testSuite, e]
