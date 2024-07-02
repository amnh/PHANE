{- |
Test-suite for the 'Alphabet' data-type.
-}
module Main (
    main,
) where

import Test.Tasty
import Test.Tasty.Ingredients.Rerun (rerunningTests)
import TestDiscretization (testSuite)


{- |
The entry point for the test-suite of the 'Data.Alphabet.Alphabet' data type.
-}
main ∷ IO ()
main = defaultMainWithIngredients [rerunningTests defaultIngredients] testSuite
