{- |
Unit-- and property-tests for the 'Alphabet' data-ype.
-}

module Data.Alphabet.Test
    ( testSuite
    ) where

import Data.Alphabet.Internal
import Data.List (nubBy)
import Data.Set (Set)
import Data.Set qualified as Set
import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC


-- |
-- The test-suite for the 'Alphabet' data type.
testSuite :: TestTree
testSuite = testGroup "Alphabet Tests" [ testPropertyCases, testExampleCases ]


testPropertyCases :: TestTree
testPropertyCases = testGroup
    "Invariant properties"
    [ alphabetStateNamesProperties
    , alphabetSymbolsProperties
    , gapSymbolProperties
--    , truncateAtSymbolProperties
--    , truncateAtMaxSymbolProperties
    ]


testExampleCases :: TestTree
testExampleCases = testGroup
    "Example cases for Data.Alphabet"
    [ alphabetDNACases
    , subsetIndex
    ]


alphabetStateNamesProperties :: TestTree
alphabetStateNamesProperties = testGroup
    "Properties of alphabetStateNames"
    [ QC.testProperty "The list of state names is always empty if none are supplied" noStateNames ]
    where
        alphabetStateNames' :: Alphabet String -> [String]
        alphabetStateNames' = alphabetStateNames

        noStateNames :: [String] -> Bool
        noStateNames = null . alphabetStateNames' . fromSymbols


alphabetSymbolsProperties :: TestTree
alphabetSymbolsProperties = testGroup
    "Properties of alphabetSymbols"
    [ QC.testProperty "The list of symbols is the same if we forget the state names" forgetStateNames ]
    where
        forgetStateNames :: [(String, String)] -> Property
        forgetStateNames strs' =
            let alphabetSymbols' :: Alphabet String -> [String]
                alphabetSymbols' = alphabetSymbols

                strs :: [(String, String)]
                strs = nubBy (\p q -> fst p == fst q) strs'

            in  (alphabetSymbols' . fromSymbolsWithStateNames $ strs)
                    === (alphabetSymbols' . fromSymbols . fmap fst $ strs)


gapSymbolProperties :: TestTree
gapSymbolProperties = testGroup
    "Properties of gapSymbol"
    [ QC.testProperty "The gap symbol is always \"-\"" constGapSymbol ]
    where
        constGapSymbol :: [(String, String)] -> Property
        constGapSymbol = (=== "-") . gapSymbol . fromSymbolsWithStateNames


{-
truncateAtSymbolProperties :: TestTree
truncateAtSymbolProperties = testGroup
    "Properties of truncateAtSymbol"
    [ QC.testProperty
        "TruncateAtSymbol y (fromSymbols (xs ++ [y] ++ ys)) == fromSymbols (xs ++ [y])"
        splitOrderedList
    ]
    where
        splitOrderedList :: (NonNegative Int, [String]) -> Property
        splitOrderedList (NonNegative n, strs') =
            let strs = toList $ fromSymbols strs'
            in  case splitAt n strs of
                    (_ , []   ) -> property True
                    (xs, y : _) -> (truncateAtSymbol y . fromSymbols $ strs) === fromSymbols (xs <> [y])


truncateAtMaxSymbolProperties :: TestTree
truncateAtMaxSymbolProperties = testGroup
    "Properties of truncateAtMaxSymbol"
    [QC.testProperty "truncateAtMaxSymbol of the input returns the original alphabet" truncatePreserve]
    where
        truncatePreserve :: [String] -> Property
        truncatePreserve strs = let alpha = fromSymbols strs in truncateAtMaxSymbol strs alpha === alpha
-}


-- Cases for unit tests


alphabetDNAString :: [(String, String)]
alphabetDNAString = [("A", "adenine"), ("C", "cytosine"), ("G", "guanine"), ("T", "thymine")]


{-
alphabetDNAText :: [(T.Text, T.Text)]
alphabetDNAText =
  fmap (\(s1, s2) -> (T.pack s1, T.pack s2)) alphabetDNAString
-}


alphabetDNA :: Alphabet String
alphabetDNA = fromSymbolsWithStateNames alphabetDNAString


alphabetDNACases :: TestTree
alphabetDNACases = testGroup
    (unlines
        [ "Cases for DNA alphabet given by:"
        , "     A   adenine"
        , "     C   cytosine"
        , "     G   guanine"
        , "     T   thymine"
        ]
    )
    [ HU.testCase "The symbols are A, C, G, T and -" symbols1
    , HU.testCase "The state names are adenine, cytosine, guanine, thymine and -" states1
    ]
    where
        symbols1 :: Assertion
        symbols1 = alphabetSymbols alphabetDNA @?= [ "-", "A", "C", "G", "T" ]

        states1 :: Assertion
        states1 = alphabetStateNames alphabetDNA @?= [ "-", "adenine", "cytosine", "guanine", "thymine" ]


subsetIndex :: TestTree
subsetIndex = testGroup
    "Subset Index Tests:"
    [ HU.testCase "getSortedLookup agrees for sorted and unsorted alphabet [0, 1, 2] and input 1" sortedLookup
    , HU.testCase "getSortedLookup agrees for sorted and unsorted alphabet [0, 1, 2] and gap input" gapLookup
    , QC.testProperty
        (unlines
            [ "getSortedLookup agrees for sorted and unsorted alphabets [0..n] for inputs"
            , "      as subsets from [0..i] with i <= n"
            ]
        )
        sortedUnsortedAgree
    , QC.testProperty
        "getSortedLookup agrees for sorted and unsorted alphabets [0..n] for gap input"
        sortedUnsortedGapAgree
    ]
    where
        alphabet' :: Alphabet String
        alphabet' = fromSymbols ["0", "1", "2"]

        sortedAlphabet' :: Alphabet String
        sortedAlphabet' = alphabet' { isSorted = True }

        idx :: Alphabet String -> Set String -> Word
        idx = flip flip 0 . getSubsetIndex

        sortedLookup :: Assertion
        sortedLookup =
            idx alphabet' (Set.singleton "1") @?= idx sortedAlphabet' (Set.singleton "1")

        gapLookup :: Assertion
        gapLookup =
            let makeSet :: Alphabet String -> Set String
                makeSet = Set.singleton . gapSymbol
            in  idx alphabet' (makeSet alphabet') @?= idx sortedAlphabet' (makeSet sortedAlphabet')

        --  This is a more elaborate version of the above version making sure
        --  both the sorted and unsorted branches agree for a given ambiguity group.
        sortedUnsortedAgree :: Int -> Int -> Property
        sortedUnsortedAgree i n =
            let symbols          = fmap show [0 .. n]
                unsortedAlphabet = fromSymbols symbols
                sortedAlphabet   = unsortedAlphabet { isSorted = True }
                -- generator for a subset of the list [0..i]
                groupGen         = Set.fromList <$> sublistOf (fmap show [0 .. i])
            in  (i <= n) ==> forAll groupGen $ \ambGroup ->
                idx unsortedAlphabet ambGroup === idx sortedAlphabet ambGroup
            -- Make sure the pre-condition holds that i is less than or equal to n.

        sortedUnsortedGapAgree :: Int -> Property
        sortedUnsortedGapAgree n =
            let symbols        = fmap show [0 .. n]
                unsortedAlphabet = fromSymbols symbols
                sortedAlphabet = unsortedAlphabet { isSorted = True }
                gap            = Set.singleton $ gapSymbol sortedAlphabet
            in  idx unsortedAlphabet gap === idx sortedAlphabet gap
            -- Test sorted and unsorted lookup is the same for gap character
