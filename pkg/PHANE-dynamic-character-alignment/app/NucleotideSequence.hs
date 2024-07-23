{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NucleotideSequence (
    NucleotideBase (..),
    NucleotideBasePair (..),
    NucleotideSequence (..),
) where

import Bio.DynamicCharacter
import Bio.DynamicCharacter.Element
import Control.Arrow ((&&&), (***))
import Data.Alphabet
import Data.Alphabet.Codec
import Data.Alphabet.IUPAC
import Data.Bimap qualified as B
import Data.Bits
import Data.Foldable
import Data.List (delete)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map, lookup)
import Data.String (fromString)
import Data.Vector.Storable (Vector, (!))
import Data.Vector.Storable qualified as V
import Measure.Transition
import Measure.Unit.SymbolCount
import Test.QuickCheck (Arbitrary (..), vector)
import Test.QuickCheck.Modifiers (Positive (..))
import TransitionMatrix
-- import Test.SmallCheck.Series hiding (NonEmpty)
import Prelude hiding (lookup)


{- |
Represents an arbitrary, non-empty ambiguity group which may include gaps.
-}
newtype NucleotideBase = NB (SlimState, SlimState, SlimState)
    deriving newtype (Eq, Ord)


{- |
A pair of 'NucleotideBase' values.
-}
newtype NucleotideBasePair = NBP (NucleotideBase, NucleotideBase)
    deriving newtype (Eq, Ord)


{- |
Represents an arbitrary, non-empty sequence of nucleotide bases that may be
ambiguous and/or include gaps.
-}
newtype NucleotideSequence = NS SlimDynamicCharacter


instance Arbitrary NucleotideSequence where
    arbitrary =
        let f (x, y, z) = (SlimState x, SlimState y, SlimState z)
        in  do
                len ← getPositive <$> arbitrary
                NS . buildSequence . fmap f <$> vector len


instance Show NucleotideSequence where
    show (NS s@(x, y, z)) =
        let grabMedian ∷ String → String
            grabMedian = pure . head . tail

            rendering ∷ [String]
            rendering = renderBase s <$> [0 .. V.length x - 1]

            shownContext = fold rendering

            shownDNA = foldMap grabMedian rendering
        in  fold ["(", shownDNA, ",", shownContext, ")"]


instance Show NucleotideBase where
    show (NB x) = renderBase (buildSequence [x]) 0


buildSequence ∷ [(SlimState, SlimState, SlimState)] → SlimDynamicCharacter
buildSequence vals =
    let (x, y, z) = unzip3 vals
    in  (V.fromList x, V.fromList y, V.fromList z)


{-
instance (Monad m) => Serial m NucleotideBase where
    series = generate $ const (NB <$> validNucleotideElements)

instance (Monad m) => Serial m NucleotideBasePair where
    series = generate $ const validPairs
        where
            validPairs =
                [ NBP (NB x, NB y)
                | x <- validNucleotideElements
                , y <- validNucleotideElements
                , x <= y
                ]
-}

validNucleotideElements ∷ [(SlimState, SlimState, SlimState)]
validNucleotideElements =
    fold
        [ delVals
        , insVals
        , medVals
        ]
    where
        gap = bit $ fromEnum gapIndex

        tcm ∷ TransitionMatrix SlimState
        tcm = discreteMetric $ symbolCount alphabet

        med = stateTransitionPairwiseMedian tcm

        validMedians ∷ [Maybe SlimState]
        validMedians =
            fmap (Just . encodeState alphabet (const zeroBits) . NE.fromList) $
                [] `delete` powerSet (toList alphabet)

        delVals ∷ [(SlimState, SlimState, SlimState)]
        delVals = buildElem Nothing <$> validMedians

        insVals ∷ [(SlimState, SlimState, SlimState)]
        insVals = flip buildElem Nothing <$> validMedians

        medVals ∷ [(SlimState, SlimState, SlimState)]
        medVals = [buildElem x y | x ← validMedians, y ← validMedians, x <= y]

        powerSet ∷ [a] → [[a]]
        powerSet [] = [[]]
        powerSet (x : xs) = [x : ps | ps ← powerSet xs] <> powerSet xs

        buildElem
            ∷ Maybe SlimState
            → Maybe SlimState
            → (SlimState, SlimState, SlimState)
        buildElem = \case
            Nothing → \case
                Nothing → (zeroBits, zeroBits, zeroBits)
                Just y → (y `xor` y, med gap y, y)
            Just x → \case
                Nothing → (x, med x gap, x `xor` x)
                Just y → (x, med x y, y)


alphabet ∷ Alphabet String
alphabet = fromSymbols $ "-" :| ["A", "C", "G", "T"]


decodeBase ∷ SlimState → Maybe Char
decodeBase v =
    let dnaIUPAC ∷ Map (NonEmpty String) Char
        dnaIUPAC = convertBimap iupacToDna

        convertBimap ∷ B.Bimap (NonEmpty String) (NonEmpty String) → Map (NonEmpty String) Char
        convertBimap = fmap (head . NE.head) . B.toMapR . B.mapR (fmap fromString)
    in  case popCount v of
            0 → Just '█'
            _ → decodeState alphabet v `lookup` dnaIUPAC


renderBase ∷ SlimDynamicCharacter → Int → String
renderBase s@(x, y, z) i =
    let errorMsg v =
            error $
                unlines
                    [ "Could not find key for:"
                    , show v
                    , show (decodeState alphabet v ∷ [String])
                    ]

        gap = bit $ fromEnum gapIndex

        (pref, median, lVal, rVal)
            | s `isGapped` i = ('G', gap, gap, gap)
            | s `isDelete` i = ('D', gap, y ! i, z ! i)
            | s `isInsert` i = ('I', x ! i, y ! i, gap)
            | otherwise = ('A', x ! i, y ! i, z ! i)
    in  case decodeBase median of
            Nothing → errorMsg median
            Just a →
                case decodeBase lVal of
                    Nothing → errorMsg lVal
                    Just b →
                        case decodeBase rVal of
                            Nothing → errorMsg rVal
                            Just c → [pref, a, b, c]
