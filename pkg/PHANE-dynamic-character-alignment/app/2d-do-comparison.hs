{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Bio.DynamicCharacter
import Data.Alphabet
import Data.Alphabet.Codec
import Data.Alphabet.IUPAC
import Data.Bimap qualified as BM
import Data.Bits
import Data.Either (fromRight)
import Data.Foldable
import Data.List (isPrefixOf, uncons)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.MonoTraversable
import Data.Vector.Storable (Vector, (!))
import Data.Vector.Storable qualified as V
import DirectOptimization.Pairwise
import DirectOptimization.Pairwise.Swapping
import DirectOptimization.Pairwise.Ukkonen
import Layout.Compact.Class
import Measure.Transition
import Measure.Unit.SymbolCount
import Measure.Unit.SymbolDistance
import Measure.Unit.SymbolIndex
import NucleotideSequence
import System.Environment (getArgs)
import Test.Tasty
import Test.Tasty.QuickCheck
import TransitionMatrix (TransitionMatrix)
import TransitionMatrix qualified as TCM
import TransitionMatrix.Diagnosis (DiagnosisFailure, TransitionMeasureDiagnosis)
import TransitionMatrix.Diagnosis qualified as TCM


data OperationalMode a
    = Search
    | SearchAgainst a
    | RenderComparison a a
    | TooManyParameters


main ∷ IO ()
main = do
    args ← getArgs
    case parseArgs args of
        Search → performCounterExampleSearch' Nothing
        SearchAgainst char1 → performCounterExampleSearch' $ Just char1
        RenderComparison char1 char2 → performImplementationComparison char1 char2
        TooManyParameters → putStrLn "Expecting only two parameters!"


parseArgs ∷ [String] → OperationalMode SlimDynamicCharacter
parseArgs args =
    case args of
        [] → Search
        arg1 : _ | "--quickcheck-replay" `isPrefixOf` arg1 → Search
        arg1 : xs →
            let char1 = readSequence arg1
            in  case xs of
                    [] → SearchAgainst char1
                    [arg2] → RenderComparison char1 (readSequence arg2)
                    _ → TooManyParameters
    where
        readSequence = encodeStream . fmap ((iupacToDna .!.) . pure . pure)

        encodeStream =
            let f = encodeState alphabet (const . bit $ fromEnum gapIndex)
                g x = (x, x, x)
                h (x, y, z) = (V.fromList x, V.fromList y, V.fromList z)
            in  h . unzip3 . fmap (g . f)

        (.!.)
            ∷ BM.Bimap (NonEmpty String) (NonEmpty String)
            → NonEmpty String
            → NonEmpty String
        (.!.) bm k = fromMaybe (error $ "Bimap left key not found: " <> show k) $ BM.lookup k bm


performCounterExampleSearch' ∷ Maybe SlimDynamicCharacter → IO ()
performCounterExampleSearch' valueMay =
    case valueMay of
        Nothing → makeMain . testProperty preamble $ uncurry counterExampleCheck
        Just char → makeMain . testProperty preamble $ counterExampleCheck (NS char)
    where
        preamble = "Performing stochastic counter-example search"
        makeMain = defaultMain . localOption (QuickCheckTests 1000000) . localOption (QuickCheckShowReplay True)


counterExampleCheck ∷ NucleotideSequence → NucleotideSequence → Property
counterExampleCheck x@(NS lhs) y@(NS rhs) =
    let message = unlines [show x, show y]
    in  idempotentIOProperty $ do
            putStrLn message
            pure . uncurry counterexample $ gatherContexts lhs rhs


performImplementationComparison ∷ SlimDynamicCharacter → SlimDynamicCharacter → IO ()
performImplementationComparison lhs rhs = putStrLn . fst $ gatherContexts lhs rhs


gatherContexts
    ∷ SlimDynamicCharacter
    → SlimDynamicCharacter
    → (String, Bool)
gatherContexts lhs rhs = (contextRendering, contextSameness)
    where
        contextSameness = sameAlignment $ snd <$> contexts

        contextRendering = renderContexts (NS lhs) (NS rhs) contexts

        contexts =
            [ ("Using C FFI", foreignDOResult)
            , ("Unboxed Swapping", swappingDOResult)
            , ("Unboxed Ukkonen Swap", ukkonenSwapDOResult)
            ]

        extrema = maxEdit tcm
        dispersion = stateTransitionPairwiseDispersion tcm

        denseTCM = fromJust $ stateTransitionCompact tcm

        foreignDOResult = slimPairwiseDO denseTCM lhs rhs
        swappingDOResult = swappingDO dispersion lhs rhs
        ukkonenSwapDOResult = ukkonenDO extrema dispersion lhs rhs


sameAlignment ∷ (Foldable t, Eq c) ⇒ t (c, SlimDynamicCharacter) → Bool
sameAlignment v =
    case uncons $ toList v of
        Nothing → True
        Just ((c, a), xs) →
            let sameCost = all ((== c) . fst) xs
                sameLen = all ((== characterLength a) . characterLength . snd) xs
                sameStr = all ((== a) . snd) xs
            in  sameCost && (not sameLen || sameStr)


renderContexts
    ∷ ( Eq c
      , Foldable f
      , Functor f
      , Show c
      )
    ⇒ NucleotideSequence
    → NucleotideSequence
    → f (String, (c, SlimDynamicCharacter))
    → String
renderContexts m n xs = unlines . (\x → [prefix] <> x <> [suffix]) . fmap f $ toList xs
    where
        f (s, c) = s <> "\n" <> renderResult c
        renderResult (cost, aligned) =
            unlines
                [ "  Cost     : " <> show cost
                , "  Alignment: " <> renderSlimDynamicCharacter alphabet tcm aligned
                , "  Shown Obj: " <> show aligned
                ]

        prefix = show (m, n)
        suffix
            | sameAlignment $ snd <$> xs = "[!] Results MATCH"
            | otherwise = "[X] Results DO NOT MATCH"

        renderSlimDynamicCharacter _ _ _ = "<TODO>"


alphabet ∷ Alphabet String
alphabet = fromSymbols $ "-" :| ["A", "C", "G", "T"]


costStructure ∷ (Ord a, Enum a) ⇒ a → a → a
-- costStructure i j = if i /= j then 1 else 0
-- costStructure i j = max i j - min i j
costStructure i j
    | i == j = toEnum 0
    | i == toEnum (fromEnum gapIndex) = toEnum 1
    | j == toEnum (fromEnum gapIndex) = toEnum 1
    | otherwise = toEnum 2


tcm ∷ TransitionMatrix SlimState
tcm =
    let limit = toEnum 5
        start = toEnum 0

        row ∷ SymbolIndex → Maybe ([Word], SymbolIndex)
        row i
            | i < limit = Just (List.unfoldr (col i) start, succ i)
            | otherwise = Nothing

        col ∷ SymbolIndex → SymbolIndex → Maybe (Word, SymbolIndex)
        col i j
            | j < limit = Just (toEnum . fromEnum $ costStructure i j, succ j)
            | otherwise = Nothing
    in  case TCM.fromRows $ List.unfoldr row start of
            Left err → error $ show err
            Right diagnosis → TCM.transitionMatrix diagnosis
