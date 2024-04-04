{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Strict #-}

{- |
Functionality for coverting the serializable representation data-type 'FileFormatTCM' to and from a nrmalized represnetation for pragmatic use.
-}
module File.Format.TransitionCostMatrix.Codec (
    -- * Codec conversions for 'FileFormatTCM'
    extractFileFormatTCM,
    implantFileFormatTCM,

    -- * Data-types
    DiscretizationOverflowError (..),
) where

import Data.Alphabet
import Data.Foldable (toList)
import Data.Foldable1 (Foldable1 (..), foldl1')
import Data.List (elemIndex, unfoldr)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Matrix qualified as BM
import Data.Matrix.Generic qualified as GM
import Data.Matrix.Unboxed (Matrix)
import Data.Matrix.Unboxed qualified as UM
import Data.Ratio (denominator, numerator, (%))
import Data.Semigroup (Arg (..))
import Data.Text.Short (ShortText)
import Data.Vector.Generic qualified as GV
import Data.Vector.Unboxed (Unbox)
import File.Format.TransitionCostMatrix.Types
import Measure.Unit.SymbolChangeCost
import Numeric.Natural


data DiscretizationOverflowError = DiscretizationOverflowError
    { discreteLimit ∷ Natural
    , numeratorGCD ∷ Natural
    , denominatorLCM ∷ Natural
    , overflowedValues ∷ NonEmpty (Arg (Int, Int) (Rational, Natural))
    }


{- |
Extract from a 'FileFormatTCM' the corresponding:

  * $\Sigma\,\colon\quad$ 'Alphabet' of symbols with $n = \left\lvert\Sigma\right\rvert$
  * $\mathbf{q}\,\colon\quad$The 'Rational' coefficient which "integerizes" the input matrix.
  * $\mathcal{M}\,\colon\quad$ Symbol Change Matrix (SCM) ($n \times n$) of pairwise 'SymbolChangeCost' values.

This codec performs the following important conversions:

  1. /If and only if/ the gap character $\mathrm{\textemdash} \in\Sigma$,
     then the output $\mathcal{M}$ will have the last row and column moved to the first row and column,
     effectively changing the gap index from $n - 1$ in the input SCM to index $0$ in the output $\mathcal{M}$.

  2. "Integerize" the input matrix $\mathtt{A}_{\mathsf{SCM}}$ by factoring out a rational coefficient $\mathbb{q}$ such that,
     $\mathbf{q} * \mathcal{M} = \mathtt{A}_{\mathsf{SCM}}$.
-}
extractFileFormatTCM
    ∷ FileFormatTCM → Either DiscretizationOverflowError (Alphabet ShortText, Rational, Matrix SymbolChangeCost)
extractFileFormatTCM (FileFormatTCM symbols matrixA) =
    let numSymbols = length symbols
        dimMatrix = BM.rows matrixA
        equalSize = numSymbols == dimMatrix

        -- Conditionally add the implict gap in last column
        symbolSuffix
            | equalSize = id
            | otherwise = (<> pure "-")

        symbols' = symbolSuffix symbols

        alphabet = fromSymbols symbols'

        -- Transformed matrix A with the input gap column and row moved to 'gapIndex'
        matrixB = case elemIndex "-" $ toList symbols' of
            -- Input symbols do **NOT** contain a gap symbol
            -- No need to transform columns
            Nothing → matrixA
            -- Input symbols does contain a gap symbol
            Just i → gapIndexTransform i matrixA

        attachAlphabet
            ∷ (Rational, Matrix SymbolChangeCost)
            → (Alphabet ShortText, Rational, Matrix SymbolChangeCost)
        attachAlphabet (coefficient, matrixC) = (alphabet, coefficient, matrixC)
    in  attachAlphabet <$> integerize matrixB


implantFileFormatTCM ∷ (Integral n, Unbox n) ⇒ Alphabet ShortText → Rational → Matrix n → FileFormatTCM
implantFileFormatTCM alphabet coefficient discretized =
    let symbolλ ∷ NonEmpty ShortText → NonEmpty ShortText
        matrixλ ∷ BM.Matrix Rational → BM.Matrix Rational
        (symbolλ, matrixλ) = case gapSymbol alphabet of
            Nothing → (id, id)
            Just _ → (NE.fromList . NE.tail, implode . rotateHeadRowAndColumn . explode)

        symbols ∷ NonEmpty ShortText
        symbols = symbolλ $ alphabetSymbols alphabet

        ratMatrix = BM.map ((coefficient *) . (% 1) . toInteger) $ GM.convert discretized
        valMatrix = matrixλ ratMatrix
    in  FileFormatTCM
            { customAlphabet = symbols
            , transitionCosts = valMatrix
            }


integerize ∷ BM.Matrix Rational → Either DiscretizationOverflowError (Rational, Matrix SymbolChangeCost)
integerize ratM =
    let dimensions@(n, _) = BM.dim ratM
        dimRange = [0 .. n - 1]

        originalValues ∷ NonEmpty ((Int, Int), Rational)
        originalValues =
            NE.fromList
                [((i, j), BM.unsafeIndex ratM (i, j)) | i ← dimRange, j ← dimRange]

        rationalValues ∷ NonEmpty Rational
        rationalValues = snd <$> originalValues

        limit ∷ Natural
        limit = fromIntegral (maxBound ∷ SymbolChangeCost)

        overflow ∷ (a, Natural) → Bool
        overflow (_, y) = y > limit

        getNumerator ∷ Rational → Natural
        getNumerator = fromInteger . abs . numerator

        getDenominator ∷ Rational → Natural
        getDenominator = fromInteger . abs . denominator

        numGCD ∷ Natural
        numGCD = foldl1' gcd $ getNumerator <$> rationalValues

        denLCM ∷ Natural
        denLCM = foldl1' lcm $ getDenominator <$> rationalValues

        coefficient ∷ Rational
        coefficient = toInteger numGCD % toInteger denLCM

        makeNatural ∷ Rational → Natural
        makeNatural rat =
            let num = getNumerator rat
                den = getDenominator rat
                -- NOTE:
                --   1.  All numerators are divisible by numGCD!
                --   2.  All denominators evenly divide denLCM!
                -- These two "integral division with truncation" operations
                -- /do not result/ in any loss of precision,
                -- because the remainder of the operation is provably 0
                val1 = num `div` numGCD
                val2 = denLCM `div` den
            in  val1 * val2

        prospectiveValues ∷ NonEmpty Natural
        prospectiveValues = makeNatural <$> rationalValues

        makeErrorPoint ∷ (((Int, Int), Rational), Natural) → Arg (Int, Int) (Rational, Natural)
        makeErrorPoint ((loc, rat), nat) = Arg loc (rat, nat)

        overflowValues ∷ [Arg (Int, Int) (Rational, Natural)]
        overflowValues = fmap makeErrorPoint . NE.filter overflow $ NE.zip originalValues prospectiveValues
    in  case overflowValues of
            x : xs →
                Left $
                    DiscretizationOverflowError
                        { discreteLimit = limit
                        , numeratorGCD = numGCD
                        , denominatorLCM = denLCM
                        , overflowedValues = x :| xs
                        }
            _ →
                let scm = UM.fromList dimensions . toList $ fromIntegral <$> prospectiveValues
                    weight = coefficient
                in  Right (weight, scm)


gapIndexTransform ∷ Int → BM.Matrix Rational → BM.Matrix Rational
gapIndexTransform oldGapCol inM =
    let newGapCol ∷ Int
        newGapCol = fromEnum gapIndex

        n ∷ Int
        n = BM.cols inM

        get = BM.unsafeIndex inM

        indexing ∷ Int → (Int, Int)
        indexing i = i `divMod` n

        gen ∷ Int → Maybe (Rational, Int)
        gen k = fmap (\v → (v, k + 1)) . go $ indexing k

        go ∷ (Int, Int) → Maybe Rational
        go (i, j)
            | i >= n || j >= n = Nothing
            | otherwise =
                let f x = case newGapCol `compare` x of
                        LT → x
                        EQ → oldGapCol
                        GT | oldGapCol <= x → x
                        GT → x - 1
                in  Just $ get (f i, f j)
    in  case oldGapCol `compare` newGapCol of
            EQ → inM
            _ → case oldGapCol `compare` (n - 1) of
                EQ → implode . rotateLastRowAndColumn $ explode inM
                _ → BM.fromList (n, n) $ unfoldr gen 0


{- |
Logically simpler transform than the generation method within 'gapIndexTransform'.
-}
rotateLastRowAndColumn ∷ (Foldable1 f, Foldable1 t) ⇒ f (t e) → NonEmpty (NonEmpty e)
rotateLastRowAndColumn =
    let initLast' ∷ NonEmpty a → ([a], a)
        initLast' (x :| xs) = case xs of
            [] → ([], x)
            y : ys →
                let (xs', z) = initLast' $ y :| ys
                in  (x : xs', z)

        rotRow ∷ (Foldable1 f) ⇒ f e → NonEmpty e
        rotRow input =
            let nel@(r :| rs) = toNonEmpty input
            in  case rs of
                    [] → nel
                    _ →
                        let (rI, rL) = initLast' $ r :| rs
                        in  rL :| rI
    in  fmap rotRow . rotRow


{- |
Logically simpler transform than the generation method within 'gapIndexTransform'.
-}
rotateHeadRowAndColumn ∷ (Foldable1 f, Foldable1 t) ⇒ f (t e) → NonEmpty (NonEmpty e)
rotateHeadRowAndColumn =
    let rotRow ∷ ∀ g e. (Foldable1 g) ⇒ g e → NonEmpty e
        rotRow input =
            let nel@(r :| rs) = toNonEmpty input
                go ∷ [e] → [e]
                go = \case
                    [] → [r]
                    x : xs → x : go xs
            in  case rs of
                    [] → nel
                    e : es → e :| go es
    in  fmap rotRow . rotRow


explode ∷ (GV.Vector v e) ⇒ GM.Matrix v e → NonEmpty (NonEmpty e)
explode = NE.fromList . fmap NE.fromList . GM.toLists


implode ∷ (GV.Vector v e) ⇒ NonEmpty (NonEmpty e) → GM.Matrix v e
implode = GM.fromLists . toList . fmap toList
