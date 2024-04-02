{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

{- |
The data-type for serializing TCM file formats to and from IO streams.
-}
module File.Format.TransitionCostMatrix.Codec (
    -- * Data-type
    extractFileFormatTCM,
    implantFileFormatTCM,
) where

import Control.DeepSeq (NFData)
import Data.Alphabet
import Data.List.NonEmpty (NonEmpty)
import Data.Matrix qualified as BM
import Data.Matrix.Unboxed (Matrix)
import Data.Matrix.Unboxed qualified as UM
import Data.Text.Short (ShortText)
import File.Format.TransitionCostMatrix.Types
import GHC.Generics
import Measure.Unit.SymbolChangeCost


{-
For reference

data FileFormatTCM = FileFormatTCM
    { customAlphabet ∷ NonEmpty ShortText
    -- ^ The custom alphabet of "Symbols" for which the TCM matrix is defined
    , transitionCosts ∷ Matrix Rational
    -- ^ The cost to transition between any two symbols, square but not necessarily symmetric
    }
    -- n+1 X n+1 matrix where n = length customAlphabet
    deriving stock (Eq, Generic, Show)
    deriving anyclass (NFData)
-}

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
extractFileFormatTCM ∷ FileFormatTCM → (Alphabet ShortText, Rational, Matrix SymbolChangeCost)
extractFileFormatTCM = undefined


implantFileFormatTCM ∷ (Integral n) ⇒ Alphabet ShortText → Rational → Matrix n → FileFormatTCM
implantFileFormatTCM = undefined
