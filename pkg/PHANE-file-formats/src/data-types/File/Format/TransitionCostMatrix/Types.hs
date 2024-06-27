{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

{- |
The data-type for serializing TCM file formats to and from IO streams.
-}
module File.Format.TransitionCostMatrix.Types (
    -- * Data-type
    FileFormatTCM (..),
) where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty)
import Data.Matrix (Matrix)
import Data.Text.Short (ShortText)
import GHC.Generics


{- |
The results of a TCM file consisting of

  * A custom alphabet of "Symbols"

  * A matrix consisting of the transition costs between symbols

The following equality will hold for an 'File.Format.TransitionCostMatrix.Types.FileFormatTCM':

> (length . customAlphabet) tcm == (rows . transitionCosts) tcm && (length . customAlphabet) tcm == (cols . transitionCosts) tcm

Note that the 'File.Format.TransitionCostMatrix.Types.transitionCosts` does not need to be a symetic matrix nor have identity values on the matrix diagonal.
-}
data FileFormatTCM = FileFormatTCM
    { customAlphabet ∷ NonEmpty ShortText
    -- ^ The custom alphabet of "Symbols" for which the TCM matrix is defined
    , transitionCosts ∷ Matrix Rational
    -- ^ The cost to transition between any two symbols, square but not necessarily symmetric
    }
    -- n+1 X n+1 matrix where n = length customAlphabet
    deriving stock (Eq, Generic, Show)
    deriving anyclass (NFData)
