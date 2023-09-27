{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}

module Layout.Special.L1Norm (
    -- * Specialized SDMs
    sdmλ,
    sdmρ2,
    sdmρ3,
    sdmρ4,
    sdmρ5,
    sdmρ6,
    sdmρ7,
    sdmρ8,

    -- * Specialized TCMs
    tcm2Dλ,
    tcm3Dλ,
    tcmρ2,
    tcmρ3,
    tcmρ4,
    tcmρ5,
    tcmρ6,
    tcmρ7,
    tcmρ8,
) where

import Data.Bits
import Data.Coerce
import Data.Word
import Foreign.C.Types (CUInt)
import Layout.Compact.States (StateTransitionsCompact)
import Layout.Compact.Symbols (SymbolDistanceMatrixSquare)
import Layout.Compact.Symbols.Unsafe (unsafeCompactStateFromSDMS, unsafeFromSDMλSquare)
import Layout.Memoize.Dispersion
import Measure.Transition
import Measure.Unit.SymbolChangeCost
import Measure.Unit.SymbolCount
import Measure.Unit.SymbolIndex


{- |
The L1 Norm.
See:
  https://en.wikipedia.org/wiki/Lp_space
-}
sdmλ ∷ SDMλ
sdmλ i j =
    let i' = coerce i ∷ Word
        j' = coerce j ∷ Word
    in  SymbolChangeCost $ max i' j' - min i' j'


sdmρ ∷ Word → SymbolDistanceMatrixSquare
sdmρ = unsafeFromSDMλSquare sdmλ . SymbolCount


sdmρ2, sdmρ3, sdmρ4, sdmρ5, sdmρ6, sdmρ7, sdmρ8 ∷ SymbolDistanceMatrixSquare
sdmρ2 = sdmρ 2
sdmρ3 = sdmρ 3
sdmρ4 = sdmρ 4
sdmρ5 = sdmρ 5
sdmρ6 = sdmρ 6
sdmρ7 = sdmρ 7
sdmρ8 = sdmρ 8


{- |
Definition of the L1 norm metric.
-}
{-# SCC tcm2Dλ #-}
{-# INLINEABLE tcm2Dλ #-}
{-# SPECIALIZE tcm2Dλ ∷ (FiniteBits b) ⇒ SymbolCount → TCM2Dλ b #-}
{-# SPECIALIZE tcm2Dλ ∷ SymbolCount → TCM2Dλ Int #-}
{-# SPECIALIZE tcm2Dλ ∷ SymbolCount → TCM2Dλ CUInt #-}
{-# SPECIALIZE tcm2Dλ ∷ SymbolCount → TCM2Dλ Word #-}
{-# SPECIALIZE tcm2Dλ ∷ SymbolCount → TCM2Dλ Word8 #-}
{-# SPECIALIZE tcm2Dλ ∷ SymbolCount → TCM2Dλ Word16 #-}
{-# SPECIALIZE tcm2Dλ ∷ SymbolCount → TCM2Dλ Word32 #-}
{-# SPECIALIZE tcm2Dλ ∷ SymbolCount → TCM2Dλ Word64 #-}
tcm2Dλ
    ∷ (FiniteBits b)
    ⇒ SymbolCount
    → TCM2Dλ b
tcm2Dλ elementBitWidth =
    let symbolRange = symbolBounds elementBitWidth
    in  bitDispersionPairwise symbolRange sdmλ


{-
tcm2Dλ lhs rhs
  | popCount intersect > 0 = (intersect, 0)
  | otherwise              = bitDispersionPairwise sdmλ lhs rhs
  where
    intersect = lhs .&. rhs
-}

{- |
Definition of the L1 norm metric in three dimensions.
-}
{-# SCC tcm3Dλ #-}
{-# INLINEABLE tcm3Dλ #-}
{-# SPECIALIZE tcm3Dλ ∷ (FiniteBits b) ⇒ SymbolCount → TCM3Dλ b #-}
{-# SPECIALIZE tcm3Dλ ∷ SymbolCount → TCM3Dλ Int #-}
{-# SPECIALIZE tcm3Dλ ∷ SymbolCount → TCM3Dλ CUInt #-}
{-# SPECIALIZE tcm3Dλ ∷ SymbolCount → TCM3Dλ Word #-}
{-# SPECIALIZE tcm3Dλ ∷ SymbolCount → TCM3Dλ Word8 #-}
{-# SPECIALIZE tcm3Dλ ∷ SymbolCount → TCM3Dλ Word16 #-}
{-# SPECIALIZE tcm3Dλ ∷ SymbolCount → TCM3Dλ Word32 #-}
{-# SPECIALIZE tcm3Dλ ∷ SymbolCount → TCM3Dλ Word64 #-}
tcm3Dλ
    ∷ (FiniteBits b)
    ⇒ SymbolCount
    → TCM3Dλ b
tcm3Dλ elementBitWidth =
    let symbolRange = symbolBounds elementBitWidth
    in  bitDispersionThreeway symbolRange sdmλ


tcmρ2, tcmρ3, tcmρ4, tcmρ5, tcmρ6, tcmρ7, tcmρ8 ∷ StateTransitionsCompact
tcmρ2 = unsafeCompactStateFromSDMS 0 sdmρ2
tcmρ3 = unsafeCompactStateFromSDMS 0 sdmρ3
tcmρ4 = unsafeCompactStateFromSDMS 0 sdmρ4
tcmρ5 = unsafeCompactStateFromSDMS 0 sdmρ5
tcmρ6 = unsafeCompactStateFromSDMS 0 sdmρ6
tcmρ7 = unsafeCompactStateFromSDMS 0 sdmρ7
tcmρ8 = unsafeCompactStateFromSDMS 0 sdmρ8
