{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}

module Layout.Compact.Symbols.Unsafe (
    -- * Unsafe
    unsafeFromSDMλSquare,
    unsafeFromSDMλTriangular,
    unsafeFromVectorSquare,
    unsafeFromVectorTriangular,
    unsafeCompactStateFromSDMS,
) where

import Data.Coerce
import Data.Vector.Storable (Vector, force, generate)
import Layout.Compact.States (StateTransitionsCompact, initialize)
import Layout.Compact.Symbols.Internal (DiscretizedResolution, SymbolDistanceMatrix (..))
import Layout.Compact.Symbols.Square (SymbolDistanceMatrixSquare (..), rowMajorVector)
import Layout.Compact.Symbols.Triangular (SymbolDistanceMatrixTriangular (..))
import Measure.Transition
import Measure.Unit.SymbolCount
import Measure.Unit.SymbolDistance
import Measure.Unit.SymbolIndex


{- |
__Time:__ \( \Theta\left( a^{5} \right) \) where \( a \le 8 \) is the size of the character alphabet.

Generate the 2D and 3D compact state transiton cost matricies from the supplied
symbol distance matrix with linear dimensions of the alphabet symbol count.
-}
unsafeCompactStateFromSDMS
    ∷ SymbolDistance
    -- ^ The gap open cost. A zero value indicates non-affine alignment context
    → SymbolDistanceMatrixSquare
    -- ^ The dense, pre-computed matrix of costs to shift between symbols.
    → StateTransitionsCompact
unsafeCompactStateFromSDMS gapOpenCost sdms =
    let dimension = coerce $ symbolCount sdms
    in  initialize dimension gapOpenCost $ rowMajorVector sdms


unsafeFromSDMλSquare ∷ SDMλ → SymbolCount → SymbolDistanceMatrixSquare
unsafeFromSDMλSquare sdmλ sc@(SymbolCount n) =
    let dim = fromEnum $ n * n
        g i =
            let (q, r) = toEnum i `quotRem` n
            in  fromIntegral . sdmλ (SymbolIndex q) $ SymbolIndex r
    in  coerce . SymbolDistanceMatrix sc . force $ generate dim g


unsafeFromSDMλTriangular ∷ SDMλ → SymbolCount → SymbolDistanceMatrixTriangular
unsafeFromSDMλTriangular sdmλ sc@(SymbolCount n) =
    let dim = fromEnum $ n * n
        g i =
            let (q, r) = toEnum i `quotRem` n
            in  fromIntegral . sdmλ (SymbolIndex q) $ SymbolIndex r
    in  coerce . SymbolDistanceMatrix sc . force $ generate dim g


unsafeFromVectorSquare ∷ SymbolCount → Vector DiscretizedResolution → SymbolDistanceMatrixSquare
unsafeFromVectorSquare = (coerce .) . (. force) . SymbolDistanceMatrix


unsafeFromVectorTriangular ∷ SymbolCount → Vector DiscretizedResolution → SymbolDistanceMatrix
unsafeFromVectorTriangular = (coerce .) . (. force) . SymbolDistanceMatrix
