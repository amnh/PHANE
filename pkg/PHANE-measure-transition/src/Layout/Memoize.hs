{-# LANGUAGE Strict #-}

module Layout.Memoize (
    -- * Measures
    SDMλ,
    TCM2Dλ,
    TCM3Dλ,

    -- * Measure Components

    -- * Smart Constructors
    TransitionMatrix (),
    discreteCrossGap,
    discreteMetric,
    linearNorm,
    metricRepresentation,

    -- * Accessor Type-classes
    HasEditExtrema (..),
    MeasurableRange (..),
    HasStateTransitions (..),
    HasStateTransitionsCompact (..),
    HasSymbolCount (..),
    HasSymbolDistances (..),
) where

import Control.DeepSeq
import Control.Monad (join)
import Data.Bits
import Data.Hashable
import Data.Hashable.Memoize
import Data.Word (Word64)
import Foreign.C.Types (CUInt)
import GHC.Generics
import Layout.Memoize.Dispersion
import Layout.Memoize.Hashable
import Layout.Memoize.States
import Measure.Range
import Measure.Transition
import Measure.Transition.Edits
import Measure.Unit.SymbolCount
import Measure.Unit.SymbolDistance
import Measure.Unit.SymbolIndex


{- |
Create a memoized, sparse state transition matrix from a /square/ symbol
transition matrix.
-}
memoizedStateTransitionsFromSquare
    :: ( FiniteBits a
       , Hashable a
       , NFData a
       )
    => SymbolDistanceMatrixSquare
    -> TransitionMatrix a
memoizedStateTransitionsFromSquare = memoizedStateTransitionsFromSymbolMatrix . Right


{- |
Create a memoized, sparse state transition matrix from a /triangular/ symbol
transition matrix.

/NOTE:/ Uses half the initial storage memory.
-}
memoizedStateTransitionsFromTriangular
    :: ( FiniteBits a
       , Hashable a
       , NFData a
       )
    => SymbolDistanceMatrixTriangle
    -> TransitionMatrix a
memoizedStateTransitionsFromTriangular = memoizedStateTransitionsFromSymbolMatrix . Left


{- |
Create a memoized, sparse state transition matrix from a eeither a square or
triangular symbol transition matrix.
-}
memoizedStateTransitionsFromSymbolMatrix
    :: ( FiniteBits a
       , Hashable a
       , NFData a
       )
    => Either SDMT SDMS
    -> Sparse a
memoizedStateTransitionsFromSymbolMatrix m =
    let both f = either f f
        sdmλ = both symbolDistances m
        maxD = both maxDeletion m
        maxI = both maxInsertion m
        minD = both minDeletion m
        minI = both minInsertion m
        range = both measureRange m
        tcm2D = memoize2 $ Lax.bitDispersionPairwise range sdmλ
        tcm3D = memoize3 $ Lax.bitDispersionThreeway range sdmλ
    in  Sparse maxD maxI minD minI m tcm2D tcm3D
