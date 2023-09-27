{- |


Construction of a dense transition cost matrix 'StateTransitionsCompact'

/O(a^5)/ where /a/ is the size of the character alphabet
-}
module Layout.Compact.States (
    -- * Compact Representation of State Transitions
    StateTransitionsCompact (..),

    -- * FFI TCM Components
    FFI2D (),
    FFI3D (),

    -- * FFI Alignment Method
    AlignmentStrategy (..),

    -- * Resultant Measure
    TCMρ,

    -- * Constant
    maximumDimension,

    -- * Construction
    initialize,
    --  , fromSymbolDistanceMatrixSquare
    --  , fromSDMλ

    -- * Accessors
    getAlignmentStrategy,
    getFFI2D,
    getFFI3D,

    -- * Rendering
    renderMatrix,
    renderSummary,
) where

import Foreign (Ptr)
import Layout.Compact.States.Allocation
import Layout.Compact.States.Structure


{- |
/ϴ(1)/

Extract sub-component required for C FFI to perform alignment between /two/
sequences.
-}
getFFI2D ∷ StateTransitionsCompact → Ptr FFI2D
getFFI2D = matrix2D


{- |
/ϴ(1)/

Extract sub-component required for C FFI to perform alignment between /three/
sequences.
-}
getFFI3D ∷ StateTransitionsCompact → Ptr FFI3D
getFFI3D = matrix3D
