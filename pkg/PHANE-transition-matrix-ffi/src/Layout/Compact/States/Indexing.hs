{- |
Allocate matricies across the FFI.
-}
module Layout.Compact.States.Indexing (
    StateTransitionsCompact (),

    -- * Indexing

    -- ** Dispersion
    both2D,
    both3D,

    -- ** Distance
    cost2D,
    cost3D,
    costSymbol,

    -- ** Median
    mean2D,
    mean3D,

    -- * Indexing via Bits interface

    -- (less preferable)

    -- ** Dispersion
    both2D',
    both3D',

    -- ** Distance
    cost2D',
    cost3D',

    -- ** Median
    mean2D',
    mean3D',
) where

import Layout.Compact.States.Structure

