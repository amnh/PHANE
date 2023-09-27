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

    -- ** Centroid
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

    -- ** Centroid
    mean2D',
    mean3D',
) where

import Layout.Compact.States.Structure

