module Layout.Compact.Class (
    HasStateTransitionsCompact (..),
    FFI2D (),
    FFI3D (),
) where

import Layout.Compact.States


{- |
Any structural representation which can produce a dense, row-major layout,
2D and 3D arrays measuring the 'Measure.Dispersion.Dispersion' between pairs
and triples of states, i.e. produce densely packed 2D and 3D "Transition Cost
Matrices."

The representation returns a @Just@ value /if and only if/ the size of the
symbol alphabet is compatible with FFI string alignment interoperability.

/NOTE:/ Measurability of 'Measure.Dispersion.Dispersion' implies measurability
of 'Measure.Centroid.Centroid' and 'Measure.Distance.Distance'.
-}
class HasStateTransitionsCompact a where
    stateTransitionCompact ∷ a → Maybe StateTransitionsCompact


{- |
/ϴ(1)/ for access for 'stateTransitionCompact'
-}
instance HasStateTransitionsCompact StateTransitionsCompact where
    stateTransitionCompact = Just
