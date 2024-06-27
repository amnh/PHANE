{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE Strict #-}

{- |
Getter "lens" type-class for 'Measure.Distance.Dispersion'.
-}
module Measure.Transition.States (
    HasStateTransitions (..),
) where

import Measure.Transition.Types


{- |
Any structural representation which can produce a functions measuring the
'Measure.Dispersion.Dispersion' between pairs and triples of states, i.e.
produce 2D and 3D "Transition Cost Matrices."

/NOTE:/ Measurability of 'Measure.Dispersion.Dispersion' implies measurability
of 'Measure.Median.Median' and 'Measure.Distance.Distance'.
-}
class HasStateTransitions a e where
    {-# MINIMAL stateTransitionPairwiseDispersion, stateTransitionThreewayDispersion #-}


    stateTransitionPairwiseDispersion ∷ a → StateTransitionPairwiseDispersionλ e


    stateTransitionPairwiseDistance ∷ a → StateTransitionPairwiseDistanceλ e
    stateTransitionPairwiseDistance = ((fst .) .) . stateTransitionPairwiseDispersion


    stateTransitionPairwiseMedian ∷ a → StateTransitionPairwiseMedianλ e
    stateTransitionPairwiseMedian = ((snd .) .) . stateTransitionPairwiseDispersion


    stateTransitionThreewayDispersion ∷ a → StateTransitionThreewayDispersionλ e


    stateTransitionThreewayDistance ∷ a → StateTransitionThreewayDistanceλ e
    stateTransitionThreewayDistance = (((fst .) .) .) . stateTransitionThreewayDispersion


    stateTransitionThreewayMedian ∷ a → StateTransitionThreewayMedianλ e
    stateTransitionThreewayMedian = (((snd .) .) .) . stateTransitionThreewayDispersion
