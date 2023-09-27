{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE Strict #-}

module Measure.Transition.States.Class (
    HasStateTransitions (..),
) where

import Data.Bits
import Data.Word (Word64)
import Foreign.C.Types (CUInt)
-- import Layout.Compact.States.Indexing
import Measure.Transition.Types


{- |
Any structural representation which can produce a functions measuring the
'Measure.Dispersion.Dispersion' between pairs and triples of states, i.e.
produce 2D and 3D "Transition Cost Matrices."

/NOTE:/ Measurability of 'Measure.Dispersion.Dispersion' implies measurability
of 'Measure.Centroid.Centroid' and 'Measure.Distance.Distance'.
-}
class HasStateTransitions a e where
    {-# MINIMAL stateTransitionPairwiseDispersion, stateTransitionThreewayDispersion #-}


    stateTransitionPairwiseDispersion :: a -> StateTransitionPairwiseDispersionλ e


    stateTransitionPairwiseDistance :: a -> StateTransitionPairwiseDistanceλ e
    stateTransitionPairwiseDistance = ((fst .) .) . stateTransitionPairwiseDispersion


    stateTransitionPairwiseCentroid :: a -> StateTransitionPairwiseCentroidλ e
    stateTransitionPairwiseCentroid = ((snd .) .) . stateTransitionPairwiseDispersion


    stateTransitionThreewayDispersion :: a -> StateTransitionThreewayDispersionλ e


    stateTransitionThreewayDistance :: a -> StateTransitionThreewayDistanceλ e
    stateTransitionThreewayDistance = (((fst .) .) .) . stateTransitionThreewayDispersion


    stateTransitionThreewayCentroid :: a -> StateTransitionThreewayCentroidλ e
    stateTransitionThreewayCentroid = (((snd .) .) .) . stateTransitionThreewayDispersion
