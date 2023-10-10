{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedSums #-}

module TransitionMatrix.Representation (
    TransitionMatrix (..),
) where

import Bio.DynamicCharacter.Element (SlimState, WideState)
import Control.DeepSeq
import Data.BitVector.LittleEndian (BitVector)
import Data.Bits
import Data.Coerce
import Data.Hashable
import GHC.Generics
import Layout.Compact.Class
import Layout.Compact.States
import Layout.Memoize.States
import Layout.Special.Type
import Measure.Range
import Measure.Transition
import Measure.Unit.SymbolCount
import Measure.Unit.SymbolIndex


{- |
Represents the metric for some discrete characters or dynamic characters.
The representation is highly optimized for both time and space efficiency.
Because of this, the term "compact" is used to describe the representation.

If any measure has an 'iota' symbol count, i.e. a symbol count of less than
or equal to 'infimumSymbolLimit', the compact representation will pre-compute
the entire transition cost matrix in a structure which is 'Storable' and
inter-operable with the C FFI. If and only if the measure is specified for an
'iota' symbol count will 'stateTransitionCompact' return a @Just@ value.

Additionally, the compact representation notes if the discrete metric, the
discrete metric adjoined by the gap symbol, or the L1 norm are the specified
measure. If any of these metrics are specified, specialized functions which
are more efficient will be returned when calling 'symbolDistances', 'stateTransitionPairwiseDispersion', and
'getStateTransitionCubeλ'.

Notably, if it is the case that /both/ the measure has an 'iota' symbol count
/and/ the measure is a specialized metric described above, then calling 'stateTransitionCompact'
returns a /compile-time/ pre-computed transition cost matrix. Multiple
"constructions" of the same metric will result in the same "singlton" compact
representation.

Finally, if a specified measure has more than an 'iota' symbol count /and/ is
not one of the specialized metrics described above, then a /sparse/ and
/memoized/ representation is stored.

It is important to use this type in the metadata decorations rather than store
a function because a function cannot be contained in a compact region.

Use the following accessor to the retrieve the desired functionality:
  * 'symbolDistances'
  * 'stateTransitionPairwiseDispersion'
  * 'getStateTransitionCubeλ'
  * 'stateTransitionCompact'
-}
data TransitionMatrix a
    = IotaMatrix {-# UNPACK #-} (Maybe SpecializableMetric) {-# UNPACK #-} StateTransitionsCompact
    | VastMatrix {-# UNPACK #-} (Sparse a)
    | VastSpecialized {-# UNPACK #-} SpecializableMetric
    deriving stock (Generic)
    deriving anyclass (NFData)


instance Eq (TransitionMatrix a) where
    {-# SPECIALIZE instance Eq (TransitionMatrix BitVector) #-}
    {-# SPECIALIZE instance Eq (TransitionMatrix SlimState) #-}
    {-# SPECIALIZE instance Eq (TransitionMatrix WideState) #-}


    (==) (VastMatrix x) (VastMatrix x') = x == x'
    (==) (VastSpecialized x) (VastSpecialized x') = x == x'
    (==) (IotaMatrix (Just x) _) (IotaMatrix (Just x') _) = x == x'
    (==) (IotaMatrix Nothing x) (IotaMatrix Nothing x') = x == x'
    (==) _ _ = False


instance HasEditExtrema (TransitionMatrix a) where
    {-# SPECIALIZE instance HasEditExtrema (TransitionMatrix BitVector) #-}
    {-# SPECIALIZE instance HasEditExtrema (TransitionMatrix SlimState) #-}
    {-# SPECIALIZE instance HasEditExtrema (TransitionMatrix WideState) #-}


    {-# INLINEABLE maxEdit #-}
    maxEdit (IotaMatrix _ mtx) = maxEdit mtx
    maxEdit (VastMatrix mtx) = maxEdit mtx
    maxEdit (VastSpecialized met) = maxEdit met


    maxDeletion (IotaMatrix _ mtx) = maxDeletion mtx
    maxDeletion (VastMatrix mtx) = maxDeletion mtx
    maxDeletion (VastSpecialized met) = maxDeletion met


    maxInsertion (IotaMatrix _ mtx) = maxInsertion mtx
    maxInsertion (VastMatrix mtx) = maxInsertion mtx
    maxInsertion (VastSpecialized met) = maxInsertion met


    {-# INLINEABLE minEdit #-}
    minEdit (IotaMatrix _ mtx) = minEdit mtx
    minEdit (VastMatrix mtx) = minEdit mtx
    minEdit (VastSpecialized met) = minEdit met


    minDeletion (IotaMatrix _ mtx) = minDeletion mtx
    minDeletion (VastMatrix mtx) = minDeletion mtx
    minDeletion (VastSpecialized met) = minDeletion met


    minInsertion (IotaMatrix _ mtx) = minInsertion mtx
    minInsertion (VastMatrix mtx) = minInsertion mtx
    minInsertion (VastSpecialized met) = minInsertion met


instance HasSymbolDistances (TransitionMatrix a) where
    {-# SPECIALIZE instance HasSymbolDistances (TransitionMatrix BitVector) #-}
    {-# SPECIALIZE instance HasSymbolDistances (TransitionMatrix SlimState) #-}
    {-# SPECIALIZE instance HasSymbolDistances (TransitionMatrix WideState) #-}


    symbolDistances (IotaMatrix _ mtx) = symbolDistances mtx
    symbolDistances (VastMatrix mtx) = symbolDistances mtx
    symbolDistances (VastSpecialized met) = symbolDistances met


instance HasSymbolCount (TransitionMatrix a) where
    {-# SPECIALIZE instance HasSymbolCount (TransitionMatrix BitVector) #-}
    {-# SPECIALIZE instance HasSymbolCount (TransitionMatrix SlimState) #-}
    {-# SPECIALIZE instance HasSymbolCount (TransitionMatrix WideState) #-}


    symbolCount (IotaMatrix _ mtx) = coerce $ matrixSize mtx
    symbolCount (VastMatrix mtx) = symbolCount mtx
    symbolCount (VastSpecialized met) = symbolCount met


instance HasStateTransitionsCompact (TransitionMatrix a) where
    {-# SPECIALIZE instance HasStateTransitionsCompact (TransitionMatrix BitVector) #-}
    {-# SPECIALIZE instance HasStateTransitionsCompact (TransitionMatrix SlimState) #-}
    {-# SPECIALIZE instance HasStateTransitionsCompact (TransitionMatrix WideState) #-}


    stateTransitionCompact (IotaMatrix _ mtx) = stateTransitionCompact mtx
    stateTransitionCompact _ = Nothing


instance (FiniteBits a, Hashable a) ⇒ HasStateTransitions (TransitionMatrix a) a where
    {-# SPECIALIZE instance HasStateTransitions (TransitionMatrix BitVector) BitVector #-}
    {-# SPECIALIZE instance HasStateTransitions (TransitionMatrix SlimState) SlimState #-}
    {-# SPECIALIZE instance HasStateTransitions (TransitionMatrix WideState) WideState #-}


    -- \|
    -- /O(1)/
    stateTransitionPairwiseDispersion (IotaMatrix _ mtx) = stateTransitionPairwiseDispersion mtx
    stateTransitionPairwiseDispersion (VastMatrix mtx) = stateTransitionPairwiseDispersion mtx
    stateTransitionPairwiseDispersion (VastSpecialized met) = stateTransitionPairwiseDispersion met


    -- \|
    -- /O(1)/
    stateTransitionPairwiseDistance (IotaMatrix _ mtx) = stateTransitionPairwiseDistance mtx
    stateTransitionPairwiseDistance (VastMatrix mtx) = stateTransitionPairwiseDistance mtx
    stateTransitionPairwiseDistance (VastSpecialized met) = stateTransitionPairwiseDistance met


    -- \|
    -- /O(1)/
    stateTransitionPairwiseCentroid (IotaMatrix _ mtx) = stateTransitionPairwiseCentroid mtx
    stateTransitionPairwiseCentroid (VastMatrix mtx) = stateTransitionPairwiseCentroid mtx
    stateTransitionPairwiseCentroid (VastSpecialized met) = stateTransitionPairwiseCentroid met


    -- \|
    -- /O(1)/
    stateTransitionThreewayDispersion (IotaMatrix _ mtx) = stateTransitionThreewayDispersion mtx
    stateTransitionThreewayDispersion (VastMatrix mtx) = stateTransitionThreewayDispersion mtx
    stateTransitionThreewayDispersion (VastSpecialized met) = stateTransitionThreewayDispersion met


    -- \|
    -- /O(1)/
    stateTransitionThreewayDistance (IotaMatrix _ mtx) = stateTransitionThreewayDistance mtx
    stateTransitionThreewayDistance (VastMatrix mtx) = stateTransitionThreewayDistance mtx
    stateTransitionThreewayDistance (VastSpecialized met) = stateTransitionThreewayDistance met


    -- \|
    -- /O(1)/
    stateTransitionThreewayCentroid (IotaMatrix _ mtx) = stateTransitionThreewayCentroid mtx
    stateTransitionThreewayCentroid (VastMatrix mtx) = stateTransitionThreewayCentroid mtx
    stateTransitionThreewayCentroid (VastSpecialized met) = stateTransitionThreewayCentroid met


instance MeasurableRange (TransitionMatrix a) SymbolIndex where
    {-# SPECIALIZE instance MeasurableRange (TransitionMatrix BitVector) SymbolIndex #-}
    {-# SPECIALIZE instance MeasurableRange (TransitionMatrix SlimState) SymbolIndex #-}
    {-# SPECIALIZE instance MeasurableRange (TransitionMatrix WideState) SymbolIndex #-}


    measureRange (IotaMatrix _ mtx) = (coerce (0 ∷ Word), coerce . pred $ matrixSize mtx)
    measureRange (VastMatrix mtx) = measureRange mtx
    measureRange (VastSpecialized met) = measureRange met


instance Show (TransitionMatrix a) where
    {-# SPECIALIZE instance Show (TransitionMatrix BitVector) #-}
    {-# SPECIALIZE instance Show (TransitionMatrix SlimState) #-}
    {-# SPECIALIZE instance Show (TransitionMatrix WideState) #-}


    show =
        \case
            IotaMatrix (Just met) mtx → renderWithBytes (fromEnum $ matrixSize mtx) met
            IotaMatrix _ mtx → renderSummary mtx
            VastMatrix mtx → show mtx
            VastSpecialized met → show met
