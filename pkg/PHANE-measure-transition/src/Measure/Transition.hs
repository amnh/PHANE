{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

{- |
Abstract data-types, type-aliases, and type-classes for PHANE measures.
-}
module Measure.Transition (
    -- * Core "Transition Matrix" Type-class
    HasTransitionMatrix,

    -- * Symbol Measure
    SymbolDistanceλ,

    -- * State Pairwise Measures
    StateTransitionPairwiseMedianλ,
    StateTransitionPairwiseDispersionλ,
    StateTransitionPairwiseDistanceλ,

    -- * State Threeway Measures
    StateTransitionThreewayMedianλ,
    StateTransitionThreewayDispersionλ,
    StateTransitionThreewayDistanceλ,

    -- * Synonyms
    SDMλ,
    TCM2Dλ,
    TCM3Dλ,
    {-
      -- * Compact
      , FFI2D()
      , FFI3D()
    -}

    -- * Type-classes
    HasStateTransitions (..),
    --  , HasStateTransitionsCompact(..)
    HasSymbolDistances (..),
    HasEditExtrema (..),
) where

import Measure.Range
import Measure.Transition.Edits
import Measure.Transition.States
import Measure.Transition.Symbols
import Measure.Transition.Types
import Measure.Unit.SymbolCount
import Measure.Unit.SymbolIndex


{- |
Collection of measure functionality required for facilitating string alignment.

Supports the following measures:

  * "Dimensionality"
    - 'symbolCount'

  * 'Measure.Range.InclusiveRange' of "Symbols"
    - 'Measure.Range.measureRange'

  * 'Measure.Dispersion.Dispersion'
    - 'stateTransitionPairwiseDispersion' : state ⨉ state         → (ℕ , state)
    - 'stateTransitionThreewayDispersion' : state ⨉ state ⨉ state → (ℕ , state)

  * 'Measure.Median.Median'
    - 'stateTransitionPairwiseMedian'   : state ⨉ state         → state
    - 'stateTransitionThreewayMedian'   : state ⨉ state ⨉ state → state

  * 'Measure.Distance.Distance'
    - 'symbolDistances'                   : symbol ⨉ symbol         → ℕ
    - 'stateTransitionPairwiseDistance'   : state  ⨉ state          → ℕ
    - 'stateTransitionThreewayDistance'   : state  ⨉ state  ⨉ state → ℕ

  * "Symbol Edit Distance"
    - 'maxEdit'      : ℕ
    - 'maxDeletion'  : ℕ
    - 'maxInsertion' : ℕ
    - 'minEdit'      : ℕ
    - 'minDeletion'  : ℕ
    - 'minInsertion' : ℕ
-}
class
    ( HasEditExtrema a
    , HasStateTransitions a b
    , --    , HasStateTransitionsCompact a
      HasSymbolCount a
    , HasSymbolDistances a
    , MeasurableRange a SymbolIndex
    ) ⇒
    HasTransitionMatrix a b

{-
    transitionList

    transitionColumns

    transitionRows

    transitionGeneration
-}
