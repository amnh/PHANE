{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

{- |
The data-types and type-aliases for PHANE measures.
-}
module Measure.Transition.Types (
    -- * Symbol Measure
    SymbolDistanceλ,

    -- * State Pairwise Measures
    StateTransitionPairwiseMedianλ,
    StateTransitionPairwiseDispersionλ,
    StateTransitionPairwiseDistanceλ,

    -- * State Threeway Measures
    StateTransitionThreewayDispersionλ,
    StateTransitionThreewayDistanceλ,
    StateTransitionThreewayMedianλ,

    -- * Synonyms
    SDMλ,
    TCM2Dλ,
    TCM3Dλ,

    -- * Abstract Measures
) where

import Measure.Dispersion
import Measure.Distance
import Measure.Median
import Measure.Unit.StateChangeCost
import Measure.Unit.SymbolDistance
import Measure.Unit.SymbolIndex


{- |
An abstract representation of the /distance/ bewteen two symbols in an alphabet.
Given the indicies of two symbols, the distance between the symbols is returned.

This is a 'Distance' measure, /specialized/ to measuring 'Measure.Unit.SymbolIndex' with
'Measure.Unit.StateChangeCost.StateChangeCost' as the differential unit of measure.

Metonymously, refered to as the Symbol Distance Matrix (SDM). There is a more
terse synonym, 'SDMλ', provided for convience.
-}
type SymbolDistanceλ = Distance SymbolDistance SymbolIndex


{- |
An abstract representation of the /distance/ bewteen /two/ states.
Given two states, the cost to transition between the states and the
median state is returned. This is a form of 'Dispersion', /specialized/ to
two elements with 'Measure.Unit.StateChangeCost.StateChangeCost' as the distance measure.

Abstractly, this function is the cross-product between the collection of all
possible states, forming a matrix of each pair's transition cost and median.
This is the "State Transition Matrix" for the collection of possible states.
Metonymously, refered to as the Transition Cost Matrix (TCM).

This data-type is precisely labeled as 'StateTransitionPairwiseDispersionλ'. While verbose,
it succinctly describes the data-type's properties. The suffix of "λ" indicates
that this is should be always considered as a function of two elements.

There is a more terse synonym, 'TCM2Dλ', provided for convience.
-}
type StateTransitionPairwiseDispersionλ e = DispersionPairwise StateChangeCost e


{- |
The /distance/ bewteen /two/ states.

The first component of the more general 'StateTransitionPairwiseDispersionλ'.

/NOTE:/ Can be more efficient that taking 'fst' of the 'Dispersion'.
-}
type StateTransitionPairwiseDistanceλ e = Distance StateChangeCost e


{- |
The /(geometric) median/ /(median)/ bewteen /two/ states.

The second component of the more general 'StateTransitionPairwiseDispersionλ'.

/NOTE:/ Can be more efficient that taking 'snd' of the 'Dispersion'.
-}
type StateTransitionPairwiseMedianλ e = MedianPairwise e


{- |
An abstract representation of the /distance/ bewteen /three/ states.
Given three states, the cost to transition between the states and the
median state is returned. This is a form of 'Dispersion', /specialized/ to
three elements with 'Measure.Unit.StateChangeCost.StateChangeCost' as the distance measure.

Abstractly, this function is the /triple/ cross-product between the collection
of all possible states, forming a /cube/ of each triple's transition cost and median.
This is the "State Transition Cube" for the collection of possible states.

There is a more terse synonym, 'TCM3Dλ', provided for convience and explained below.

A 'StateTransitionThreewayDispersionλ' is a higher dimensional version of 'StateTransitionPairwiseDispersionλ'.
Naturally, matrices are 2-dimensional. However, this cube is a similar three-way
measure of transition cost and median state. Rather than call refer to this as
a Transition Cost Cube (TCC), we instead call both TCMs and refer to thier
dimensionality. Hence, the provided synonyms of 'TCM2Dλ' and 'TCM3Dλ' for the
data-types, 'StateTransitionPairwiseDispersionλ' and 'StateTransitionThreewayDispersionλ', respectively.
-}
type StateTransitionThreewayDispersionλ e = DispersionThreeway StateChangeCost e


{- |
The /distance/ bewteen /three/ states.

The first component of the more general 'StateTransitionThreewayDispersionλ'.

/NOTE:/ Can be more efficient that taking 'fst' of the 'Dispersion'.
-}
type StateTransitionThreewayDistanceλ e = e → e → e → StateChangeCost


{- |
The /(geometric) median/ /(median)/ bewteen /three/ states.

The second component of the more general 'StateTransitionThreewayDispersionλ'.

/NOTE:/ Can be more efficient that taking 'snd' of the 'Dispersion'.
-}
type StateTransitionThreewayMedianλ e = MedianThreeway e


{- |
Synonym for 'SymbolDistanceλ', provided for terseness.
-}
type SDMλ = SymbolDistanceλ


{- |
Synonym for 'StateTransitionPairwiseDispersionλ', provided for terseness.
-}
type TCM2Dλ e = StateTransitionPairwiseDispersionλ e


{- |
Synonym for 'StateTransitionThreewayDispersionλ', provided for terseness.
-}
type TCM3Dλ e = StateTransitionThreewayDispersionλ e
