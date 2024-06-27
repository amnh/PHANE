{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module Measure.Transition.Edits (
    HasEditExtrema (..),
) where

import Measure.Unit.SymbolDistance


{- |
Memoized/pre-computed access to the extrema costs for each type of edit.
-}
class HasEditExtrema a where
    {-# MINIMAL maxDeletion, maxInsertion, minDeletion, minInsertion #-}


    {-# INLINEABLE maxEdit #-}
    maxEdit ∷ a → SymbolDistance
    maxEdit a = max (maxDeletion a) $ maxInsertion a


    maxDeletion ∷ a → SymbolDistance


    maxInsertion ∷ a → SymbolDistance


    {-# INLINEABLE minEdit #-}
    minEdit ∷ a → SymbolDistance
    minEdit a = min (minDeletion a) $ minInsertion a


    minDeletion ∷ a → SymbolDistance


    minInsertion ∷ a → SymbolDistance
