{-# LANGUAGE Safe #-}

module Measure.Transition.EditExtrema (
    HasEditExtrema (..),
) where

import Measure.Unit.SymbolChangeCost


{- |
Memoized/pre-computed access to the extrema costs for each type of edit.
-}
class HasEditExtrema a where
    {-# MINIMAL maxDeletion, maxInsertion, minDeletion, minInsertion #-}


    {-# INLINEABLE maxEdit #-}
    maxEdit :: a -> SymbolChangeCost
    maxEdit a = max (maxDeletion a) $ maxInsertion a


    maxDeletion :: a -> SymbolChangeCost


    maxInsertion :: a -> SymbolChangeCost


    {-# INLINEABLE minEdit #-}
    minEdit :: a -> SymbolChangeCost
    minEdit a = min (minDeletion a) $ minInsertion a


    minDeletion :: a -> SymbolChangeCost


    minInsertion :: a -> SymbolChangeCost
