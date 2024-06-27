{-# LANGUAGE Safe #-}
{-# LANGUAGE Strict #-}

{- |
Getter "lens" type-class for 'Measure.Distance.Distance'.
-}
module Measure.Transition.Symbols (
    HasSymbolDistances (..),
) where

import Measure.Transition.Types


{- |
Any structural representation which can produce a function measuring the
'Measure.Distance.Distance' between two symbols, i.e. produce a "Symbol
Distance Matrix."
-}
class HasSymbolDistances a where
    symbolDistances ∷ a → SymbolDistanceλ
