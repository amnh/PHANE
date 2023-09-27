{-# LANGUAGE Safe #-}
{-# LANGUAGE Strict #-}

module Measure.Transition.Symbols (
    HasSymbolDistances (..),
) where

import Measure.Transition.Types


{- |
Any structural representation which can produce a function measuring the
'Measure.Distance.Distance' between two symbols, i.e. produce a "Symbol
Change Matrix."
-}
class HasSymbolDistances a where
    symbolDistances ∷ a → SymbolDistanceλ
