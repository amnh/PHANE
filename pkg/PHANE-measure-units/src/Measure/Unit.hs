{-# LANGUAGE Safe #-}

{- |
Data-types for common units of measure and related type-classes.
-}
module Measure.Unit (
    -- * Measure Components
    Distance (..),
    SymbolCount (..),
    SymbolIndex (..),

    -- * Component Type-classes
    HasEditExtrema (..),
    HasSymbolCount (..),

    -- * Symbol Count Threshold
    iota,
    infimumSymbolLimit,
) where

import Measure.Unit.Distance
import Measure.Unit.SymbolCount
import Measure.Unit.SymbolIndex

