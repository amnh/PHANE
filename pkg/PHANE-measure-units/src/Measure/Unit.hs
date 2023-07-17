-----------------------------------------------------------------------------
-- |
-- Module      :  Measure.Unit
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# Language Safe #-}

module Measure.Unit
  ( -- * Measure Components
    Distance(..)
  , SymbolCount(..)
  , SymbolIndex(..)
    -- * Component Type-classes
  , HasEditExtrema(..)
  , HasSymbolCount(..)
    -- * Symbol Count Threshold
  , iota
  , infimumSymbolLimit
  ) where

import Measure.Unit.Distance
import Measure.Unit.SymbolCount
import Measure.Unit.SymbolIndex