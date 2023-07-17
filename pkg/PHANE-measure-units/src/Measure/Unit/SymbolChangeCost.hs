-----------------------------------------------------------------------------
-- |
-- Module      :  Measure.Unit.SymbolChangeCost
-- Copyright   :  (c) 2015-2021 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language Safe #-}

module Measure.Unit.SymbolChangeCost
  ( SymbolChangeCost(..)
  ) where

import Control.DeepSeq
import Data.Bifunctor
import Data.Data
import Data.Hashable
import Foreign.Ptr (castPtr)
import Foreign.Storable
import GHC.Generics
import Text.Read


-- |
-- The distance between two measurable elements.
newtype SymbolChangeCost = SymbolChangeCost Word
    deriving stock (Eq, Data, Generic, Ord)



instance Bounded SymbolChangeCost where

    minBound = SymbolChangeCost minBound

    maxBound = SymbolChangeCost maxBound


instance Enum SymbolChangeCost where

    toEnum = SymbolChangeCost . toEnum

    fromEnum (SymbolChangeCost c) = fromEnum c


instance Hashable SymbolChangeCost where

    hashWithSalt salt (SymbolChangeCost c) = hashWithSalt salt c


instance Integral SymbolChangeCost where

    quotRem (SymbolChangeCost x) (SymbolChangeCost y) = bimap SymbolChangeCost SymbolChangeCost $ quotRem x y

    toInteger (SymbolChangeCost c) = toInteger c


instance NFData SymbolChangeCost where

    rnf (SymbolChangeCost c) = rnf c


instance Num SymbolChangeCost where

    (+) (SymbolChangeCost x) (SymbolChangeCost y) = SymbolChangeCost $ x + y

    (*) (SymbolChangeCost x) (SymbolChangeCost y) = SymbolChangeCost $ x * y

    (-) (SymbolChangeCost x) (SymbolChangeCost y) = SymbolChangeCost $ x - y

    abs    (SymbolChangeCost c) = SymbolChangeCost $ abs c

    signum (SymbolChangeCost c) = SymbolChangeCost $ signum c

    negate (SymbolChangeCost c) = SymbolChangeCost $ negate c

    fromInteger i = SymbolChangeCost $ fromInteger i


instance Real SymbolChangeCost where

    toRational (SymbolChangeCost c) = toRational c


instance Read SymbolChangeCost where

    readPrec = SymbolChangeCost <$> readPrec

    readListPrec = readListPrecDefault


instance Show SymbolChangeCost where

    show (SymbolChangeCost c) = show c

    showsPrec n (SymbolChangeCost c) = showsPrec n c


instance Storable SymbolChangeCost where

    sizeOf (SymbolChangeCost c) = sizeOf c

    alignment (SymbolChangeCost c) = alignment c

    peek = fmap SymbolChangeCost . peek . castPtr

    poke ptr (SymbolChangeCost c) = poke (castPtr ptr) c
