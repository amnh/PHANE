-----------------------------------------------------------------------------
-- |
-- Module      :  Measure.Unit.SymbolIndex
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

module Measure.Unit.SymbolIndex
  ( SymbolIndex(..)
  , atSymbolIndex
  ) where

import Control.DeepSeq
import Data.Data
import Data.Hashable
import Data.Int
import Data.Ix
import Data.Word
import Foreign.C.Types (CUInt)
import Foreign.Ptr (castPtr)
import Foreign.Storable
import GHC.Generics
import Text.Read


-- |
-- The index of a symbol in an alphabet.
newtype SymbolIndex = SymbolIndex Word
    deriving stock (Eq, Data, Generic, Ix, Ord)


instance Enum SymbolIndex where

    toEnum = SymbolIndex . toEnum 

    fromEnum (SymbolIndex i) = fromEnum i


instance Hashable SymbolIndex where

    hashWithSalt salt (SymbolIndex i) = hashWithSalt salt i


instance NFData SymbolIndex where

    rnf (SymbolIndex i) = rnf i


instance Read SymbolIndex where

    readPrec = SymbolIndex <$> readPrec

    readListPrec = readListPrecDefault


instance Show SymbolIndex where

    show (SymbolIndex i) = show i

    showsPrec n (SymbolIndex i) = showsPrec n i


instance Storable SymbolIndex where

    sizeOf (SymbolIndex i) = sizeOf i

    alignment (SymbolIndex i) = alignment i

    peek = fmap SymbolIndex . peek . castPtr

    poke ptr (SymbolIndex i) = poke (castPtr ptr) i


-- |
-- Use a 'SymbolIndex' in place of an 'Integral' value for index related operations.
{-# SCC        atSymbolIndex #-}
{-# INLINEABLE atSymbolIndex #-}
{-# SPECIALISE atSymbolIndex :: SymbolIndex -> CUInt  #-}
{-# SPECIALISE atSymbolIndex :: SymbolIndex -> Int    #-}
{-# SPECIALISE atSymbolIndex :: SymbolIndex -> Int8   #-}
{-# SPECIALISE atSymbolIndex :: SymbolIndex -> Int16  #-}
{-# SPECIALISE atSymbolIndex :: SymbolIndex -> Int32  #-}
{-# SPECIALISE atSymbolIndex :: SymbolIndex -> Int64  #-}
{-# SPECIALISE atSymbolIndex :: SymbolIndex -> Word   #-}
{-# SPECIALISE atSymbolIndex :: SymbolIndex -> Word8  #-}
{-# SPECIALISE atSymbolIndex :: SymbolIndex -> Word16 #-}
{-# SPECIALISE atSymbolIndex :: SymbolIndex -> Word32 #-}
{-# SPECIALISE atSymbolIndex :: SymbolIndex -> Word64 #-}
atSymbolIndex :: Integral i => SymbolIndex -> i
atSymbolIndex (SymbolIndex i) = fromIntegral i
