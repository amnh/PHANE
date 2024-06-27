{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

{- |
The number of symbols within a collection of symbol, such as an alphabet.
-}
module Measure.Unit.SymbolCount (
    SymbolCount (..),
    HasSymbolCount (..),
    iota,
    infimumSymbolLimit,
    symbolBounds,
) where

import Control.DeepSeq
import Data.Data
import Data.Hashable
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr (castPtr)
import Foreign.Storable
import GHC.Generics
import GHC.Natural
import Measure.Unit.SymbolIndex
import Text.Read


{- |
The cardinality of an alphabet.
-}
newtype SymbolCount = SymbolCount Word
    deriving stock (Eq, Data, Generic, Ord)


{- |
A structure which can derive the number of alphabet symbols associated with it.
-}
class HasSymbolCount a where
    symbolCount ∷ a → SymbolCount


instance Hashable SymbolCount where
    hashWithSalt salt (SymbolCount x) = hashWithSalt salt x


instance NFData SymbolCount where
    rnf (SymbolCount x) = rnf x


instance Read SymbolCount where
    readPrec = SymbolCount <$> readPrec


    readListPrec = readListPrecDefault


instance Show SymbolCount where
    show (SymbolCount x) = show x


    showsPrec i (SymbolCount x) = showsPrec i x


instance Storable SymbolCount where
    sizeOf (SymbolCount x) = sizeOf x


    alignment (SymbolCount x) = alignment x


    peek = fmap SymbolCount . peek . castPtr


    poke ptr (SymbolCount x) = poke (castPtr ptr) x


instance HasSymbolCount CChar where
    symbolCount = clamp


instance HasSymbolCount CSChar where
    symbolCount = clamp


instance HasSymbolCount CUChar where
    symbolCount = clamp


instance HasSymbolCount CShort where
    symbolCount = clamp


instance HasSymbolCount CUShort where
    symbolCount = clamp


instance HasSymbolCount CInt where
    symbolCount = clamp


instance HasSymbolCount CUInt where
    symbolCount = clamp


instance HasSymbolCount CLong where
    symbolCount = clamp


instance HasSymbolCount CULong where
    symbolCount = clamp


instance HasSymbolCount CSize where
    symbolCount = clamp


instance HasSymbolCount CLLong where
    symbolCount = clamp


instance HasSymbolCount CULLong where
    symbolCount = clamp


instance HasSymbolCount Int where
    symbolCount = clamp


instance HasSymbolCount Int8 where
    symbolCount = clamp


instance HasSymbolCount Int16 where
    symbolCount = clamp


instance HasSymbolCount Int32 where
    symbolCount = clamp


instance HasSymbolCount Int64 where
    symbolCount = clamp


instance HasSymbolCount Integer where
    symbolCount = clamp


instance HasSymbolCount Natural where
    symbolCount = SymbolCount . naturalToWord


instance HasSymbolCount SymbolCount where
    symbolCount = id


instance HasSymbolCount Word where
    symbolCount = SymbolCount


instance HasSymbolCount Word8 where
    symbolCount = SymbolCount . fromIntegral


instance HasSymbolCount Word16 where
    symbolCount = SymbolCount . fromIntegral


instance HasSymbolCount Word32 where
    symbolCount = SymbolCount . fromIntegral


instance HasSymbolCount Word64 where
    symbolCount = SymbolCount . fromIntegral


{- |
The largest 'Measure.Unit.SymbolCount' value for which the predicate 'iota' holds.

Useful for partitioning a collection of symbols based on whether it is too large for the C FFI.
-}
infimumSymbolLimit ∷ SymbolCount
infimumSymbolLimit = SymbolCount 8


{- |
Predicate to deciding if a 'Measure.Unit.SymbolCount' is small enough to be compatible with the C FFI.
-}
iota ∷ (HasSymbolCount a) ⇒ a → Bool
iota = (<= infimumSymbolLimit) . symbolCount


{- |
Extract the inclusive bounds of a structure indexable by a contiguous symbol ordering.
-}
symbolBounds ∷ (HasSymbolCount a) ⇒ a → (SymbolIndex, SymbolIndex)
symbolBounds x =
    let (SymbolCount n) = symbolCount x
    in  (SymbolIndex 0, SymbolIndex $ n - 1)


clamp ∷ (Integral i) ⇒ i → SymbolCount
clamp i
    | i <= 0 = SymbolCount 0
    | otherwise = SymbolCount $ fromIntegral i
