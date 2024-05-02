{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

{- |
The distance between two symbols indicating the "cost" of transitioning from one symbol to the other.
-}
module Measure.Unit.SymbolChangeCost (
    SymbolChangeCost (..),
) where

import Control.DeepSeq
import Data.Bifunctor
import Data.Coerce (coerce)
import Data.Data
import Data.Hashable
import Data.Monoid (Sum (..))
import Data.Vector.Generic qualified as GV
import Data.Vector.Generic.Mutable qualified as MGV
import Data.Vector.Primitive qualified as PV
import Data.Vector.Unboxed qualified as UV
import Foreign.Ptr (castPtr)
import Foreign.Storable
import GHC.Generics
import Text.Read


{- |
The distance between two measurable elements.
-}
newtype SymbolChangeCost = SymbolChangeCost Word
    deriving stock (Eq, Data, Generic, Ord)
    deriving (Monoid, Semigroup) via (Sum Word)


newtype instance UV.MVector s SymbolChangeCost = MV_SymbolChangeCost (PV.MVector s Word)


newtype instance UV.Vector SymbolChangeCost = V_SymbolChangeCost (PV.Vector Word)


instance UV.Unbox SymbolChangeCost


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


    abs (SymbolChangeCost c) = SymbolChangeCost $ abs c


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


instance MGV.MVector UV.MVector SymbolChangeCost where
    {-# INLINE basicLength #-}
    basicLength (MV_SymbolChangeCost v) = MGV.basicLength v


    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (MV_SymbolChangeCost v) = MV_SymbolChangeCost $ MGV.basicUnsafeSlice i n v


    {-# INLINE basicOverlaps #-}
    basicOverlaps (MV_SymbolChangeCost v1) (MV_SymbolChangeCost v2) = MGV.basicOverlaps v1 v2


    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = MV_SymbolChangeCost <$> MGV.basicUnsafeNew n


    {-# INLINE basicInitialize #-}
    basicInitialize (MV_SymbolChangeCost v) = MGV.basicInitialize v


    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeReplicate n x = MV_SymbolChangeCost <$> MGV.basicUnsafeReplicate n (fromSymbolChangeCost x)


    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MV_SymbolChangeCost v) i = SymbolChangeCost <$> MGV.basicUnsafeRead v i


    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MV_SymbolChangeCost v) i x = MGV.basicUnsafeWrite v i (fromSymbolChangeCost x)


    {-# INLINE basicClear #-}
    basicClear (MV_SymbolChangeCost v) = MGV.basicClear v


    {-# INLINE basicSet #-}
    basicSet (MV_SymbolChangeCost v) x = MGV.basicSet v (fromSymbolChangeCost x)


    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_SymbolChangeCost v1) (MV_SymbolChangeCost v2) = MGV.basicUnsafeCopy v1 v2


    basicUnsafeMove (MV_SymbolChangeCost v1) (MV_SymbolChangeCost v2) = MGV.basicUnsafeMove v1 v2


    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeGrow (MV_SymbolChangeCost v) n = MV_SymbolChangeCost <$> MGV.basicUnsafeGrow v n


instance GV.Vector UV.Vector SymbolChangeCost where
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MV_SymbolChangeCost v) = V_SymbolChangeCost <$> GV.basicUnsafeFreeze v


    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (V_SymbolChangeCost v) = MV_SymbolChangeCost <$> GV.basicUnsafeThaw v


    {-# INLINE basicLength #-}
    basicLength (V_SymbolChangeCost v) = GV.basicLength v


    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (V_SymbolChangeCost v) = V_SymbolChangeCost $ GV.basicUnsafeSlice i n v


    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (V_SymbolChangeCost v) i = SymbolChangeCost <$> GV.basicUnsafeIndexM v i


    basicUnsafeCopy (MV_SymbolChangeCost mv) (V_SymbolChangeCost v) = GV.basicUnsafeCopy mv v


    {-# INLINE elemseq #-}
    elemseq = const seq


fromSymbolChangeCost ∷ SymbolChangeCost → Word
fromSymbolChangeCost = coerce
