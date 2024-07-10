{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

{- |
The distance between two symbols indicating the "cost" of transitioning from one symbol to the other.
-}
module Measure.Unit.SymbolDistance (
    SymbolDistance (..),
    fromSymbolDistance,
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
newtype SymbolDistance = SymbolDistance Word
    deriving stock (Eq, Data, Generic, Ord)
    deriving (Monoid, Semigroup) via (Sum Word)


newtype instance UV.MVector s SymbolDistance = MV_SymbolDistance (PV.MVector s Word)


newtype instance UV.Vector SymbolDistance = V_SymbolDistance (PV.Vector Word)


instance UV.Unbox SymbolDistance


instance Bounded SymbolDistance where
    minBound = SymbolDistance minBound


    maxBound = SymbolDistance maxBound


instance Enum SymbolDistance where
    toEnum = SymbolDistance . toEnum


    fromEnum (SymbolDistance c) = fromEnum c


instance Hashable SymbolDistance where
    hashWithSalt salt (SymbolDistance c) = hashWithSalt salt c


instance Integral SymbolDistance where
    quotRem (SymbolDistance x) (SymbolDistance y) = bimap SymbolDistance SymbolDistance $ quotRem x y


    toInteger (SymbolDistance c) = toInteger c


instance NFData SymbolDistance where
    rnf (SymbolDistance c) = rnf c


instance Num SymbolDistance where
    (+) (SymbolDistance x) (SymbolDistance y) = SymbolDistance $ x + y


    (*) (SymbolDistance x) (SymbolDistance y) = SymbolDistance $ x * y


    (-) (SymbolDistance x) (SymbolDistance y) = SymbolDistance $ x - y


    abs (SymbolDistance c) = SymbolDistance $ abs c


    signum (SymbolDistance c) = SymbolDistance $ signum c


    negate (SymbolDistance c) = SymbolDistance $ negate c


    fromInteger i = SymbolDistance $ fromInteger i


instance Real SymbolDistance where
    toRational (SymbolDistance c) = toRational c


instance Read SymbolDistance where
    readPrec = SymbolDistance <$> readPrec


    readListPrec = readListPrecDefault


instance Show SymbolDistance where
    show (SymbolDistance c) = show c


    showsPrec n (SymbolDistance c) = showsPrec n c


instance Storable SymbolDistance where
    sizeOf (SymbolDistance c) = sizeOf c


    alignment (SymbolDistance c) = alignment c


    peek = fmap SymbolDistance . peek . castPtr


    poke ptr (SymbolDistance c) = poke (castPtr ptr) c


instance MGV.MVector UV.MVector SymbolDistance where
    {-# INLINE basicLength #-}
    basicLength (MV_SymbolDistance v) = MGV.basicLength v


    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (MV_SymbolDistance v) = MV_SymbolDistance $ MGV.basicUnsafeSlice i n v


    {-# INLINE basicOverlaps #-}
    basicOverlaps (MV_SymbolDistance v1) (MV_SymbolDistance v2) = MGV.basicOverlaps v1 v2


    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = MV_SymbolDistance <$> MGV.basicUnsafeNew n


    {-# INLINE basicInitialize #-}
    basicInitialize (MV_SymbolDistance v) = MGV.basicInitialize v


    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeReplicate n x = MV_SymbolDistance <$> MGV.basicUnsafeReplicate n (fromSymbolDistance x)


    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MV_SymbolDistance v) i = SymbolDistance <$> MGV.basicUnsafeRead v i


    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MV_SymbolDistance v) i x = MGV.basicUnsafeWrite v i (fromSymbolDistance x)


    {-# INLINE basicClear #-}
    basicClear (MV_SymbolDistance v) = MGV.basicClear v


    {-# INLINE basicSet #-}
    basicSet (MV_SymbolDistance v) x = MGV.basicSet v (fromSymbolDistance x)


    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_SymbolDistance v1) (MV_SymbolDistance v2) = MGV.basicUnsafeCopy v1 v2


    basicUnsafeMove (MV_SymbolDistance v1) (MV_SymbolDistance v2) = MGV.basicUnsafeMove v1 v2


    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeGrow (MV_SymbolDistance v) n = MV_SymbolDistance <$> MGV.basicUnsafeGrow v n


instance GV.Vector UV.Vector SymbolDistance where
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MV_SymbolDistance v) = V_SymbolDistance <$> GV.basicUnsafeFreeze v


    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (V_SymbolDistance v) = MV_SymbolDistance <$> GV.basicUnsafeThaw v


    {-# INLINE basicLength #-}
    basicLength (V_SymbolDistance v) = GV.basicLength v


    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (V_SymbolDistance v) = V_SymbolDistance $ GV.basicUnsafeSlice i n v


    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (V_SymbolDistance v) i = SymbolDistance <$> GV.basicUnsafeIndexM v i


    basicUnsafeCopy (MV_SymbolDistance mv) (V_SymbolDistance v) = GV.basicUnsafeCopy mv v


    {-# INLINE elemseq #-}
    elemseq = const seq


{- |
__Time:__ \( \mathcal{O}\left( 0 \right) \)

Convert a 'Measure.Unit.SymbolDistance.SymbolDistance' to a 'Word'
as a "no-op" via type coercion.
-}
{-# INLINE fromSymbolDistance #-}
fromSymbolDistance ∷ SymbolDistance → Word
fromSymbolDistance = coerce
