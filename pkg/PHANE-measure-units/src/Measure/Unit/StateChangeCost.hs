{-# LANGUAGE Strict #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

{- |
The distance between two states indicating the "cost" of transitioning from one state to another.
-}
module Measure.Unit.StateChangeCost (
    StateChangeCost (..),
) where

import Control.DeepSeq
import Data.Bifunctor
import Data.Data
import Data.Hashable
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
newtype StateChangeCost = StateChangeCost {fromStateChangeCost ∷ Word}
    deriving stock (Eq, Data, Generic, Ord)


newtype instance UV.MVector s StateChangeCost = MV_StateChangeCost (PV.MVector s Word)


newtype instance UV.Vector StateChangeCost = V_StateChangeCost (PV.Vector Word)


instance UV.Unbox StateChangeCost


instance Bounded StateChangeCost where
    minBound = StateChangeCost minBound


    maxBound = StateChangeCost maxBound


instance Enum StateChangeCost where
    toEnum = StateChangeCost . toEnum


    fromEnum (StateChangeCost c) = fromEnum c


instance Hashable StateChangeCost where
    hashWithSalt salt (StateChangeCost c) = hashWithSalt salt c


instance Integral StateChangeCost where
    quotRem (StateChangeCost x) (StateChangeCost y) = bimap StateChangeCost StateChangeCost $ quotRem x y


    toInteger (StateChangeCost c) = toInteger c


instance NFData StateChangeCost where
    rnf (StateChangeCost c) = rnf c


instance Num StateChangeCost where
    (+) (StateChangeCost x) (StateChangeCost y) = StateChangeCost $ x + y


    (*) (StateChangeCost x) (StateChangeCost y) = StateChangeCost $ x * y


    (-) (StateChangeCost x) (StateChangeCost y) = StateChangeCost $ x - y


    abs (StateChangeCost c) = StateChangeCost $ abs c


    signum (StateChangeCost c) = StateChangeCost $ signum c


    negate (StateChangeCost c) = StateChangeCost $ negate c


    fromInteger i = StateChangeCost $ fromInteger i


instance Real StateChangeCost where
    toRational (StateChangeCost c) = toRational c


instance Read StateChangeCost where
    readPrec = StateChangeCost <$> readPrec


    readListPrec = readListPrecDefault


instance Show StateChangeCost where
    show (StateChangeCost c) = show c


    showsPrec n (StateChangeCost c) = showsPrec n c


instance Storable StateChangeCost where
    sizeOf (StateChangeCost c) = sizeOf c


    alignment (StateChangeCost c) = alignment c


    peek = fmap StateChangeCost . peek . castPtr


    poke ptr (StateChangeCost c) = poke (castPtr ptr) c


instance MGV.MVector UV.MVector StateChangeCost where
    {-# INLINE basicLength #-}
    basicLength (MV_StateChangeCost v) = MGV.basicLength v


    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (MV_StateChangeCost v) = MV_StateChangeCost $ MGV.basicUnsafeSlice i n v


    {-# INLINE basicOverlaps #-}
    basicOverlaps (MV_StateChangeCost v1) (MV_StateChangeCost v2) = MGV.basicOverlaps v1 v2


    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = MV_StateChangeCost <$> MGV.basicUnsafeNew n


    {-# INLINE basicInitialize #-}
    basicInitialize (MV_StateChangeCost v) = MGV.basicInitialize v


    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeReplicate n x = MV_StateChangeCost <$> MGV.basicUnsafeReplicate n (fromStateChangeCost x)


    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MV_StateChangeCost v) i = StateChangeCost <$> MGV.basicUnsafeRead v i


    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MV_StateChangeCost v) i x = MGV.basicUnsafeWrite v i (fromStateChangeCost x)


    {-# INLINE basicClear #-}
    basicClear (MV_StateChangeCost v) = MGV.basicClear v


    {-# INLINE basicSet #-}
    basicSet (MV_StateChangeCost v) x = MGV.basicSet v (fromStateChangeCost x)


    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_StateChangeCost v1) (MV_StateChangeCost v2) = MGV.basicUnsafeCopy v1 v2


    basicUnsafeMove (MV_StateChangeCost v1) (MV_StateChangeCost v2) = MGV.basicUnsafeMove v1 v2


    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeGrow (MV_StateChangeCost v) n = MV_StateChangeCost <$> MGV.basicUnsafeGrow v n


instance GV.Vector UV.Vector StateChangeCost where
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MV_StateChangeCost v) = V_StateChangeCost <$> GV.basicUnsafeFreeze v


    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (V_StateChangeCost v) = MV_StateChangeCost <$> GV.basicUnsafeThaw v


    {-# INLINE basicLength #-}
    basicLength (V_StateChangeCost v) = GV.basicLength v


    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (V_StateChangeCost v) = V_StateChangeCost $ GV.basicUnsafeSlice i n v


    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (V_StateChangeCost v) i = StateChangeCost <$> GV.basicUnsafeIndexM v i


    basicUnsafeCopy (MV_StateChangeCost mv) (V_StateChangeCost v) = GV.basicUnsafeCopy mv v


    {-# INLINE elemseq #-}
    elemseq = const seq
