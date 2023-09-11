{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Concrete encodings for a "wide" element of a dynamic character whose alphabet
has between \( 1 \) and \( 8 \) states (inclusive).
-}
module Bio.DynamicCharacter.Element.SlimState (
    SlimState (..),
) where

import Control.DeepSeq (NFData)
import Data.Bits (Bits (xor), FiniteBits)
import Data.Hashable (Hashable (..))
import Data.Ix (Ix)
import Data.Vector.Generic qualified as GV
import Data.Vector.Generic.Mutable qualified as MGV
import Data.Vector.Primitive qualified as PV
import Data.Vector.Unboxed qualified as UV
import Foreign.C.Types (CUInt)
import Foreign.Storable (Storable)


{- |
Encoding for a dynamic character element with an alphabet size in the range
\( \left[\;1,\, 8\;\right] \).

/NOTE:/ This encoding uses more bits than required! This is due to the C FFI
implementation details. It would be possible to reduce this to a @CUChar@ if
and only iff the C interface and implementation is updated.
-}
newtype SlimState = SlimState {fromSlimState ∷ CUInt}


newtype instance UV.MVector s SlimState = MV_SlimState (PV.MVector s CUInt)


newtype instance UV.Vector SlimState = V_SlimState (PV.Vector CUInt)


instance UV.Unbox SlimState


deriving newtype instance Bits SlimState


deriving newtype instance Bounded SlimState


deriving newtype instance Enum SlimState


deriving newtype instance Eq SlimState


deriving newtype instance FiniteBits SlimState


deriving newtype instance Integral SlimState


deriving newtype instance Ix SlimState


deriving newtype instance NFData SlimState


deriving newtype instance Num SlimState


deriving newtype instance Ord SlimState


deriving newtype instance Real SlimState


deriving newtype instance Storable SlimState


instance Hashable SlimState where
    hashWithSalt salt = (salt `xor`) . fromEnum . fromSlimState


instance Show SlimState where
    show = show . fromSlimState


-- unwords . decodeState (makeAlphabet "ACGTXYZ")

instance MGV.MVector UV.MVector SlimState where
    {-# INLINE basicLength #-}
    basicLength (MV_SlimState v) = MGV.basicLength v


    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (MV_SlimState v) = MV_SlimState $ MGV.basicUnsafeSlice i n v


    {-# INLINE basicOverlaps #-}
    basicOverlaps (MV_SlimState v1) (MV_SlimState v2) = MGV.basicOverlaps v1 v2


    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = MV_SlimState <$> MGV.basicUnsafeNew n


    {-# INLINE basicInitialize #-}
    basicInitialize (MV_SlimState v) = MGV.basicInitialize v


    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeReplicate n x = MV_SlimState <$> MGV.basicUnsafeReplicate n (fromSlimState x)


    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MV_SlimState v) i = SlimState <$> MGV.basicUnsafeRead v i


    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MV_SlimState v) i x = MGV.basicUnsafeWrite v i (fromSlimState x)


    {-# INLINE basicClear #-}
    basicClear (MV_SlimState v) = MGV.basicClear v


    {-# INLINE basicSet #-}
    basicSet (MV_SlimState v) x = MGV.basicSet v (fromSlimState x)


    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_SlimState v1) (MV_SlimState v2) = MGV.basicUnsafeCopy v1 v2


    basicUnsafeMove (MV_SlimState v1) (MV_SlimState v2) = MGV.basicUnsafeMove v1 v2


    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeGrow (MV_SlimState v) n = MV_SlimState <$> MGV.basicUnsafeGrow v n


instance GV.Vector UV.Vector SlimState where
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MV_SlimState v) = V_SlimState <$> GV.basicUnsafeFreeze v


    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (V_SlimState v) = MV_SlimState <$> GV.basicUnsafeThaw v


    {-# INLINE basicLength #-}
    basicLength (V_SlimState v) = GV.basicLength v


    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (V_SlimState v) = V_SlimState $ GV.basicUnsafeSlice i n v


    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (V_SlimState v) i = SlimState <$> GV.basicUnsafeIndexM v i


    basicUnsafeCopy (MV_SlimState mv) (V_SlimState v) = GV.basicUnsafeCopy mv v


    {-# INLINE elemseq #-}
    elemseq _ = seq
