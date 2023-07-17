{- |
Concrete encodings for a "wide" element of a dynamic character whose alphabet
has between \( 9 \) and \( 64 \) states (inclusive).
-}

{-# Language DeriveAnyClass #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language Strict #-}
{-# Language TypeFamilies #-}

module Bio.DynamicCharacter.Element.WideState
  ( WideState(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (Bits, FiniteBits)
import Data.Data ()
import Data.Hashable (Hashable(..))
import Data.Ix (Ix)
import Data.Vector.Generic qualified as GV 
import Data.Vector.Generic.Mutable qualified as MGV
import Data.Vector.Primitive qualified as PV
import Data.Vector.Unboxed qualified as UV
import Data.Word (Word64)
import Foreign.Storable (Storable)


{- |
Encoding for a dynamic character element with an alphabet size in the range
\( \left[\;9,\, 64\;\right] \).
-}
newtype WideState = WideState { fromWideState :: Word64 }


newtype instance UV.MVector s WideState = MV_WideState (PV.MVector s Word64)


newtype instance UV.Vector    WideState = V_WideState  (PV.Vector    Word64)


instance UV.Unbox WideState


deriving newtype instance Bits WideState


deriving newtype instance Bounded WideState


deriving newtype instance Enum WideState


deriving newtype instance Eq WideState


deriving newtype instance FiniteBits WideState


deriving newtype instance Hashable WideState


deriving newtype instance Integral WideState


deriving newtype instance Ix WideState


deriving newtype instance NFData WideState


deriving newtype instance Num WideState


deriving newtype instance Ord WideState


deriving newtype instance Real WideState


deriving newtype instance Storable WideState


instance Show WideState where

    show = show . fromWideState
--        let symbolsBase64 = makeAlphabet $  ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> ['+']
--        in  unwords . decodeState symbolsBase64


instance MGV.MVector UV.MVector WideState where

    {-# INLINE basicLength #-}
    basicLength (MV_WideState v) = MGV.basicLength v

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (MV_WideState v) = MV_WideState $ MGV.basicUnsafeSlice i n v

    {-# INLINE basicOverlaps #-}
    basicOverlaps (MV_WideState v1) (MV_WideState v2) = MGV.basicOverlaps v1 v2

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = MV_WideState <$> MGV.basicUnsafeNew n

    {-# INLINE basicInitialize #-}
    basicInitialize (MV_WideState v) = MGV.basicInitialize v

    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeReplicate n x = MV_WideState <$> MGV.basicUnsafeReplicate n (fromWideState x)

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MV_WideState v) i = WideState <$> MGV.basicUnsafeRead v i

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MV_WideState v) i x = MGV.basicUnsafeWrite v i (fromWideState x)

    {-# INLINE basicClear #-}
    basicClear (MV_WideState v) = MGV.basicClear v

    {-# INLINE basicSet #-}
    basicSet (MV_WideState v) x = MGV.basicSet v (fromWideState x)

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_WideState v1) (MV_WideState v2) = MGV.basicUnsafeCopy v1 v2

    basicUnsafeMove (MV_WideState v1) (MV_WideState v2) = MGV.basicUnsafeMove v1 v2

    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeGrow (MV_WideState v) n = MV_WideState <$> MGV.basicUnsafeGrow v n


instance GV.Vector UV.Vector WideState where

    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MV_WideState v) = V_WideState <$> GV.basicUnsafeFreeze v

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (V_WideState v) = MV_WideState <$> GV.basicUnsafeThaw v

    {-# INLINE basicLength #-}
    basicLength (V_WideState v) = GV.basicLength v

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (V_WideState v) = V_WideState $ GV.basicUnsafeSlice i n v

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (V_WideState v) i = WideState <$> GV.basicUnsafeIndexM v i

    basicUnsafeCopy (MV_WideState mv) (V_WideState v) = GV.basicUnsafeCopy mv v

    {-# INLINE elemseq #-}
    elemseq _ = seq
