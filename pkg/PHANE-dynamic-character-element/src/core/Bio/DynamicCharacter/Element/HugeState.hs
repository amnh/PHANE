{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Concrete encodings for a "huge" element of a dynamic character whose alphabet
has exceeds \( 64 \) states.
-}
module Bio.DynamicCharacter.Element.HugeState (
    HugeState (..),
) where

import Bio.DynamicCharacter.Element.Class (StateOfAmbiguity (..))
import Control.DeepSeq (NFData)
import Data.Bit
import Data.Bits (Bits (..), FiniteBits (..))
import Data.Coerce (coerce)
import Data.Hashable (Hashable (..))
import Data.Vector.Unboxed qualified as UV
import GHC.IsList qualified as List (IsList (..))
import Unsafe.Coerce (unsafeCoerce)


{- |
Encoding for a dynamic character element with an alphabet size \( \lvert\, \Sigma \,\rvert \in \left[\;65,\, \infty\;\right) \).
-}
newtype HugeState = HugeState {fromHugeState ∷ UV.Vector Bit}


deriving newtype instance Bits HugeState


deriving newtype instance Eq HugeState


instance FiniteBits HugeState where
    finiteBitSize = UV.length . fromHugeState


instance Hashable HugeState where
    hashWithSalt salt = unsafeCoerce . UV.foldr xor (unsafeCoerce salt) . cloneToWords . fromHugeState


deriving newtype instance NFData HugeState


deriving newtype instance Ord HugeState


instance Show HugeState where
    show =
        let showBit (Bit True) = "1"
            showBit _ = "0"

            enclose ∷ String → String
            enclose x = '[' : x <> "]"
        in  enclose . UV.foldMap showBit . fromHugeState


instance StateOfAmbiguity HugeState where
    toBits = List.fromList . fmap coerce . UV.toList . coerce


    fromBits = coerce . UV.fromList . fmap Bit . List.toList


    fromNumber !dimValue !intValue =
        let n = fromEnum dimValue
            v = toInteger intValue
        in  coerce . UV.generate n $ \i → Bit $ v `testBit` i
