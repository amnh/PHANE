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

import Control.DeepSeq (NFData)
import Data.Bit
import Data.Bits (Bits (..), FiniteBits (..))
import Data.Data ()
import Data.Hashable (Hashable (..))
import Data.Vector.Unboxed qualified as UV
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
