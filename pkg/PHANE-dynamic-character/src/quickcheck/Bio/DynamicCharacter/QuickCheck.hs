{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Exposes the QuickCheck instances for dynamic charcters.
-}
module Bio.DynamicCharacter.QuickCheck (

) where

import Bio.DynamicCharacter
import Data.Bits
import Data.Kind (Type)
import Data.Vector qualified as Box
import Data.Vector.Generic (Vector, basicLength, (!))
import Data.Vector.Generic qualified as V
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Modifiers (Positive (..))


data PsudeoAlign = FromL | FromR | Merge


instance (Arbitrary e, Bits e, Vector v e) ⇒ Arbitrary (OpenDynamicCharacter (v ∷ Type → Type) e) where
    arbitrary =
        let alignGen ∷ Gen PsudeoAlign
            alignGen =
                let f = \case
                        (True, False) → FromL
                        (False, True) → FromR
                        _ → Merge
                in  f <$> arbitrary

            splitting ∷ (Arbitrary a, Bits a) ⇒ PsudeoAlign → a → Gen (a, a, a)
            splitting from val =
                let nil = val `xor` val
                in  case from of
                        FromL → pure (val, val, nil)
                        FromR → pure (nil, val, val)
                        Merge → do
                            takeFromL ← (.|. nil) <$> arbitrary
                            takeFromR ← (.|. nil) <$> arbitrary
                            let takeFromB = complement takeFromL .&. complement takeFromR
                            let maskL = takeFromB .|. takeFromL
                            let maskR = takeFromB .|. takeFromR
                            pure (maskL .&. val, val, maskR .&. val)
        in  do
                Positive lenM ← arbitrary ∷ Gen (Positive Int)
                from ← V.replicateM lenM alignGen ∷ Gen (Box.Vector PsudeoAlign)
                meds ← V.replicateM lenM arbitrary
                vals ← V.zipWithM splitting from meds

                pure . generateCharacter (toEnum lenM) $ \i →
                    let (x, y, z) = vals ! fromEnum i
                    in  (# x, y, z #)


    shrink (x, y, z) =
        let len = toEnum $ basicLength x
            makeSmaller ∷ Word → OpenDynamicCharacter v e
            makeSmaller e = generateCharacter (len - 1) $ \i →
                let i'
                        | i < e = fromEnum i
                        | otherwise = fromEnum $ i + 1
                in  (# x ! i', y ! i', z ! i' #)
        in  makeSmaller <$> [0 .. len - 1]
