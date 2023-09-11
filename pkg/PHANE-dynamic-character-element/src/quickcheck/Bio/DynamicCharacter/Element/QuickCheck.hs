{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Exposes the 'Arbitrary' and 'CoArbitrary' instances for dynamic character elements.
-}
module Bio.DynamicCharacter.Element.QuickCheck (
    -- * Element Varieties of a Dynamic Character
    SlimState (),
    WideState (),
) where

import Bio.DynamicCharacter.Element.SlimState
import Bio.DynamicCharacter.Element.WideState
import Test.QuickCheck.Arbitrary


instance Arbitrary SlimState where
    arbitrary = arbitrarySizedIntegral


    shrink = shrinkIntegral


instance Arbitrary WideState where
    arbitrary = arbitrarySizedIntegral


    shrink = shrinkIntegral


instance CoArbitrary SlimState where
    coarbitrary = coarbitraryIntegral


instance CoArbitrary WideState where
    coarbitrary = coarbitraryIntegral
