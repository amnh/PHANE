{- |
Pairwise direct optimization alignment functions using a variety of techniques.
-}
module DirectOptimization.Pairwise (
    -- * Slim characters
    SlimDynamicCharacter,
    SlimState,
    slimPairwiseDO,

    -- * Wide characters
    WideDynamicCharacter,
    WideState,
    widePairwiseDO,

    -- * Huge characters
    HugeDynamicCharacter,
    HugeState,
    hugePairwiseDO,
) where

import DirectOptimization.Pairwise.Huge
import DirectOptimization.Pairwise.Slim
import DirectOptimization.Pairwise.Wide

