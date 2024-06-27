module DirectOptimization.Pairwise.Slim (
    SlimDynamicCharacter,
    SlimState,
    slimPairwiseDO,
) where

import Bio.DynamicCharacter
import Bio.DynamicCharacter.Element (SlimState)
import DirectOptimization.Pairwise.Internal (AlignmentCost)
import DirectOptimization.Pairwise.Slim.FFI


{- |
Pairwise alignment of dynamic characters for /slim/ alphabets.
-}
slimPairwiseDO
    ∷ TCMρ
    → SlimDynamicCharacter
    → SlimDynamicCharacter
    → (AlignmentCost, SlimDynamicCharacter)
slimPairwiseDO = smallAlphabetPairwiseDO
