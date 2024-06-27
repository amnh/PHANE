module DirectOptimization.Pairwise.Wide (
    WideDynamicCharacter,
    WideState,
    widePairwiseDO,
) where

import Bio.DynamicCharacter
import Bio.DynamicCharacter.Element (WideState)
import DirectOptimization.Pairwise.Internal (AlignmentCost, SymbolDistance, TCM2Dλ)
import DirectOptimization.Pairwise.Ukkonen


{- |
Pairwise alignment of dynamic characters for /wide/ alphabets.
-}
widePairwiseDO
    ∷ SymbolDistance
    → TCM2Dλ WideState
    → WideDynamicCharacter
    → WideDynamicCharacter
    → (AlignmentCost, WideDynamicCharacter)
widePairwiseDO = ukkonenDO
