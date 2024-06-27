module DirectOptimization.Pairwise.Huge (
    HugeDynamicCharacter,
    HugeState,
    hugePairwiseDO,
) where

import Bio.DynamicCharacter
import Bio.DynamicCharacter.Element (HugeState)
import DirectOptimization.Pairwise.Internal (AlignmentCost, SymbolDistance, TCM2Dλ)
import DirectOptimization.Pairwise.Ukkonen


{- |
Pairwise alignment of dynamic characters for /huge/ alphabets.
-}
hugePairwiseDO
    ∷ SymbolDistance
    → TCM2Dλ HugeState
    → HugeDynamicCharacter
    → HugeDynamicCharacter
    → (AlignmentCost, HugeDynamicCharacter)
hugePairwiseDO = ukkonenDO
