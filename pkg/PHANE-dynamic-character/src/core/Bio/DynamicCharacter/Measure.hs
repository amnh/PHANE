{-# LANGUAGE Strict #-}

{- |
Pre-processing functions used to compare which of the two characters is "longer" prior to beginning direct optimization pairwise alignment.
-}
module Bio.DynamicCharacter.Measure (
    measureCharacters,
    measureCharactersWithoutGaps,
) where

import Bio.DynamicCharacter
import Bio.DynamicCharacter.HandleGaps
import Data.Bits
import Data.Ord
import Data.Vector.Generic (Vector, basicLength)


{- |
__Time:__ \( \Theta \left(\, 1 \,\right) \) for input characters of differing lengths.

__Time:__ \( \mathcal{O}\left(\, k \,\right) \) for input characters of equal length,
where \( k \) is the shared prefix of both characters.

Returns the dynamic character that is shorter first, longer second, and notes
whether or not the inputs were swapped to place the characters in this ordering.

Handles equal length characters by considering the lexicographically larger
character as longer.

Handles equality of inputs by /not/ swapping.
-}
{-# INLINEABLE measureCharacters #-}
{-# SPECIALIZE measureCharacters ∷
    SlimDynamicCharacter → SlimDynamicCharacter → (Ordering, SlimDynamicCharacter, SlimDynamicCharacter)
    #-}
{-# SPECIALIZE measureCharacters ∷
    WideDynamicCharacter → WideDynamicCharacter → (Ordering, WideDynamicCharacter, WideDynamicCharacter)
    #-}
{-# SPECIALIZE measureCharacters ∷
    HugeDynamicCharacter → HugeDynamicCharacter → (Ordering, HugeDynamicCharacter, HugeDynamicCharacter)
    #-}
measureCharacters
    ∷ ( Ord (v e)
      , Vector v e
      )
    ⇒ OpenDynamicCharacter v e
    → OpenDynamicCharacter v e
    → (Ordering, OpenDynamicCharacter v e, OpenDynamicCharacter v e)
measureCharacters lhs rhs
    | lhsOrdering == GT = (lhsOrdering, rhs, lhs)
    | otherwise = (lhsOrdering, lhs, rhs)
    where
        lhsMedians = extractMediansGapped lhs
        rhsMedians = extractMediansGapped rhs
        lhsOrdering =
            -- First, compare inputs by length.
            case comparing basicLength lhsMedians rhsMedians of
                -- If the inputs are equal length,
                -- Then compare by the (arbitrary) lexicographical ordering of the median states.
                EQ → case lhsMedians `compare` rhsMedians of
                    -- If the input median states have the same ordering,
                    -- Lastly, we compare by the lexicographic ordering of the "tagged triples."
                    --
                    -- If they are equal after this step,
                    -- Then the inputs are representationally equal.
                    -- Actually, honest to goodness 100% equal!
                    EQ → lhs `compare` rhs
                    v → v
                v → v


{- |
__Time:__ \( \mathcal{O}\left(\, k \,\right) \)
where \( k \) is the length of the longest character.

Considers the median values of the characters, ignores the left/right tagging.

First remove the gaps from the input characters.

If both "ungapped" inputs are empty, we measure the original "gapped" inputs to
determine if the inputs need to be swapped. This is required to ensure comutativity
of subsequent operations which use this method.

Returns the "ungapped" dynamic character that is "shorter" first, "longer" second,
the removed gap mappings (in the same order), and notes whether or not the inputs
were swapped to place the characters in this ordering.

Handles equal length characters by considering the lexicographically larger
character as longer.

Handles equality of inputs by /not/ swapping.
-}
{-# INLINEABLE measureCharactersWithoutGaps #-}
{-# SPECIALIZE measureCharactersWithoutGaps ∷
    SlimDynamicCharacter → SlimDynamicCharacter → (Bool, GapSet, GapSet, SlimDynamicCharacter, SlimDynamicCharacter)
    #-}
{-# SPECIALIZE measureCharactersWithoutGaps ∷
    WideDynamicCharacter → WideDynamicCharacter → (Bool, GapSet, GapSet, WideDynamicCharacter, WideDynamicCharacter)
    #-}
{-# SPECIALIZE measureCharactersWithoutGaps ∷
    HugeDynamicCharacter → HugeDynamicCharacter → (Bool, GapSet, GapSet, HugeDynamicCharacter, HugeDynamicCharacter)
    #-}
measureCharactersWithoutGaps
    ∷ ( FiniteBits e
      , Ord (v e)
      , Vector v e
      )
    ⇒ OpenDynamicCharacter v e
    -- ^ First  dynamic character
    → OpenDynamicCharacter v e
    -- ^ Second dynamic character
    → (Bool, GapSet, GapSet, OpenDynamicCharacter v e, OpenDynamicCharacter v e)
measureCharactersWithoutGaps char1 char2
    | swapInputs = (True, gapsChar2, gapsChar1, ungappedChar2, ungappedChar1)
    | otherwise = (False, gapsChar1, gapsChar2, ungappedChar1, ungappedChar2)
    where
        swapInputs = measure == GT
        (gapsChar1, ungappedChar1) = deleteGaps char1
        (gapsChar2, ungappedChar2) = deleteGaps char2
        (measure, _, _) =
            case measureCharacters ungappedChar1 ungappedChar2 of
                (EQ, _, _) → measureCharacters char1 char2
                x → x
