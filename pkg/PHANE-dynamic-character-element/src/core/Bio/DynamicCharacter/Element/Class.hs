{-# LANGUAGE Safe #-}

{- |
Type class for converting "bit-vector like" representations to an character ambiguity group.
-}
module Bio.DynamicCharacter.Element.Class (
    -- * Type-class
    StateOfAmbiguity (..),
) where

import GHC.IsList (IsList (..))


{- |
Conversion of a character which ambiguously exists in a super position of possible states
to/from a linear ordering of bits.
-}
class StateOfAmbiguity e where
    toBits ∷ (IsList f, Item f ~ Bool) ⇒ e → f


    fromBits ∷ (IsList f, Item f ~ Bool) ⇒ f → e


    fromNumber ∷ (Integral v) ⇒ Word → v → e


    toUnsignedNumber ∷ (Integral v) ⇒ e → v
