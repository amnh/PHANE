{- |
Special 'Alphabet' constructions and queries for popular symbol sets.
-}
module Data.Alphabet.Special (
    -- * Constructions
    aminoAcidAlphabet,
    discreteAlphabet,
    dnaAlphabet,
    rnaAlphabet,

    -- * Queries
    isAlphabetAminoAcid,
    isAlphabetDiscrete,
    isAlphabetDna,
    isAlphabetRna,
) where

import Data.Alphabet.IUPAC
import Data.Alphabet.Internal
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Char (isLower)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.String


{- |
Alphabet of amino acids.
-}
aminoAcidAlphabet ∷ (IsString s, Ord s) ⇒ Alphabet s
aminoAcidAlphabet = fromBimap iupacToAminoAcid


{- |
Alphabet of DNA bases.
-}
dnaAlphabet ∷ (IsString s, Ord s) ⇒ Alphabet s
dnaAlphabet = fromBimap iupacToDna


{- |
Alphabet of RNA bases.
-}
rnaAlphabet ∷ (IsString s, Ord s) ⇒ Alphabet s
rnaAlphabet = fromBimap iupacToRna


{- |
Alphabet of "discrete" values.

The discrete alphabet includes the following 63 values:

@ [\'0\'..\'9\'] <> [\'A\'..\'Z\'] <> [\'a\'..\'z\'] <> "-" @
-}
discreteAlphabet ∷ (IsString s, Ord s) ⇒ Alphabet s
discreteAlphabet =
    let symbolChars ∷ NonEmpty Char
        symbolChars = NE.fromList $ fold [['0' .. '9'], ['A' .. 'Z'], ['a' .. 'z'], "-"]
    in  fromSymbols $ fromString . pure <$> symbolChars


{- |
\$\mathcal{O}\left(\, n \,\right)$

Determines if the supplied alphabet represents amino acid symbols.

Useful for determining if an 'NonEmpty' should be rendered as an IUPAC code.
-}
isAlphabetAminoAcid ∷ (IsString s, Ord s) ⇒ Alphabet s → Bool
isAlphabetAminoAcid = isAlphabetSubsetOf aminoAcidAlphabet


{- |
\$\mathcal{O}\left(\, n \,\right)$

Determines if the supplied alphabet represents DNA symbols.

Useful for determining if an 'NonEmpty' should be rendered as an IUPAC code.
-}
isAlphabetDna ∷ (IsString s, Ord s) ⇒ Alphabet s → Bool
isAlphabetDna = isAlphabetSubsetOf dnaAlphabet


{- |
\$\mathcal{O}\left(\, n \,\right)$

Determines if the supplied alphabet represents DNA symbols.

Useful for determining if an 'NonEmpty' should be rendered as an IUPAC code.
-}
isAlphabetRna ∷ (IsString s, Ord s) ⇒ Alphabet s → Bool
isAlphabetRna = isAlphabetSubsetOf rnaAlphabet


{- |
\$\mathcal{O}\left(\, n \,\right)$

Determines if the supplied alphabet represents DNA symbols.

Useful for determining if an 'NonEmpty' should be rendered as an IUPAC code.
-}
isAlphabetDiscrete ∷ (IsString s, Ord s) ⇒ Alphabet s → Bool
isAlphabetDiscrete = isAlphabetSubsetOf discreteAlphabet


isAlphabetSubsetOf ∷ (Ord s) ⇒ Alphabet s → Alphabet s → Bool
isAlphabetSubsetOf specialAlphabet queryAlphabet = querySet `Set.isSubsetOf` specialSet
    where
        querySet = symbolSet queryAlphabet
        specialSet = symbolSet specialAlphabet


fromBimap ∷ (IsString s, Ord s) ⇒ Bimap (NonEmpty String) a → Alphabet s
fromBimap =
    let -- WLOG get element of sungleton ambiguity group.
        extraction ∷ (Functor f) ⇒ f (NonEmpty b) → f b
        extraction = fmap NE.head

        -- Predicate to remove the lower-case alphabet symbols.
        -- Desire to retain the upper case alphabet symbols and
        -- any punctuation symbols, such as gap '-'.
        filtration =
            let isLowerCaseStr = \case
                    [] → False
                    x : _ → isLower x
            in  filter $ not . isLowerCaseStr

        -- Convert the remaining symbol set to data-types for
        -- polymorphic alphabet construction.
        projection = fmap fromString . NE.fromList
    in  fromSymbols . projection . filtration . extraction . BM.keys
