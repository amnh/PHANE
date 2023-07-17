{- |
We must ensure that missing and gap are appropriately code as "-" & "?", respectively,
before this module is used, i.e., as output from either parsers or in unification step.
-}

{-# Language Safe #-}
{-# Language Strict #-}

module Data.Alphabet.Gap
    ( gapIndex
    ) where

import Measure.Unit.SymbolIndex


{- |
The index of the vector where the gap state is stored.

/NOTE:/ This index value is very important for many gap-related operations,
both internally for the 'Alphabet' module/library and externally in general.
-}
gapIndex :: SymbolIndex
gapIndex = SymbolIndex 0
