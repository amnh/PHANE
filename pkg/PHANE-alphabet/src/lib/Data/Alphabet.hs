{- |
An 'Alphabet' represents an /ordered/ list of unique symbols with constant time random access.
Symbols are any data type which are coercible from a'String' through the 'Data.String.IsString'
type-class.

An 'Alphabet' is constructed in one of two ways:

  1. Supplying a `Foldable` structure of symbols which are 'Data.String.IsString'
     instances to the 'fromSymbols' function.

  2. Supplying a `Foldable` structure of symbols and state name pairs,
     both of which are 'Data.String.IsString' instances to the
     'fromSymbolsWithStateNames' function.

Both 'Alphabet' construction methods are order preserving with respect to the
input symbol order.

Every 'Alphabet' contains a "gap" symbol denoted by the expression:
> fromString "-"
The "gap" character is always located at 'gapIndex' in the ordered
list regardless of its presence or position in the construction structure.

An 'Alphabet' will never contain the "missing" symbol denoted by the expression:
> fromString "?"
This symbol will be removed from the 'Alphabet' if it is present in the supplied input.
-}
module Data.Alphabet (
    Alphabet (),

    -- * Constructors
    fromSymbols,
    fromSymbolsWithStateNames,
    --  , fromSymbolsWithStateNamesAndTCM
    --  , fromSymbolsWithTCM

    -- * Queries
    alphabetStateNames,
    alphabetSymbols,

    -- * Gap Symbol Queries
    gapIndex,
    gapSymbol,
    --  -- * Truncation
    --  , truncateAtSymbol
    --  , truncateAtMaxSymbol
) where

import Data.Alphabet.Internal

