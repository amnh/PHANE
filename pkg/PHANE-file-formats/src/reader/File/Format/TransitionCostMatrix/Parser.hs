{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Functions for for parsing TCM files into an alphabet and square matrix.
-}
module File.Format.TransitionCostMatrix.Parser (
    alphabetLine,
    tcmAlphabet,
    tcmMatrix,
    tcmStreamReader,
    matrixBlock,
) where

import Control.Applicative.Combinators.NonEmpty
import Control.DeepSeq
import Data.Char (isSpace)
import Data.Data
import Data.Foldable
import Data.Functor (($>))
import Data.List (sort, sortBy)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map (Map, assocs, insertWith)
import Data.Matrix (Matrix, cols, rows)
import Data.Matrix qualified as M (fromList)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.String
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Short (ShortText)
import Data.Void
import File.Format.TransitionCostMatrix.Types
import GHC.Generics
import Text.Megaparsec hiding (someTill)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (scientific)


{- |
Intermediate parse result prior to consistency validation
-}
data TCMParseResult
    = TCMParseResult (NonEmpty ShortText) (Matrix Rational)
    deriving stock (Show)


{- |
Parses the entirety of a stream producing a TCM result.
The result will contain an Alphabet with no duplicate elements
and a square Matrix with dimension @(n+1) x (n+1)@ where @n@ is
the length of the Alphabet.
-}
{-# INLINEABLE tcmStreamReader #-}
{-# SPECIALIZE tcmStreamReader ∷ Parsec Void T.Text FileFormatTCM #-}
{-# SPECIALIZE tcmStreamReader ∷ Parsec Void LT.Text FileFormatTCM #-}
{-# SPECIALIZE tcmStreamReader ∷ Parsec Void String FileFormatTCM #-}
tcmStreamReader ∷ (MonadFail m, MonadParsec e s m, Token s ~ Char) ⇒ m FileFormatTCM
tcmStreamReader = validateTCMParseResult =<< tcmDefinition <* eof


{- |
Parses an intermediary result consisting of an Alphabet and a Matrix.
Both the Alphabet and Matrix have been validated independently for
consistencey, but no validation has been performed to ensure that the
dimensions of the Matrix and the length of the Alphabet are consistent
with each other.
-}
{-# INLINE tcmDefinition #-}
{-# SPECIALIZE tcmDefinition ∷ Parsec Void T.Text TCMParseResult #-}
{-# SPECIALIZE tcmDefinition ∷ Parsec Void LT.Text TCMParseResult #-}
{-# SPECIALIZE tcmDefinition ∷ Parsec Void String TCMParseResult #-}
tcmDefinition ∷ (MonadFail m, MonadParsec e s m, Token s ~ Char) ⇒ m TCMParseResult
tcmDefinition = do
    _ ← space
    alphabet ← symbol tcmAlphabet
    matrix ← symbol tcmMatrix
    pure $ TCMParseResult alphabet matrix


{- |
Shorthand for the expected format of the alphabet lin in a TCM file.
The same as 'alphabetLine hspace'.
-}
{-# INLINEABLE tcmAlphabet #-}
{-# SPECIALIZE tcmAlphabet ∷ Parsec Void T.Text (NonEmpty ShortText) #-}
{-# SPECIALIZE tcmAlphabet ∷ Parsec Void LT.Text (NonEmpty ShortText) #-}
{-# SPECIALIZE tcmAlphabet ∷ Parsec Void String (NonEmpty ShortText) #-}
tcmAlphabet ∷ (MonadFail m, MonadParsec e s m, Token s ~ Char) ⇒ m (NonEmpty ShortText)
tcmAlphabet = alphabetLine hspace


{- |
Shorthand for the expected format of the matrix block in a TCM file
The same as 'matrixBlock hspace'.
-}
{-# INLINEABLE tcmMatrix #-}
{-# SPECIALIZE tcmMatrix ∷ Parsec Void T.Text (Matrix Rational) #-}
{-# SPECIALIZE tcmMatrix ∷ Parsec Void LT.Text (Matrix Rational) #-}
{-# SPECIALIZE tcmMatrix ∷ Parsec Void String (Matrix Rational) #-}
tcmMatrix ∷ (MonadFail m, MonadParsec e s m, Token s ~ Char) ⇒ m (Matrix Rational)
tcmMatrix = matrixBlock hspace


{- |
The 'alphabetLine' function takes a combinator to consume delimiters between
elements in the alphabet line and returns a list of elements in the alphabet.

==== __Examples__

Basic usage:

>>> parse (alphabetLine hspace) "" "a b c d\n"
Right ["a","b","c","d"]

>>> parse (alphabetLine (hspace *> char '|' <* hspace)) "" "2 | 3 | 5 | 7\n"
Right ["2","3","5","7"]
-}
{-# INLINEABLE alphabetLine #-}
{-# SPECIALIZE alphabetLine ∷ Parsec Void T.Text () → Parsec Void T.Text (NonEmpty ShortText) #-}
{-# SPECIALIZE alphabetLine ∷ Parsec Void LT.Text () → Parsec Void LT.Text (NonEmpty ShortText) #-}
{-# SPECIALIZE alphabetLine ∷ Parsec Void String () → Parsec Void String (NonEmpty ShortText) #-}
alphabetLine ∷ (MonadFail m, MonadParsec e s m, Token s ~ Char) ⇒ m () → m (NonEmpty ShortText)
alphabetLine spacing = validateAlphabet =<< ((alphabetSymbol <* spacing) `someTill` endOfLine)
    where
        alphabetSymbol = takeWhile1P Nothing (not . isSpace)


{- |
The 'matrixBlock' function takes a combinator to consume delimiters between
entries in a line of the matrix and returns a square 'Matrix Rational'.

==== __Examples__

Basic usage:

>>> parse (matrixBlock hspace) "" "1 2 3 \n 4 5 6\n7 8 9\n"
Right (( 1 2 3 )
       ( 4 5 6 )
       ( 7 8 9 ))

>>> parse (matrixBlock (char ':') "" "1.0:1.0\n1.0:0.0"
Right (( 1 2 )
       ( 3 4 ))
-}
{-# INLINEABLE matrixBlock #-}
{-# SPECIALIZE matrixBlock ∷ Parsec Void T.Text () → Parsec Void T.Text (Matrix Rational) #-}
{-# SPECIALIZE matrixBlock ∷ Parsec Void LT.Text () → Parsec Void LT.Text (Matrix Rational) #-}
{-# SPECIALIZE matrixBlock ∷ Parsec Void String () → Parsec Void String (Matrix Rational) #-}
matrixBlock ∷ (MonadFail m, MonadParsec e s m, Token s ~ Char) ⇒ m () → m (Matrix Rational)
matrixBlock spacing = validateMatrix =<< many (symbol matrixRow)
    where
        matrixRow = (spacing *> matrixEntry <* spacing) `manyTill` endOfLine
        matrixEntry = force toRational <$> scientific


{- |
Validates that the dimensions of the Matrix are @(n+1) x (n+1)@
where @n@ is the length of the Alphabet.
-}
{-# INLINE validateTCMParseResult #-}
{-# SPECIALIZE validateTCMParseResult ∷ TCMParseResult → Parsec Void T.Text FileFormatTCM #-}
{-# SPECIALIZE validateTCMParseResult ∷ TCMParseResult → Parsec Void LT.Text FileFormatTCM #-}
{-# SPECIALIZE validateTCMParseResult ∷ TCMParseResult → Parsec Void String FileFormatTCM #-}
validateTCMParseResult ∷ (MonadFail m) ⇒ TCMParseResult → m FileFormatTCM
validateTCMParseResult (TCMParseResult alphabet matrix)
    | dimMismatch = fail errorMessage
    | otherwise = pure $ FileFormatTCM alphabet matrix
    where
        size = length alphabet
        rows' = rows matrix
        cols' = cols matrix
        dimMismatch =
            size + 1 /= rows'
                || size + 1 /= cols'
        errorMessage =
            concat
                [ "The alphabet length is "
                , show size
                , " but the matrix dimensions are "
                , show rows'
                , " x "
                , show cols'
                , ". The expected matrix dimensions were "
                , show $ size + 1
                , " x "
                , show $ size + 1
                , "."
                ]


{- |
Validates the information contained in the Alphabet.

Ensures that the Alphabet:

  * Contains no duplicate elements
-}
{-# INLINE validateAlphabet #-}
{-# SPECIALIZE validateAlphabet ∷ NonEmpty T.Text → Parsec Void T.Text (NonEmpty ShortText) #-}
{-# SPECIALIZE validateAlphabet ∷ NonEmpty LT.Text → Parsec Void LT.Text (NonEmpty ShortText) #-}
{-# SPECIALIZE validateAlphabet ∷ NonEmpty String → Parsec Void String (NonEmpty ShortText) #-}
validateAlphabet
    ∷ ∀ e s m. (MonadFail m, MonadParsec e s m, Token s ~ Char) ⇒ NonEmpty (Tokens s) → m (NonEmpty ShortText)
validateAlphabet alphabet
    | duplicatesExist = fail $ "The following symbols were listed multiple times in the custom alphabet: " <> shownDuplicates
    | otherwise = pure $ toShortText <$> alphabet
    where
        duplicatesExist = not $ null dupes
        dupes = duplicates $ toList alphabet
        shownDuplicates = show $ chunkToTokens pxy <$> dupes
        toShortText = fromString . chunkToTokens pxy
        pxy = Proxy ∷ Proxy s


{- |
Validates the information contained in the Matrix constitutes a square matrix.

Ensures that the Matrix:

  * Is not empty

  * Each row has the same number of columns

  * The number of rows' match the number of columns
-}
{-# INLINEABLE validateMatrix #-}
{-# SPECIALIZE validateMatrix ∷ [[Rational]] → Parsec Void T.Text (Matrix Rational) #-}
{-# SPECIALIZE validateMatrix ∷ [[Rational]] → Parsec Void LT.Text (Matrix Rational) #-}
{-# SPECIALIZE validateMatrix ∷ [[Rational]] → Parsec Void String (Matrix Rational) #-}
validateMatrix ∷ (MonadFail m, MonadParsec e s m) ⇒ [[Rational]] → m (Matrix Rational)
validateMatrix matrix
    | null matrix = fail "No matrix specified"
    | null matrixErrors = pure . M.fromList (rows', cols') $ concat matrix
    | otherwise = fails matrixErrors
    where
        rows' = length matrix
        cols' = fromJust . mostCommon $ length <$> matrix
        badCols' = foldr getBadCols' [] $ zip [(1 ∷ Int) ..] matrix
        getBadCols' (n, e) a = let x = length e in if x /= cols' then (n, x) : a else a
        colMsg (x, y) = (:) (Just $ fold ["Matrix row ", show x, " has ", show y, " columns but ", show cols', " columns were expected"])
        matrixErrors = catMaybes $ badRowCount : badColCount
        badColCount = foldr colMsg [] badCols'
        badRowCount =
            if rows' == cols'
                then Nothing
                else
                    Just $
                        concat
                            [ "The matrix is not a square matrix. The matrix has "
                            , show rows'
                            , " rows but "
                            , show cols'
                            , " rows were expected"
                            ]


{- |
Whitespace consuming combinator wrapper
-}
{-# INLINE symbol #-}
{-# SPECIALIZE symbol ∷ Parsec Void T.Text a → Parsec Void T.Text a #-}
{-# SPECIALIZE symbol ∷ Parsec Void LT.Text a → Parsec Void LT.Text a #-}
{-# SPECIALIZE symbol ∷ Parsec Void String a → Parsec Void String a #-}
symbol ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m a → m a
symbol x = x <* space


{- |
\( \mathcal{O} \left( n * \log_2 n \right) \)

Returns the list of elements which are not unique in the input list.

==_Example==

>>> duplicates "duplicate string"
"it"

>>> duplicates "GATACACATCAGATT"
"ACGT"

>>> duplicates [ 'A' .. 'Z' ]
[]
-}
duplicates ∷ ∀ a t. (Foldable t, Ord a) ⇒ t a → [a]
duplicates =
    let duplicates' ∷ [a] → [a]
        duplicates' = \case
            [] → []
            [_] → []
            x : y : ys | x == y → (x :) . duplicates $ dropWhile (== y) ys
            _ : xs → duplicates xs
    in  duplicates' . sort . toList


{- |
\( \mathcal{O} \left( n * \log_2 n \right) \)

Returns the element that occurs the most often in the list.

==_Example==

>>> mostCommon "GATACACATCAGATT"
Just 'A'

>>> mostCommon "AABCDDDEFGGT"
Just 'D'

>>> mostCommon []
Nothing
-}
mostCommon ∷ (Foldable t, Ord a) ⇒ t a → Maybe a
mostCommon xs
    | null xs = Nothing
    | otherwise = case occurrences xs of
        [] → Nothing
        (x, _) : _ → Just x


{- |
\( \mathcal{O} \left( n * \log_2 n \right) \)

Returns a mapping of each unique element in the list paired with how often
the element occurs in the list.

The elements are in descending order of occurrence.

==_Example==

>>> occurrences "GATACACATCAGATT"
[('A',6),('T',4),('C',3),('G',2)]

>>> occurrences "AABCDDDEFGGT"
[('D',3),('A',2),('G',2),('B',1),('C',1),('E',1),('F',1),('T',1)]
-}
occurrences ∷ ∀ a t. (Foldable t, Ord a) ⇒ t a → [(a, Word)]
occurrences =
    let buildOccuranceMap =
            let occurrence ∷ a → Map a Word → Map a Word
                occurrence e = insertWith (const succ) e 1
            in  foldr occurrence mempty

        comparator ∷ (a, Word) → (a, Word) → Ordering
        comparator x y = descending $ comparing snd x y

        descending LT = GT
        descending GT = LT
        descending x = x

        collateOccuranceMap ∷ Map a Word → [(a, Word)]
        collateOccuranceMap = sortBy comparator . assocs
    in  collateOccuranceMap . buildOccuranceMap


{- |
Custom 'eol' combinator to account for /very/ old Mac file formats ending
lines in a single @\'\\r\'@.
-}
{-# INLINE endOfLine #-}
{-# SPECIALIZE endOfLine ∷ Parsec Void T.Text () #-}
{-# SPECIALIZE endOfLine ∷ Parsec Void LT.Text () #-}
{-# SPECIALIZE endOfLine ∷ Parsec Void String () #-}
endOfLine ∷ (Enum (Token s), MonadParsec e s m) ⇒ m ()
endOfLine = choice [nl, try (cr *> nl), cr] $> ()
    where
        newLineChar = enumCoerce '\n'
        carriageChar = enumCoerce '\r'
        nl = single newLineChar $> ()
        cr = single carriageChar $> ()


{- |
Accepts zero or more Failure messages.
-}
{-# INLINEABLE fails #-}
{-# SPECIALIZE fails ∷ [String] → Parsec Void T.Text a #-}
{-# SPECIALIZE fails ∷ [String] → Parsec Void LT.Text a #-}
{-# SPECIALIZE fails ∷ [String] → Parsec Void String a #-}
fails ∷ (MonadParsec e s m) ⇒ [String] → m a
fails = failure Nothing . S.fromList . fmap Label . mapMaybe nonEmpty


{- |
Convert one Enum to another through the Int value.
-}
enumCoerce ∷ (Enum a, Enum b) ⇒ a → b
enumCoerce = toEnum . fromEnum
