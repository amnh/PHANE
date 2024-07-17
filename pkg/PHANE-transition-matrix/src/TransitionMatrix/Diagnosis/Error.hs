{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedSums #-}

{- |
Data-types for representing and encoding failures during the matrix diagnosis process.
-}
module TransitionMatrix.Diagnosis.Error (
    -- * Failure
    DiagnosisFailure (),

    -- * Error types
    DiagnosisError (..),

    -- * Smart constructor
    makeDiagnosisFailure,

    -- * Error extraction
    getDiagnosisErrors,

    -- ** Conditional bounds
    dicretizedValueBounds,
) where

import Control.DeepSeq
import Data.Bits
import Data.Char (toLower)
import Data.Data
import Data.Foldable
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import GHC.Exts (fromList)
import GHC.Generics
import Layout.Compact.States (DiscretizedResolutionIota)
import Layout.Compact.Symbols.Internal (DiscretizedResolution)
import Measure.Unit.SymbolCount


{- |
Encoding of which type of error occured during the matrix diagnosis process.
-}
data DiagnosisError a
    = JaggedColumns !Word !IntSet
    | JaggedRows !Word !IntSet
    | NotSquareGrid !Word !Word
    | NotSquareList !Word
    | MatrixDimension0
    | MatrixDimension1
    | ValueNegative !(Set a)
    | ValueOverflow SymbolCount !(Set a)
    deriving stock (Data, Eq, Ord, Generic)
    deriving anyclass (NFData)


type role DiagnosisError nominal


{- |
Collection of failures which occured while diagnosing a matrix.
-}
newtype DiagnosisFailure a = DiagnosisFailure (NonEmpty (DiagnosisError a))
    deriving newtype (NFData, Eq)
    deriving stock (Data, Generic)


type role DiagnosisFailure nominal


instance (Ord a) ⇒ Semigroup (DiagnosisFailure a) where
    (<>) (DiagnosisFailure lhs@(x :| xs)) (DiagnosisFailure rhs) =
        let rhs' = filter (`notElem` lhs) $ toList rhs
            end = List.sort $ xs <> rhs'
        in  DiagnosisFailure $ x :| end


instance (Show a) ⇒ Show (DiagnosisError a) where
    show =
        \case
            JaggedColumns n xs →
                joinLines
                    [ "Unequal column lengths in the proffered transition measure range."
                    , fold ["Expected all columns to be of length ", show n, " but found the following:"]
                    , nicelyRenderList $ IS.toList xs
                    ]
            JaggedRows n xs →
                joinLines
                    [ "Unequal row lengths in the proffered transition measure range."
                    , fold ["Expected all rows to be of length ", show n, " but found the following:"]
                    , nicelyRenderList $ IS.toList xs
                    ]
            NotSquareGrid m n →
                joinLines
                    [ "Unequal dimensions proffered for transition measure range."
                    , fold ["Cannot construct a non-square, ", show m, "⨉", show n, " transition measure"]
                    ]
            NotSquareList n →
                joinLines
                    [ "List of non-square length proffered as transition measure range."
                    , let root = sqrt $ fromIntegral n ∷ Double
                          lower = floor root ∷ Word
                          upper = ceiling root ∷ Word
                      in  unwords
                            [ "The number of elements"
                            , show n
                            , "lies between the valid square numbers"
                            , show lower
                            , "and"
                            , show upper <> "."
                            ]
                    ]
            MatrixDimension0 →
                joinLines
                    [ "0-dimensional transition measure proffered."
                    , "Cannot construct an empty transition measure"
                    ]
            MatrixDimension1 →
                joinLines
                    [ "1-dimensional transition measure proffered."
                    , "Cannot construct a transition measure of size 1, must be of size 2 or greater."
                    ]
            ValueNegative xs →
                joinLines
                    [ "Negative values in the proffered transition measure."
                    , "Expected only non-negative values in range but found the following:"
                    , nicelyRenderList $ toList xs
                    ]
            ValueOverflow sCount xs →
                let upper = snd $ dicretizedValueBounds sCount
                in  joinLines
                        [ unwords
                            [ "Overflow values in the proffered the transition measure of"
                            , show sCount
                            , "symbols."
                            ]
                        , unwords
                            [ "Expected values less than"
                            , show upper
                            , "but found the following:"
                            ]
                        , nicelyRenderList $ toList xs
                        ]


instance (Show a) ⇒ Show (DiagnosisFailure a) where
    show =
        let tenseSensitiveShow = \case
                x :| [] →
                    case show x of
                        -- Empty case *should* never happen!
                        [] → "Transition Measure diagnosis failed for an unknown reason"
                        c : str → "Transition Measure diagnosis failed due to " <> (toLower c : str)
                xs →
                    fold
                        [ "Transition Measure diagnosis failed for the following reasons:\n"
                        , prettifyErr xs
                        ]
            bullet [] = []
            bullet (x : xs) = ("  • " <> drop 4 x) : xs
            prettifyErr = joinLines . fmap renderError . toList
            indentError = joinLines . bullet . fmap ("    " <>) . lines
            renderError = indentError . show
        in  tenseSensitiveShow . getDiagnosisErrors


nicelyRenderList ∷ (Show a) ⇒ [a] → String
nicelyRenderList =
    let indent = "  "
        limit = 75
        enclose xs = "{ " <> xs <> " }"
        showTokens = words . enclose . List.intercalate ", " . fmap show
        revUnwords = unwords . reverse
        revUnlines = List.intercalate ("\n" <> indent) . reverse
        breakLines =
            let go acc = \case
                    [] → revUnlines acc
                    ts →
                        let (taken, leftover) = takeLine (0, []) ts
                        in  go (taken : acc) leftover
            in  go []
        takeLine (n, taken) = \case
            [] → (revUnwords taken, [])
            toks@(t : ts) →
                let n' = n + length t
                in  if n' > limit
                        then (revUnwords taken, toks)
                        else takeLine (n' + 1, t : taken) ts
    in  breakLines . showTokens


{- |
Ensures that ther is at most /one/ 'TransitionMatrix.Diagnosis.Error.DiagnosisFailure'
which satisfies the predicate 'malformedInputShape' and that if such an error exists,
that it is the first error in the list.
-}
makeDiagnosisFailure ∷ (Ord a) ⇒ NonEmpty (DiagnosisError a) → DiagnosisFailure a
makeDiagnosisFailure xs =
    let sortedErrors = List.sort $ toList xs
        uniqueErrors = pruneDuplicates sortedErrors
        (badShapes, other) = List.partition malformedInputShape uniqueErrors
    in  DiagnosisFailure $ case badShapes of
            [] → fromList other
            x : _ → x :| other


{- |
Extract the list of diagnosed errors with the proffered transition measure.
-}
getDiagnosisErrors ∷ DiagnosisFailure a → NonEmpty (DiagnosisError a)
getDiagnosisErrors (DiagnosisFailure xs) = xs


arithmeticOperations ∷ Int
arithmeticOperations = 8


{- |
The /minimum/ and /maximum/ discretized values permitted for a matrix cell.
This is based on the representation of the discretized encoding.

/Note:/ the limit ensures that 2 raised to the 'arithmeticOperations' power
addition operations will not cause arithmetic overflow of the underlying type.
-}
dicretizedValueBounds ∷ (Integral i) ⇒ SymbolCount → (i, i)
dicretizedValueBounds symbols =
    let getBitWidth ∷ (FiniteBits b) ⇒ b → Int
        getBitWidth = subtract arithmeticOperations . finiteBitSize

        upperBitWidth ∷ Int
        upperBitWidth = getBitWidth (undefined ∷ DiscretizedResolution)

        lowerBitWidth ∷ Int
        lowerBitWidth = getBitWidth (undefined ∷ DiscretizedResolutionIota)

        bitWidth ∷ Int
        bitWidth
            | iota symbols = lowerBitWidth
            | otherwise = upperBitWidth

        upperValueFromBits ∷ Int → Integer
        upperValueFromBits b = pred $ (1 ∷ Integer) `shiftL` b
    in  (0, fromInteger $ upperValueFromBits bitWidth)


pruneDuplicates ∷ [DiagnosisError a] → [DiagnosisError a]
pruneDuplicates [] = []
pruneDuplicates v@(x : xs) =
    let f ∷ DiagnosisError a → DiagnosisError a → Bool
        f = sameErrorType
    in  case xs of
            [] → v
            y : ys | f x y → x : pruneDuplicates ys
            _ → x : pruneDuplicates xs


malformedInputShape ∷ DiagnosisError a → Bool
malformedInputShape =
    \case
        JaggedColumns{} → True
        JaggedRows{} → True
        NotSquareGrid{} → True
        NotSquareList{} → True
        _ → False


sameErrorType ∷ DiagnosisError a → DiagnosisError a → Bool
sameErrorType JaggedColumns{} JaggedColumns{} = True
sameErrorType JaggedRows{} JaggedRows{} = True
sameErrorType NotSquareGrid{} NotSquareGrid{} = True
sameErrorType NotSquareList{} NotSquareList{} = True
sameErrorType MatrixDimension0{} MatrixDimension0{} = True
sameErrorType MatrixDimension1{} MatrixDimension1{} = True
sameErrorType ValueNegative{} ValueNegative{} = True
sameErrorType ValueOverflow{} ValueOverflow{} = True
sameErrorType _ _ = False


joinLines ∷ [String] → String
joinLines = List.intercalate "\n"
