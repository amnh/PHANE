{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
A 'LogMessage' represents the atomic logging component for the 'Control.Evaluation' monad.

They constructed by using the 'IsString' type-class method 'fromString'
as well as with the following functions:

  * 'loggedChar'
  * 'loggedDecimal'
  * 'loggedHexadecimal'
  * 'loggedReal'
  * 'loggedText'

Outputting of a component 'LogMessage' can be performed by'outputMessage'.

The 'LogMessage' data-type internally uses the linearized 'Builder' for 'Text' values to
improve efficiency.
-}
module Control.Evaluation.Logging.Message (
    -- * Data-type
    LogMessage (),

    -- * Constructors
    loggedChar,
    loggedDecimal,
    loggedHexadecimal,
    loggedReal,
    loggedText,

    -- * Consumer
    finalizedText,
    outputMessage,
) where

import Control.Applicative (Alternative (..))
import Data.Bits (FiniteBits)
import Data.Coerce (coerce)
import Data.Foldable (fold, foldl', toList)
import Data.List (intersperse)
import Data.Ratio
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Builder.Linear
import Data.Text.IO.Utf8 (hPutStr)
import System.IO (Handle)


{- |
Log message builder.

Use ('<>') to append two LogMessage in O(1).
-}
newtype LogMessage = LogMessage Builder


deriving newtype instance Semigroup LogMessage


deriving newtype instance Monoid LogMessage


deriving newtype instance IsString LogMessage


{- |
__Time:__ \[ \mathcal{O}\left( 1 \right) \]

Construct a 'LogMessage' from a unicode character.
-}
loggedChar ∷ Char → LogMessage
loggedChar = coerce @Builder @LogMessage . fromChar


{- |
__Time:__ \[ \mathcal{O}\left( log_10\left( v \right) \right) \]

Construct a 'LogMessage' from an integral value, rendering it in base @10@.
-}
loggedDecimal ∷ (Integral a, FiniteBits a) ⇒ a → LogMessage
loggedDecimal = coerce @Builder @LogMessage . fromDec


{- |
__Time:__ \[ \mathcal{O}\left( log_16\left( v \right) \right) \]

Construct a 'LogMessage' from an integral value, rendering it in base @16@.
-}
loggedHexadecimal ∷ (Integral a, FiniteBits a) ⇒ a → LogMessage
loggedHexadecimal = coerce @Builder @LogMessage . fromHex


{- |
__Time:__ \[ \mathcal{O}\left( log_10\left( v \right) \right) \]

Construct a 'LogMessage' from a 'Real' value.

Converts the number to a rational number and renders it in decimal notation with a decimal point.
-}
loggedReal ∷ (Real a) ⇒ a → LogMessage
loggedReal = coerce @Builder @LogMessage . fromString . renderRational . toRational


{- |
__Time:__ \[ \mathcal{O}\left( 1 \right) \]

Construct a 'LogMessage' from a 'Text' value.
-}
loggedText ∷ Text → LogMessage
loggedText = coerce @Builder @LogMessage . fromText


{- |
Convert the 'LogMessage' to 'Text', finalizing the underlying text builder.
-}
finalizedText ∷ LogMessage → Text
finalizedText = runBuilder . coerce @LogMessage @Builder


{- |
Output the 'LogMessage' to the file 'Handle'.

Only at the time of invoking 'outputMessage' does the the 'LogMessage' finalize
the 'Text' value to be output to the handle's buffer.
-}
outputMessage ∷ Handle → LogMessage → IO ()
outputMessage handle = hPutStr handle . finalizedText


type SeqTwoInts = Seq (Integer, Integer)


renderRational ∷ Rational → String
renderRational rat =
    let num = numerator rat
        den = denominator rat
    in  case num `quotRem` den of
            (q, 0) → show q
            (q, r) →
                let -- Unicode literal for the "combining overline character".
                    -- Place an overline above the character /before/ itself in the string.
                    addOverline =
                        let !c = '\x0305'
                        in  (c :) . intersperse c

                    digitMax = length . show $ max num den

                    render ∷ Seq (Integer, Integer) → String
                    render = foldMap (show . fst)

                    go ∷ Integer → SeqTwoInts → String
                    go !v xs =
                        case hasCycle xs of
                            Just (static, repeating) → render static <> addOverline (render repeating)
                            _ | v == 0 || length xs > digitMax → render xs
                            _ →
                                let t@(_, e) = (v * 10) `quotRem` den
                                in  go e $ xs |> t

                    hasCycle ∷ SeqTwoInts → Maybe (SeqTwoInts, SeqTwoInts)
                    hasCycle x =
                        let dropEmptySeq ∷ Seq a → Seq a
                            dropEmptySeq xs@Empty = xs
                            dropEmptySeq (xs :|> _) = xs

                            !n = Seq.length x

                            f a e =
                                a
                                    <|> let !m = Seq.length e
                                            !(s, t) = Seq.splitAt (n - (2 * m)) x
                                        in  if t == e <> e
                                                then Just (s, e)
                                                else Nothing
                        in  foldl' f Nothing . Seq.reverse . dropEmptySeq $ Seq.tails x
                in  fold [show q, ".", toList (go (if q == 0 then num else r) mempty)]
