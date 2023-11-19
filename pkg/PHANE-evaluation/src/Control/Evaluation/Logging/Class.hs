{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Monadic extension classes that enables the support of logging.
-}
module Control.Evaluation.Logging.Class (
    -- * Logging Type-classes
    Logger (..),
    Loggable (..),

    -- ** Log Structuring
    logTokenStructure,

    -- ** Log Level Annotation
    LogLevel (..),
) where

import Control.Evaluation.Logging.Message
import Control.Monad.Trans
import Data.Foldable (fold, toList)
import Data.Foldable1 (intercalate1)
import Data.Int
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ratio (Ratio)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Word
import Foreign.C.Types
import Foreign.Ptr (IntPtr, WordPtr)
import System.Posix.Types


{- |
The /seven/ 'LogLevel' values should be thought of as being partitioned into /three/ main categories:

  * __Exceptional__ (@1 - 2@): These are for notifying of unexpected conditions

  * __User Facing__ (@3 - 5@): These report progress to the user with adjustable specificity

  * __Debug Trace__ (@6 - 7@): These are for software developers to trace and inspect program state.

The 'LogLevel' values are coupled with corresponding 'Verbosity' specification values.
-}
data LogLevel
    = LogFail
    | LogWarn
    | LogDone
    | LogInfo
    | LogMore
    | LogTech
    | LogDump


deriving stock instance Eq LogLevel


deriving stock instance Ord LogLevel


deriving stock instance Read LogLevel


deriving stock instance Show LogLevel


{- |
  A 'MonadFail' that has been extended to support "information" and "warning"
  level messages.

  Typeclass Laws:

  Failure nullification:

 > fail x <?> y === fail x
 > fail x <@> y === fail x

  Assocativity:

 > let a = v <?> x in a <?> y <?> z === let a = v <?> x <?> y in a <?> z
 > let a = v <@> x in a <@> y <@> z === let a = v <@> x <@> y in a <@> z
-}
class (MonadIO m) ⇒ Logger m where
    logWith ∷ (Loggable s) ⇒ LogLevel → s → m ()


{- |
Data-types with an explicit formatted rendering for logs.
-}
class Loggable a where
    logToken ∷ a → LogMessage


instance Loggable LogMessage where
    {-# INLINE logToken #-}
    logToken = id


instance Loggable Char where
    {-# INLINE logToken #-}
    logToken = loggedChar


instance Loggable Text where
    {-# INLINE logToken #-}
    logToken = loggedText


instance Loggable String where
    {-# INLINE logToken #-}
    logToken = fromString


instance Loggable CBool where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CChar where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CInt where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CIntMax where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CIntPtr where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable CLLong where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CLong where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CPtrdiff where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable CSChar where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CShort where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CSigAtomic where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable CSize where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CUChar where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CUInt where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CUIntMax where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CUIntPtr where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable CULLong where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CULong where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CUShort where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CWchar where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable IntPtr where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable WordPtr where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable Int16 where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable Int32 where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable Int64 where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable Int8 where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable Word16 where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable Word32 where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable Word64 where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable Word8 where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CBlkCnt where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CBlkSize where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CClockId where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CDev where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable CFsBlkCnt where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CFsFilCnt where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CGid where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable CId where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable CIno where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable CKey where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable CMode where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable CNfds where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable CNlink where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable COff where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable CPid where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable CRLim where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CSocklen where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CSsize where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CTcflag where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable CUid where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable Fd where
    {-# INLINE logToken #-}
    logToken = loggedHexadecimal


instance Loggable Integer where
    {-# INLINE logToken #-}
    logToken = fromString . show


instance Loggable Int where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable Word where
    {-# INLINE logToken #-}
    logToken = loggedDecimal


instance Loggable CDouble where
    {-# INLINE logToken #-}
    logToken = loggedReal


instance Loggable CFloat where
    {-# INLINE logToken #-}
    logToken = loggedReal


instance Loggable Double where
    {-# INLINE logToken #-}
    logToken = loggedReal


instance Loggable Float where
    {-# INLINE logToken #-}
    logToken = loggedReal


instance (Integral a) ⇒ Loggable (Ratio a) where
    {-# INLINE logToken #-}
    logToken = loggedReal


{- |
Logs many tokens into a structured rendering.
A call to @'logTokenStructure' vacated opening divider closing tokens@ has these semantics:

  * /If and only if/ @'null' tokens@; only log @vacated@.

  * Otherwise the call will /sequentially/ log the following:
      1. @opening@
      2. each element of @tokens@ seperated by @divider@
      3. @closing@
-}
logTokenStructure
    ∷ (Foldable f, Loggable vacated, Loggable opening, Loggable divider, Loggable closing, Loggable token)
    ⇒ vacated
    → opening
    → divider
    → closing
    → f token
    → LogMessage
logTokenStructure vacated opening divider closing tokens = case toList tokens of
    [] → logToken vacated
    x : xs →
        let first = logToken opening
            final = logToken closing
            fence = logToken divider
            hoard = logToken <$> x :| xs
        in  fold [first, intercalate1 fence hoard, final]
