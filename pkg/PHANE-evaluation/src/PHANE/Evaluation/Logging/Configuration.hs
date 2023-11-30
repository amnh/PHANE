{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedSums #-}

{- |
Configuration and initialization for logging within the 'PHANE.Evaluation' monad.
-}
module PHANE.Evaluation.Logging.Configuration (
    -- * Data-types
    LogConfiguration (..),
    LogFeed (..),

    -- * Constructor
    initializeLogging,

    -- * Destructor
    finalizeLogging,

    -- * Operations
    flushLogs,
    processMessage,

    -- * Mutators
    modConfigSTDERR,
    modConfigSTDOUT,
    modConfigStream,
    setFeedLevel,
) where

import Control.Monad.IO.Class
import Control.Monad.Random.Strict
import Data.Foldable (fold, traverse_)
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Data.String
import PHANE.Evaluation.Logging.Class
import PHANE.Evaluation.Logging.Message
import PHANE.Evaluation.Verbosity
import System.Directory
import System.FilePath.Posix
import System.IO (Handle, IOMode (WriteMode), hClose, hFlush, openFile, stderr, stdout)
import Test.QuickCheck.Arbitrary (Arbitrary (..), CoArbitrary (..), coarbitraryEnum)
import Test.QuickCheck.Gen (Gen (..), variant)
import Text.Read (readMaybe)


{- |
An individual feed which logs output.
-}
data LogFeed
    = Quashed
    | Defined
        {-# UNPACK #-} Bool
        {-# UNPACK #-} LogLevel
        -- ^ Verbosity level of the logger feed
        {-# UNPACK #-} LogLevel
        -- ^ Verbosity of last processed message
        {-# UNPACK #-} Handle


{- |
Configuration specifying how log messages are to be handled.
-}
data LogConfiguration = LogConfiguration
    { configSTDERR ∷ {-# UNPACK #-} LogFeed
    , configSTDOUT ∷ {-# UNPACK #-} LogFeed
    , configStream ∷ {-# UNPACK #-} LogFeed
    }


instance Arbitrary LogConfiguration where
    arbitrary =
        let gen h = do
                level ← (arbitrary ∷ Gen Word)
                occur ← (arbitrary ∷ Gen Word)
                let prev = case occur `mod` 7 of
                        0 → LogFail
                        1 → LogWarn
                        2 → LogDone
                        3 → LogInfo
                        4 → LogMore
                        5 → LogTech
                        _ → LogDump

                case level `mod` 8 of
                    0 → (\x → Defined x LogFail prev h) <$> arbitrary
                    1 → (\x → Defined x LogWarn prev h) <$> arbitrary
                    2 → (\x → Defined x LogDone prev h) <$> arbitrary
                    3 → (\x → Defined x LogInfo prev h) <$> arbitrary
                    4 → (\x → Defined x LogMore prev h) <$> arbitrary
                    5 → (\x → Defined x LogTech prev h) <$> arbitrary
                    6 → (\x → Defined x LogDump prev h) <$> arbitrary
                    _ → pure Quashed
        in  LogConfiguration <$> gen stderr <*> gen stdout <*> pure Quashed


instance CoArbitrary LogConfiguration where
    coarbitrary config =
        let x = configSTDERR config
            y = configSTDOUT config
            z = configStream config
        in  coarbitrary z . coarbitrary y . coarbitrary x


instance CoArbitrary LogFeed where
    coarbitrary = \case
        Quashed → variant 0
        Defined x y z _ → variant 1 . coarbitraryEnum z . coarbitraryEnum y . coarbitraryEnum x


{- |
__Time:__ \[ \mathcal{O}\left( 1 \right) \]

Create configuration for logging output stream to initialize an 'Evaluation'.

Allocates required file handles. Performs basic sanity checking.
-}
initializeLogging
    ∷ Verbosity
    -- ^ Verbosity level for STDOUT
    → Verbosity
    -- ^ Verbosity level for STDERR
    → Maybe (Verbosity, FilePath)
    -- ^ optional verbosity level for a file stream
    → IO LogConfiguration
initializeLogging vOut vErr vFile =
    let builderFilePath ∷ Maybe (Verbosity, FilePath) → IO LogFeed
        builderFilePath fileMay =
            let withVerbosity ∷ (Verbosity, FilePath) → Maybe (LogLevel, FilePath)
                withVerbosity (verb, path) = (\level → (level, path)) <$> verbosityToLogLevel verb

                openLogFileFeed ∷ (LogLevel, FilePath) → IO LogFeed
                openLogFileFeed (level, path) =
                    Defined False level maxBound
                        <$> (safelyMoveFile path *> openFile path WriteMode)
            in  maybe (pure Quashed) openLogFileFeed $ fileMay >>= withVerbosity

        builderStandard ∷ Verbosity → Handle → IO LogFeed
        builderStandard verb handle = pure $ case verbosityToLogLevel verb of
            Nothing → Quashed
            Just level → Defined True level maxBound handle
    in  LogConfiguration
            <$> builderStandard vErr stderr
            <*> builderStandard vOut stdout
            <*> builderFilePath vFile


{- |
__Time:__ \[ \mathcal{O}\left( n \right) \] where \[ n \] is the length of any
bytes remaining in the log feed buffers.

Release logging resources of the supplied 'LogConfiguration'.
All log feeds will have thier buffers flushed and subsequently no further logging
will be possible with the given 'LogConfiguration'.
-}
finalizeLogging ∷ LogConfiguration → IO ()
finalizeLogging config =
    let closeLogFeed ∷ LogFeed → IO ()
        closeLogFeed = \case
            Quashed → pure ()
            Defined _ _ _ x → hClose x
    in  flushLogs config *> closeLogFeed (configStream config)


{- |
__Time:__ \[ \mathcal{O}\left( n \right) \] where \[ n \] is the length of any
bytes remaining in the log feed buffers.

Flush all logging feeds in the 'LogConfiguration' to thier appropriate file handles.
-}
flushLogs ∷ LogConfiguration → IO ()
flushLogs =
    let flushFeed ∷ LogFeed → IO ()
        flushFeed = \case
            Quashed → pure ()
            Defined _ _ _ x → hFlush x
    in  traverse_ flushFeed . ([configSTDERR, configSTDOUT, configStream] <*>) . pure


{- |
__Time:__ \[ \mathcal{O}\left( n \right) \] where \[ n \] is the 'LogMessage' length.

Perform the I/O logging effect for the supplied 'LogMessage' to each of the approppriate streams.
The 'LogConfiguration' defines how to log to the STDOUT feed, STDERR feed, and an optional file
on disk feed.
Furthermore, the supplied 'LogLevel' dictates the whether the 'LogMessage' should be emitted or
supressed based on the 'PHANE.Evaluation.Verbosity' of each feed.

The semantics of @'processMessage' config level txt@

Emits on STDERR feed:

  */If and only if/ @level ≤ feedLevel (configSTDERR config)@

Emits on STDOUT feed:

  */If and only if/ __both__

    1. @level ≤ feedLevel (configSTDOUT config)@
    2. STDERR did /not/ emit!

Emits on Stream feed:

  */If and only if/ @level ≤ feedLevel (configStream config)@
-}
processMessage ∷ (MonadIO m) ⇒ LogConfiguration → LogLevel → LogMessage → m ()
processMessage config level txt =
    let admissible ∷ LogFeed → Bool
        admissible = permissibleVerbosity level

        emitStream = admissible feedForStream
        emitSTDERR = admissible feedForSTDERR
        emitSTDOUT = not emitSTDERR && admissible feedForSTDOUT

        feedForSTDERR = configSTDERR config
        feedForSTDOUT = configSTDOUT config
        feedForStream = configStream config

        whileOutputingTo ∷ (LogLevel → LogMessage) → LogFeed → IO ()
        whileOutputingTo prefixer = \case
            Quashed → pure ()
            Defined tinted _ prev handle →
                let prefix = renderingLevelLine prev level <> prefixer level
                    payload
                        | not tinted = prefix <> txt
                        | otherwise =
                            let (setColorPrefix, setColorSuffix) = renderedColorVals level
                                resetColor = "\o33[0;0m"
                            in  fold
                                    [ setColorPrefix
                                    , prefix
                                    , setColorSuffix
                                    , txt
                                    , resetColor
                                    ]
                in  outputMessage handle payload
    in  liftIO $ do
            when emitSTDERR $ renderingLevelNice `whileOutputingTo` feedForSTDERR
            when emitSTDOUT $ renderingLevelNice `whileOutputingTo` feedForSTDOUT
            when emitStream $ renderingLevelFull `whileOutputingTo` feedForStream


modConfigSTDERR ∷ (LogFeed → LogFeed) → LogConfiguration → LogConfiguration
modConfigSTDERR f x = x{configSTDERR = f $ configSTDERR x}


modConfigSTDOUT ∷ (LogFeed → LogFeed) → LogConfiguration → LogConfiguration
modConfigSTDOUT f x = x{configSTDOUT = f $ configSTDOUT x}


modConfigStream ∷ (LogFeed → LogFeed) → LogConfiguration → LogConfiguration
modConfigStream f x = x{configStream = f $ configStream x}


setFeedLevel ∷ Verbosity → LogFeed → LogFeed
setFeedLevel =
    let f v = \case
            Quashed → Quashed
            Defined x _ y z → Defined x v y z
    in  maybe (const Quashed) f . verbosityToLogLevel


permissibleVerbosity ∷ LogLevel → LogFeed → Bool
permissibleVerbosity level = \case
    Quashed → False
    Defined _ allowed _ _ → level <= allowed


{- |
If the 'LogLevel' of the current message is the same as the previous message,
then do not insert a new line. Rather append the message to the logger feed.
However, if the levels differ, then insert a newline into the log feed between
the previous and current log message.
-}
renderingLevelLine ∷ LogLevel → LogLevel → LogMessage
renderingLevelLine prev curr
    | not renderingContextSensitivity || prev == curr = ""
    | otherwise = "\n"


renderingContextSensitivity ∷ Bool
renderingContextSensitivity = False


renderingLevelFull ∷ LogLevel → LogMessage
renderingLevelFull = \case
    LogFail → "[FAIL] "
    LogWarn → "[WARN] "
    LogDone → "[DONE] "
    LogInfo → "[INFO] "
    LogMore → "[MORE] "
    LogTech → "[TECH] "
    LogDump → "[DUMP] "


renderingLevelNice ∷ LogLevel → LogMessage
renderingLevelNice = \case
    level@LogFail → renderingLevelFull level
    level@LogWarn → renderingLevelFull level
    level@LogTech → renderingLevelFull level
    level@LogDump → renderingLevelFull level
    _ → mempty


renderedColorVals ∷ LogLevel → (LogMessage, LogMessage)
renderedColorVals =
    let mkColorNum ∷ Word → Word → LogMessage
        mkColorNum m n = fromString . show $ m + n

        mkColorPref ∷ Word → LogMessage
        mkColorPref n = "\o33[0;" <> mkColorNum 90 n <> "m"

        mkColorSuff ∷ Word → LogMessage
        mkColorSuff n = "\o33[0;" <> mkColorNum 30 n <> "m"

        mkColor ∷ Word → (LogMessage, LogMessage)
        mkColor x = (mkColorPref x, mkColorSuff x)
    in  mkColor . \case
            LogFail → 1
            LogWarn → 3
            LogDone → 2
            LogInfo → 7
            LogMore → 6
            LogTech → 5
            LogDump → 5


{- |
 Checks to see if the supplied file path exists.

 If it does, it moves the existing file path, so that the supplied file path
 can be written to without overwriting data.

 The existing file path is renamed, adding a numeric suffix to the end. The
 function will try to rename the existing file path by adding the suffix ".0",
 however if that filepath also exists, it will add ".1", ".2", ".3", ",.4", etc.
 The suffix added will be one greater than the highest existing numeric suffix.
-}
safelyMoveFile ∷ FilePath → IO ()
safelyMoveFile fp =
    let getFilePathPrefixes = fmap (drop (length fp)) . filter (fp `isPrefixOf`)

        hasDotThenNumberSuffix ∷ FilePath → Maybe Word
        hasDotThenNumberSuffix = \case
            '.' : xs → readMaybe xs
            _ → Nothing

        getNumericSuffixes ∷ [FilePath] → [Word]
        getNumericSuffixes = mapMaybe (hasDotThenNumberSuffix . takeExtension)
    in  do
            absPath ← makeAbsolute fp
            exists ← doesFileExist absPath
            when exists $ do
                allFiles ← getDirectoryContents $ takeDirectory absPath
                let prefixed = getFilePathPrefixes allFiles
                let nextNum = case getNumericSuffixes prefixed of
                        [] → 0
                        x : xs → succ . maximum $ x :| xs
                let newName = absPath <> "." <> show nextNum
                renameFile absPath newName


{- |
Convert 'Verbosity' specification to a 'LogLevel'.
-}
verbosityToLogLevel ∷ Verbosity → Maybe LogLevel
verbosityToLogLevel = \case
    None → Nothing
    Fail → Just LogFail
    Warn → Just LogWarn
    Done → Just LogDone
    Info → Just LogInfo
    More → Just LogMore
    Tech → Just LogTech
    Dump → Just LogDump
