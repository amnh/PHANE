{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedSums #-}

{- |
Configuration and initialization for logging within the 'Control.Evaluation' monad.
-}
module Control.Evaluation.Logging.Configuration (
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

import Control.Applicative (Alternative (..))
import Control.Arrow ((&&&))
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Evaluation.Logging.Class
import Control.Evaluation.Logging.Message
import Control.Evaluation.Result
import Control.Evaluation.Verbosity
import Control.Exception
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Random.Strict
import Control.Monad.Reader (MonadReader (..), ReaderT (..), withReaderT)
import Control.Monad.Zip (MonadZip (..))
import Control.Parallel.Strategies
import Data.Bimap qualified as BM
import Data.Bits (xor)
import Data.Foldable (fold, toList, traverse_)
import Data.Function ((&))
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe, maybe)
import Data.Semigroup (sconcat)
import Data.String
import Data.Text.IO.Utf8 (hPutStr)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Conc (getNumCapabilities)
import GHC.Generics
import GHC.Stack (CallStack)
import GHC.Stack qualified as GHC
import System.CPUTime (cpuTimePrecision, getCPUTime)
import System.Directory
import System.ErrorPhase
import System.Exit
import System.FilePath.Posix
import System.IO (Handle, IOMode (WriteMode), hClose, hFlush, openFile, stderr, stdout)
import System.Log.FastLogger hiding (check)
import System.Random.Stateful
import Text.Read (readMaybe)


{- |
An individual feed which logs output.
-}
data LogFeed
    = Quashed
    | Defined
        { feedColor ∷ {-# UNPACK #-} Bool
        , feedLevel ∷ {-# UNPACK #-} LogLevel
        , feedSpout ∷ {-# UNPACK #-} Handle
        }


{- |
Configuration specifying how log messages are to be handled.
-}
data LogConfiguration = LogConfiguration
    { configSTDERR ∷ {-# UNPACK #-} LogFeed
    , configSTDOUT ∷ {-# UNPACK #-} LogFeed
    , configStream ∷ {-# UNPACK #-} LogFeed
    }


{-
doLogCs ∷ (MonadIO m) ⇒ LogConfiguration → LogLevel → CallStack → LogMessage → m ()
doLogCs config level cs txt =
    let loc = case GHC.getCallStack cs of
            ((_, l) : _) → GHC.srcLocFile l <> ":" <> show (GHC.srcLocStartLine l)
            _ → "unknown"
    in  doLog config level (toLogMessage loc) txt
-}

{- |
Create configuration for logging output stream to initialize an 'Evaluation'.
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

                blankFile ∷ (Verbosity, FilePath) → Maybe (Verbosity, FilePath)
                blankFile = \case
                    (_, []) → Nothing
                    x → Just x

                openLogFileFeed ∷ (LogLevel, FilePath) → IO LogFeed
                openLogFileFeed (level, path) =
                    Defined False level
                        <$> (safelyMoveFile path *> openFile path WriteMode)
            in  maybe (pure Quashed) openLogFileFeed $ fileMay >>= withVerbosity

        builderStandard ∷ Verbosity → Handle → IO LogFeed
        builderStandard verb handle = pure $ case verbosityToLogLevel verb of
            Nothing → Quashed
            Just level → Defined True level handle
    in  LogConfiguration
            <$> builderStandard vErr stderr
            <*> builderStandard vOut stdout
            <*> builderFilePath vFile


{- |
Release logging resources of the supplied 'LogConfiguration'.
All log feeds will have thier buffers flushed and subsequently no further logging
will be possible with the given 'LogConfiguration'.
-}
finalizeLogging ∷ LogConfiguration → IO ()
finalizeLogging config =
    let closeLogFeed ∷ LogFeed → IO ()
        closeLogFeed = \case
            Quashed → pure ()
            defined → hClose $ feedSpout defined
    in  flushLogs config *> closeLogFeed (configStream config)


{- |
Flush all logging feeds in the 'LogConfiguration' to thier appropriate file handles.
-}
flushLogs ∷ LogConfiguration → IO ()
flushLogs =
    let flushFeed ∷ LogFeed → IO ()
        flushFeed = \case
            Quashed → pure ()
            defined → hFlush $ feedSpout defined
    in  traverse_ flushFeed . ([configSTDERR, configSTDOUT, configStream] <*>) . pure


{- |
Perform the I/O logging effect for the supplied 'LogMessage' to each of the approppriate streams.
The 'LogConfiguration' defines how to log to the STDOUT feed, STDERR feed, and an optional file
on disk feed.
Furthermore, the supplied 'LogLevel' dictates the whether the 'LogMessage' should be emitted or
supressed based on the 'Control.Evaluation.Verbosity' of each feed.

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
            Defined tinted _ handle →
                let prefix = prefixer level
                in  outputMessage handle $ case tinted of
                        False → prefix <> txt
                        True →
                            let (setColorPrefix, setColorSuffix) = renderedColorVals level
                                resetColor = "\o33[0;0m"
                            in  fold
                                    [ setColorPrefix
                                    , prefix
                                    , setColorSuffix
                                    , txt
                                    , resetColor
                                    ]
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
    let f v x = x{feedLevel = v}
    in  maybe (const Quashed) f . verbosityToLogLevel


permissibleVerbosity ∷ LogLevel → LogFeed → Bool
permissibleVerbosity level = \case
    Quashed → False
    Defined _ allowed _ → level <= allowed


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
