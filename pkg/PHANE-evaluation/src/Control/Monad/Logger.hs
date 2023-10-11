{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
A monadic extension classes that enables the support of logging.
-}
module Control.Monad.Logger (
    -- * Logging Type-class
    Logger (..),

    -- * Log Level Annotation
    LogLevel (..),

    -- * Verbosity Specification
    Verbosity (..),
    verbosityToLogLevel,
) where

import Control.Monad.Trans
import GHC.Stack (CallStack)
import System.Log.FastLogger


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


{- |
The 'Verbosity' specification has /eight/ levels. The verbosity levels form a total ordering from
minimal but severe information to maximal and excessive information.

The verbosity levels are as follows:

  (0) 'None': No information is output

  (1) 'Fail': Details related to an unrecoverable error

  (2) 'Warn': Indicates an unexpected, but recoverable event.
      May cause behavior which surprises the user as the evaluation continues.

  (3) 'Done': Concluding message of a major computational milestone.

  (4) 'Info': General information indicating the state of the computation to the user

  (5) 'More': Additional computational context for the user.
      This should be more expansive explanation of information from the previous 'Info' message

  (6) 'Tech': Debugging information to assist software developers in tracking execution.

  (7) 'Dump': Copious output of internal data-structures to permit inspection of computational state.

The 'Verbosity' specification values are coupled with corresponding 'LogLevel' values.
-}
data Verbosity
    = None
    | Fail
    | Warn
    | Done
    | Info
    | More
    | Tech
    | Dump


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
    logWith ∷ (?callStack ∷ CallStack, ToLogStr s) ⇒ LogLevel → s → m ()


deriving stock instance Eq LogLevel


deriving stock instance Ord LogLevel


deriving stock instance Read LogLevel


deriving stock instance Show LogLevel


deriving stock instance Eq Verbosity


deriving stock instance Ord Verbosity


deriving stock instance Read Verbosity


deriving stock instance Show Verbosity


instance Enum Verbosity where
    toEnum = \case
        1 → Fail
        2 → Warn
        3 → Done
        4 → Info
        5 → More
        6 → Tech
        i | i <= 0 → None
        _ → Dump


    fromEnum = \case
        None → 0
        Fail → 1
        Warn → 2
        Done → 3
        Info → 4
        More → 5
        Tech → 6
        Dump → 7


    succ = \case
        None → Fail
        Fail → Warn
        Warn → Done
        Done → Info
        Info → More
        More → Tech
        Tech → Dump
        Dump → Dump


    pred = \case
        None → None
        Fail → None
        Warn → Fail
        Done → Warn
        Info → Done
        More → Info
        Tech → More
        Dump → Tech


    enumFrom = \case
        None → [None, Fail, Warn, Done, Info, More, Tech, Dump]
        Fail → [Fail, Warn, Done, Info, More, Tech, Dump]
        Warn → [Warn, Done, Info, More, Tech, Dump]
        Done → [Done, Info, More, Tech, Dump]
        Info → [Info, More, Tech, Dump]
        More → [More, Tech, Dump]
        Tech → [Tech, Dump]
        Dump → [Dump]


    enumFromTo x y = takeWhile (<= y) $ enumFrom x


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
