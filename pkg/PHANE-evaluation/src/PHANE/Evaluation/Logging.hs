{- |
Logging functionality for the 'PHANE.Evaluation.Evaluation' monad.

There are two primary type-classes used for logging:

  1.  'Logger' exposes logging functionality within a monadic context.

  2.  'Loggable' provides an abstraction interface to define how to render a
      data-type within a log feed.

Additionally, the 'logTokenStructure' provides a convient method for defining a
rendering scheme of a more complex structure composed of 'Loggable' components.
-}
module PHANE.Evaluation.Logging (
    -- ** Type-classes
    Logger (..),
    Loggable (..),

    -- ** Data-types
    LogConfiguration (),
    LogLevel (..),
    LogMessage,

    -- ** Operations
    initializeLogging,
    logTokenStructure,
) where

import PHANE.Evaluation.Logging.Class
import PHANE.Evaluation.Logging.Configuration
import PHANE.Evaluation.Logging.Message

