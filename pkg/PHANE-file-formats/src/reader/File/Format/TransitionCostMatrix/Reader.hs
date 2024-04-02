{- |
Functions for for parsing TCM files into an alphabet and native matrix form.
-}
module File.Format.TransitionCostMatrix.Reader (
    FileFormatTCM (..),
    tcmStreamReader,
) where

import File.Format.TransitionCostMatrix.Parser
import File.Format.TransitionCostMatrix.Types

