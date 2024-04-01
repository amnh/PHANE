------------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      :  Benchmark.TCM.Space
Copyright   :  (c) 2015-2021 Ward Wheeler
License     :  BSD-style

Maintainer  :  wheeler@amnh.org
Stability   :  provisional
Portability :  portable
-}
module Benchmark.TCM.Space (
    benchSpace,
) where

import Benchmark.Internal (measureParserSpace)
import Benchmark.TCM.Files
import Data.Foldable
-- import qualified Data.Text.IO          as T
import Data.Text.Lazy.IO qualified as TL
import File.Format.TransitionCostMatrix
import Text.Megaparsec
import Weigh


{- |
Perform the space allocation benchmarking of the TCM file parser.
-}
benchSpace ∷ [Weigh ()]
benchSpace =
    fold
        [ parserBenchmark ("lazy-text", TL.readFile) <$> tcmFiles
        --    , parserBenchmark (     "text",  T.readFile) <$> fastaInlineSequenceFiles
        ]


parserBenchmark
    ∷ ( Token s ~ Char
      , TraversableStream s
      , VisualStream s
      )
    ⇒ (String, FilePath → IO s)
    → FilePath
    → Weigh ()
parserBenchmark (prefix, reader) filePath = measureParserSpace prefix filePath reader tcmStreamParser
