{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Timing benchmarks for TCM file parser
-}
module Benchmark.TCM.Time (
    benchTime,
) where

import Benchmark.Internal (measureParserTime)
import Benchmark.TCM.Files
import Control.DeepSeq (NFData)
import Criterion.Main
import Data.Foldable
-- import qualified Data.Text.IO          as T
import Data.Text.Lazy.IO qualified as TL
import File.Format.TransitionCostMatrix.Reader
import Text.Megaparsec


{- |
Perform the run time benchmarking of the TCM file parser.
-}
benchTime ∷ [Benchmark]
benchTime =
    fold
        [ parserBenchmark ("lazy-text", TL.readFile) <$> tcmFiles
        --    , parserBenchmark (     "text",  T.readFile) <$> fastcSequenceFiles
        ]


parserBenchmark
    ∷ ( NFData s
      , Token s ~ Char
      , TraversableStream s
      , VisualStream s
      )
    ⇒ (String, FilePath → IO s)
    → FilePath
    → Benchmark
parserBenchmark (prefix, reader) filePath = measureParserTime prefix filePath reader tcmStreamReader
