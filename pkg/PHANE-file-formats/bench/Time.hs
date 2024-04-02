{- |
Benchmark file parser runtime(s).
-}
module Main (main) where

-- import Benchmark.FASTA.Time qualified as FASTA
-- import Benchmark.FASTC.Time qualified as FASTC
-- import Benchmark.Newick.Time qualified as Newick
import Benchmark.TCM.Time qualified as TCM
import Criterion.Main
import Data.Foldable


{- |
Entry point for the run time performance benchmark suite /all/ the file parsers.
-}
main ∷ IO ()
main =
    defaultMain $
        fold [TCM.benchTime]

{-
            [ FASTA.benchTime
            , FASTC.benchTime
            , Newick.benchTime
            , TCM.benchTime
            ]
-}
