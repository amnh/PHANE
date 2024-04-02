{- |
Measure the usage of allocated space for various file parsers.
-}
module Main (main) where

-- import Benchmark.FASTA.Space qualified as FASTA
-- import Benchmark.FASTC.Space qualified as FASTC
-- import Benchmark.Newick.Space qualified as Newick
import Benchmark.TCM.Space qualified as TCM
import Data.Foldable
import Weigh


{- |
Entry point for the space allocation performance benchmark suite /all/ the file parsers.
-}
main ∷ IO ()
main = mainWith $ do
    setColumns [Case, Allocated, GCs, Max]
    sequenceA_ $
        fold [TCM.benchSpace]

{-
            [ FASTA.benchSpace
            , FASTC.benchSpace
            , Newick.benchSpace
            , TCM.benchSpace
            ]
-}
