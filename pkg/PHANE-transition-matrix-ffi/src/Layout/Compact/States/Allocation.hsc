{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Strict #-}

#include "c_code_alloc_setup.h"

{- |
License     :  BSD-style

Maintainer  :  wheeler@amnh.org
Portability :  portable

Generate the 2D and 3D compact TCM matricies used for FFI calls.

For notes on usage, data construction and external see referenced C
compilation units, and also driver.c, which is not imported, but is
included indirectory for reference.
-}
module Layout.Compact.States.Allocation (
    -- * Type synonym
    DiscretizedResolutionIota,
    
    -- * Construction
    initialize,
) where

import Data.Foldable (fold)
import Data.List qualified as List
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as V
import Foreign
import Foreign.C.Types
import Layout.Compact.States.Structure
import System.IO.Unsafe (unsafePerformIO)


debugging :: Bool
debugging = False


nice :: Show a => String -> a -> [String]
nice key vals
    | debugging = [ key <> ":\t" <> show vals ]
    | otherwise = []


{- |
The representation of values sent across the FFI.
-}
type DiscretizedResolutionIota = CUShort


{- |
Create and allocate cost matrix first argument, TCM, is only for non-ambiguous
nucleotides, and it used to generate the entire cost matrix, which includes ambiguous elements.
TCM is row-major, with each row being the left character element.
It is therefore indexed not by powers of two, but by cardinal integer.
-}
foreign import ccall unsafe "c_code_alloc_setup.h setUp2dCostMtx"
    initializeCostMatrix2D_FFI
        ∷ Ptr FFI2D
        → Ptr DiscretizedResolutionIota
        -- ^ tcm (The SDM row-major vector)
        → DiscretizedResolutionIota
        -- ^ gap_open_cost
        → CSize
        -- ^ alphSize
        → IO ()


foreign import ccall unsafe "c_code_alloc_setup.h setUp3dCostMtx"
    initializeCostMatrix3D_FFI
        ∷ Ptr FFI3D
        → Ptr DiscretizedResolutionIota
        -- ^ tcm (The SDM row-major vector)
        → DiscretizedResolutionIota
        -- ^ gap_open_cost
        → CSize
        -- ^ alphSize
        → IO ()


{-
-- |
-- /ϴ(a⁵)/ where /a ≤ 8/ is the size of the character alphabet.
--
-- Generate the 2D and 3D dense transiton cost matricies ('TCMρ') from the
-- supplied symbol change matrix function ('SDMρ') with linear dimensions of
-- the alphabet symbol count.
--
-- Prefer 'fromSDMρ' to 'fromSDMλ', as this function performs /ϴ(a²)/ less
-- allocation than that function.
fromSDMρ
  :: SymbolDistance           -- ^ The gap open cost. A zero value indicates non-affine alignment context
  -> SymbolDistanceMatrixSquare -- ^ The dense, pre-computed matrix of costs to shift between symbols.
  -> TCMρ
fromSDMρ penalty scmρ =
    let (clip,size) = clampSize $ sizeOfSDM scmρ
        scmVector   = clip $ rowMajorVector scmρ
    in  initialize size penalty scmVector

-- |
-- /ϴ(a⁵)/ where /a ≤ 8/ is the size of the character alphabet.
--
-- Generate the 2D and 3D dense transiton cost matricies ('TCMρ') from the
-- supplied symbol change matrix function ('SDMλ') with linear dimensions of
-- the alphabet symbol count.
--
-- Prefer 'fromSDMρ' to 'fromSDMλ', as that function performs /ϴ(a²)/ less
-- allocation than this function.
fromSDMλ
  :: SymbolCount      -- ^ The character alphabet size
  -> SymbolDistance -- ^ The gap open cost. A zero value indicates non-affine alignment context
  -> SDMλ             -- ^ The function defining the costs to shift between symbols.
  -> TCMρ
fromSDMλ alphabetSize openningCost scmλ =
    let (_, size) = clampSize alphabetSize
        scmVector = coerce . rowMajorVector $ unsafeFromSDMλ scmλ size
    in  initialize size openningCost scmVector
-}

{- |
/ϴ(a⁵)/ where /a ≤ 8/ is the number of symbols in the character alphabet.

Generate the 2D and 3D compact state transition matricies from the supplied
row-major vecotr of symbol transtion distances linear dimensions of the
symbol count.
-}
initialize
    ∷ forall e. (Integral e, Storable e) => Word
    -- ^ Number of symbols to allocate for
    → Word
    -- ^ Penalty cost to begin a gap sequence
    → Vector e
    → TCMρ
initialize dim penalty inputVector =
    let unsafeTruncateElements :: Vector e -> Vector DiscretizedResolutionIota
        unsafeTruncateElements = V.map fromIntegral

        clip ∷ Vector DiscretizedResolutionIota → Vector DiscretizedResolutionIota
        (clip, size) = clampSize dim
        safeVector = clip $ unsafeTruncateElements inputVector
        openPenalty = fromIntegral penalty
        dimension = fromIntegral size
        rowLen = fromEnum size
        firstRow = V.slice 1 (rowLen - 1) safeVector
        firstCol = V.generate (rowLen - 1) $ \i → safeVector V.! ((i + 1) * rowLen)
        maxDel = V.maximum firstCol
        maxIns = V.maximum firstRow
        minDel = V.minimum firstCol
        minIns = V.minimum firstRow

        conditionalOutput :: IO ()
        conditionalOutput
            | not debugging = pure ()
            | otherwise = putStrLn . unlines' $ fold
                [ nice "size" size
                , nice "safeVector" safeVector
                , nice "openPenalty" openPenalty
                , nice "dimension" dimension
                , nice "rowLen" rowLen
                , nice "firstRow" firstRow
                , nice "firstCol" firstCol
                , nice "maxDel" maxDel
                , nice "maxIns" maxIns
                , nice "minDel" minDel
                , nice "minIns" minIns
                ]
        
    in  unsafePerformIO $ conditionalOutput *>
            ( V.unsafeWith safeVector $ \arr → do
                cm2D ← malloc ∷ IO (Ptr FFI2D)
                cm3D ← malloc ∷ IO (Ptr FFI3D)
                _ ← initializeCostMatrix2D_FFI cm2D arr openPenalty dimension
                _ ← initializeCostMatrix3D_FFI cm3D arr openPenalty dimension
                pure
                    StateTransitionsCompact
                        { gapPenalty = penalty
                        , maxDelCost = fromIntegral maxDel
                        , maxInsCost = fromIntegral maxIns
                        , minDelCost = fromIntegral minDel
                        , minInsCost = fromIntegral minIns
                        , matrixSize = size
                        , matrix2D = cm2D
                        , matrix3D = cm3D
                        }
            )

{- |
Ensure that the size does not exceed 'maximumDimension'.
-}
clampSize ∷ Word → (Vector DiscretizedResolutionIota → Vector DiscretizedResolutionIota, Word)
clampSize n =
    let dim = min maximumDimension n
        
        truncateVector ∷ Vector DiscretizedResolutionIota → Vector DiscretizedResolutionIota
        truncateVector =
            let x = fromEnum dim
            in  V.slice 0 $ x * x

        transform ∷ Vector DiscretizedResolutionIota → Vector DiscretizedResolutionIota
        transform
            | n <= maximumDimension = id
            | otherwise = truncateVector
    in  (transform, dim)


unlines' :: [String] -> String
unlines' = List.intercalate "\n"
