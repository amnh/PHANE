{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}

module Layout.Compact.Symbols.Triangular (
    -- * Representational Type
    SymbolDistanceMatrixTriangular (..),
    SDMT,

    -- * Queries
    bytesSizeMatrixTriangular,
    rowMajorVector,
    lowerTriangleOfSquare,
) where

import Control.DeepSeq
import Data.Bits (shiftR)
import Data.Coerce
import Data.Data
import Data.Foldable
import Data.Ix (Ix (range))
import Data.Ord
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as V
import Data.Word
import GHC.Generics
import Layout.Compact.Symbols.Internal (SymbolDistanceMatrix (..))
import Layout.Compact.Symbols.Internal qualified as SDM
import Layout.Compact.Symbols.Square (SymbolDistanceMatrixSquare)
import Layout.Compact.Symbols.Square qualified as SDMS
import Measure.Range
import Measure.Transition
import Measure.Unit.SymbolCount
import Measure.Unit.SymbolIndex


{- |
A data structure for storing a square array of sizeality
greater than or equal to two, with positive cost values at the array indices.
Values are stored in an unboxed structure for cache efficiency.

Attempts to construct an empty or singleton 'SDMT' through the above
constructors will result in a runtime exception.
-}
newtype SymbolDistanceMatrixTriangular = SDMT {sdm ∷ SymbolDistanceMatrix}
    deriving stock (Eq, Data, Generic)


type SDMT = SymbolDistanceMatrixTriangular


instance NFData SymbolDistanceMatrixTriangular where
    rnf (SDMT m) = rnf m


{- |
Any structural representation which can produce a Symbol Change Matrix.
-}
instance HasEditExtrema SymbolDistanceMatrixTriangular where
    {-# INLINEABLE maxEdit #-}
    maxEdit a = max (maxInsertion a) $ maxDeletion a


    {-# INLINEABLE minEdit #-}
    minEdit a = min (minInsertion a) $ minDeletion a


    maxDeletion = firstColExtrama maximumBy


    minDeletion = firstColExtrama minimumBy


    maxInsertion = firstRowExtrema V.maximum


    minInsertion = firstRowExtrema V.minimum


{- |
Any structural representation which can produce a Symbol Change Matrix.
-}
instance HasSymbolDistances SymbolDistanceMatrixTriangular where
    {-# INLINE symbolDistances #-}
    symbolDistances = symbolIndexing


{- |
A structure which can derive the number of alphabet symbols associated with it.
-}
instance HasSymbolCount SymbolDistanceMatrixTriangular where
    {-# INLINE symbolCount #-}
    symbolCount = symbolCount . sdm


instance MeasurableRange SymbolDistanceMatrixTriangular SymbolIndex where
    {-# INLINE measureRange #-}
    measureRange = measureRange . sdm


{- |
A pretty printed custom show instance for Symbol Distance Matrix.
-}
instance Show SymbolDistanceMatrixTriangular where
    show sdmt = headerLine <> matrixLines
        where
            renderRow i = ("  " <>) . unwords $ renderValue <$> [symbolIndexing sdmt i j | j ← rangeValues]
            matrixLines = unlines $ renderRow <$> rangeValues
            rangeValues = range $ measureRange sdmt
            headerLine = '\n' : unwords ["SDM:", show $ symbolCount sdmt, "x", show $ symbolCount sdmt, "\n"]
            maxValue = V.maximum $ rowMajorVector sdmt
            padSpacing = length $ show maxValue
            renderValue x = pad <> shown
                where
                    shown = show x
                    pad = (padSpacing - length shown) `replicate` ' '


lowerTriangleOfSquare ∷ SymbolDistanceMatrixSquare → SymbolDistanceMatrixTriangular
lowerTriangleOfSquare square =
    let (SymbolDistanceMatrix sc@(SymbolCount w) vec) = coerce square
        dim = fromEnum w
        len = dim * (dim + 1) `shiftR` 1
        arr =
            let r = [0 .. dim - 1]
                p = [V.unsafeIndex vec (dim * i + j) | i ← r, j ← r, i >= j]
            in  V.fromListN len p
    in  SDMT $ SymbolDistanceMatrix sc arr


{- |
/O(1)/

Indexing without bounds checking.
-}
{-# INLINE symbolIndexing #-}
symbolIndexing ∷ SymbolDistanceMatrixTriangular → SymbolDistanceλ
symbolIndexing sdmt i j | i < j = symbolIndexing sdmt j i
symbolIndexing sdmt i j =
    let i' = coerce i ∷ Word
        j' = coerce j ∷ Word
        t = i' * (i' + 1) `shiftR` 1
        p = fromEnum $ t + j'
    in  SDM.index (sdm sdmt) p


{- |
/O(1)/

Computes the number of bytes used to store the 'SymbolDistanceMatrixTriangular'.
-}
{-# INLINE bytesSizeMatrixTriangular #-}
bytesSizeMatrixTriangular ∷ SymbolDistanceMatrixTriangular → Int
bytesSizeMatrixTriangular = SDM.bytesSizeSymbolMatrix . sdm


{- |
Deconstructs the 'SymbolDistanceMatrixTriangular' to expose the underlying unboxed 'Vector'.
-}
{-# INLINE rowMajorVector #-}
rowMajorVector ∷ SymbolDistanceMatrixTriangular → Vector Word16
rowMajorVector = SDM.rowMajorVector . sdm


firstRowExtrema
    ∷ ( Integral b
      , Num c
      )
    ⇒ (Vector Word16 → b)
    → SymbolDistanceMatrixTriangular
    → c
firstRowExtrema f (SDMT (SymbolDistanceMatrix (SymbolCount n) v)) =
    fromIntegral . f $ V.slice 0 (fromEnum n) v


firstColExtrama
    ∷ ( Integral a
      , Num b
      )
    ⇒ ((Int → Int → Ordering) → [Int] → a)
    → SymbolDistanceMatrixTriangular
    → b
firstColExtrama f (SDMT (SymbolDistanceMatrix (SymbolCount n) v)) =
    let r = fromEnum n
        g i = v V.! (i * (i + 1) `shiftR` 1)
    in  fromIntegral $ f (comparing g) [0 .. r - 1]
