{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}

module Layout.Compact.Symbols.Internal (
    -- * Representational Type
    SymbolDistanceMatrix (..),
    DiscretizedResolution,

    -- * Queries
    index,
    rowMajorVector,
    bytesSizeSymbolMatrix,
) where

import Control.DeepSeq
import Data.Data


#if MIN_VERSION_base(4,20,0)
#else
import Data.Foldable (foldl')
#endif
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as V
import Data.Word
import Foreign.Ptr (Ptr)
import Foreign.Storable
import GHC.Generics
import Measure.Range
import Measure.Unit.SymbolCount
import Measure.Unit.SymbolDistance
import Measure.Unit.SymbolIndex


type DiscretizedResolution = Word32


{- |
General symbol distance matrix layout.
-}
data SymbolDistanceMatrix
    = SymbolDistanceMatrix
        {-# UNPACK #-} !SymbolCount
        -- ^ Matrix dimension
        {-# UNPACK #-} !(Vector DiscretizedResolution)
        -- ^ Contiguous array of matrix values
    deriving stock (Data, Eq, Generic)


instance NFData SymbolDistanceMatrix where
    rnf (SymbolDistanceMatrix x y) = rnf x `seq` rnf y


instance MeasurableRange SymbolDistanceMatrix SymbolIndex where
    measureRange = symbolBounds . symbolCount


{- |
A structure which can derive the number of alphabet symbols associated with it.
-}
instance HasSymbolCount SymbolDistanceMatrix where
    {-# INLINE symbolCount #-}
    symbolCount (SymbolDistanceMatrix n _) = n


{- |
/O(1)/

Indexing without bounds checking.
-}
{-# INLINE index #-}
{-# SPECIALIZE INLINE index ∷ SymbolDistanceMatrix → Int → SymbolDistance #-}
index ∷ (Integral c) ⇒ SymbolDistanceMatrix → Int → c
index (SymbolDistanceMatrix _ v) i =
    fromIntegral $ v `V.unsafeIndex` i


{- |
Deconstructs the 'Layout.Compact.Symbols.Internal.SymbolDistanceMatrix' to expose the underlying unboxed 'Vector'.
-}
{-# INLINE rowMajorVector #-}
rowMajorVector ∷ SymbolDistanceMatrix → Vector DiscretizedResolution
rowMajorVector (SymbolDistanceMatrix _ v) = v


{- |
Computes the number of bytes used to store the symbol distance matrix.
-}
{-# INLINE bytesSizeSymbolMatrix #-}
bytesSizeSymbolMatrix ∷ SymbolDistanceMatrix → Int
bytesSizeSymbolMatrix (SymbolDistanceMatrix _ vec) =
    let c = sizeOf (undefined ∷ SymbolCount)
        p = sizeOf (undefined ∷ Ptr Word)
        i = sizeOf (undefined ∷ Ptr Int)
        e = sizeOf (undefined ∷ Word16)
        n = V.length vec
    in  foldl'
            (+)
            0
            [ c
            , p + p
            , i
            , e * n
            ]
