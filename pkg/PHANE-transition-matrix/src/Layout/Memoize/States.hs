{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}

module Layout.Memoize.States (
    Sparse (..),
    initialize,
) where

import Control.DeepSeq
import Data.Bits


#if MIN_VERSION_base(4,20,0)
import Data.Foldable (fold)
#else
import Data.Foldable (fold, foldl')
#endif
import Data.Hashable
import Data.Word (Word64)
import Foreign.C.Types (CUInt)
import Foreign.Ptr (Ptr)
import Foreign.Storable (sizeOf)
import GHC.Generics
import Layout.Compact.Class
import Layout.Compact.Symbols.Square (SymbolDistanceMatrixSquare, bytesSizeMatrixSquare)
import Layout.Compact.Symbols.Triangular (SymbolDistanceMatrixTriangular, bytesSizeMatrixTriangular)
import Layout.Memoize.Dispersion
import Layout.Memoize.Hashable
import Measure.Range
import Measure.Transition
import Measure.Unit.SymbolCount
import Measure.Unit.SymbolDistance
import Measure.Unit.SymbolIndex


data Sparse a = Sparse
    { maxDelCost ∷ {-# UNPACK #-} !SymbolDistance
    , maxInsCost ∷ {-# UNPACK #-} !SymbolDistance
    , minDelCost ∷ {-# UNPACK #-} !SymbolDistance
    , minInsCost ∷ {-# UNPACK #-} !SymbolDistance
    , matrixForm ∷ {-# UNPACK #-} !(Either SymbolDistanceMatrixTriangular SymbolDistanceMatrixSquare)
    , memoized2Dλ ∷ !(TCM2Dλ a)
    , memoized3Dλ ∷ !(TCM3Dλ a)
    }
    deriving stock (Generic)
    deriving anyclass (NFData)


type role Sparse representational


instance Eq (Sparse a) where
    (==) lhs rhs =
        and
            [ maxDelCost lhs == maxDelCost rhs
            , maxInsCost lhs == maxInsCost rhs
            , minDelCost lhs == minDelCost rhs
            , minInsCost lhs == minInsCost rhs
            , matrixForm lhs == matrixForm rhs
            ]


instance HasEditExtrema (Sparse a) where
    maxDeletion = maxDelCost


    maxInsertion = maxInsCost


    minDeletion = minDelCost


    minInsertion = minInsCost


instance HasSymbolCount (Sparse a) where
    symbolCount = either symbolCount symbolCount . matrixForm


instance HasSymbolDistances (Sparse a) where
    symbolDistances = either symbolDistances symbolDistances . matrixForm


instance HasStateTransitions (Sparse a) a where
    {-# INLINE stateTransitionPairwiseDispersion #-}
    {-# SPECIALIZE INLINE stateTransitionPairwiseDispersion ∷ Sparse CUInt → TCM2Dλ CUInt #-}
    {-# SPECIALIZE INLINE stateTransitionPairwiseDispersion ∷ Sparse Word64 → TCM2Dλ Word64 #-}
    stateTransitionPairwiseDispersion = memoized2Dλ


    {-# INLINE stateTransitionThreewayDispersion #-}
    {-# SPECIALIZE INLINE stateTransitionThreewayDispersion ∷ Sparse CUInt → TCM3Dλ CUInt #-}
    {-# SPECIALIZE INLINE stateTransitionThreewayDispersion ∷ Sparse Word64 → TCM3Dλ Word64 #-}
    stateTransitionThreewayDispersion = memoized3Dλ


instance HasStateTransitionsCompact (Sparse a) where
    {-# INLINE stateTransitionCompact #-}
    stateTransitionCompact = const Nothing


instance MeasurableRange (Sparse a) SymbolIndex where
    {-# INLINE measureRange #-}
    measureRange = either measureRange measureRange . matrixForm


instance Show (Sparse a) where
    show = renderSummary


{-# INLINEABLE initialize #-}
{-# SPECIALIZE INLINE initialize ∷ Either SymbolDistanceMatrixTriangular SymbolDistanceMatrixSquare → Sparse Word64 #-}
initialize
    ∷ ( FiniteBits b
      , Hashable b
      , NFData b
      )
    ⇒ Either SymbolDistanceMatrixTriangular SymbolDistanceMatrixSquare
    → Sparse b
initialize eMatrix =
    let range = either measureRange measureRange eMatrix
        sdmλ = either symbolDistances symbolDistances eMatrix
        tcm2D = memoize2 $ bitDispersionPairwise range sdmλ
        tcm3D = memoize3 $ bitDispersionThreeway range sdmλ
    in  Sparse
            { maxDelCost = either maxDeletion maxDeletion eMatrix
            , maxInsCost = either maxInsertion maxInsertion eMatrix
            , minDelCost = either minDeletion minDeletion eMatrix
            , minInsCost = either minInsertion minInsertion eMatrix
            , matrixForm = eMatrix
            , memoized2Dλ = tcm2D
            , memoized3Dλ = tcm3D
            }


renderSummary ∷ Sparse a → String
renderSummary tcm =
    let b = bytesSizeOfSparse tcm
        n = symbolCount tcm
    in  unlines
            [ "General Metric (Memoized) with"
            , fold ["  allocated ", show b, " bytes + memoized value space"]
            , fold ["  dimension ", show n]
            ]


bytesSizeOfSparse ∷ Sparse a → Int
bytesSizeOfSparse tms =
    let bytesPtr = sizeOf (undefined ∷ Ptr Word)
        bytesWord = sizeOf (undefined ∷ Word)
        bytesMatrix = either bytesSizeMatrixTriangular bytesSizeMatrixSquare $ matrixForm tms
    in  foldl'
            (+)
            0
            [ 4 * bytesWord -- 4 unpacked fields
            , bytesPtr -- Point to Either
            , bytesPtr + bytesPtr -- Either points to Left / Right
            , bytesMatrix -- Space behind Either's pointer
            , bytesPtr -- Point to memoized 2D
            , bytesPtr -- Point to memoized 3D
            ]
