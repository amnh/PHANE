{- |
Allocate matricies across the FFI.
-}

{-# Language FlexibleInstances #-}
{-# Language ForeignFunctionInterface #-}
{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language Strict #-}

#include "costMatrix.h"
#include "alignmentMatrices.h"

module Layout.Compact.States.Structure
  ( AlignmentStrategy(..)
  , StateTransitionsCompact(..)
  , DiscretizedResolutionIota
  , TCMρ
  , FFI2D()
  , FFI3D()
  -- * Constant
  , maximumDimension
  -- * Accessor
  , getAlignmentStrategy
  -- * Indexing
  -- ** Dispersion
  , both2D
  , both3D
  -- ** Distance
  , cost2D
  , cost3D
  , costSymbol
  -- ** Median
  , mean2D
  , mean3D
  -- * Indexing via 'Bits' interface
  -- (less preferable)
  -- ** Dispersion
  , both2D'
  , both3D'
  -- ** Distance
  , cost2D'
  , cost3D'
  -- ** Median
  , mean2D'
  , mean3D'
  -- * Byte measurements
  , bytesSizeFFI2D
  , bytesSizeFFI3D
  , bytesSizeOfCompact
  -- * Rendering
  , renderMatrix
  , renderSummary
  ) where

import Control.DeepSeq
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Foreign
import Foreign.C.Types
import GHC.Generics (Generic)
import Measure.Transition
import Measure.Unit.SymbolDistance
import Measure.Unit.SymbolIndex
import Prelude hiding (head, last)
import System.IO.Unsafe (unsafePerformIO)
import Data.Vector.Storable (Vector, (!))


{- |
The representation of values sent across the FFI.
-}
--type DiscretizedResolutionIota = CUInt
type DiscretizedResolutionIota = CULong


type ElementEncoding = CUInt


type SequencePointer = Ptr DiscretizedResolutionIota


-- |
-- Specify which alignment to perform
data  AlignmentStrategy = Linear | Affine
    deriving stock    (Eq, Generic, Show)
    deriving anyclass (NFData)


-- |
-- Holds a single "State Transition Matrix," which contains the distances and
-- medians between all possible /pairs/ of states. This representation has the
-- following properites:
--
--   * Contiguous, compact memory layout of a "square" transition matrix.
--   * Completely pre-computed values of transition measures.
--   * Limited matrix dimension of 'maximumDimension' due to size restrictions.
--   * No-cost interoperability with C FFI.
--
data  FFI2D
    = FFI2D
    { alphSize2D            :: CSize
    -- ^ alphabet size including gap, and including ambiguities if
    , costMatrixDimension2D :: CSize
    -- ^ ceiling of log_2 (alphSize)
    , gapChar2D             :: ElementEncoding
    -- ^ gap value (least significant bit set)
    , costModelType2D       :: CInt
    {- ^ The type of cost model to be used in the alignment, i.e. affine or not.
         Based on cost_matrix.ml, values are:
           • linear == 0
           • no_alignment == 2
           • affine == 3
         but I updated it. See costMatrix.h.
    -}
    , include_ambiguities2D :: CInt
    {- ^ This is a flag set to true if we are going to accept all possible
         combinations of the elements in the alphabet in the alignments. This is
         not true for protein characters for example, where the number of elements
         of the alphabet is already too big to build all the possible combinations.
    -}
    , gapOpenCost2D         :: DiscretizedResolutionIota
    {- ^ The cost of opening a gap. This is only useful in certain cost_model_types
         (type 3: affine, based on my reading of ML code).
    -}
    , isMetric2D            :: CInt
    -- ^ if tcm is metric
    , allElems2D            :: CSize
    -- ^ total number of elements
    , bestCost2D            :: SequencePointer
    {- ^ The transformation cost matrix, including ambiguities, storing the **best**
         cost for each ambiguity pair
    -}
    , medians2D             :: Ptr ElementEncoding
    {- ^ The matrix of possible medians between elements in the alphabet. The best
         possible medians according to the cost matrix.
    -}
    , worstCost2D           :: SequencePointer
    {- ^ The transformation cost matrix, including ambiguities, storing the **worst**
         cost for each ambiguity pair
    -}
    , prependCost2D         :: SequencePointer
    {- ^ The cost of going from gap -> each base. For ambiguities, use best cost.
         Set up as num_elements x num_elements matrix, but seemingly only first row is used.
    -}
    , tailCost2D            :: SequencePointer
    {- ^ As prepend_cost, but with reverse directionality, so base -> gap.
         As with prepend_cost, seems to be allocated as too large.
    -}
    }
    deriving stock    (Eq, Generic)
    deriving anyclass (NFData)


-- |
-- Holds a single "State Transition Cube," which contains the distances and
-- medians between all possible /triples/ of states. This representation has the
-- following properites:
--
--   * Contiguous, compact memory layout of a "cubed" transition "matrix."
--   * Completely pre-computed values of transition measures.
--   * Limited matrix dimension of 'maximumDimension' due to size restrictions.
--   * No-cost interoperability with C FFI.
--
data  FFI3D
    = FFI3D          -- See FFI2D datatype for field description
    { alphSize3D            :: CSize
    , costMatrixDimension3D :: CSize
    , gapChar3D             :: ElementEncoding
    , costModelType3D       :: CInt
    , include_ambiguities3D :: CInt
    , gapOpenCost3D         :: DiscretizedResolutionIota
    , allElems3D            :: CSize
    , bestCost3D            :: SequencePointer
    , medians3D             :: Ptr ElementEncoding
    }
    deriving stock    (Generic)
    deriving anyclass (NFData)


-- |
-- Exposed wrapper for C allocated cost matrix structures.
--
-- The 'matrix2D' and 'matrix3D' components are required for string alignment
-- utilizing the C FFI.
--
-- Supports the following measures:
--
--   * Dispersion
--     - 'both2D'     : state ⨉ state         → (ℕ , state)
--     - 'both3D'     : state ⨉ state ⨉ state → (ℕ , state)
--
--   * Distance
--     - 'costSymbol' : symbol ⨉ symbol         → ℕ
--     - 'cost2D'     : state  ⨉ state          → ℕ
--     - 'cost3D'     : state  ⨉ state  ⨉ state → ℕ
--
--   * Median
--     - 'mean2D'     : state ⨉ state         → state
--     - 'mean3D'     : state ⨉ state ⨉ state → state
--
data  StateTransitionsCompact
    = StateTransitionsCompact
    { matrix2D   :: {-# UNPACK #-} (Ptr FFI2D)
    , matrix3D   :: {-# UNPACK #-} (Ptr FFI3D)
    , matrixDist :: {-# UNPACK #-} Vector DiscretizedResolutionIota
    , matrixSize :: {-# UNPACK #-} Word
    , gapPenalty :: {-# UNPACK #-} Word
    , maxDelCost :: {-# UNPACK #-} Word
    , maxInsCost :: {-# UNPACK #-} Word
    , minDelCost :: {-# UNPACK #-} Word
    , minInsCost :: {-# UNPACK #-} Word
    }
    deriving stock    (Generic)
    deriving anyclass (NFData)


-- |
-- Synonym for 'Layout.Compact.States.Indexing.StateTransitionsCompact', provided for terseness.
type TCMρ = StateTransitionsCompact


instance Enum AlignmentStrategy where

    fromEnum = \case
        Linear -> 0
        Affine -> 3

    toEnum = \case
        3 -> Affine
        _ -> Linear


instance Eq StateTransitionsCompact where

    (==) lhs rhs =
        let n =  matrixSize lhs
        in  and
            [ n == matrixSize rhs
            , gapPenalty lhs == gapPenalty rhs
            , maxDelCost lhs == maxDelCost rhs
            , maxInsCost lhs == maxInsCost rhs
            , minDelCost lhs == minDelCost rhs
            , minInsCost lhs == minInsCost rhs
            ] &&
            let symbolRange = [ 0 .. n - 1 ]
                symbolλ i j = costSymbol lhs i j == costSymbol rhs i j
            in  and $ symbolλ <$> symbolRange <*> symbolRange


{- |
/O(1)/
-}
instance HasEditExtrema StateTransitionsCompact where

    {-# INLINE maxDeletion #-}
    maxDeletion  = SymbolDistance . maxDelCost

    {-# INLINE maxInsertion #-}
    maxInsertion = SymbolDistance . maxInsCost

    {-# INLINE minDeletion #-}
    minDeletion  = SymbolDistance . minDelCost

    {-# INLINE minInsertion #-}
    minInsertion = SymbolDistance . minInsCost


{- |
/O(1)/

Generally less desireable instance.
-}
instance Bits b => HasStateTransitions StateTransitionsCompact b where

    stateTransitionPairwiseDispersion = both2D'

    stateTransitionPairwiseDistance   = cost2D'

    stateTransitionPairwiseMedian   = mean2D'

    stateTransitionThreewayDispersion = both3D'

    stateTransitionThreewayDistance   = cost3D'

    stateTransitionThreewayMedian   = mean3D'


{- |
/O(1)/
-}
instance {-# OVERLAPPING #-} HasStateTransitions StateTransitionsCompact DiscretizedResolutionIota where

    {-# SPECIALISE INLINE stateTransitionPairwiseDispersion :: StateTransitionsCompact -> StateTransitionPairwiseDispersionλ DiscretizedResolutionIota #-}
    stateTransitionPairwiseDispersion = both2D

    {-# SPECIALISE INLINE stateTransitionPairwiseDistance   :: StateTransitionsCompact -> StateTransitionPairwiseDistanceλ   DiscretizedResolutionIota #-}
    stateTransitionPairwiseDistance   = cost2D

    {-# SPECIALISE INLINE stateTransitionPairwiseMedian   :: StateTransitionsCompact -> StateTransitionPairwiseMedianλ   DiscretizedResolutionIota #-}
    stateTransitionPairwiseMedian   = mean2D

    {-# SPECIALISE INLINE stateTransitionThreewayDispersion :: StateTransitionsCompact -> StateTransitionThreewayDispersionλ DiscretizedResolutionIota #-}
    stateTransitionThreewayDispersion = both3D

    {-# SPECIALISE INLINE stateTransitionThreewayDistance   :: StateTransitionsCompact -> StateTransitionThreewayDistanceλ   DiscretizedResolutionIota #-}
    stateTransitionThreewayDistance   = cost3D

    {-# SPECIALISE INLINE stateTransitionThreewayMedian   :: StateTransitionsCompact -> StateTransitionThreewayMedianλ   DiscretizedResolutionIota #-}
    stateTransitionThreewayMedian   = mean3D


{- |
/O(1)/
-}
instance {-# OVERLAPPING #-} HasStateTransitions StateTransitionsCompact Word64 where

    {-# SPECIALISE INLINE stateTransitionPairwiseDispersion :: StateTransitionsCompact -> StateTransitionPairwiseDispersionλ Word64 #-}
    stateTransitionPairwiseDispersion = both2D

    {-# SPECIALISE INLINE stateTransitionPairwiseDistance   :: StateTransitionsCompact -> StateTransitionPairwiseDistanceλ   Word64 #-}
    stateTransitionPairwiseDistance   = cost2D

    {-# SPECIALISE INLINE stateTransitionPairwiseMedian   :: StateTransitionsCompact -> StateTransitionPairwiseMedianλ   Word64 #-}
    stateTransitionPairwiseMedian   = mean2D

    {-# SPECIALISE INLINE stateTransitionThreewayDispersion :: StateTransitionsCompact -> StateTransitionThreewayDispersionλ Word64 #-}
    stateTransitionThreewayDispersion = both3D

    {-# SPECIALISE INLINE stateTransitionThreewayDistance   :: StateTransitionsCompact -> StateTransitionThreewayDistanceλ   Word64 #-}
    stateTransitionThreewayDistance   = cost3D

    {-# SPECIALISE INLINE stateTransitionThreewayMedian   :: StateTransitionsCompact -> StateTransitionThreewayMedianλ   Word64 #-}
    stateTransitionThreewayMedian   = mean3D


instance HasSymbolDistances StateTransitionsCompact where

    symbolDistances tcm i =
        let f (SymbolIndex w) = w
        in  SymbolDistance . costSymbol tcm (f i) . f


instance Show StateTransitionsCompact where

    show = (<>) <$> renderSummary <*> renderMatrix


{-
instance MeasurableEditExtrema StateTransitionsCompact Word where

    maxDeletion  = maxDelCost

    maxInsertion = maxInsCost

    minDeletion  = minDelCost

    minInsertion = minInsCost


instance MeasurableRange StateTransitionsCompact SymbolIndex where

    measureRange = symbolBounds . symbolCount


instance HasSymbolCount StateTransitionsCompact where

    symbolCount = matrixSize
-}


instance Show FFI2D where


    show ffi2D =
        let n     = fromEnum $ alphSize2D    ffi2D
            cost  = fromEnum $ gapOpenCost2D ffi2D
            bytes = bytesSizeFFI2D n
        in  unwords
            [ "FFI 3D", "(", show n, "⨉", show n, "⨉", show n, ")", "state transitions and"
            , "(", show cost, ")", "gap sequence penalty totaling", show bytes, "bytes"
            ]
{-
    show = unlines . (fieldRendering <*>)  . pure
      where
        fieldRendering =
            [ show .            alphSize2D
            , show . costMatrixDimension2D
            , show .             gapChar2D
            , show .       costModelType2D
            , show . include_ambiguities2D
            , show .         gapOpenCost2D
            , show .            isMetric2D
            , show .            allElems2D
            , show .            bestCost2D
            , show .             medians2D
            , show .           worstCost2D
            , show .         prependCost2D
            , show .            tailCost2D
            ]
-}


instance Show FFI3D where

    show ffi3D =
        let n     = fromEnum $ alphSize3D    ffi3D
            cost  = fromEnum $ gapOpenCost3D ffi3D
            bytes = bytesSizeFFI3D n
        in  unwords
            [ "FFI 3D", "(", show n, "⨉", show n, "⨉", show n, ")", "state transitions and"
            , "(", show cost, ")", "gap sequence penalty totaling", show bytes, "bytes"
            ]
{-
    show = unlines . (fieldRendering <*>)  . pure
      where
        fieldRendering =
            [ show .            alphSize3D
            , show . costMatrixDimension3D
            , show .             gapChar3D
            , show .       costModelType3D
            , show . include_ambiguities3D
            , show .         gapOpenCost3D
            , show .            allElems3D
            , show .            bestCost3D
            , show .             medians3D
            ]
-}


instance Storable FFI2D where

    sizeOf = const $ #size struct cost_matrices_2d_t

    alignment = sizeOf -- alignment (undefined :: StablePtr FFI2D)

    peek ptr = FFI2D
        <$> (#peek struct cost_matrices_2d_t, alphSize           ) ptr
        <*> (#peek struct cost_matrices_2d_t, costMatrixDimension) ptr
        <*> (#peek struct cost_matrices_2d_t, gap_char           ) ptr
        <*> (#peek struct cost_matrices_2d_t, cost_model_type    ) ptr
        <*> (#peek struct cost_matrices_2d_t, include_ambiguities) ptr
        <*> (#peek struct cost_matrices_2d_t, gap_open_cost      ) ptr
        <*> (#peek struct cost_matrices_2d_t, is_metric          ) ptr
        <*> (#peek struct cost_matrices_2d_t, num_elements       ) ptr
        <*> (#peek struct cost_matrices_2d_t, cost               ) ptr
        <*> (#peek struct cost_matrices_2d_t, median             ) ptr
        <*> (#peek struct cost_matrices_2d_t, worst              ) ptr
        <*> (#peek struct cost_matrices_2d_t, prepend_cost       ) ptr
        <*> (#peek struct cost_matrices_2d_t, tail_cost          ) ptr

    poke ptr cm2D = do -- to modify values in the C app
        (#poke struct cost_matrices_2d_t, alphSize           ) ptr $            alphSize2D cm2D
        (#poke struct cost_matrices_2d_t, costMatrixDimension) ptr $ costMatrixDimension2D cm2D
        (#poke struct cost_matrices_2d_t, gap_char           ) ptr $             gapChar2D cm2D
        (#poke struct cost_matrices_2d_t, cost_model_type    ) ptr $       costModelType2D cm2D
        (#poke struct cost_matrices_2d_t, include_ambiguities) ptr $ include_ambiguities2D cm2D
        (#poke struct cost_matrices_2d_t, gap_open_cost      ) ptr $         gapOpenCost2D cm2D
        (#poke struct cost_matrices_2d_t, is_metric          ) ptr $            isMetric2D cm2D
        (#poke struct cost_matrices_2d_t, num_elements       ) ptr $             medians2D cm2D
        (#poke struct cost_matrices_2d_t, cost               ) ptr $            bestCost2D cm2D
        (#poke struct cost_matrices_2d_t, median             ) ptr $             medians2D cm2D
        (#poke struct cost_matrices_2d_t, worst              ) ptr $           worstCost2D cm2D
        (#poke struct cost_matrices_2d_t, prepend_cost       ) ptr $         prependCost2D cm2D
        (#poke struct cost_matrices_2d_t, tail_cost          ) ptr $            tailCost2D cm2D


instance Storable FFI3D where

    sizeOf = const $ #size struct cost_matrices_2d_t

    alignment = sizeOf -- alignment (undefined :: StablePtr FFI2D)

    peek ptr = FFI3D
        <$> (#peek struct cost_matrices_3d_t, alphSize           ) ptr
        <*> (#peek struct cost_matrices_3d_t, costMatrixDimension) ptr
        <*> (#peek struct cost_matrices_3d_t, gap_char           ) ptr
        <*> (#peek struct cost_matrices_3d_t, cost_model_type    ) ptr
        <*> (#peek struct cost_matrices_3d_t, include_ambiguities) ptr
        <*> (#peek struct cost_matrices_3d_t, gap_open_cost      ) ptr
        <*> (#peek struct cost_matrices_3d_t, num_elements       ) ptr
        <*> (#peek struct cost_matrices_3d_t, cost               ) ptr
        <*> (#peek struct cost_matrices_3d_t, median             ) ptr

    poke ptr cm3D = do -- to modify values in the C app
        (#poke struct cost_matrices_3d_t, alphSize           ) ptr $            alphSize3D cm3D
        (#poke struct cost_matrices_3d_t, costMatrixDimension) ptr $ costMatrixDimension3D cm3D
        (#poke struct cost_matrices_3d_t, gap_char           ) ptr $             gapChar3D cm3D
        (#poke struct cost_matrices_3d_t, cost_model_type    ) ptr $       costModelType3D cm3D
        (#poke struct cost_matrices_3d_t, include_ambiguities) ptr $ include_ambiguities3D cm3D
        (#poke struct cost_matrices_3d_t, gap_open_cost      ) ptr $         gapOpenCost3D cm3D
        (#poke struct cost_matrices_3d_t, num_elements       ) ptr $            allElems3D cm3D
        (#poke struct cost_matrices_3d_t, cost               ) ptr $            bestCost3D cm3D
        (#poke struct cost_matrices_3d_t, median             ) ptr $             medians3D cm3D


-- |
-- The maximum dimension for which a compact state transition matrix can be built.
--
-- The value is defined as @8@; the bit-width of a byte 'Word8'.
maximumDimension :: Word
maximumDimension = toEnum $ finiteBitSize (0 :: Word8)


-- |
-- /ϴ(1)/
--
-- Determine the alignment strategy encoded for the matrix.
getAlignmentStrategy :: FFI2D -> AlignmentStrategy
getAlignmentStrategy = toEnum . fromEnum . costModelType2D


-- |
-- /O(1)/
--
-- Get the transition cost between two symbols by thier indices.
costSymbol :: TCMρ -> Word -> Word -> Word
costSymbol tcm i j =
    let dim = matrixSize tcm
        idx = fromEnum $ (dim * i) + j
        sdm = matrixDist tcm
    in  fromIntegral $ sdm ! idx


-- |
-- /O(1)/
{-# INLINEABLE both2D #-}
{-# SPECIALISE INLINE both2D :: TCMρ -> DiscretizedResolutionIota -> DiscretizedResolutionIota -> (Word, DiscretizedResolutionIota) #-}
both2D :: (Enum b, Num c) => TCMρ -> b -> b -> (c, b)
both2D tcmρ e1 e2 = unsafePerformIO $ do
    cm2D <- peek $ matrix2D tcmρ
    let dim = fromEnum $ alphSize2D cm2D
    
        getC :: Ptr DiscretizedResolutionIota -> IO DiscretizedResolutionIota
        getC = peek . getOffsetPtr2D dim fromEnum e1 e2
        
        getM :: Ptr ElementEncoding -> IO ElementEncoding
        getM = peek . getOffsetPtr2D dim fromEnum e1 e2

    cost <- getC $ bestCost2D cm2D
    med  <- getM $  medians2D cm2D
    pure (fromIntegral cost, cuint2Enum med)


-- |
-- /O(1)/
{-# INLINEABLE cost2D #-}
{-# SPECIALISE INLINE cost2D :: TCMρ -> DiscretizedResolutionIota -> DiscretizedResolutionIota -> Word #-}
cost2D :: forall b c. (Enum b, Num c) => TCMρ -> b -> b -> c
cost2D tcmρ e1 e2 = unsafePerformIO $ do
    cm2D <- peek $ matrix2D tcmρ
    let dim = fromEnum $ alphSize2D cm2D
    let ptr = getOffsetPtr2D dim fromEnum e1 e2 $ bestCost2D cm2D
    fromIntegral <$> (peek ptr :: IO DiscretizedResolutionIota)


-- |
-- /O(1)/
{-# INLINE mean2D #-}
{-# SPECIALISE INLINE mean2D :: TCMρ -> DiscretizedResolutionIota -> DiscretizedResolutionIota -> DiscretizedResolutionIota #-}
mean2D :: Enum b => TCMρ -> b -> b -> b
mean2D tcmρ e1 e2 = unsafePerformIO $ do
    cm2D <- peek $ matrix2D tcmρ
    let dim = fromEnum $ alphSize2D cm2D
    let ptr = getOffsetPtr2D dim fromEnum e1 e2 $ medians2D cm2D
    cuint2Enum <$> peek ptr


-- |
-- /O(1)/
{-# INLINEABLE both3D #-}
{-# SPECIALISE INLINE both3D :: TCMρ -> DiscretizedResolutionIota -> DiscretizedResolutionIota -> DiscretizedResolutionIota -> (Word, DiscretizedResolutionIota) #-}
both3D :: (Enum b, Num c) => TCMρ -> b -> b -> b -> (c, b)
both3D tcmρ e1 e2 e3 = unsafePerformIO $ do
    cm3D <- peek $ matrix3D tcmρ
    let dim = fromEnum $ alphSize3D cm3D
   
        getC :: Ptr DiscretizedResolutionIota -> IO DiscretizedResolutionIota
        getC = peek . getOffsetPtr3D dim fromEnum e1 e2 e3
        
        getM :: Ptr ElementEncoding -> IO ElementEncoding
        getM = peek . getOffsetPtr3D dim fromEnum e1 e2 e3

    cost <- getC $ bestCost3D cm3D
    med  <- getM $  medians3D cm3D
    pure (fromIntegral cost, cuint2Enum med)


-- |
-- /O(1)/
{-# INLINEABLE cost3D #-}
{-# SPECIALISE INLINE cost3D :: TCMρ -> DiscretizedResolutionIota -> DiscretizedResolutionIota -> DiscretizedResolutionIota -> Word #-}
cost3D :: (Enum b, Num c) => TCMρ -> b -> b -> b -> c
cost3D tcmρ e1 e2 e3 = unsafePerformIO $ do
    cm3D <- peek $ matrix3D tcmρ
    let dim = fromEnum $ alphSize3D cm3D
    let ptr = getOffsetPtr3D dim fromEnum e1 e2 e3 $ bestCost3D cm3D
    fromIntegral <$> peek ptr


-- |
-- /O(1)/
{-# INLINE mean3D #-}
{-# SPECIALISE INLINE mean3D :: TCMρ -> DiscretizedResolutionIota -> DiscretizedResolutionIota -> DiscretizedResolutionIota -> DiscretizedResolutionIota #-}
mean3D :: Enum b => TCMρ -> b -> b -> b -> b
mean3D tcmρ e1 e2 e3 = unsafePerformIO $ do
    cm3D <- peek $ matrix3D tcmρ
    let dim = fromEnum $ alphSize3D cm3D
    let ptr = getOffsetPtr3D dim fromEnum e1 e2 e3 $ medians3D cm3D
    cuint2Enum <$> peek ptr


-- |
-- /O(1)/
{-# INLINEABLE both2D' #-}
{-# SPECIALISE INLINE both2D' :: TCMρ -> DiscretizedResolutionIota -> DiscretizedResolutionIota -> (Word, DiscretizedResolutionIota) #-}
both2D' :: (Bits b, Num c) => TCMρ -> b -> b -> (c, b)
both2D' tcmρ e1 e2 = unsafePerformIO $ do
    cm2D <- peek $ matrix2D tcmρ
    let dim = fromEnum $ alphSize2D cm2D
        idx = indexFromBits dim
    
        getC :: Ptr DiscretizedResolutionIota -> IO DiscretizedResolutionIota
        getC = peek . getOffsetPtr2D dim idx e1 e2
        
        getM :: Ptr ElementEncoding -> IO ElementEncoding
        getM = peek . getOffsetPtr2D dim idx e1 e2

    cost <- getC $ bestCost2D cm2D
    med  <- getM $  medians2D cm2D
    pure (fromIntegral cost, valueToBits e1 med)


-- |
-- /O(1)/
{-# INLINEABLE cost2D' #-}
{-# SPECIALISE INLINE cost2D' :: TCMρ -> DiscretizedResolutionIota -> DiscretizedResolutionIota -> Word #-}
cost2D' :: (Bits b, Num c) => TCMρ  -> b -> b -> c
cost2D' tcmρ e1 e2 = unsafePerformIO $ do
    cm2D <- peek $ matrix2D tcmρ
    let dim = fromEnum $ alphSize2D cm2D
    let idx = indexFromBits dim
    let ptr = getOffsetPtr2D dim idx e1 e2 $ bestCost2D cm2D
    fromIntegral <$> peek ptr


-- |
-- /O(1)/
{-# INLINE mean2D' #-}
{-# SPECIALISE INLINE mean2D' :: TCMρ -> DiscretizedResolutionIota -> DiscretizedResolutionIota -> DiscretizedResolutionIota #-}
mean2D' :: Bits b => TCMρ -> b -> b -> b
mean2D' tcmρ e1 e2 = unsafePerformIO $ do
    cm2D <- peek $ matrix2D tcmρ
    let dim = fromEnum $ alphSize2D cm2D
    let idx = indexFromBits dim
    let ptr = getOffsetPtr2D dim idx e1 e2 $ medians2D cm2D
    valueToBits e1 <$> peek ptr


-- |
-- /O(1)/
{-# INLINEABLE both3D' #-}
{-# SPECIALISE INLINE both3D' :: TCMρ -> DiscretizedResolutionIota -> DiscretizedResolutionIota -> DiscretizedResolutionIota -> (Word, DiscretizedResolutionIota) #-}
both3D' :: (Bits b, Num c) => TCMρ -> b -> b -> b -> (c, b)
both3D' tcmρ e1 e2 e3 = unsafePerformIO $ do
    cm3D <- peek $ matrix3D tcmρ
    let dim = fromEnum $ alphSize3D cm3D
        idx = indexFromBits dim
   
        getC :: Ptr DiscretizedResolutionIota -> IO DiscretizedResolutionIota
        getC = peek . getOffsetPtr3D dim idx e1 e2 e3
        
        getM :: Ptr ElementEncoding -> IO ElementEncoding
        getM = peek . getOffsetPtr3D dim idx e1 e2 e3

    cost <- getC $ bestCost3D cm3D
    med  <- getM $  medians3D cm3D
    pure (fromIntegral cost, valueToBits e1 med)


-- |
-- /O(1)/
{-# INLINEABLE cost3D' #-}
{-# SPECIALISE INLINE cost3D' :: TCMρ -> DiscretizedResolutionIota -> DiscretizedResolutionIota -> DiscretizedResolutionIota -> Word #-}
cost3D' :: (Bits b, Num c) => TCMρ -> b -> b -> b -> c
cost3D' tcmρ e1 e2 e3 = unsafePerformIO $ do
    cm3D <- peek $ matrix3D tcmρ
    let dim = fromEnum $ alphSize3D cm3D
    let idx = indexFromBits dim
    let ptr = getOffsetPtr3D dim idx e1 e2 e3 $ bestCost3D cm3D
    fromIntegral <$> peek ptr


-- |
-- /O(1)/
{-# INLINE mean3D' #-}
{-# SPECIALISE INLINE mean3D' :: TCMρ -> DiscretizedResolutionIota -> DiscretizedResolutionIota -> DiscretizedResolutionIota -> DiscretizedResolutionIota #-}
mean3D' :: Bits b => TCMρ -> b -> b -> b -> b
mean3D' tcmρ e1 e2 e3 = unsafePerformIO $ do
    cm3D <- peek $ matrix3D tcmρ
    let dim = fromEnum $ alphSize3D cm3D
    let idx = indexFromBits dim
    let ptr = getOffsetPtr3D dim idx e1 e2 e3 $ medians3D cm3D
    valueToBits e1 <$> peek ptr


getOffsetPtr2D :: Storable b => Int -> (a -> Int) -> a -> a -> Ptr b -> Ptr b
getOffsetPtr2D dim f e1 e2 =
    let off = (f e1 `shiftL` dim) + f e2
    in  flip advancePtr off


getOffsetPtr3D :: Storable b => Int -> (a -> Int) -> a -> a -> a -> Ptr b -> Ptr b
getOffsetPtr3D dim f e1 e2 e3 =
    let off = (((f e1 `shiftL` dim) + f e2) `shiftL` dim) + f e3
    in  flip advancePtr off


cuint2Enum :: Enum b => ElementEncoding -> b
cuint2Enum = toEnum . fromEnum


valueToBits :: Bits b => b -> ElementEncoding -> b
valueToBits b v =
    let z = b `xor` b
        f :: Bits a => a -> Int -> a
        f a i
          | v `testBit` i = a `setBit` i
          | otherwise     = a
    in foldl' f z bitsBuildRange


indexFromBits :: Bits b => Int ->  b -> Int
indexFromBits n b =
    let f :: Int -> (Int, Int) -> Int
        f a (i,v)
          | b `testBit` i = a + v
          | otherwise     = a
    in foldl' f 0 $ take n bitsIndexRange


bitsBuildRange :: [Int]
bitsBuildRange =
    [ 0 .. fromEnum maximumDimension - 1 ]


bitsIndexRange :: [(Int, Int)]
bitsIndexRange = (\x -> (x, 2 ^ x)) <$> bitsBuildRange


{- |
Compute the number of bytes within the 2D lookup table portion of a compact FFI cost structure.
-}
bytesSizeFFI2D :: Int -> Int
bytesSizeFFI2D n =
    let structConst    = fromEnum $ sizeOf (undefined :: FFI2D)
        bytesBestCost  = n * n *    sizeOf (undefined :: DiscretizedResolutionIota)
        bytesMedians   = n * n *    sizeOf (undefined :: DiscretizedResolutionIota)
        bytesWorstCost = n * n *    sizeOf (undefined ::  CInt)
        bytesPrepend   = n *        sizeOf (undefined ::  CInt)
        bytesTail      = n *        sizeOf (undefined ::  CInt)
    in  sum
        [ structConst
        , bytesBestCost
        , bytesMedians
        , bytesWorstCost
        , bytesPrepend
        , bytesTail
        ]


{- |
Compute the number of bytes within the 3D lookup table portion of a compact FFI cost structure.
-}
bytesSizeFFI3D :: Int -> Int
bytesSizeFFI3D n =
    let structConst   = fromEnum  $ sizeOf (undefined :: FFI3D)
        bytesBestCost = n * n * n * sizeOf (undefined :: DiscretizedResolutionIota)
        bytesMedians  = n * n * n * sizeOf (undefined :: DiscretizedResolutionIota)
    in  sum
        [ structConst
        , bytesBestCost
        , bytesMedians
        ]


{- |
Compute the /total/ number of bytes allocated for the /entire/ FFI cost structure.
-}
bytesSizeOfCompact :: Integral i => i -> Int
bytesSizeOfCompact i =
    let dimension = fromIntegral i
        bytesWord = sizeOf (undefined :: Word)
    in  sum
        [ 6 * bytesWord
        , sizeOf (undefined :: Ptr FFI2D)
        , sizeOf (undefined :: Ptr FFI3D)
        , bytesSizeFFI3D dimension
        , bytesSizeFFI3D dimension
        ]


{- |
Pretty prints a consise, human-readable summary of the 'Layout.Compact.States.Indexing.StateTransitionsCompact'..
-}
renderSummary :: StateTransitionsCompact -> String
renderSummary tcm =
    let b = bytesSizeOfCompact n
        g = gapPenalty tcm
        n = matrixSize tcm
    in  unlines $ fold
            [      [ "General Metric (Compacted) with"]
            ,      [ fold [ "  allocated "     , show b, " bytes" ] ]
            ,      [ fold [ "  dimension "     , show n          ] ]
            , if g > 0
              then [ fold [ "  affine penalty ", show g, "Δ" ] ]
              else []
            ]


{- |
Pretty prints the full 'Layout.Compact.States.Indexing.StateTransitionsCompact' matrix in a human-readable format.
-}
renderMatrix :: StateTransitionsCompact -> String
renderMatrix tcm =
    let n =  matrixSize tcm

        pad :: Show p => p -> String
        pad v  =
            let shown = show v
            in replicate (maxVal - length shown) ' ' <> shown

        range :: NonEmpty Word
        range = 0 :| [ 1 .. n - 1 ]

        values :: NonEmpty (NonEmpty Word)
        values = do
            i <- range
            pure $ do
                j <- range
                pure $ costSymbol tcm i j

        uncons :: NonEmpty a -> ([a], a)
        uncons = (,) <$> NE.init <*> NE.last

        rowHead :| rowTail = values
        (rowBody, rowLast) = case rowTail of
            [] -> ([], rowHead)
            x:xs -> uncons $ x :| xs

        maxVal = length . show . maximum $ maximum <$> values
        endcap = \b e str -> indent $ fold [ b, " ", fold . NE.intersperse " " $ pad <$> str, " ", e ]
        indent = ("  " <>)
        fstStr = endcap "┌" "┐"
        rowStr = endcap "│" "│"
        endStr = endcap "└" "┘"
        mtxStr = fold
            [ fstStr <$> [ rowHead ]
            , rowStr <$> rowBody
            , endStr <$> [ rowLast ]
            ]
    in  unlines mtxStr
