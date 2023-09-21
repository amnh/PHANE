-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedSums #-}

{- |
Module      :  Layout.Memoize
Copyright   :  (c) 2015-2021 Ward Wheeler
License     :  BSD-style

Maintainer  :  wheeler@amnh.org
Stability   :  provisional
Portability :  portable
-}
module Layout.Memoize (
    -- * Measures
    SDMλ,
    TCM2Dλ,
    TCM3Dλ,

    -- * Measure Components

    -- * Smart Constructors
    TransitionMatrix (),
    discreteCrossGap,
    discreteMetric,
    linearNorm,
    metricRepresentation,

    -- * Accessor Type-classes
    HasEditExtrema (..),
    MeasurableRange (..),
    HasStateTransitions (..),
    HasStateTransitionsCompact (..),
    HasSymbolCount (..),
    HasSymbolDistances (..),
) where

import Control.DeepSeq
import Data.Bits
import Data.Either (either)
import Data.Hashable
import Data.Hashable.Memoize
import Data.Word (Word64)
import Foreign.C.Types (CUInt)
import GHC.Generics
import Layout.Compact.Symbols.Square (SymbolDistanceMatrixSquare)
import Layout.Compact.Symbols.Triangular (SymbolDistanceMatrixTriangular)
import Measure.Range
import Measure.Transition
import Measure.Transition.Edits
import Measure.Unit.SymbolChangeCost
import Measure.Unit.SymbolCount
import Measure.Unit.SymbolIndex


data Sparse a = Sparse
    { maxDelCost :: {-# UNPACK #-} SymbolChangeCost
    , maxInsCost :: {-# UNPACK #-} SymbolChangeCost
    , minDelCost :: {-# UNPACK #-} SymbolChangeCost
    , minInsCost :: {-# UNPACK #-} SymbolChangeCost
    , matrixForm :: {-# UNPACK #-} (Either SymbolDistanceMatrixSquare SymbolDistanceMatrixTriangular)
    , memoized2Dλ :: TCM2Dλ a
    , memoized3Dλ :: TCM3Dλ a
    }
    deriving stock (Generic)
    deriving anyclass (NFData)


instance Eq (Sparse a) where
    (==) lhs rhs =
        and
            [ gapPenalty lhs == gapPenalty rhs
            , maxDelCost lhs == maxDelCost rhs
            , maxInsCost lhs == maxInsCost rhs
            , minDelCost lhs == minDelCost rhs
            , minInsCost lhs == minInsCost rhs
            , matrixForm lhs == matrixForm rhs
            ]


instance HasSymbolCount (Sparse a) where
    {-# INLINE symbolCount #-}
    symbolCount = either symbolCount symbolCount . matrixForm


instance HasSymbolDistances (Sparse a) where
    {-# INLINE symbolDistances #-}
    symbolDistances = either symbolDistances symbolDistances . matrixForm


instance HasEditExtrema (Sparse a) SymbolChangeCost where
    maxDeletion = maxDelCost


    maxInsertion = maxInsCost


    minDeletion = minDelCost


    minInsertion = minInsCost


instance MeasurableRange (Sparse a) SymbolIndex where
    {-# INLINE measureRange #-}
    measureRange = symbolBounds . symbolCount


instance Eq (TransitionMatrix3 a) where
    (==) (IotaMatrix x y) (IotaMatrix x' y') = x == x' && y == y'
    (==) (VastMatrix x) (VastMatrix x') = x == x'
    (==) (VastSpecialized x) (VastSpecialized x') = x == x'


instance HasStateTransitionsCompact (TransitionMatrix3 a) where
    getCompactPairwise (IotaMatrix _ tcm) = Just tcm
    getCompactPairwise _ = Nothing


instance HasEditExtrema (TransitionMatrix3 a) SymbolChangeCost where
    {-# INLINEABLE maxEdit #-}
    maxEdit (IotaMatrix _ tcm) = maxEdit tcm
    maxEdit (VastMatrix sdm) = maxEdit sdm
    maxEdit (VastSpecialized m) = maxEdit m


    maxDeletion (IotaMatrix _ tcm) = maxDeletion tcm
    maxDeletion (VastMatrix sdm) = maxDeletion sdm
    maxDeletion (VastSpecialized m) = maxDeletion m


    maxInsertion (IotaMatrix _ tcm) = maxInsertion tcm
    maxInsertion (VastMatrix sdm) = maxInsertion sdm
    maxInsertion (VastSpecialized m) = maxInsertion m


    {-# INLINEABLE minEdit #-}
    minEdit (IotaMatrix _ tcm) = minEdit tcm
    minEdit (VastMatrix sdm) = minEdit sdm
    minEdit (VastSpecialized m) = minEdit m


    minDeletion (IotaMatrix _ tcm) = minDeletion tcm
    minDeletion (VastMatrix sdm) = minDeletion sdm
    minDeletion (VastSpecialized m) = minDeletion m


    minInsertion (IotaMatrix _ tcm) = minInsertion tcm
    minInsertion (VastMatrix sdm) = minInsertion sdm
    minInsertion (VastSpecialized m) = minInsertion m


instance MeasurableRange (TransitionMatrix3 a) SymbolIndex where
    measureRange (IotaMatrix _ tcm) = measureRange tcm
    measureRange (VastMatrix sdm) = measureRange sdm
    measureRange (VastSpecialized m) = measureRange m


instance HasSymbolDistances (TransitionMatrix a) where
    symbolDistances (LayoutSquareρ _ tcm) = symbolDistances tcm
    symbolDistances (LayoutSquareλ sdm _ _ _ _) = symbolDistances sdm
    symbolDistances (DiscreteCrossGapρ tcm) = symbolDistances tcm
    symbolDistances (DiscreteCrossGapλ _ g s) = Gap.sdmλ g s
    symbolDistances (DiscreteMetricρ tcm) = symbolDistances tcm
    symbolDistances DiscreteMetricλ{} = Dis.sdmλ
    symbolDistances (LinearNormρ tcm) = symbolDistances tcm
    symbolDistances LinearNormλ{} = L1N.sdmλ


instance HasSymbolCount (TransitionMatrix a) where
    symbolCount (LayoutSquareρ _ tcm) = symbolCount tcm
    symbolCount (LayoutSquareλ sdm _ _ _ _) = symbolCount sdm
    symbolCount (DiscreteCrossGapρ tcm) = symbolCount tcm
    symbolCount (DiscreteCrossGapλ n _ _) = n
    symbolCount (DiscreteMetricρ tcm) = symbolCount tcm
    symbolCount (DiscreteMetricλ n) = n
    symbolCount (LinearNormρ tcm) = symbolCount tcm
    symbolCount (LinearNormλ n) = n


instance Eq (TransitionMatrix a) where
    (==) (LayoutSquareρ sdm _) (LayoutSquareρ sdm' _) = sdm == sdm'
    (==) (LayoutSquareλ sdm _ _ _ _) (LayoutSquareλ sdm' _ _ _ _) = sdm == sdm'
    (==) (DiscreteCrossGapρ tcm) (DiscreteCrossGapρ tcm') = tcm == tcm'
    (==) (DiscreteCrossGapλ n g s) (DiscreteCrossGapλ n' g' s') = n == n' && g == g' && s == s'
    (==) (DiscreteMetricρ tcm) (DiscreteMetricρ tcm') = symbolCount tcm == symbolCount tcm'
    (==) (DiscreteMetricλ n) (DiscreteMetricλ n') = n == n'
    (==) (LinearNormρ tcm) (LinearNormρ tcm') = symbolCount tcm == symbolCount tcm'
    (==) (LinearNormλ n) (LinearNormλ n') = n == n'
    (==) _ _ = False


instance HasStateTransitionsCompact (TransitionMatrix a) where
    getCompactPairwise (LayoutSquareρ _ tcm) = Just tcm
    getCompactPairwise LayoutSquareλ{} = Nothing
    getCompactPairwise (DiscreteCrossGapρ tcm) = Just tcm
    getCompactPairwise DiscreteCrossGapλ{} = Nothing
    getCompactPairwise (DiscreteMetricρ tcm) = Just tcm
    getCompactPairwise DiscreteMetricλ{} = Nothing
    getCompactPairwise (LinearNormρ tcm) = Just tcm
    getCompactPairwise LinearNormλ{} = Nothing


instance HasEditExtrema (TransitionMatrix a) SymbolChangeCost where
    {-# INLINEABLE maxEdit #-}
    maxEdit (LayoutSquareρ _ tcm) = maxEdit tcm
    maxEdit (LayoutSquareλ _ _ e _ _) = e
    maxEdit (DiscreteCrossGapρ tcm) = maxEdit tcm
    maxEdit (DiscreteCrossGapλ _ g _) = g
    maxEdit (DiscreteMetricρ tcm) = maxEdit tcm
    maxEdit DiscreteMetricλ{} = 1
    maxEdit (LinearNormρ tcm) = maxEdit tcm
    maxEdit LinearNormλ{} = 1


    maxDeletion (LayoutSquareρ _ tcm) = maxDeletion tcm
    maxDeletion (LayoutSquareλ sdm _ _ _ _) = maxDeletion sdm
    maxDeletion (DiscreteCrossGapρ tcm) = maxDeletion tcm
    maxDeletion (DiscreteCrossGapλ _ g _) = g
    maxDeletion (DiscreteMetricρ tcm) = maxDeletion tcm
    maxDeletion DiscreteMetricλ{} = 1
    maxDeletion (LinearNormρ tcm) = maxDeletion tcm
    maxDeletion LinearNormλ{} = 1


    maxInsertion (LayoutSquareρ _ tcm) = maxInsertion tcm
    maxInsertion (LayoutSquareλ sdm _ _ _ _) = maxInsertion sdm
    maxInsertion (DiscreteCrossGapρ tcm) = maxInsertion tcm
    maxInsertion (DiscreteCrossGapλ _ g _) = g
    maxInsertion (DiscreteMetricρ tcm) = maxInsertion tcm
    maxInsertion DiscreteMetricλ{} = 1
    maxInsertion (LinearNormρ tcm) = maxInsertion tcm
    maxInsertion LinearNormλ{} = 1


    {-# INLINEABLE minEdit #-}
    minEdit (LayoutSquareρ _ tcm) = minEdit tcm
    minEdit (LayoutSquareλ _ e _ _ _) = e
    minEdit (DiscreteCrossGapρ tcm) = minEdit tcm
    minEdit (DiscreteCrossGapλ _ g _) = g
    minEdit (DiscreteMetricρ tcm) = minEdit tcm
    minEdit DiscreteMetricλ{} = 1
    minEdit (LinearNormρ tcm) = minEdit tcm
    minEdit LinearNormλ{} = 1


    minDeletion (LayoutSquareρ _ tcm) = minDeletion tcm
    minDeletion (LayoutSquareλ sdm _ _ _ _) = minDeletion sdm
    minDeletion (DiscreteCrossGapρ tcm) = minDeletion tcm
    minDeletion (DiscreteCrossGapλ _ g _) = g
    minDeletion (DiscreteMetricρ tcm) = minDeletion tcm
    minDeletion DiscreteMetricλ{} = 1
    minDeletion (LinearNormρ tcm) = minDeletion tcm
    minDeletion LinearNormλ{} = 1


    minInsertion (LayoutSquareρ _ tcm) = minInsertion tcm
    minInsertion (LayoutSquareλ sdm _ _ _ _) = minInsertion sdm
    minInsertion (DiscreteCrossGapρ tcm) = minInsertion tcm
    minInsertion (DiscreteCrossGapλ _ g _) = g
    minInsertion (DiscreteMetricρ tcm) = minInsertion tcm
    minInsertion DiscreteMetricλ{} = 1
    minInsertion (LinearNormρ tcm) = minInsertion tcm
    minInsertion LinearNormλ{} = 1


instance MeasurableRange (TransitionMatrix a) SymbolIndex where
    measureRange = symbolBounds . symbolCount


instance HasSymbolDistances (TransitionMatrix a) where
    symbolDistances (LayoutSquareρ _ tcm) = symbolDistances tcm
    symbolDistances (LayoutSquareλ sdm _ _ _ _) = symbolDistances sdm
    symbolDistances (DiscreteCrossGapρ tcm) = symbolDistances tcm
    symbolDistances (DiscreteCrossGapλ _ g s) = Gap.sdmλ g s
    symbolDistances (DiscreteMetricρ tcm) = symbolDistances tcm
    symbolDistances DiscreteMetricλ{} = Dis.sdmλ
    symbolDistances (LinearNormρ tcm) = symbolDistances tcm
    symbolDistances LinearNormλ{} = L1N.sdmλ


instance HasSymbolCount (TransitionMatrix a) where
    symbolCount (LayoutSquareρ _ tcm) = symbolCount tcm
    symbolCount (LayoutSquareλ sdm _ _ _ _) = symbolCount sdm
    symbolCount (DiscreteCrossGapρ tcm) = symbolCount tcm
    symbolCount (DiscreteCrossGapλ n _ _) = n
    symbolCount (DiscreteMetricρ tcm) = symbolCount tcm
    symbolCount (DiscreteMetricλ n) = n
    symbolCount (LinearNormρ tcm) = symbolCount tcm
    symbolCount (LinearNormλ n) = n


instance (FiniteBits a) => HasStateTransitions (TransitionMatrix a) a where
    stateTransitionPairwiseDispersion (LayoutSquareρ _ tcm) = stateTransitionPairwiseDispersion tcm
    stateTransitionPairwiseDispersion (LayoutSquareλ _ _ _ tcmλ _) = tcmλ
    stateTransitionPairwiseDispersion (DiscreteCrossGapρ tcm) = stateTransitionPairwiseDispersion tcm
    stateTransitionPairwiseDispersion (DiscreteCrossGapλ n g s) = Gap.tcm2Dλ n g s
    stateTransitionPairwiseDispersion (DiscreteMetricρ tcm) = stateTransitionPairwiseDispersion tcm
    stateTransitionPairwiseDispersion DiscreteMetricλ{} = Dis.tcm2Dλ
    stateTransitionPairwiseDispersion (LinearNormρ tcm) = stateTransitionPairwiseDispersion tcm
    stateTransitionPairwiseDispersion (LinearNormλ n) = L1N.tcm2Dλ n


    stateTransitionThreewayDispersion (LayoutSquareρ _ tcm) = stateTransitionThreewayDispersion tcm
    stateTransitionThreewayDispersion (LayoutSquareλ _ _ _ _ tcmλ) = tcmλ
    stateTransitionThreewayDispersion (DiscreteCrossGapρ tcm) = stateTransitionThreewayDispersion tcm
    stateTransitionThreewayDispersion (DiscreteCrossGapλ n g s) = Gap.tcm3Dλ n g s
    stateTransitionThreewayDispersion (DiscreteMetricρ tcm) = stateTransitionThreewayDispersion tcm
    stateTransitionThreewayDispersion DiscreteMetricλ{} = Dis.tcm3Dλ
    stateTransitionThreewayDispersion (LinearNormρ tcm) = stateTransitionThreewayDispersion tcm
    stateTransitionThreewayDispersion (LinearNormλ n) = L1N.tcm3Dλ n


instance {-# OVERLAPPING #-} HasStateTransitions (TransitionMatrix CUInt) CUInt where
    stateTransitionPairwiseDispersion (LayoutSquareρ _ tcm) = stateTransitionPairwiseDispersion tcm
    stateTransitionPairwiseDispersion (LayoutSquareλ _ _ _ tcmλ _) = tcmλ
    stateTransitionPairwiseDispersion (DiscreteCrossGapρ tcm) = stateTransitionPairwiseDispersion tcm
    stateTransitionPairwiseDispersion (DiscreteCrossGapλ n g s) = Gap.tcm2Dλ n g s
    stateTransitionPairwiseDispersion (DiscreteMetricρ tcm) = stateTransitionPairwiseDispersion tcm
    stateTransitionPairwiseDispersion DiscreteMetricλ{} = Dis.tcm2Dλ
    stateTransitionPairwiseDispersion (LinearNormρ tcm) = stateTransitionPairwiseDispersion tcm
    stateTransitionPairwiseDispersion (LinearNormλ n) = L1N.tcm2Dλ n


    stateTransitionThreewayDispersion (LayoutSquareρ _ tcm) = stateTransitionThreewayDispersion tcm
    stateTransitionThreewayDispersion (LayoutSquareλ _ _ _ _ tcmλ) = tcmλ
    stateTransitionThreewayDispersion (DiscreteCrossGapρ tcm) = stateTransitionThreewayDispersion tcm
    stateTransitionThreewayDispersion (DiscreteCrossGapλ n g s) = Gap.tcm3Dλ n g s
    stateTransitionThreewayDispersion (DiscreteMetricρ tcm) = stateTransitionThreewayDispersion tcm
    stateTransitionThreewayDispersion DiscreteMetricλ{} = Dis.tcm3Dλ
    stateTransitionThreewayDispersion (LinearNormρ tcm) = stateTransitionThreewayDispersion tcm
    stateTransitionThreewayDispersion (LinearNormλ n) = L1N.tcm3Dλ n


instance {-# OVERLAPPING #-} HasStateTransitions (TransitionMatrix Word64) Word64 where
    stateTransitionPairwiseDispersion (LayoutSquareρ _ tcm) = stateTransitionPairwiseDispersion tcm
    stateTransitionPairwiseDispersion (LayoutSquareλ _ _ _ tcmλ _) = tcmλ
    stateTransitionPairwiseDispersion (DiscreteCrossGapρ tcm) = stateTransitionPairwiseDispersion tcm
    stateTransitionPairwiseDispersion (DiscreteCrossGapλ n g s) = Gap.tcm2Dλ n g s
    stateTransitionPairwiseDispersion (DiscreteMetricρ tcm) = stateTransitionPairwiseDispersion tcm
    stateTransitionPairwiseDispersion DiscreteMetricλ{} = Dis.tcm2Dλ
    stateTransitionPairwiseDispersion (LinearNormρ tcm) = stateTransitionPairwiseDispersion tcm
    stateTransitionPairwiseDispersion (LinearNormλ n) = L1N.tcm2Dλ n


    stateTransitionThreewayDispersion (LayoutSquareρ _ tcm) = stateTransitionThreewayDispersion tcm
    stateTransitionThreewayDispersion (LayoutSquareλ _ _ _ _ tcmλ) = tcmλ
    stateTransitionThreewayDispersion (DiscreteCrossGapρ tcm) = stateTransitionThreewayDispersion tcm
    stateTransitionThreewayDispersion (DiscreteCrossGapλ n g s) = Gap.tcm3Dλ n g s
    stateTransitionThreewayDispersion (DiscreteMetricρ tcm) = stateTransitionThreewayDispersion tcm
    stateTransitionThreewayDispersion DiscreteMetricλ{} = Dis.tcm3Dλ
    stateTransitionThreewayDispersion (LinearNormρ tcm) = stateTransitionThreewayDispersion tcm
    stateTransitionThreewayDispersion (LinearNormλ n) = L1N.tcm3Dλ n


instance Show (TransitionMatrix a) where
    show LayoutSquareρ{} = "General Metric"
    show LayoutSquareλ{} = "General Metric"
    show DiscreteCrossGapρ{} = "Discrete Metric ⨯ Gap Symbol"
    show DiscreteCrossGapλ{} = "Discrete Metric ⨯ Gap Symbol"
    show DiscreteMetricρ{} = "Discrete Metric"
    show DiscreteMetricλ{} = "Discrete Metric"
    show LinearNormρ{} = "1st Linear Norm"
    show LinearNormλ{} = "1st Linear Norm"


{- |
Nullary constructor for the <https://en.wikipedia.org/wiki/Discrete_space discrete metric>.
-}
discreteMetric :: SymbolCount -> TransitionMatrix a
discreteMetric =
    \case
        SymbolCount 2 -> DiscreteMetricρ Dis.tcmρ2
        SymbolCount 3 -> DiscreteMetricρ Dis.tcmρ3
        SymbolCount 4 -> DiscreteMetricρ Dis.tcmρ4
        SymbolCount 5 -> DiscreteMetricρ Dis.tcmρ5
        SymbolCount 6 -> DiscreteMetricρ Dis.tcmρ6
        SymbolCount 7 -> DiscreteMetricρ Dis.tcmρ7
        SymbolCount 8 -> DiscreteMetricρ Dis.tcmρ8
        n -> DiscreteMetricλ n


{- |
Nullary constructor for the <https://en.wikipedia.org/wiki/Discrete_space discrete metric>.
-}
discreteCrossGap :: SymbolChangeCost -> SymbolChangeCost -> SymbolCount -> TransitionMatrix a
discreteCrossGap 1 2 =
    \case
        SymbolCount 2 -> DiscreteCrossGapρ Gap.tcm12ρ2
        SymbolCount 3 -> DiscreteCrossGapρ Gap.tcm12ρ3
        SymbolCount 4 -> DiscreteCrossGapρ Gap.tcm12ρ4
        SymbolCount 5 -> DiscreteCrossGapρ Gap.tcm12ρ5
        SymbolCount 6 -> DiscreteCrossGapρ Gap.tcm12ρ6
        SymbolCount 7 -> DiscreteCrossGapρ Gap.tcm12ρ7
        SymbolCount 8 -> DiscreteCrossGapρ Gap.tcm12ρ8
        n -> DiscreteCrossGapλ n 1 2
discreteCrossGap gap sub =
    \case
        n | iota n -> DiscreteCrossGapρ $ Gap.tcmρ n gap sub
        n -> DiscreteCrossGapλ n gap sub


{- |
Nullary constructor for the <https://en.wikipedia.org/wiki/Lp_space 1st linear norm>.
-}
linearNorm :: SymbolCount -> TransitionMatrix a
linearNorm =
    \case
        SymbolCount 2 -> LinearNormρ L1N.tcmρ2
        SymbolCount 3 -> LinearNormρ L1N.tcmρ3
        SymbolCount 4 -> LinearNormρ L1N.tcmρ4
        SymbolCount 5 -> LinearNormρ L1N.tcmρ5
        SymbolCount 6 -> LinearNormρ L1N.tcmρ6
        SymbolCount 7 -> LinearNormρ L1N.tcmρ7
        SymbolCount 8 -> LinearNormρ L1N.tcmρ8
        n -> LinearNormλ n


{- |
General constructor for an arbitrary metric.

Performs memoization so repeated value queries are not recomputed.
-}
metricRepresentation
    :: ( FiniteBits a
       , Hashable a
       , NFData a
       )
    => SymbolDistanceMatrixSquare
    -> TransitionMatrix a
metricRepresentation sdmρ
    | iota sdmρ = LayoutSquareρ sdmρ $ fromSymbolDistanceMatrixSquare 0 sdmρ
    | otherwise =
        let sdmλ = symbolDistances sdmρ
            minΔ = minEdit sdmρ
            maxΔ = maxEdit sdmρ
            range = symbolBounds $ symbolCount sdmρ
            tcm2D = memoize2 $ Lax.bitDispersionPairwise range sdmλ
            tcm3D = memoize3 $ Lax.bitDispersionThreeway range sdmλ
        in  LayoutSquareλ sdmρ minΔ maxΔ tcm2D tcm3D
