{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedSums #-}

module Layout.Special.Type (
    -- * Specializable Metrics
    SpecializableMetric (..),
    renderWithBytes,
) where

import Control.DeepSeq
import Data.Bits
import Data.Data
import Data.Foldable (fold)
import Data.Hashable
import Data.Word (Word64)
import Foreign.C.Types (CUInt)
import GHC.Generics
import Layout.Special.Discrete qualified as Dis
import Layout.Special.DiscreteCrossGap qualified as Gap
import Layout.Special.L1Norm qualified as L1N
import Measure.Range
import Measure.Transition
import Measure.Unit.SymbolCount
import Measure.Unit.SymbolDistance
import Measure.Unit.SymbolIndex


{- |
Represents and encodes a matrix with a metric which can be "special cased" for improved efficiency.
-}
data SpecializableMetric
    = DiscreteCrossGap
        {-# UNPACK #-} !SymbolCount
        {-# UNPACK #-} !SymbolDistance
        {-# UNPACK #-} !SymbolDistance
    | DiscreteMetric {-# UNPACK #-} !SymbolCount
    | L1Norm {-# UNPACK #-} !SymbolCount
    deriving stock (Data, Eq, Generic)
    deriving anyclass (NFData)


instance HasEditExtrema SpecializableMetric where
    maxDeletion =
        \case
            DiscreteCrossGap _ _ g → g
            DiscreteMetric{} → 1
            L1Norm (SymbolCount n) → SymbolDistance $ n - 1


    maxInsertion =
        \case
            DiscreteCrossGap _ _ g → g
            DiscreteMetric{} → 1
            L1Norm (SymbolCount n) → SymbolDistance $ n - 1


    minDeletion =
        \case
            DiscreteCrossGap _ _ g → g
            DiscreteMetric{} → 1
            L1Norm{} → 1


    minInsertion =
        \case
            DiscreteCrossGap _ _ g → g
            DiscreteMetric{} → 1
            L1Norm{} → 1


instance HasSymbolCount SpecializableMetric where
    symbolCount =
        \case
            DiscreteCrossGap n _ _ → n
            DiscreteMetric n → n
            L1Norm n → n


instance HasSymbolDistances SpecializableMetric where
    symbolDistances =
        \case
            DiscreteCrossGap _ s g → Gap.sdmλ s g
            DiscreteMetric{} → Dis.sdmλ
            L1Norm{} → L1N.sdmλ


{- |
/O(1)/
-}
instance (FiniteBits b, Hashable b) ⇒ HasStateTransitions SpecializableMetric b where
    stateTransitionPairwiseDispersion =
        \case
            DiscreteCrossGap n s g → Gap.tcm2Dλ n s g
            DiscreteMetric{} → Dis.tcm2Dλ
            L1Norm n → L1N.tcm2Dλ n


    stateTransitionThreewayDispersion =
        \case
            DiscreteMetric{} → Dis.tcm3Dλ
            DiscreteCrossGap n s g → Gap.tcm3Dλ n s g
            L1Norm n → L1N.tcm3Dλ n


{- |
/O(1)/
-}
instance {-# OVERLAPPING #-} HasStateTransitions SpecializableMetric CUInt where
    stateTransitionPairwiseDispersion =
        \case
            DiscreteCrossGap n s g → Gap.tcm2Dλ n s g
            DiscreteMetric{} → Dis.tcm2Dλ
            L1Norm n → L1N.tcm2Dλ n


    stateTransitionThreewayDispersion =
        \case
            DiscreteMetric{} → Dis.tcm3Dλ
            DiscreteCrossGap n s g → Gap.tcm3Dλ n s g
            L1Norm n → L1N.tcm3Dλ n


{- |
/O(1)/
-}
instance {-# OVERLAPPING #-} HasStateTransitions SpecializableMetric Word64 where
    stateTransitionPairwiseDispersion =
        \case
            DiscreteCrossGap n s g → Gap.tcm2Dλ n s g
            DiscreteMetric{} → Dis.tcm2Dλ
            L1Norm n → L1N.tcm2Dλ n


    stateTransitionThreewayDispersion =
        \case
            DiscreteMetric{} → Dis.tcm3Dλ
            DiscreteCrossGap n s g → Gap.tcm3Dλ n s g
            L1Norm n → L1N.tcm3Dλ n


instance MeasurableRange SpecializableMetric SymbolIndex where
    measureRange = symbolBounds . symbolCount


instance Show SpecializableMetric where
    show = renderWithBytes 0


{- |
A human readable, pretty printer for the 'SpecializableMetric' type.
-}
renderWithBytes ∷ Int → SpecializableMetric → String
renderWithBytes b =
    \case
        DiscreteCrossGap n s g →
            unlines
                [ "Discrete Metric ⨯ Gap Symbol with"
                , fold ["allocated ", show b, "bytes"]
                , fold ["dimension ", show n]
                , fold ["substitution ", show s, "Δ"]
                , fold ["ins/del edit ", show g, "Δ"]
                ]
        DiscreteMetric n →
            unlines
                [ "Discrete Metric"
                , fold ["allocated ", show b, "bytes"]
                , fold ["dimension ", show n]
                ]
        L1Norm n →
            unlines
                [ "1st Linear Norm"
                , fold ["allocated ", show b, "bytes"]
                , fold ["dimension ", show n]
                ]
