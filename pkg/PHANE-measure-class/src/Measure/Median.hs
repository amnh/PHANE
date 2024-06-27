{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

{- |
Defines the data-types and type-classes for measuring the (geometric) median
between multiple objects in a metric space.

For more information, see [Wikipedia's "Geometric median" article](https://en.wikipedia.org/wiki/Geometric_median).
-}
module Measure.Median (
    -- * Data-types
    Median,
    MedianPairwise,
    MedianThreeway,

    -- * Type-classes
    MeasurableMedian (..),
    MeasurableMedianPairwise (..),
    MeasurableMedianThreeway (..),
) where


{- |
Abstract function computing a [(Geometric) Median](https://en.wikipedia.org/wiki/Geometric_median).
-}
type Median e = ∀ f. (Foldable f) ⇒ f e → e


{- |
Compute the [(Geometric) Median](https://en.wikipedia.org/wiki/Geometric_median) of a pair.
-}
type MedianPairwise e = e → e → e


{- |
Compute the [(Geometric) Median](https://en.wikipedia.org/wiki/Geometric_median) of a triple.
-}
type MedianThreeway e = e → e → e → e


{- |
Structure which can compute a [(Geometric) Median](https://en.wikipedia.org/wiki/Geometric_median).
-}
class MeasurableMedian a e where
    measureMedian ∷ a → Median e


{- |
Structure which can compute the [(Geometric) Median](https://en.wikipedia.org/wiki/Geometric_median) of a pair.
-}
class MeasurableMedianPairwise a e where
    measureMedianPairwise ∷ a → MedianPairwise e


{- |
Structure which can compute the [(Geometric) Median](https://en.wikipedia.org/wiki/Geometric_median) of a triple.
-}
class MeasurableMedianThreeway a e where
    measureMedianThreeway ∷ a → MedianThreeway e
