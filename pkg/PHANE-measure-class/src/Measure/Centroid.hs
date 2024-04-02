{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

{- |
Defines the data-types and type-classes for measuring the centroid between multiple objects in a metric space.
-}
module Measure.Centroid (
    -- * Data-types
    Centroid,
    CentroidPairwise,
    CentroidThreeway,

    -- * Type-classes
    MeasurableCentroid (..),
    MeasurableCentroidPairwise (..),
    MeasurableCentroidThreeway (..),
) where


{- |
Abstract function computing a [Centroid](https://en.wikipedia.org/wiki/Centroid).
-}
type Centroid e = ∀ f. (Foldable f) ⇒ f e → e


{- |
Compute the [Centroid](https://en.wikipedia.org/wiki/Centroid) of a pair.
-}
type CentroidPairwise e = e → e → e


{- |
Compute the [Centroid](https://en.wikipedia.org/wiki/Centroid) of a triple.
-}
type CentroidThreeway e = e → e → e → e


{- |
Structure which can compute a [Centroid](https://en.wikipedia.org/wiki/Centroid).
-}
class MeasurableCentroid a e where
    measureCentroid ∷ a → Centroid e


{- |
Structure which can compute the [Centroid](https://en.wikipedia.org/wiki/Centroid) of a pair.
-}
class MeasurableCentroidPairwise a e where
    measureCentroidPairwise ∷ a → CentroidPairwise e


{- |
Structure which can compute the [Centroid](https://en.wikipedia.org/wiki/Centroid) of a triple.
-}
class MeasurableCentroidThreeway a e where
    measureCentroidThreeway ∷ a → CentroidThreeway e
