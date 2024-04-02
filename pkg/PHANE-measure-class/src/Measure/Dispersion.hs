{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

{- |
Defines the data-types and type-classes for measuring the dispersion between multiple objects in a metric space.
-}
module Measure.Dispersion (
    -- * Data-types
    Dispersion,
    DispersionPairwise,
    DispersionThreeway,

    -- * Type-classes
    MeasurableDispersion (..),
    MeasurableDispersionPairwise (..),
    MeasurableDispersionThreeway (..),
) where


{- |
Abstract function computing the [Dispersion](https://en.wikipedia.org/wiki/Statistical_dispersion)
of a collection of points, returning the dispersion 'Measure.Distance.Distance'
and 'Measure.Centroid.Centroid'.
-}
type Dispersion c e = ∀ f. (Foldable f) ⇒ f e → (c, e)


{- |
Abstract function computing the [Dispersion](https://en.wikipedia.org/wiki/Statistical_dispersion)
of a pair.
-}
type DispersionPairwise c e = e → e → (c, e)


{- |
Abstract function computing the [Dispersion](https://en.wikipedia.org/wiki/Statistical_dispersion)
of a triple.
-}
type DispersionThreeway c e = e → e → e → (c, e)


{- |
Structure which can compute a [Dispersion](https://en.wikipedia.org/wiki/Statistical_dispersion).
-}
class MeasurableDispersion a c e where
    measureDispersion ∷ a → Dispersion c e


{- |
Structure which can compute the [Dispersion](https://en.wikipedia.org/wiki/Statistical_dispersion) of a pair.
-}
class MeasurableDispersionPairwise a c e where
    measureDispersionPairwise ∷ a → DispersionPairwise c e


{- |
Structure which can compute the [Dispersion](https://en.wikipedia.org/wiki/Statistical_dispersion) of a triple.
-}
class MeasurableDispersionThreeway a c e where
    measureDispersionThreeway ∷ a → DispersionThreeway c e
