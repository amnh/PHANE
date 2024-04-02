{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

{- |
Defines the data-type and type-class for measuring the distance between two objects in a metric space.
-}
module Measure.Distance (
    -- * Data-type
    Distance,

    -- * Type-class
    MeasurableDistance (..),
) where


{- |
Abstract computation of the [Distance](https://en.wikipedia.org/wiki/Distance) between two "points."
-}
type Distance c e = e → e → c


{- |
Structure which computes the [Distance](https://en.wikipedia.org/wiki/Distance) between two "points."
-}
class MeasurableDistance a c e where
    measureDistance ∷ a → Distance c e
