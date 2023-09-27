{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module Measure.Distance (
    Distance,
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
