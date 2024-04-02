{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

{- |
The range of a measure which consists of an upper and lower bound.
-}
module Measure.Range (
    -- * Type-class
    MeasurableRange (..),

    -- * Associated type-alias
    InclusiveRange,
) where

import Data.Ix


{- |
The /inclusive/ lower and upper bounds of a range, respectively.
-}
type InclusiveRange i = (i, i)


{- |
Structure which measures a contiguous 'InclusiveRange'.
-}
class (Ix i) ⇒ MeasurableRange a i where
    measureRange ∷ a → InclusiveRange i
