{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}

{- |
The cost of aligning dynamic characters.
-}
module Measure.Unit.AlignmentCost (
    AlignmentCost (..),
) where

import Control.DeepSeq
import Data.Bifunctor
import Data.Data
import Data.Hashable
import Foreign.Ptr (castPtr)
import Foreign.Storable
import GHC.Generics
import Text.Read


{- |
The distance between two measurable elements.
-}
newtype AlignmentCost = AlignmentCost Word
    deriving stock (Eq, Data, Generic, Ord)


instance Bounded AlignmentCost where
    minBound = AlignmentCost minBound


    maxBound = AlignmentCost maxBound


instance Enum AlignmentCost where
    toEnum = AlignmentCost . toEnum


    fromEnum (AlignmentCost c) = fromEnum c


instance Hashable AlignmentCost where
    hashWithSalt salt (AlignmentCost c) = hashWithSalt salt c


instance Integral AlignmentCost where
    quotRem (AlignmentCost x) (AlignmentCost y) = bimap AlignmentCost AlignmentCost $ quotRem x y


    toInteger (AlignmentCost c) = toInteger c


instance NFData AlignmentCost where
    rnf (AlignmentCost c) = rnf c


instance Num AlignmentCost where
    (+) (AlignmentCost x) (AlignmentCost y) = AlignmentCost $ x + y


    (*) (AlignmentCost x) (AlignmentCost y) = AlignmentCost $ x * y


    (-) (AlignmentCost x) (AlignmentCost y) = AlignmentCost $ x - y


    abs (AlignmentCost c) = AlignmentCost $ abs c


    signum (AlignmentCost c) = AlignmentCost $ signum c


    negate (AlignmentCost c) = AlignmentCost $ negate c


    fromInteger i = AlignmentCost $ fromInteger i


instance Real AlignmentCost where
    toRational (AlignmentCost c) = toRational c


instance Read AlignmentCost where
    readPrec = AlignmentCost <$> readPrec


    readListPrec = readListPrecDefault


instance Show AlignmentCost where
    show (AlignmentCost c) = show c


    showsPrec n (AlignmentCost c) = showsPrec n c


instance Storable AlignmentCost where
    sizeOf (AlignmentCost c) = sizeOf c


    alignment (AlignmentCost c) = alignment c


    peek = fmap AlignmentCost . peek . castPtr


    poke ptr (AlignmentCost c) = poke (castPtr ptr) c
