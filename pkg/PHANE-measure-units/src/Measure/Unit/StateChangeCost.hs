{- |
The distance between two states indicating the "cost" of transitioning from one state to another.
-}

{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language Safe #-}

module Measure.Unit.StateChangeCost
  ( StateChangeCost(..)
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
newtype StateChangeCost = StateChangeCost Word
    deriving stock (Eq, Data, Generic, Ord)


instance Bounded StateChangeCost where

    minBound = StateChangeCost minBound

    maxBound = StateChangeCost maxBound


instance Enum StateChangeCost where

    toEnum = StateChangeCost . toEnum

    fromEnum (StateChangeCost c) = fromEnum c


instance Hashable StateChangeCost where

    hashWithSalt salt (StateChangeCost c) = hashWithSalt salt c


instance Integral StateChangeCost where

    quotRem (StateChangeCost x) (StateChangeCost y) = bimap StateChangeCost StateChangeCost $ quotRem x y

    toInteger (StateChangeCost c) = toInteger c


instance NFData StateChangeCost where

    rnf (StateChangeCost c) = rnf c


instance Num StateChangeCost where

    (+) (StateChangeCost x) (StateChangeCost y) = StateChangeCost $ x + y

    (*) (StateChangeCost x) (StateChangeCost y) = StateChangeCost $ x * y

    (-) (StateChangeCost x) (StateChangeCost y) = StateChangeCost $ x - y

    abs    (StateChangeCost c) = StateChangeCost $ abs c

    signum (StateChangeCost c) = StateChangeCost $ signum c

    negate (StateChangeCost c) = StateChangeCost $ negate c

    fromInteger i = StateChangeCost $ fromInteger i


instance Real StateChangeCost where

    toRational (StateChangeCost c) = toRational c


instance Read StateChangeCost where

    readPrec = StateChangeCost <$> readPrec

    readListPrec = readListPrecDefault


instance Show StateChangeCost where

    show (StateChangeCost c) = show c

    showsPrec n (StateChangeCost c) = showsPrec n c


instance Storable StateChangeCost where

    sizeOf (StateChangeCost c) = sizeOf c

    alignment (StateChangeCost c) = alignment c

    peek = fmap StateChangeCost . peek . castPtr

    poke ptr (StateChangeCost c) = poke (castPtr ptr) c
