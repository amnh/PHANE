{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE StrictData #-}

module Layout.Memoize.Dispersion (
    bitDispersion,
    bitDispersionPairwise,
    bitDispersionThreeway,
) where

import Data.Bits
import Data.Foldable
import Data.Ix
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust)
import Data.Monoid (First (..))
import Data.Word
import Foreign.C.Types (CUInt)
import Measure.Dispersion
import Measure.Distance
import Measure.Unit.SymbolDistance
import Measure.Unit.SymbolIndex


data Bounds b i = Bounds
    { _lBound ∷ i
    , _uBound ∷ i
    , _bValue ∷ b
    }


type role Bounds representational representational


{- |
Takes one or more elements of 'FiniteBits' and a symbol distance cost function
and returns a tuple of a new character, along with the cost of obtaining that
character. The return character may be (or is even likely to be) ambiguous.
Will attempt to intersect the two characters, but will union them if that is
not possible, based on the symbol distance cost function.

To clarify, the return character is an intersection of all possible least-cost
combinations, so for instance, if @ char1 == A,T @ and @ char2 == G,C @, and
the two (non-overlapping) least cost pairs are A,C and T,G, then the return
value is A,C,G,T.
-}

{-
{-# INLINEABLE bitDispersion #-}
{-# SPECIALISE bitDispersion :: (Bounded c, Enum i, Integral c, Ix i, Num d) => (i, i) -> Distance c i -> Dispersion d CUInt  #-}
{-# SPECIALISE bitDispersion :: (Bounded c, Enum i, Integral c, Ix i, Num d) => (i, i) -> Distance c i -> Dispersion d Word   #-}
{-# SPECIALISE bitDispersion :: (Bounded c, Enum i, Integral c, Ix i, Num d) => (i, i) -> Distance c i -> Dispersion d Word8  #-}
{-# SPECIALISE bitDispersion :: (Bounded c, Enum i, Integral c, Ix i, Num d) => (i, i) -> Distance c i -> Dispersion d Word16 #-}
{-# SPECIALISE bitDispersion :: (Bounded c, Enum i, Integral c, Ix i, Num d) => (i, i) -> Distance c i -> Dispersion d Word32 #-}
{-# SPECIALISE bitDispersion :: (Bounded c, Enum i, Integral c, Ix i, Num d) => (i, i) -> Distance c i -> Dispersion d Word64 #-}
{-# SPECIALISE bitDispersion :: (Bounded c, Integral c, Num d) => (SymbolIndex, SymbolIndex) -> Distance c SymbolIndex -> Dispersion d CUInt  #-}
{-# SPECIALISE bitDispersion :: (Bounded c, Integral c, Num d) => (SymbolIndex, SymbolIndex) -> Distance c SymbolIndex -> Dispersion d Word   #-}
{-# SPECIALISE bitDispersion :: (Bounded c, Integral c, Num d) => (SymbolIndex, SymbolIndex) -> Distance c SymbolIndex -> Dispersion d Word8  #-}
{-# SPECIALISE bitDispersion :: (Bounded c, Integral c, Num d) => (SymbolIndex, SymbolIndex) -> Distance c SymbolIndex -> Dispersion d Word16 #-}
{-# SPECIALISE bitDispersion :: (Bounded c, Integral c, Num d) => (SymbolIndex, SymbolIndex) -> Distance c SymbolIndex -> Dispersion d Word32 #-}
{-# SPECIALISE bitDispersion :: (Bounded c, Integral c, Num d) => (SymbolIndex, SymbolIndex) -> Distance c SymbolIndex -> Dispersion d Word64 #-}
{-# SPECIALISE bitDispersion :: (Enum i, Ix i) => (i, i) -> Distance SymbolDistance i -> Dispersion SymbolDistance CUInt  #-}
{-# SPECIALISE bitDispersion :: (Enum i, Ix i) => (i, i) -> Distance SymbolDistance i -> Dispersion SymbolDistance Word   #-}
{-# SPECIALISE bitDispersion :: (Enum i, Ix i) => (i, i) -> Distance SymbolDistance i -> Dispersion SymbolDistance Word8  #-}
{-# SPECIALISE bitDispersion :: (Enum i, Ix i) => (i, i) -> Distance SymbolDistance i -> Dispersion SymbolDistance Word16 #-}
{-# SPECIALISE bitDispersion :: (Enum i, Ix i) => (i, i) -> Distance SymbolDistance i -> Dispersion SymbolDistance Word32 #-}
{-# SPECIALISE bitDispersion :: (Enum i, Ix i) => (i, i) -> Distance SymbolDistance i -> Dispersion SymbolDistance Word64 #-}
{-# SPECIALISE bitDispersion :: (SymbolIndex, SymbolIndex) -> Distance SymbolDistance SymbolIndex -> Dispersion SymbolDistance CUInt  #-}
{-# SPECIALISE bitDispersion :: (SymbolIndex, SymbolIndex) -> Distance SymbolDistance SymbolIndex -> Dispersion SymbolDistance Word   #-}
{-# SPECIALISE bitDispersion :: (SymbolIndex, SymbolIndex) -> Distance SymbolDistance SymbolIndex -> Dispersion SymbolDistance Word8  #-}
{-# SPECIALISE bitDispersion :: (SymbolIndex, SymbolIndex) -> Distance SymbolDistance SymbolIndex -> Dispersion SymbolDistance Word16 #-}
{-# SPECIALISE bitDispersion :: (SymbolIndex, SymbolIndex) -> Distance SymbolDistance SymbolIndex -> Dispersion SymbolDistance Word32 #-}
{-# SPECIALISE bitDispersion :: (SymbolIndex, SymbolIndex) -> Distance SymbolDistance SymbolIndex -> Dispersion SymbolDistance Word64 #-}
-}
bitDispersion
    ∷ ( Bounded c
      , Enum i
      , FiniteBits b
      , Integral c
      , Ix i
      , Num d
      )
    ⇒ (i, i)
    -- ^ Magnitude
    → Distance c i
    -- ^ symbol distance matrix (SDM) to determine cost
    → Dispersion d b
    -- ^ List of elements for of which to find the k-median and cost
bitDispersion ixBounds sigma xs = (fromIntegral distance, gMedian)
    where
        (distance, gMedian) = foldl' processRange (maxBound, zero) $ range ixBounds
        withBounds = getBitBounds <$> toList xs
        --    wlog  = getFirst $ foldMap1 First xs
        wlog = fromJust . getFirst $ foldMap (First . Just) xs
        zero = wlog `xor` wlog

        processRange (oldCost, bits) i =
            let newCost = foldl' (+) 0 $ getDistance i <$> withBounds
            in  case oldCost `compare` newCost of
                    LT → (oldCost, bits)
                    EQ → (oldCost, bits `setBit` fromEnum i)
                    GT → (newCost, zero `setBit` fromEnum i)

        getDistance i (Bounds lo hi b) = foldl' processSubrange maxBound $ range (lo, hi)
            where
                processSubrange cost j
                    | b `testBit` fromEnum j = min cost $ sigma i j
                    | otherwise = cost


{- |
Calculate the median between /two/ states.
-}
{-# INLINEABLE bitDispersionPairwise #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (Bounded c, Enum i, Integral c, Ix i, Num d) ⇒ (i, i) → Distance c i → DispersionPairwise d CUInt
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (Bounded c, Enum i, Integral c, Ix i, Num d) ⇒ (i, i) → Distance c i → DispersionPairwise d Word
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (Bounded c, Enum i, Integral c, Ix i, Num d) ⇒ (i, i) → Distance c i → DispersionPairwise d Word8
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (Bounded c, Enum i, Integral c, Ix i, Num d) ⇒ (i, i) → Distance c i → DispersionPairwise d Word16
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (Bounded c, Enum i, Integral c, Ix i, Num d) ⇒ (i, i) → Distance c i → DispersionPairwise d Word32
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (Bounded c, Enum i, Integral c, Ix i, Num d) ⇒ (i, i) → Distance c i → DispersionPairwise d Word64
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (Bounded c, Integral c, Num d) ⇒ (SymbolIndex, SymbolIndex) → Distance c SymbolIndex → DispersionPairwise d CUInt
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (Bounded c, Integral c, Num d) ⇒ (SymbolIndex, SymbolIndex) → Distance c SymbolIndex → DispersionPairwise d Word
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (Bounded c, Integral c, Num d) ⇒ (SymbolIndex, SymbolIndex) → Distance c SymbolIndex → DispersionPairwise d Word8
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (Bounded c, Integral c, Num d) ⇒ (SymbolIndex, SymbolIndex) → Distance c SymbolIndex → DispersionPairwise d Word16
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (Bounded c, Integral c, Num d) ⇒ (SymbolIndex, SymbolIndex) → Distance c SymbolIndex → DispersionPairwise d Word32
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (Bounded c, Integral c, Num d) ⇒ (SymbolIndex, SymbolIndex) → Distance c SymbolIndex → DispersionPairwise d Word64
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (Enum i, Ix i) ⇒ (i, i) → Distance SymbolDistance i → DispersionPairwise SymbolDistance CUInt
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (Enum i, Ix i) ⇒ (i, i) → Distance SymbolDistance i → DispersionPairwise SymbolDistance Word
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (Enum i, Ix i) ⇒ (i, i) → Distance SymbolDistance i → DispersionPairwise SymbolDistance Word8
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (Enum i, Ix i) ⇒ (i, i) → Distance SymbolDistance i → DispersionPairwise SymbolDistance Word16
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (Enum i, Ix i) ⇒ (i, i) → Distance SymbolDistance i → DispersionPairwise SymbolDistance Word32
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (Enum i, Ix i) ⇒ (i, i) → Distance SymbolDistance i → DispersionPairwise SymbolDistance Word64
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (SymbolIndex, SymbolIndex) → Distance SymbolDistance SymbolIndex → DispersionPairwise SymbolDistance CUInt
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (SymbolIndex, SymbolIndex) → Distance SymbolDistance SymbolIndex → DispersionPairwise SymbolDistance Word
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (SymbolIndex, SymbolIndex) → Distance SymbolDistance SymbolIndex → DispersionPairwise SymbolDistance Word8
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (SymbolIndex, SymbolIndex) → Distance SymbolDistance SymbolIndex → DispersionPairwise SymbolDistance Word16
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (SymbolIndex, SymbolIndex) → Distance SymbolDistance SymbolIndex → DispersionPairwise SymbolDistance Word32
    #-}
{-# SPECIALIZE bitDispersionPairwise ∷
    (SymbolIndex, SymbolIndex) → Distance SymbolDistance SymbolIndex → DispersionPairwise SymbolDistance Word64
    #-}
bitDispersionPairwise
    ∷ ( Bounded c
      , Enum i
      , FiniteBits b
      , Integral c
      , Ix i
      , Num d
      )
    ⇒ (i, i)
    → Distance c i
    → DispersionPairwise d b
bitDispersionPairwise size sigma char1 char2 = bitDispersion size sigma $ char1 :| [char2]


{- |
Calculate the median between /three/ states.
-}
{-# INLINEABLE bitDispersionThreeway #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (Bounded c, Enum i, Integral c, Ix i, Num d) ⇒ (i, i) → Distance c i → DispersionThreeway d CUInt
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (Bounded c, Enum i, Integral c, Ix i, Num d) ⇒ (i, i) → Distance c i → DispersionThreeway d Word
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (Bounded c, Enum i, Integral c, Ix i, Num d) ⇒ (i, i) → Distance c i → DispersionThreeway d Word8
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (Bounded c, Enum i, Integral c, Ix i, Num d) ⇒ (i, i) → Distance c i → DispersionThreeway d Word16
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (Bounded c, Enum i, Integral c, Ix i, Num d) ⇒ (i, i) → Distance c i → DispersionThreeway d Word32
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (Bounded c, Enum i, Integral c, Ix i, Num d) ⇒ (i, i) → Distance c i → DispersionThreeway d Word64
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (Bounded c, Integral c, Num d) ⇒ (SymbolIndex, SymbolIndex) → Distance c SymbolIndex → DispersionThreeway d CUInt
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (Bounded c, Integral c, Num d) ⇒ (SymbolIndex, SymbolIndex) → Distance c SymbolIndex → DispersionThreeway d Word
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (Bounded c, Integral c, Num d) ⇒ (SymbolIndex, SymbolIndex) → Distance c SymbolIndex → DispersionThreeway d Word8
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (Bounded c, Integral c, Num d) ⇒ (SymbolIndex, SymbolIndex) → Distance c SymbolIndex → DispersionThreeway d Word16
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (Bounded c, Integral c, Num d) ⇒ (SymbolIndex, SymbolIndex) → Distance c SymbolIndex → DispersionThreeway d Word32
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (Bounded c, Integral c, Num d) ⇒ (SymbolIndex, SymbolIndex) → Distance c SymbolIndex → DispersionThreeway d Word64
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (Enum i, Ix i) ⇒ (i, i) → Distance SymbolDistance i → DispersionThreeway SymbolDistance CUInt
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (Enum i, Ix i) ⇒ (i, i) → Distance SymbolDistance i → DispersionThreeway SymbolDistance Word
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (Enum i, Ix i) ⇒ (i, i) → Distance SymbolDistance i → DispersionThreeway SymbolDistance Word8
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (Enum i, Ix i) ⇒ (i, i) → Distance SymbolDistance i → DispersionThreeway SymbolDistance Word16
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (Enum i, Ix i) ⇒ (i, i) → Distance SymbolDistance i → DispersionThreeway SymbolDistance Word32
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (Enum i, Ix i) ⇒ (i, i) → Distance SymbolDistance i → DispersionThreeway SymbolDistance Word64
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (SymbolIndex, SymbolIndex) → Distance SymbolDistance SymbolIndex → DispersionThreeway SymbolDistance CUInt
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (SymbolIndex, SymbolIndex) → Distance SymbolDistance SymbolIndex → DispersionThreeway SymbolDistance Word
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (SymbolIndex, SymbolIndex) → Distance SymbolDistance SymbolIndex → DispersionThreeway SymbolDistance Word8
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (SymbolIndex, SymbolIndex) → Distance SymbolDistance SymbolIndex → DispersionThreeway SymbolDistance Word16
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (SymbolIndex, SymbolIndex) → Distance SymbolDistance SymbolIndex → DispersionThreeway SymbolDistance Word32
    #-}
{-# SPECIALIZE bitDispersionThreeway ∷
    (SymbolIndex, SymbolIndex) → Distance SymbolDistance SymbolIndex → DispersionThreeway SymbolDistance Word64
    #-}
bitDispersionThreeway
    ∷ ( Bounded c
      , Enum i
      , FiniteBits b
      , Integral c
      , Ix i
      , Num d
      )
    ⇒ (i, i)
    → Distance c i
    → DispersionThreeway d b
bitDispersionThreeway size sigma char1 char2 char3 = bitDispersion size sigma $ char1 :| [char2, char3]


{- |
Gets the lowest set bit and the highest set bit in the collection.
-}
getBitBounds
    ∷ ( Enum i
      , FiniteBits b
      )
    ⇒ b
    → Bounds b i
getBitBounds b =
    let bitZero = (b `xor` b) `setBit` 0
        bigEndian = countLeadingZeros bitZero > 0 -- Check the endianness
        (f, g)
            | bigEndian = (countTrailingZeros, countLeadingZeros)
            | otherwise = (countLeadingZeros, countTrailingZeros)

        lZeroes = f b
        uZeroes = g b
        lower = toEnum lZeroes
        upper = toEnum . max 0 $ finiteBitSize b - uZeroes - 1
    in  Bounds lower upper b
