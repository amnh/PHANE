{- |
Facilitates encoding and decoding symbols from an 'Alphabet' into a
bit-packed state. Operating on encodings is useful for abstracting
functonality while simultaneously reducing memory presure.

Works for any 'Bits' instance.
-}

{-# Language GADTs #-}
{-# Language Strict #-}
{-# Language TypeOperators #-}

module Data.Alphabet.Codec
    ( decodeState
    , encodeState
    ) where

import Data.Alphabet.Internal
import Data.Bits
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as V
import Data.Word
import Foreign.C.Types
import GHC.Exts (IsList(fromList), Item)


{- |
$\mathcal{O}\left(\, \log_{2}\lvert\Sigma\rvert * n \,\right)$

Encode a collection of symbols as a unique subset of the supplied 'Alphabet'.
The result is bit-packed encoding of the collection of symbols which uniquely
corresponds to an element of the power-set of the alphabet.

Requires a specification as to how to created an "empty" 'Bits' element based
on the size of the alphabet.

/NOTE:/ This is the inverse of 'decodeState'.
-}
{-# INLINABLE encodeState #-}
{-# SPECIALISE encodeState :: (Foldable f, Ord s) => Alphabet s -> (Word -> CUChar ) -> f s -> CUChar  #-}
{-# SPECIALISE encodeState :: (Foldable f, Ord s) => Alphabet s -> (Word -> CUShort) -> f s -> CUShort #-}
{-# SPECIALISE encodeState :: (Foldable f, Ord s) => Alphabet s -> (Word -> CUInt  ) -> f s -> CUInt   #-}
{-# SPECIALISE encodeState :: (Foldable f, Ord s) => Alphabet s -> (Word -> CULong ) -> f s -> CULong  #-}
{-# SPECIALISE encodeState :: (Foldable f, Ord s) => Alphabet s -> (Word -> Word   ) -> f s -> Word    #-}
{-# SPECIALISE encodeState :: (Foldable f, Ord s) => Alphabet s -> (Word -> Word8  ) -> f s -> Word8   #-}
{-# SPECIALISE encodeState :: (Foldable f, Ord s) => Alphabet s -> (Word -> Word16 ) -> f s -> Word16  #-}
{-# SPECIALISE encodeState :: (Foldable f, Ord s) => Alphabet s -> (Word -> Word32 ) -> f s -> Word32  #-}
{-# SPECIALISE encodeState :: (Foldable f, Ord s) => Alphabet s -> (Word -> Word64 ) -> f s -> Word64  #-}
{-# SPECIALISE encodeState :: Ord s => Alphabet s -> (Word -> CUChar ) -> Set s -> CUChar  #-}
{-# SPECIALISE encodeState :: Ord s => Alphabet s -> (Word -> CUShort) -> Set s -> CUShort #-}
{-# SPECIALISE encodeState :: Ord s => Alphabet s -> (Word -> CUInt  ) -> Set s -> CUInt   #-}
{-# SPECIALISE encodeState :: Ord s => Alphabet s -> (Word -> CULong ) -> Set s -> CULong  #-}
{-# SPECIALISE encodeState :: Ord s => Alphabet s -> (Word -> Word   ) -> Set s -> Word    #-}
{-# SPECIALISE encodeState :: Ord s => Alphabet s -> (Word -> Word8  ) -> Set s -> Word8   #-}
{-# SPECIALISE encodeState :: Ord s => Alphabet s -> (Word -> Word16 ) -> Set s -> Word16  #-}
{-# SPECIALISE encodeState :: Ord s => Alphabet s -> (Word -> Word32 ) -> Set s -> Word32  #-}
{-# SPECIALISE encodeState :: Ord s => Alphabet s -> (Word -> Word64 ) -> Set s -> Word64  #-}
{-# SPECIALISE encodeState :: Alphabet String -> (Word -> CUChar ) -> Set String -> CUChar  #-}
{-# SPECIALISE encodeState :: Alphabet String -> (Word -> CUShort) -> Set String -> CUShort #-}
{-# SPECIALISE encodeState :: Alphabet String -> (Word -> CUInt  ) -> Set String -> CUInt   #-}
{-# SPECIALISE encodeState :: Alphabet String -> (Word -> CULong ) -> Set String -> CULong  #-}
{-# SPECIALISE encodeState :: Alphabet String -> (Word -> Word   ) -> Set String -> Word    #-}
{-# SPECIALISE encodeState :: Alphabet String -> (Word -> Word8  ) -> Set String -> Word8   #-}
{-# SPECIALISE encodeState :: Alphabet String -> (Word -> Word16 ) -> Set String -> Word16  #-}
{-# SPECIALISE encodeState :: Alphabet String -> (Word -> Word32 ) -> Set String -> Word32  #-}
{-# SPECIALISE encodeState :: Alphabet String -> (Word -> Word64 ) -> Set String -> Word64  #-}
{-# SPECIALISE encodeState :: Alphabet String -> (Word -> CUChar ) -> NonEmpty String -> CUChar  #-}
{-# SPECIALISE encodeState :: Alphabet String -> (Word -> CUShort) -> NonEmpty String -> CUShort #-}
{-# SPECIALISE encodeState :: Alphabet String -> (Word -> CUInt  ) -> NonEmpty String -> CUInt   #-}
{-# SPECIALISE encodeState :: Alphabet String -> (Word -> CULong ) -> NonEmpty String -> CULong  #-}
{-# SPECIALISE encodeState :: Alphabet String -> (Word -> Word   ) -> NonEmpty String -> Word    #-}
{-# SPECIALISE encodeState :: Alphabet String -> (Word -> Word8  ) -> NonEmpty String -> Word8   #-}
{-# SPECIALISE encodeState :: Alphabet String -> (Word -> Word16 ) -> NonEmpty String -> Word16  #-}
{-# SPECIALISE encodeState :: Alphabet String -> (Word -> Word32 ) -> NonEmpty String -> Word32  #-}
{-# SPECIALISE encodeState :: Alphabet String -> (Word -> Word64 ) -> NonEmpty String -> Word64  #-}
encodeState
    :: (Bits e, Foldable f, Ord s)
    => Alphabet s  -- ^ Alphabet of symbols
    -> (Word -> e) -- ^ Constructor for an empty element, taking the alphabet size
    -> f s         -- ^ Ambiguity group of symbols
    -> e           -- ^ Encoded state
encodeState alphabet f symbols = getSubsetIndex alphabet symbolsSet emptyElement
    where
        emptyElement = f . toEnum $ length alphabet
        symbolsSet   = Set.fromList $ toList symbols


{- |
$\mathcal{O}\left(\, \log_{2}\left(\Sigma\right) * n \,\right)$

Decode a bit-packed endcoding to a collection of symbols which is a unique
subset of the supplied 'Alphabet'.

/NOTE:/ This is the inverse of 'encodeState'.
-}
{-# INLINABLE decodeState #-}
{-# SPECIALISE decodeState :: Alphabet s -> CUChar  -> [s] #-}
{-# SPECIALISE decodeState :: Alphabet s -> CUShort -> [s] #-}
{-# SPECIALISE decodeState :: Alphabet s -> CUInt   -> [s] #-}
{-# SPECIALISE decodeState :: Alphabet s -> CULong  -> [s] #-}
{-# SPECIALISE decodeState :: Alphabet s -> Word    -> [s] #-}
{-# SPECIALISE decodeState :: Alphabet s -> Word8   -> [s] #-}
{-# SPECIALISE decodeState :: Alphabet s -> Word16  -> [s] #-}
{-# SPECIALISE decodeState :: Alphabet s -> Word32  -> [s] #-}
{-# SPECIALISE decodeState :: Alphabet s -> Word64  -> [s] #-}
{-# SPECIALISE decodeState :: Alphabet String -> CUChar  -> [String] #-}
{-# SPECIALISE decodeState :: Alphabet String -> CUShort -> [String] #-}
{-# SPECIALISE decodeState :: Alphabet String -> CUInt   -> [String] #-}
{-# SPECIALISE decodeState :: Alphabet String -> CULong  -> [String] #-}
{-# SPECIALISE decodeState :: Alphabet String -> Word    -> [String] #-}
{-# SPECIALISE decodeState :: Alphabet String -> Word8   -> [String] #-}
{-# SPECIALISE decodeState :: Alphabet String -> Word16  -> [String] #-}
{-# SPECIALISE decodeState :: Alphabet String -> Word32  -> [String] #-}
{-# SPECIALISE decodeState :: Alphabet String -> Word64  -> [String] #-}
{-# SPECIALISE decodeState :: Ord s => Alphabet s -> CUChar  -> Set s #-}
{-# SPECIALISE decodeState :: Ord s => Alphabet s -> CUShort -> Set s #-}
{-# SPECIALISE decodeState :: Ord s => Alphabet s -> CUInt   -> Set s #-}
{-# SPECIALISE decodeState :: Ord s => Alphabet s -> CULong  -> Set s #-}
{-# SPECIALISE decodeState :: Ord s => Alphabet s -> Word    -> Set s #-}
{-# SPECIALISE decodeState :: Ord s => Alphabet s -> Word8   -> Set s #-}
{-# SPECIALISE decodeState :: Ord s => Alphabet s -> Word16  -> Set s #-}
{-# SPECIALISE decodeState :: Ord s => Alphabet s -> Word32  -> Set s #-}
{-# SPECIALISE decodeState :: Ord s => Alphabet s -> Word64  -> Set s #-}
{-# SPECIALISE decodeState :: Alphabet String -> CUChar  -> Set String #-}
{-# SPECIALISE decodeState :: Alphabet String -> CUShort -> Set String #-}
{-# SPECIALISE decodeState :: Alphabet String -> CUInt   -> Set String #-}
{-# SPECIALISE decodeState :: Alphabet String -> CULong  -> Set String #-}
{-# SPECIALISE decodeState :: Alphabet String -> Word    -> Set String #-}
{-# SPECIALISE decodeState :: Alphabet String -> Word8   -> Set String #-}
{-# SPECIALISE decodeState :: Alphabet String -> Word16  -> Set String #-}
{-# SPECIALISE decodeState :: Alphabet String -> Word32  -> Set String #-}
{-# SPECIALISE decodeState :: Alphabet String -> Word64  -> Set String #-}
{-# SPECIALISE decodeState :: Alphabet s -> CUChar  -> NonEmpty s #-}
{-# SPECIALISE decodeState :: Alphabet s -> CUShort -> NonEmpty s #-}
{-# SPECIALISE decodeState :: Alphabet s -> CUInt   -> NonEmpty s #-}
{-# SPECIALISE decodeState :: Alphabet s -> CULong  -> NonEmpty s #-}
{-# SPECIALISE decodeState :: Alphabet s -> Word    -> NonEmpty s #-}
{-# SPECIALISE decodeState :: Alphabet s -> Word8   -> NonEmpty s #-}
{-# SPECIALISE decodeState :: Alphabet s -> Word16  -> NonEmpty s #-}
{-# SPECIALISE decodeState :: Alphabet s -> Word32  -> NonEmpty s #-}
{-# SPECIALISE decodeState :: Alphabet s -> Word64  -> NonEmpty s #-}
{-# SPECIALISE decodeState :: Alphabet String -> CUChar  -> NonEmpty String #-}
{-# SPECIALISE decodeState :: Alphabet String -> CUShort -> NonEmpty String #-}
{-# SPECIALISE decodeState :: Alphabet String -> CUInt   -> NonEmpty String #-}
{-# SPECIALISE decodeState :: Alphabet String -> CULong  -> NonEmpty String #-}
{-# SPECIALISE decodeState :: Alphabet String -> Word    -> NonEmpty String #-}
{-# SPECIALISE decodeState :: Alphabet String -> Word8   -> NonEmpty String #-}
{-# SPECIALISE decodeState :: Alphabet String -> Word16  -> NonEmpty String #-}
{-# SPECIALISE decodeState :: Alphabet String -> Word32  -> NonEmpty String #-}
{-# SPECIALISE decodeState :: Alphabet String -> Word64  -> NonEmpty String #-}
decodeState
    :: (Bits e, IsList (f s), Item (f s) ~ s)
    => Alphabet s  -- ^ Alphabet of symbols
    -> e           -- ^ State to decode
    -> f s
decodeState alphabet state = 
    let indices = [ 0 .. len - 1 ]
        len     = length vec
        vec     = alphabetSymbols alphabet
        pollSymbol i polled
            | state `testBit` i = (vec V.! i) : polled
            | otherwise         = polled
    in  fromList $ foldr pollSymbol mempty indices
