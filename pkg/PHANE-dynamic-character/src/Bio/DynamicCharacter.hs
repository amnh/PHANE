{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedTuples #-}

module Bio.DynamicCharacter (
    -- * Element Varieties of a Dynamic Character
    HugeState,

    -- * Generic Dynamic Character Constructions
    OpenDynamicCharacter,
    TempOpenDynamicCharacter,

    -- * Dynamic Character Immutable Varieties
    SlimDynamicCharacter,
    WideDynamicCharacter,
    HugeDynamicCharacter,

    -- * Dynamic Character Mutable Varieties
    TempSlimDynamicCharacter,
    TempWideDynamicCharacter,
    TempHugeDynamicCharacter,

    -- * Constructors

    -- ** Immutable Constructors
    encodeDynamicCharacter,
    generateCharacter,

    -- ** Mutable Constructors
    newTempCharacter,
    freezeTempCharacter,
    unsafeCharacterBuiltByST,
    unsafeCharacterBuiltByBufferedST,

    -- * Accessors
    extractMedians,
    extractMediansSingle,
    extractMediansLeft,
    extractMediansRight,
    extractMediansGapped,
    extractMediansLeftGapped,
    extractMediansRightGapped,
    removeGapAndNil,

    -- * Mutators
    setAlign,
    setDelete,
    setInsert,
    setGapped,
    setFrom,
    transposeCharacter,

    -- * Queries
    isAlign,
    isDelete,
    isInsert,
    isGapped,
    isGap,
    isMissing,
    characterLength,

    -- * Strictness
    forceDynamicCharacter,

    -- * Rendering
    renderDynamicCharacter,
) where

import Bio.DynamicCharacter.Element
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Alphabet
import Data.Alphabet.Codec
import Data.BitVector.LittleEndian
import Data.Bits
import Data.Foldable
import Data.Ord
import Data.STRef
import Data.Set (Set)
import Data.Vector qualified as RV
import Data.Vector.Generic (Mutable, Vector, unsafeFreeze, (!))
import Data.Vector.Generic qualified as GV
import Data.Vector.Generic.Mutable (MVector, unsafeNew, unsafeRead, unsafeWrite)
import Data.Vector.Generic.Mutable qualified as GMV (length)
import Data.Vector.Storable qualified as SV
import Data.Vector.Unboxed qualified as UV
import Measure.Unit.SymbolIndex


{- |
Encoding for a dynamic character element with an alphabet size of /65/ or greater.
-}
type HugeState = BitVector


{- |
This triple of vectors is the general structure of all dynamic character
representations. Each vector is equal length.

A triple of empty vectors represents a missing character. This can be queried
by 'isMissing'.

All triples are arranged thusly:

  * 1: "Left" character. When normalized, this will be the shorter character.
       Delete events leave a void in this character.

  * 2: "Median" character. The alignment of both the Left and Right characters.
       Align, Delete, & Insert do *not* leave a void. This vector will always
       contain states with one or more bits set, i.e. never contain voids.

  * 3: "Right" character. When normalized, this will be the longer character.
       Insert events leave a void in this character.
-}
type OpenDynamicCharacter v e = (v e, v e, v e)


{- |
Encoding for dynamic characters with an alphabet size of /8/ or less.
-}
type SlimDynamicCharacter = OpenDynamicCharacter SV.Vector SlimState


{- |
Encoding for dynamic characters with an alphabet size in the range [9, 64].
-}
type WideDynamicCharacter = OpenDynamicCharacter UV.Vector WideState


{- |
Encoding for dynamic characters with an alphabet size of /65/ or greater.
-}
type HugeDynamicCharacter = OpenDynamicCharacter RV.Vector HugeState


{- |
Generic representation of a /mutable/ dynamic character.
-}
type TempOpenDynamicCharacter m v e = OpenDynamicCharacter (Mutable v (PrimState m)) e


{- |
Mutable encoding of 'SlimDynamicCharacter'.
-}
type TempSlimDynamicCharacter m = TempOpenDynamicCharacter m SV.Vector SlimState


{- |
Mutable encoding of 'WideDynamicCharacter'.
-}
type TempWideDynamicCharacter m = TempOpenDynamicCharacter m UV.Vector WideState


{- |
Mutable encoding of 'HugeDynamicCharacter'.
-}
type TempHugeDynamicCharacter m = TempOpenDynamicCharacter m RV.Vector HugeState


isAlign, isDelete, isInsert, isGapped ∷ (FiniteBits e, Vector v e) ⇒ OpenDynamicCharacter v e → Int → Bool
{-# INLINEABLE isAlign #-}
{-# SPECIALIZE isAlign ∷ SlimDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isAlign ∷ WideDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isAlign ∷ HugeDynamicCharacter → Int → Bool #-}
{-# INLINEABLE isDelete #-}
{-# SPECIALIZE isDelete ∷ SlimDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isDelete ∷ WideDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isDelete ∷ HugeDynamicCharacter → Int → Bool #-}
{-# INLINEABLE isInsert #-}
{-# SPECIALIZE isInsert ∷ SlimDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isInsert ∷ WideDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isInsert ∷ HugeDynamicCharacter → Int → Bool #-}
{-# INLINEABLE isGapped #-}
{-# SPECIALIZE isGapped ∷ SlimDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isGapped ∷ WideDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isGapped ∷ HugeDynamicCharacter → Int → Bool #-}
isAlign (lc, _, rc) i = i < GV.length lc && popCount (lc ! i) /= 0 && popCount (rc ! i) /= 0
isDelete (lc, _, rc) i = i < GV.length lc && popCount (lc ! i) == 0 && popCount (rc ! i) /= 0
isInsert (lc, _, rc) i = i < GV.length lc && popCount (lc ! i) /= 0 && popCount (rc ! i) == 0
isGapped (lc, _, rc) i = i < GV.length lc && popCount (lc ! i) == 0 && popCount (rc ! i) == 0


{-# INLINEABLE isGap #-}
{-# SPECIALIZE isGap ∷ SlimDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isGap ∷ WideDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isGap ∷ HugeDynamicCharacter → Int → Bool #-}
isGap ∷ (Bits e, Vector v e) ⇒ OpenDynamicCharacter v e → Int → Bool
isGap (_, mc, _) i =
    i < GV.length mc
        && let val = mc ! i
               gap = buildGap val
           in  gap == val


{-# INLINEABLE isMissing #-}
{-# SPECIALIZE isMissing ∷ SlimDynamicCharacter → Bool #-}
{-# SPECIALIZE isMissing ∷ WideDynamicCharacter → Bool #-}
{-# SPECIALIZE isMissing ∷ HugeDynamicCharacter → Bool #-}
isMissing ∷ (Vector v e) ⇒ OpenDynamicCharacter v e → Bool
isMissing (x, y, z) = GV.length x == 0 && GV.length y == 0 && GV.length z == 0


{-# INLINEABLE setFrom #-}
{-# SPECIALIZE setFrom ∷ SlimDynamicCharacter → TempSlimDynamicCharacter (ST s) → Int → Int → ST s () #-}
{-# SPECIALIZE setFrom ∷ WideDynamicCharacter → TempWideDynamicCharacter (ST s) → Int → Int → ST s () #-}
{-# SPECIALIZE setFrom ∷ HugeDynamicCharacter → TempHugeDynamicCharacter (ST s) → Int → Int → ST s () #-}
setFrom
    ∷ ( PrimMonad m
      , Vector v e
      )
    ⇒ OpenDynamicCharacter v e
    -- ^ source
    → TempOpenDynamicCharacter m v e
    -- ^ destination
    → Int
    -- ^ Index to read from source
    → Int
    -- ^ Index to write to destination
    → m ()
setFrom (slc, smc, src) (dlc, dmc, drc) i j =
    unsafeWrite dlc j (slc ! i) *> unsafeWrite dmc j (smc ! i) *> unsafeWrite drc j (src ! i)


{-# INLINEABLE setAlign #-}
{-# SPECIALIZE setAlign ∷ TempSlimDynamicCharacter (ST s) → Int → SlimState → SlimState → SlimState → ST s () #-}
{-# SPECIALIZE setAlign ∷ TempWideDynamicCharacter (ST s) → Int → WideState → WideState → WideState → ST s () #-}
{-# SPECIALIZE setAlign ∷ TempHugeDynamicCharacter (ST s) → Int → HugeState → HugeState → HugeState → ST s () #-}
setAlign
    ∷ ( PrimMonad m
      , Vector v e
      )
    ⇒ TempOpenDynamicCharacter m v e
    → Int
    -- ^ Index to set
    → e
    -- ^ Aligned ``Left'' element
    → e
    -- ^ Median Element
    → e
    -- ^ Aligned ``Right'' Element
    → m ()
setAlign (lc, mc, rc) i le me re =
    unsafeWrite lc i le *> unsafeWrite mc i me *> unsafeWrite rc i re


{-# INLINEABLE setDelete #-}
{-# SPECIALIZE setDelete ∷ TempSlimDynamicCharacter (ST s) → Int → SlimState → SlimState → ST s () #-}
{-# SPECIALIZE setDelete ∷ TempWideDynamicCharacter (ST s) → Int → WideState → WideState → ST s () #-}
{-# SPECIALIZE setDelete ∷ TempHugeDynamicCharacter (ST s) → Int → HugeState → HugeState → ST s () #-}
setDelete
    ∷ ( Bits e
      , PrimMonad m
      , Vector v e
      )
    ⇒ TempOpenDynamicCharacter m v e
    -- ^ Modifiable character
    → Int
    -- ^ Index to set
    → e
    -- ^ Deleted ``Right'' element
    → e
    -- ^ Median Element
    → m ()
setDelete (lc, mc, rc) i me re =
    unsafeWrite lc i (me `xor` me) *> unsafeWrite mc i me *> unsafeWrite rc i re


{-# INLINEABLE setInsert #-}
{-# SPECIALIZE setInsert ∷ TempSlimDynamicCharacter (ST s) → Int → SlimState → SlimState → ST s () #-}
{-# SPECIALIZE setInsert ∷ TempWideDynamicCharacter (ST s) → Int → WideState → WideState → ST s () #-}
{-# SPECIALIZE setInsert ∷ TempHugeDynamicCharacter (ST s) → Int → HugeState → HugeState → ST s () #-}
setInsert
    ∷ ( Bits e
      , PrimMonad m
      , Vector v e
      )
    ⇒ TempOpenDynamicCharacter m v e
    -- ^ Modifiable character
    → Int
    -- ^ Index to set
    → e
    -- ^ Median Element
    → e
    -- ^ Inserted ``Left'' element
    → m ()
setInsert (lc, mc, rc) i le me =
    unsafeWrite lc i le *> unsafeWrite mc i me *> unsafeWrite rc i (me `xor` me)


{-# INLINEABLE setGapped #-}
{-# SPECIALIZE setGapped ∷ TempSlimDynamicCharacter (ST s) → Int → ST s () #-}
{-# SPECIALIZE setGapped ∷ TempWideDynamicCharacter (ST s) → Int → ST s () #-}
{-# SPECIALIZE setGapped ∷ TempHugeDynamicCharacter (ST s) → Int → ST s () #-}
setGapped
    ∷ ( Bits e
      , PrimMonad m
      , Vector v e
      )
    ⇒ TempOpenDynamicCharacter m v e
    -- ^ Modifiable character
    → Int
    -- ^ Index to set
    → m ()
setGapped (lc, mc, rc) i = do
    tmp ← unsafeRead mc 0 -- Get cell dimension context from first cell in vector
    let (# gap, nil #) = buildGapAndNil tmp
    unsafeWrite lc i nil
    unsafeWrite mc i gap
    unsafeWrite rc i nil


{-# INLINEABLE transposeCharacter #-}
transposeCharacter ∷ OpenDynamicCharacter v e → OpenDynamicCharacter v e
transposeCharacter (lc, mc, rc) = (rc, mc, lc)


{-# INLINEABLE characterLength #-}
characterLength ∷ (Vector v e) ⇒ OpenDynamicCharacter v e → Word
characterLength = toEnum . GV.length . extractMediansGapped


{- |
Utility function to enforce strict evaluation of the dynmaic character.
-}
{-# INLINEABLE forceDynamicCharacter #-}
{-# SPECIALIZE forceDynamicCharacter ∷ SlimDynamicCharacter → SlimDynamicCharacter #-}
{-# SPECIALIZE forceDynamicCharacter ∷ WideDynamicCharacter → WideDynamicCharacter #-}
{-# SPECIALIZE forceDynamicCharacter ∷ HugeDynamicCharacter → HugeDynamicCharacter #-}
forceDynamicCharacter ∷ (Vector v e) ⇒ OpenDynamicCharacter v e → OpenDynamicCharacter v e
forceDynamicCharacter (lv, mv, rv) =
    let -- Force the individual vectors
        lv' = GV.force lv
        mv' = GV.force mv
        rv' = GV.force rv
    in  -- Then force the tuple containing the vectors
        lv' `seq` mv' `seq` rv' `seq` (lv', mv', rv')


{- |
Extract the /ungapped/ medians of a dynamic character.
-}
{-# INLINEABLE extractMedians #-}
{-# SPECIALIZE extractMedians ∷ SlimDynamicCharacter → SV.Vector SlimState #-}
{-# SPECIALIZE extractMedians ∷ WideDynamicCharacter → UV.Vector WideState #-}
{-# SPECIALIZE extractMedians ∷ HugeDynamicCharacter → RV.Vector HugeState #-}
extractMedians ∷ (FiniteBits e, Vector v e) ⇒ OpenDynamicCharacter v e → v e
extractMedians (_, me, _)
    | GV.null me = me
    | otherwise =
        let gap = buildGap $ me ! 0
        in  GV.filter (/= gap) me


{- |
Extract the /ungapped/ medians of a single field of a dynamic character.
-}
{-# INLINEABLE extractMediansSingle #-}
{-# SPECIALIZE extractMediansSingle ∷ SV.Vector SlimState → SV.Vector SlimState #-}
{-# SPECIALIZE extractMediansSingle ∷ UV.Vector WideState → UV.Vector WideState #-}
{-# SPECIALIZE extractMediansSingle ∷ RV.Vector HugeState → RV.Vector HugeState #-}
extractMediansSingle ∷ (FiniteBits e, Vector v e) ⇒ v e → v e
extractMediansSingle me
    | GV.null me = me
    | otherwise =
        let gap = buildGap $ me ! 0
        in  GV.filter (/= gap) me


{- |
Extract the left child's /ungapped/ medians used to construct the dynamic character.
-}
{-# INLINEABLE extractMediansLeft #-}
{-# SPECIALIZE extractMediansLeft ∷ SlimDynamicCharacter → SV.Vector SlimState #-}
{-# SPECIALIZE extractMediansLeft ∷ WideDynamicCharacter → UV.Vector WideState #-}
{-# SPECIALIZE extractMediansLeft ∷ HugeDynamicCharacter → RV.Vector HugeState #-}
extractMediansLeft ∷ (FiniteBits e, Vector v e) ⇒ OpenDynamicCharacter v e → v e
extractMediansLeft (lc, _, _)
    | GV.null lc = lc
    | otherwise =
        let nil = buildNil $ lc ! 0
        in  GV.filter (/= nil) lc


{- |
Extract the right child's /ungapped/ medians used to construct the dynamic character.
-}
{-# INLINEABLE extractMediansRight #-}
{-# SPECIALIZE extractMediansRight ∷ SlimDynamicCharacter → SV.Vector SlimState #-}
{-# SPECIALIZE extractMediansRight ∷ WideDynamicCharacter → UV.Vector WideState #-}
{-# SPECIALIZE extractMediansRight ∷ HugeDynamicCharacter → RV.Vector HugeState #-}
extractMediansRight ∷ (FiniteBits e, Vector v e) ⇒ OpenDynamicCharacter v e → v e
extractMediansRight (_, _, rc)
    | GV.null rc = rc
    | otherwise =
        let nil = buildNil $ rc ! 0
        in  GV.filter (/= nil) rc


{- |
Extract the /gapped/ medians of a dynamic character.
-}
{-# INLINEABLE extractMediansGapped #-}
extractMediansGapped ∷ OpenDynamicCharacter v e → v e
extractMediansGapped (_, me, _) = me


{- |
Extract the left child's /gapped/ medians used to construct the dynamic character.
-}
{-# INLINEABLE extractMediansLeftGapped #-}
extractMediansLeftGapped ∷ OpenDynamicCharacter v e → v e
extractMediansLeftGapped (lc, _, _) = lc


{- |
Extract the right child's /gapped/ medians used to construct the dynamic character.
-}
{-# INLINEABLE extractMediansRightGapped #-}
extractMediansRightGapped ∷ OpenDynamicCharacter v e → v e
extractMediansRightGapped (_, _, rc) = rc


{- |
Extract the  /ungapped/ medians used to construct the dynamic character.

Output medians will /not/ contain "gap" or "nil" states
-}
{-# INLINEABLE removeGapAndNil #-}
{-# SPECIALIZE removeGapAndNil ∷ SV.Vector SlimState → SV.Vector SlimState #-}
{-# SPECIALIZE removeGapAndNil ∷ UV.Vector WideState → UV.Vector WideState #-}
{-# SPECIALIZE removeGapAndNil ∷ RV.Vector HugeState → RV.Vector HugeState #-}
removeGapAndNil ∷ (FiniteBits e, Vector v e) ⇒ v e → v e
removeGapAndNil rc
    | GV.null rc = rc
    | otherwise =
        let (# gap, nil #) = buildGapAndNil $ rc ! 0
        in  GV.filter (\e → e /= gap && e /= nil) rc


{-# INLINEABLE encodeDynamicCharacter #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Foldable f, Foldable g, Ord s) ⇒ Alphabet s → (Word → SlimState) → f (g s) → SlimDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Foldable f, Foldable g, Ord s) ⇒ Alphabet s → (Word → WideState) → f (g s) → WideDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Foldable f, Foldable g, Ord s) ⇒ Alphabet s → (Word → HugeState) → f (g s) → HugeDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Foldable f, Ord s) ⇒ Alphabet s → (Word → SlimState) → f (Set s) → SlimDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Foldable f, Ord s) ⇒ Alphabet s → (Word → WideState) → f (Set s) → WideDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Foldable f, Ord s) ⇒ Alphabet s → (Word → HugeState) → f (Set s) → HugeDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Foldable f) ⇒ Alphabet String → (Word → SlimState) → f (Set String) → SlimDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Foldable f) ⇒ Alphabet String → (Word → WideState) → f (Set String) → WideDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Foldable f) ⇒ Alphabet String → (Word → HugeState) → f (Set String) → HugeDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Ord s) ⇒ Alphabet s → (Word → SlimState) → RV.Vector (Set s) → SlimDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Ord s) ⇒ Alphabet s → (Word → WideState) → RV.Vector (Set s) → WideDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Ord s) ⇒ Alphabet s → (Word → HugeState) → RV.Vector (Set s) → HugeDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    Alphabet String → (Word → SlimState) → RV.Vector (Set String) → SlimDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    Alphabet String → (Word → WideState) → RV.Vector (Set String) → WideDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    Alphabet String → (Word → HugeState) → RV.Vector (Set String) → HugeDynamicCharacter
    #-}
encodeDynamicCharacter
    ∷ ( Bits e
      , Foldable f
      , Foldable g
      , Ord s
      , Vector v e
      )
    ⇒ Alphabet s
    -- ^ Alphabet of symbols
    → (Word → e)
    -- ^ Constructor for an empty element, taking the alphabet size
    → f (g s)
    -- ^ Sequence of ambiguity groups of symbols
    → OpenDynamicCharacter v e
    -- ^ Encoded dynamic character
encodeDynamicCharacter alphabet f sequenceOfSymbols = dynamicCharater
    where
        len = length sequenceOfSymbols

        dynamicCharater = unsafeCharacterBuiltByST (toEnum len) $ \char → do
            iRef ← newSTRef 0

            let writeElement symbols =
                    let encodedVal = encodeState alphabet f symbols
                    in  do
                            i ← readSTRef iRef
                            setAlign char i encodedVal encodedVal encodedVal
                            modifySTRef iRef succ

            traverse_ writeElement sequenceOfSymbols


{-# INLINEABLE newTempCharacter #-}
{-# SPECIALIZE newTempCharacter ∷ Word → ST s (TempSlimDynamicCharacter (ST s)) #-}
{-# SPECIALIZE newTempCharacter ∷ Word → ST s (TempWideDynamicCharacter (ST s)) #-}
{-# SPECIALIZE newTempCharacter ∷ Word → ST s (TempHugeDynamicCharacter (ST s)) #-}
newTempCharacter
    ∷ ( Vector v e
      , PrimMonad m
      )
    ⇒ Word
    → m (TempOpenDynamicCharacter m v e)
newTempCharacter n =
    let i = fromEnum n
    in  (,,) <$> unsafeNew i <*> unsafeNew i <*> unsafeNew i


{-# INLINEABLE freezeTempCharacter #-}
{-# SPECIALIZE freezeTempCharacter ∷ TempSlimDynamicCharacter (ST s) → ST s SlimDynamicCharacter #-}
{-# SPECIALIZE freezeTempCharacter ∷ TempWideDynamicCharacter (ST s) → ST s WideDynamicCharacter #-}
{-# SPECIALIZE freezeTempCharacter ∷ TempHugeDynamicCharacter (ST s) → ST s HugeDynamicCharacter #-}
freezeTempCharacter
    ∷ ( PrimMonad m
      , Vector v e
      )
    ⇒ TempOpenDynamicCharacter m v e
    → m (OpenDynamicCharacter v e)
freezeTempCharacter (lc, mc, rc) =
    (,,) <$> unsafeFreeze lc <*> unsafeFreeze mc <*> unsafeFreeze rc


{-# INLINEABLE generateCharacter #-}
{-# SPECIALIZE generateCharacter ∷ Word → (Word → (# SlimState, SlimState, SlimState #)) → SlimDynamicCharacter #-}
{-# SPECIALIZE generateCharacter ∷ Word → (Word → (# WideState, WideState, WideState #)) → WideDynamicCharacter #-}
{-# SPECIALIZE generateCharacter ∷ Word → (Word → (# HugeState, HugeState, HugeState #)) → HugeDynamicCharacter #-}
generateCharacter
    ∷ (Vector v e)
    ⇒ Word
    → (Word → (# e, e, e #))
    → OpenDynamicCharacter v e
generateCharacter n f = runST $ do
    char ← newTempCharacter n
    forM_ [0 .. n - 1] $ \i →
        let (# x, y, z #) = f i
        in  setAlign char (fromEnum i) x y z
    freezeTempCharacter char


{-# INLINEABLE unsafeCharacterBuiltByST #-}
{-# SPECIALIZE unsafeCharacterBuiltByST ∷
    Word → (∀ s. TempSlimDynamicCharacter (ST s) → ST s ()) → SlimDynamicCharacter
    #-}
{-# SPECIALIZE unsafeCharacterBuiltByST ∷
    Word → (∀ s. TempWideDynamicCharacter (ST s) → ST s ()) → WideDynamicCharacter
    #-}
{-# SPECIALIZE unsafeCharacterBuiltByST ∷
    Word → (∀ s. TempHugeDynamicCharacter (ST s) → ST s ()) → HugeDynamicCharacter
    #-}
unsafeCharacterBuiltByST
    ∷ (Vector v e)
    ⇒ Word
    → (∀ s. TempOpenDynamicCharacter (ST s) v e → ST s ())
    → OpenDynamicCharacter v e
unsafeCharacterBuiltByST n f = runST $ do
    char ← newTempCharacter n
    f char
    freezeTempCharacter char


{- |
Allocated a buffer of specified size.
Uses function to generate character and return the final size.
Copies character from buffer to character of final size.
-}
{-# INLINEABLE unsafeCharacterBuiltByBufferedST #-}
{-# SPECIALIZE unsafeCharacterBuiltByBufferedST ∷
    Word → (∀ s. TempSlimDynamicCharacter (ST s) → ST s Word) → SlimDynamicCharacter
    #-}
{-# SPECIALIZE unsafeCharacterBuiltByBufferedST ∷
    Word → (∀ s. TempWideDynamicCharacter (ST s) → ST s Word) → WideDynamicCharacter
    #-}
{-# SPECIALIZE unsafeCharacterBuiltByBufferedST ∷
    Word → (∀ s. TempHugeDynamicCharacter (ST s) → ST s Word) → HugeDynamicCharacter
    #-}
unsafeCharacterBuiltByBufferedST
    ∷ (Vector v e)
    ⇒ Word
    -- ^ Buffer length
    → (∀ s. TempOpenDynamicCharacter (ST s) v e → ST s Word)
    → OpenDynamicCharacter v e
unsafeCharacterBuiltByBufferedST b f = runST $ do
    buff ← newTempCharacter b
    char ← f buff >>= newTempCharacter
    copyCharacter buff char
    freezeTempCharacter char
    where
        copyCharacter
            ∷ ( PrimMonad m
              , MVector v1 a1
              , MVector v2 a1
              , MVector v3 a2
              , MVector v4 a2
              , MVector v5 a3
              , MVector v6 a3
              )
            ⇒ (v2 (PrimState m) a1, v3 (PrimState m) a2, v5 (PrimState m) a3)
            → (v1 (PrimState m) a1, v4 (PrimState m) a2, v6 (PrimState m) a3)
            → m ()
        copyCharacter src@(x, _, _) des@(y, _, _) =
            let m = GMV.length x
                n = GMV.length y
                o = m - n
            in  forM_ [0 .. n - 1] $ copyAt src des o

        copyAt
            ∷ ( PrimMonad m
              , MVector v1 a1
              , MVector v2 a1
              , MVector v3 a2
              , MVector v4 a2
              , MVector v5 a3
              , MVector v6 a3
              )
            ⇒ (v2 (PrimState m) a1, v3 (PrimState m) a2, v5 (PrimState m) a3)
            → (v1 (PrimState m) a1, v4 (PrimState m) a2, v6 (PrimState m) a3)
            → Int
            → Int
            → m ()
        copyAt (slc, smc, src) (dlc, dmc, drc) o i =
            let i' = o + i
            in  do
                    unsafeRead slc i' >>= unsafeWrite dlc i
                    unsafeRead smc i' >>= unsafeWrite dmc i
                    unsafeRead src i' >>= unsafeWrite drc i


renderDynamicCharacter
    ∷ ( FiniteBits e
      , Show e
      , Vector v e
      )
    ⇒ OpenDynamicCharacter v e
    → String
renderDynamicCharacter (lc, mc, rc) =
    unlines
        [ "Character Length: " <> show (GV.length mc)
        , printVector lcStr
        , printVector mcStr
        , printVector rcStr
        ]
    where
        show' ∷ (Bits a, Show a) ⇒ a → String
        show' x
            | popCount x > 0 = show x
            | otherwise = [voidC]

        voidC = '█'
        lcStr = show' <$> GV.toList lc
        mcStr = show' <$> GV.toList mc
        rcStr = show' <$> GV.toList rc
        eSize = length . maximumBy (comparing length) $ lcStr <> mcStr <> rcStr <> [[voidC]]
        pad s =
            let c
                    | s == [voidC] = voidC
                    | otherwise = ' '
            in  replicate (eSize - length s) c <> s

        intercalate' [] = []
        intercalate' [x] = x
        intercalate' (x : xs) =
            let sep = case x of
                    e : _ | e == voidC → [voidC]
                    _ → " "
            in  x <> sep <> intercalate' xs

        printVector vec = "[ " <> intercalate' (pad <$> vec) <> " ]"


buildGap ∷ (Bits e) ⇒ e → e
buildGap e = buildNil e `setBit` atSymbolIndex gapIndex


buildNil ∷ (Bits e) ⇒ e → e
buildNil e = e `xor` e


buildGapAndNil ∷ (Bits e) ⇒ e → (# e, e #)
buildGapAndNil e =
    let nil = buildNil e
        gap = nil `setBit` atSymbolIndex gapIndex
    in  (# gap, nil #)
