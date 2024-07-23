{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedTuples #-}

{- |
Dynamic characters are character which constitute a variable length sequence of states instead of a single state having fixed width.
Consequently, the states contained within two dynamic characters have a /dynamic/ relationship between each other depending on how they are aligned.
This module provides the functionality to construct, query, and modify dynamic characters.
-}
module Bio.DynamicCharacter.Rendering (
    -- * General
    renderDynamicCharacter,

    -- * Slim specialization
    renderSlimCharacter,
) where

import Bio.DynamicCharacter
import Bio.DynamicCharacter.Element (renderSlimStateChar)
import Data.Bits
import Data.Foldable
import Data.List qualified as List
import Data.Ord
import Data.Vector.Generic (Vector)
import Data.Vector.Generic qualified as GV


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


renderSlimCharacter ∷ SlimDynamicCharacter → String
renderSlimCharacter char =
    let len = characterLength char

        slimChar = renderSlimStateChar

        renderIndex ∷ Int → (Char, Char, Char)
        renderIndex i = case char `getContext` i of
            Gapped → ('█', '█', '█')
            Delete med rhs → ('█', slimChar med, slimChar rhs)
            Insert lhs med → (slimChar lhs, slimChar med, '█')
            Merged lhs med rhs → (slimChar lhs, slimChar med, slimChar rhs)

        (lhsStr, medStr, rhsStr) = unzip3 $ renderIndex <$> [0 .. fromEnum len - 1]
    in  List.intercalate
            "\n"
            [ "Length: " <> show len
            , "Lesser: " <> lhsStr
            , "Median: " <> medStr
            , "Longer: " <> rhsStr
            ]
