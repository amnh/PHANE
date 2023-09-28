{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Concrete encodings for the elements of a dynamic character.

There are the three encodings base on the size of the alphabet \( \Sigma \) which
corresponds to the dynamic character:

  * If \( \mathtt{~~0} < \lvert\, \Sigma \,\rvert \le \mathtt{~~8} \mapsto \) 'SlimState'

  * If \( \mathtt{~~8} < \lvert\, \Sigma \,\rvert \le \mathtt{64}  \mapsto \) 'WideState'

  * If \( \mathtt{64}  < \lvert\, \Sigma \,\rvert \le \infty       \mapsto \) @BitVector@

Each dynamic character element encoding has carefull selected optimization of the
associated representations, measures, and scoring functions.
-}
module Bio.DynamicCharacter.Element (
    -- * Element Varieties of a Dynamic Character
    SlimState (..),
    WideState (..),
) where

import Bio.DynamicCharacter.Element.SlimState
import Bio.DynamicCharacter.Element.WideState

