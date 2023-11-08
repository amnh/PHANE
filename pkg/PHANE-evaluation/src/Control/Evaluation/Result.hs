{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

{- |
The core semigroupoid state of an 'Control.Evaluation.Evaluation' monad.
-}
module Control.Evaluation.Result (
    EvaluationResult (..),
    evalUnitWithPhase,
) where

import Control.Applicative (Alternative (..))
import Control.DeepSeq
import Control.Monad.Fix (MonadFix)
import Control.Monad.Zip (MonadZip (..))
import Data.Functor.Classes (Eq1, Ord1 (..), Show1)
import Data.Semigroup (Semigroup (..))
import Data.String (IsString (..))
import GHC.Generics
import System.ErrorPhase
import System.Log.FastLogger (LogStr, ToLogStr (..), fromLogStr)
import Test.QuickCheck
import Test.QuickCheck.Instances ()


{- |
The internal state of the computation. A short-circuiting evaluation unit
which returns either an error that occurred preventing the evaluation for being
completed or a value of the evaluation or

In the case that an error occurred, an 'ErrorPhase' is stored along with a
'LogStr' value describing the error.

Note that multiple errors can be aggregated before calling 'fail' or
'evalUnitWithPhase' using another 'Applicative' or 'Monad' locally. We will
use the @Validation@ type to collect many error of the same "phase" before
failing in the 'Control.Evaluation' monad. Consequently, the textual error message can
be quite long, representing the entire list of aggregated failures. We use
'LogStr' instead of 'String' to store the error message to save space and
efficient rendering.
-}
newtype EvaluationResult a = EU {runEvaluationResult ∷ Either (ErrorPhase, LogStr) a}
    deriving stock (Eq, Foldable, Generic, Generic1, Show, Traversable)
    deriving newtype (Applicative, Eq1, Functor, MonadFix, Show1)


type role EvaluationResult representational


instance Alternative EvaluationResult where
    {-# INLINEABLE (<|>) #-}
    lhs <|> rhs =
        case runEvaluationResult lhs of
            Right _ → lhs
            _ → rhs


    empty = fail "Alternative identity (empty)"


instance (Arbitrary a) ⇒ Arbitrary (EvaluationResult a) where
    {-# INLINE arbitrary #-}
    arbitrary = liftArbitrary arbitrary


instance Arbitrary1 EvaluationResult where
    {-# INLINE liftArbitrary #-}
    liftArbitrary g = do
        n ← choose (0, 9) ∷ Gen Word -- 1/10 chance of 'error' value
        case n of
            0 → (`evalUnitWithPhase` errorMessage) <$> arbitrary
            _ → pure <$> g
        where
            errorMessage ∷ LogStr
            errorMessage = "Error Description"


instance (CoArbitrary a) ⇒ CoArbitrary (EvaluationResult a) where
    {-# INLINE coarbitrary #-}
    coarbitrary (EU e) = case e of
        Left (p, txt) → variant 0 . coarbitrary (p, fromLogStr txt)
        Right res → variant 1 . coarbitrary res


instance Monad EvaluationResult where
    {-# INLINEABLE (>>=) #-}
    {-# INLINE (>>) #-}
    {-# INLINE return #-}


    (>>=) e f =
        case runEvaluationResult e of
            Left l → EU . Left $ l
            Right v → f v


    (>>) = (*>)


    return = pure


instance MonadFail EvaluationResult where
    {-# INLINE fail #-}
    fail =
        EU
            . Left
            . (\x → (Computing, x))
            . \case
                [] → "Unspecified error."
                x : xs → fromString $ x : xs


instance MonadZip EvaluationResult where
    {-# INLINEABLE mzip #-}
    {-# INLINEABLE munzip #-}
    {-# INLINE mzipWith #-}


    mzip = liftA2 (,)


    mzipWith = liftA2


    munzip x =
        case runEvaluationResult x of
            Left s → (EU $ Left s, EU $ Left s)
            Right (a, b) → (pure a, pure b)


instance (NFData a) ⇒ NFData (EvaluationResult a) where
    rnf (EU (Left (e, s))) = e `seq` s `seq` ()
    rnf (EU (Right value)) = rnf value


instance (Ord a) ⇒ Ord (EvaluationResult a) where
    {-# INLINE compare #-}
    compare = liftCompare compare


instance Ord1 EvaluationResult where
    {-# INLINE liftCompare #-}
    liftCompare cmp lhs rhs =
        case (runEvaluationResult lhs, runEvaluationResult rhs) of
            (Left x, Left y) → fmap show x `compare` fmap show y
            (Left _, Right _) → GT
            (Right _, Left _) → LT
            (Right x, Right y) → x `cmp` y


instance Semigroup (EvaluationResult a) where
    lhs <> rhs =
        case lhs of
            EU (Left _) → lhs
            _ → rhs


    stimes _ e = e


{- |
Create a failure result with a specified 'ErrorPhase'.

Use in place of 'fail' when you want to the associated 'ErrorPhase' to be a value other than 'Computing'.
-}
{-# INLINE [1] evalUnitWithPhase #-}
evalUnitWithPhase ∷ (ToLogStr s) ⇒ ErrorPhase → s → EvaluationResult a
evalUnitWithPhase p s = EU $ Left (p, toLogStr s)


{-# RULES
"evalUnitWithPhase/Text" ∀ p (s ∷ LogStr). evalUnitWithPhase p s = EU $ Left (p, s)
    #-}