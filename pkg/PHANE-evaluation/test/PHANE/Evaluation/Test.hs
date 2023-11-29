{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{- HLINT ignore "Monad law, left identity" -}
{- HLINT ignore "Monad law, right identity" -}
{- HLINT ignore "Functor law" -}
{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Use >=>" -}
{- HLINT ignore "Use =<<" -}
{- HLINT ignore "Use /=" -}
{- HLINT ignore "Use null" -}

{- |
Tests for the Evaluation type.
-}
module PHANE.Evaluation.Test (
    testSuite,
) where

import Control.Applicative (Alternative (..))
import Control.Arrow ((***))
import Control.DeepSeq
import Control.Monad (join, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Zip (MonadZip (..))
import Data.Bifunctor (bimap, first)
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Semigroup
import PHANE.Evaluation
import PHANE.Evaluation.Result
import PHANE.Evaluation.Verbosity (Verbosity (..))
import Test.QuickCheck.Exception (tryEvaluateIO)
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic (PropertyM, monadicIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck hiding ((=/=))


{- |
This alias exists for brevity in type signatures
-}
type W = Word


{- |
This alias exists for brevity in type signatures
-}
type ENV = ()


{- |
This alias exists for brevity in type signatures
-}
type RunEval0 = ENV → Evaluation ENV W → IO W


type RunEval1 = ENV → Evaluation ENV (Evaluation ENV W) → IO (Evaluation ENV W)


type RunEval2 = ENV → Evaluation ENV (W, W) → IO (W, W)


type RunEval3 = ENV → Evaluation ENV () → IO ()


type RunEvals = (RunEval0, RunEval1, RunEval2, RunEval3)


{- |
A way to evaluate if two 'Evaluation' results are the same.
-}
(====) ∷ (Eq a, Show a) ⇒ IO a → IO a → Property
(====) lhs rhs =
    let check ∷ IO c → PropertyM IO (Either String c)
        check = liftIO . fmap (first show) . tryEvaluateIO
    in  monadicIO $ do
            x ← check lhs
            y ← check rhs
            pure $ x === y


{- |
A way to evaluate if two 'Evaluation' results are the same.
-}
(<==>) ∷ (Eq a, Show a) ⇒ (IO a, IO a) → (IO a, IO a) → Property
(<==>) lhs rhs =
    let check ∷ IO c → PropertyM IO (Either String c)
        check = liftIO . fmap (first show) . tryEvaluateIO

        checkTuple ∷ (IO c, IO c) → PropertyM IO (Either String c, Either String c)
        checkTuple (x, y) = do
            x' ← check x
            y' ← check y
            pure (x', y')
    in  monadicIO $ do
            x ← checkTuple lhs
            y ← checkTuple rhs
            pure $ x === y


{- |
A way to evaluate if two 'Evaluation' results are the same.
-}
(=//=) ∷ (Eq a, Show a) ⇒ IO a → IO a → Property
(=//=) lhs rhs =
    let check ∷ IO c → PropertyM IO (Either String c)
        check = liftIO . fmap (first show) . tryEvaluateIO
    in  monadicIO $ do
            x ← check lhs
            y ← check rhs
            pure $ x =/= y


{- |
Test suite for the 'EvaluationT' data-type.
-}
testSuite ∷ IO TestTree
testSuite = do
    logs ← initializeLogging None None Nothing
    seed ← initializeRandomSeed
    let evaluators =
            ( runEvaluation logs seed ()
            , runEvaluation logs seed ()
            , runEvaluation logs seed ()
            , runEvaluation logs seed ()
            )
                ∷ RunEvals
    pure $
        testGroup
            "Computational Evaluation Monad"
            [ evaluationResultLaws
            , evaluationTLaws evaluators
            ]


evaluationResultLaws ∷ TestTree
evaluationResultLaws =
    testGroup
        "EvaluationResult"
        -- Basic control structures
        [ functorLaws @EvaluationResult
        , applicativeLaws @EvaluationResult
        , monadLaws @EvaluationResult
        , -- Extended control structures
          monadFailLaws @EvaluationResult
        , monadZipLaws @EvaluationResult
        , -- Refined control structures
          altLaws @EvaluationResult
        , applyLaws @EvaluationResult
        , bindLaws @EvaluationResult
        , -- Ordered container structures
          foldableLaws @EvaluationResult
        , traversableLaws @EvaluationResult
        , -- Data structures
          equalityLaws @(EvaluationResult W)
        , normalFormDataLaws @(EvaluationResult W)
        , orderingLaws @(EvaluationResult W)
        , semigroupLaws @(EvaluationResult W)
        , showProperties @(EvaluationResult W)
        ]


evaluationTLaws ∷ RunEvals → TestTree
evaluationTLaws evaluators =
    testGroup "EvaluationT" $
        -- Basic control structures
        [ functorLaws'
        , applicativeLaws'
        , monadLaws'
        , -- Extended control structures
          monadFailLaws'
        , -- , monadLoggerLaws'
          monadZipLaws'
        , -- Refined control structures
          altLaws'
        , applyLaws'
        , bindLaws'
        , -- Data structures
          semigroupLaws'
        ]
            <*> [evaluators]


functorLaws
    ∷ ∀ f
     . ( Arbitrary (f W)
       , Eq (f W)
       , Functor f
       , Show (f W)
       )
    ⇒ TestTree
functorLaws =
    testGroup
        "Functor Laws"
        [ testLaw functorIdentity "Identity" "fmap id === id"
        , testLaw functorComposition "Composition" "fmap (f . g) === fmap f . fmap g"
        ]
    where
        functorIdentity ∷ f W → Property
        functorIdentity x =
            fmap id x === x

        functorComposition ∷ f W → Fun W W → Fun W W → Property
        functorComposition x (apply → f) (apply → g) =
            fmap (g . f) x === (fmap g . fmap f $ x)


functorLaws'
    ∷ RunEvals → TestTree
functorLaws' (runEvaluation', _, _, _) =
    testGroup
        "Functor Laws"
        [ testLaw functorIdentity "Identity" "fmap id === id"
        , testLaw functorComposition "Composition" "fmap (f . g) === fmap f . fmap g"
        ]
    where
        functorIdentity ∷ ENV → Blind (Evaluation ENV W) → Property
        functorIdentity w (Blind x) =
            runEvaluation' w (fmap id x) ==== runEvaluation' w x

        functorComposition ∷ ENV → Blind (Evaluation ENV W) → Fun W W → Fun W W → Property
        functorComposition w (Blind x) (apply → f) (apply → g) =
            runEvaluation' w (fmap (g . f) x) ==== runEvaluation' w ((fmap g . fmap f) x)


applicativeLaws
    ∷ ∀ f
     . ( Applicative f
       , Arbitrary (f W)
       , Arbitrary (f (Fun W W))
       , Eq (f W)
       , Show (f W)
       , Show (f (Fun W W))
       )
    ⇒ TestTree
applicativeLaws =
    testGroup
        "Applicative Laws"
        [ testLaw applicativeIdentity "Identity" "pure id <*> v === v"
        , testLaw applicativeComposition "Composition" "pure (.) <*> u <*> v <*> w === u <*> (v <*> w)"
        , testLaw applicativeHomomorphism "Homomorphism" "pure f <*> pure x = pure (f x)"
        , testLaw applicativeInterchange "Interchange" "u <*> pure y === pure ($ y) <*> u"
        ]
    where
        applicativeIdentity ∷ f W → Property
        applicativeIdentity x =
            (pure id <*> x) === x

        applicativeComposition ∷ f (Fun W W) → f (Fun W W) → f W → Property
        applicativeComposition (fmap apply → x) (fmap apply → y) z =
            (pure (.) <*> x <*> y <*> z) === (x <*> (y <*> z))

        applicativeInterchange ∷ f (Fun W W) → W → Property
        applicativeInterchange (fmap apply → x) y =
            (x <*> pure y) === (pure ($ y) <*> x)

        applicativeHomomorphism ∷ Fun W W → W → Property
        applicativeHomomorphism (apply → f) x =
            (pure f <*> pure x) === (pure (f x) ∷ f W)


applicativeLaws' ∷ RunEvals → TestTree
applicativeLaws' (runEvaluation', _, _, _) =
    testGroup
        "Applicative Laws"
        [ testLaw applicativeIdentity "Identity" "pure id <*> v === v"
        , testLaw applicativeComposition "Composition" "pure (.) <*> u <*> v <*> w === u <*> (v <*> w)"
        , testLaw applicativeHomomorphism "Homomorphism" "pure f <*> pure x = pure (f x)"
        , testLaw applicativeInterchange "Interchange" "u <*> pure y === pure ($ y) <*> u"
        ]
    where
        applicativeIdentity ∷ ENV → Blind (Evaluation ENV W) → Property
        applicativeIdentity w (Blind x) =
            runEvaluation' w (pure id <*> x) ==== runEvaluation' w x

        applicativeComposition
            ∷ ENV → Blind (Evaluation ENV (Fun W W)) → Blind (Evaluation ENV (Fun W W)) → Blind (Evaluation ENV W) → Property
        applicativeComposition w (Blind (fmap apply → x)) (Blind (fmap apply → y)) (Blind z) =
            runEvaluation' w (pure (.) <*> x <*> y <*> z) ==== runEvaluation' w (x <*> (y <*> z))

        applicativeInterchange ∷ ENV → Blind (Evaluation ENV (Fun W W)) → W → Property
        applicativeInterchange w (Blind (fmap apply → x)) y =
            runEvaluation' w (x <*> pure y) ==== runEvaluation' w (pure ($ y) <*> x)

        applicativeHomomorphism ∷ ENV → Fun W W → W → Property
        applicativeHomomorphism w (apply → f) x =
            runEvaluation' w (pure f <*> pure x) ==== runEvaluation' w (pure (f x) ∷ Evaluation ENV W)


monadLaws
    ∷ ∀ m
     . ( Arbitrary (m W)
       , Eq (m W)
       , Monad m
       , Show (m W)
       )
    ⇒ TestTree
monadLaws =
    testGroup
        "Monad Laws"
        [ testLaw monadLeftIdentity "Left Identity" "return a >>= k === k a"
        , testLaw monadRightIdentity "Right Identity" "m >>= return === m"
        , testLaw monadAssociativity "Associativity" "m >>= (x -> k x >>= h) === (m >>= k) >>= h"
        ]
    where
        monadRightIdentity ∷ m W → Property
        monadRightIdentity x =
            (x >>= pure) === x

        monadLeftIdentity ∷ W → Fun W (m W) → Property
        monadLeftIdentity x (apply → f) =
            (pure x >>= f) === f x

        monadAssociativity ∷ m W → Fun W (m W) → Fun W (m W) → Property
        monadAssociativity x (apply → f) (apply → g) =
            ((x >>= f) >>= g) === (x >>= (\x' → f x' >>= g))


monadLaws' ∷ RunEvals → TestTree
monadLaws' (runEvaluation', _, _, _) =
    testGroup
        "Monad Laws"
        [ testLaw monadLeftIdentity "Left Identity" "return a >>= k === k a"
        , testLaw monadRightIdentity "Right Identity" "m >>= return === m"
        , testLaw monadAssociativity "Associativity" "m >>= (x -> k x >>= h) === (m >>= k) >>= h"
        ]
    where
        monadRightIdentity ∷ ENV → Blind (Evaluation ENV W) → Property
        monadRightIdentity w (Blind x) =
            runEvaluation' w (x >>= pure) ==== runEvaluation' w x

        monadLeftIdentity ∷ ENV → W → Fun W (Blind (Evaluation ENV W)) → Property
        monadLeftIdentity w x (fmap getBlind . apply → f) =
            runEvaluation' w (pure x >>= f) ==== runEvaluation' w (f x)

        monadAssociativity
            ∷ ENV
            → Blind (Evaluation ENV W)
            → Fun W (Blind (Evaluation ENV W))
            → Fun W (Blind (Evaluation ENV W))
            → Property
        monadAssociativity w (Blind x) (fmap getBlind . apply → f) (fmap getBlind . apply → g) =
            runEvaluation' w ((x >>= f) >>= g) ==== runEvaluation' w (x >>= (\x' → f x' >>= g))


monadFailLaws
    ∷ ∀ m
     . ( Arbitrary (m W)
       , Eq (m W)
       , MonadFail m
       , Show (m W)
       )
    ⇒ TestTree
monadFailLaws =
    testGroup
        "MonadFail Laws"
        [ testLaw leftNullification "Left Nullification" "fail s >>= f === fail s"
        ]
    where
        leftNullification ∷ Fun W (m W) → String → Property
        leftNullification (apply → f) s =
            (fail s >>= f) === (fail s ∷ m W)


monadFailLaws' ∷ RunEvals → TestTree
monadFailLaws' (runEvaluation', _, _, _) =
    testGroup
        "MonadFail Laws"
        [ testLaw leftNullification "Left Nullification" "fail s >>= f === fail s"
        ]
    where
        leftNullification ∷ ENV → Fun W (Blind (Evaluation ENV W)) → String → Property
        leftNullification w (fmap getBlind . apply → f) s =
            runEvaluation' w (fail s >>= f) ==== runEvaluation' w (fail s ∷ Evaluation ENV W)


{-
monadLoggerLaws' :: RunEvals -> TestTree
monadLoggerLaws' =
    testGroup
        "MonadLogger Laws"
        [ testLaw
            failureInfoNullification
            "Info Nullification"
            "fail x <?> y === fail x"
        , testLaw
            failureWarnNullification
            "Warn Nullification"
            "fail x <@> y === fail x"
        , testLaw
            associativityInfo
            "Info Assocaiativity"
            "let a = v <?> x in a <?> y <?> z === let a = v <?> x <?> y in a <?> z"
        , testLaw
            associativityWarn
            "Warn Assocaiativity"
            "let a = v <@> x in a <@> y <@> z === let a = v <@> x <@> y in a <@> z"
        ]
    where
        failureInfoNullification :: W -> String -> String -> Property
        failureInfoNullification w x y =
            runEvaluation' w (fail x <?> y) === runEvaluation' w (fail x :: Evaluation ENV W)

        failureWarnNullification :: W -> String -> String -> Property
        failureWarnNullification w x y =
            runEvaluation' w (fail x <@> y) === runEvaluation' w (fail x :: Evaluation ENV W)

        associativityInfo :: W -> Blind (Evaluation ENV W) -> String -> String -> String -> Property
        associativityInfo w (Blind e) x y z =
            runEvaluation' w (let v = e <?> x in v <?> y <?> z) === runEvaluation' w (let v = e <?> x <?> y in v <?> z)

        associativityWarn :: W -> Blind (Evaluation ENV W) -> String -> String -> String -> Property
        associativityWarn w (Blind e) x y z =
            runEvaluation' w (let v = e <@> x in v <@> y <@> z) === runEvaluation' w (let v = e <@> x <@> y in v <@> z)
-}

monadZipLaws
    ∷ ∀ m
     . ( Arbitrary (m W)
       , Eq (m W)
       , Eq (m ())
       , Eq (m (W, W))
       , MonadZip m
       , Show (m W)
       , Show (m ())
       , Show (m (W, W))
       )
    ⇒ TestTree
monadZipLaws =
    testGroup
        "MonadZip Laws"
        [ testLaw
            naturality
            "Naturality"
            "fmap (f *** g) (mzip ma mb) === mzip (fmap f ma) (fmap g mb)"
        , testLaw
            infoPreservation
            "Information preservation"
            "fmap (const ()) ma === fmap (const ()) mb ==> munzip (mzip ma mb) === (ma, mb)"
        ]
    where
        naturality ∷ Fun W W → Fun W W → m W → m W → Property
        naturality (apply → f) (apply → g) x y =
            fmap (f *** g) (mzip x y) === mzip (fmap f x) (fmap g y)

        infoPreservation ∷ m W → m W → Property
        infoPreservation x y =
            (void x =/= void y)
                .||. (munzip (mzip x y) === (x, y))


monadZipLaws' ∷ RunEvals → TestTree
monadZipLaws' (runEvaluation', _, runEvaluation''', runEvaluation'''') =
    testGroup
        "MonadZip Laws"
        [ testLaw
            naturality
            "Naturality"
            "fmap (f *** g) (mzip ma mb) === mzip (fmap f ma) (fmap g mb)"
        , testLaw
            infoPreservation
            "Information preservation"
            "fmap (const ()) ma === fmap (const ()) mb ==> munzip (mzip ma mb) === (ma, mb)"
        ]
    where
        naturality ∷ ENV → Fun W W → Fun W W → Blind (Evaluation ENV W) → Blind (Evaluation ENV W) → Property
        naturality w (apply → f) (apply → g) (Blind x) (Blind y) =
            runEvaluation''' w (fmap (f *** g) (mzip x y)) ==== runEvaluation''' w (mzip (fmap f x) (fmap g y))

        infoPreservation ∷ ENV → Blind (Evaluation ENV W) → Blind (Evaluation ENV W) → Property
        infoPreservation w (Blind x) (Blind y) =
            let lhs = runEvaluation'''' w $ g x
                rhs = runEvaluation'''' w $ g y
            in  lhs =//= rhs
                    .||. f (munzip (mzip x y))
                        <==> f (x, y)
            where
                g ∷ Evaluation ENV W → Evaluation ENV ()
                g = void
                f = bimap (runEvaluation' w) (runEvaluation' w)


altLaws
    ∷ ∀ f
     . ( Alternative f
       , Arbitrary (f W)
       , Eq (f W)
       , Show (f W)
       )
    ⇒ TestTree
altLaws =
    testGroup
        "Alternative Laws"
        [ testLaw altAssociativity "Associativity" "x <|> (y <|> z) === (x <|> y) <|> z"
        , testLaw altLeftCatch "Left Catch" "pure x <|> y = pure x"
        , testLaw altLeftDistributivity1 "Left Distributivity I" "f <$> (x <|> y) === (f <$> x) <|> (f <$> y)"
        -- These laws do not hold for our 'Either-like' data type.
        -- This is okay (apparently) since the 'Left Catch' law holds.
        --    , "Left Distributivity II" "(x <|> y) <*> z === (x <*> z) <|> (y <*> z)"
        --    , "Right Distributivity"   "(m <|> n) >>= f === (m >>= f) <|> (m >>= f)"
        ]
    where
        altAssociativity ∷ f W → f W → f W → Property
        altAssociativity x y z =
            ((x <|> y) <|> z) === (x <|> (y <|> z))

        altLeftCatch ∷ W → f W → Property
        altLeftCatch x y =
            (pure x <|> y) === pure x

        altLeftDistributivity1 ∷ Fun W W → f W → f W → Property
        altLeftDistributivity1 (apply → f) x y =
            (f <$> (x <|> y)) === ((f <$> x) <|> (f <$> y))


altLaws' ∷ RunEvals → TestTree
altLaws' (runEvaluation', _, _, _) =
    testGroup
        "Alternative Laws"
        [ testLaw altAssociativity "Associativity" "x <|> (y <|> z) === (x <|> y) <|> z"
        , testLaw altLeftCatch "Left Catch" "pure x <|> y = pure x"
        , testLaw altLeftDistributivity1 "Left Distributivity I" "f <$> (x <|> y) === (f <$> x) <|> (f <$> y)"
        -- These laws do not hold for our 'Either-like' data type.
        -- This is okay (apparently) since the 'Left Catch' law holds.
        --    , "Left Distributivity II" "(x <|> y) <*> z === (x <*> z) <|> (y <*> z)"
        --    , "Right Distributivity"   "(m <|> n) >>= f === (m >>= f) <|> (m >>= f)"
        ]
    where
        altAssociativity ∷ ENV → Blind (Evaluation ENV W) → Blind (Evaluation ENV W) → Blind (Evaluation ENV W) → Property
        altAssociativity w (Blind x) (Blind y) (Blind z) =
            runEvaluation' w ((x <|> y) <|> z) ==== runEvaluation' w (x <|> (y <|> z))

        altLeftCatch ∷ ENV → W → Blind (Evaluation ENV W) → Property
        altLeftCatch w x (Blind y) =
            runEvaluation' w (pure x <|> y) ==== runEvaluation' w (pure x)

        altLeftDistributivity1 ∷ ENV → Fun W W → Blind (Evaluation ENV W) → Blind (Evaluation ENV W) → Property
        altLeftDistributivity1 w (apply → f) (Blind x) (Blind y) =
            runEvaluation' w (f <$> (x <|> y)) ==== runEvaluation' w ((f <$> x) <|> (f <$> y))


applyLaws
    ∷ ∀ f
     . ( Applicative f
       , Arbitrary (f W)
       , Arbitrary (f (Fun W W))
       , Eq (f W)
       , Eq (f (f W))
       , Show (f W)
       , Show (f (f W))
       , Show (f (Fun W W))
       )
    ⇒ TestTree
applyLaws =
    testGroup
        "Applicative Laws"
        [ testLaw composition "Composition" "(.) <$> u <*> v <*> w = u <*> (v <*> w)"
        , testLaw leftInterchange "Left Interchange" "x <*> (f <$> y) = (. f) <$> x <*> y"
        , testLaw rightInterchange "Right Interchange" "f <$> (x <*> y) = (f .) <$> x <*> y"
        , testLaw leftNullification "Left Nullification" "(mf <$> m) *> (nf <$> n) = nf <$> (m *> n)"
        , testLaw rightNullification "Right Nullification" "(mf <$> m) <* (nf <$> n) = mf <$> (m <* n)"
        ]
    where
        composition ∷ f (Fun W W) → f (Fun W W) → f W → Property
        composition (fmap apply → x) (fmap apply → y) z =
            ((.) <$> x <*> y <*> z) === (x <*> (y <*> z))

        leftInterchange ∷ Fun W W → f (Fun W W) → f W → Property
        leftInterchange (apply → f) (fmap apply → x) y =
            (x <*> (f <$> y)) === ((. f) <$> x <*> y)

        rightInterchange ∷ Fun W (f W) → f (Fun W W) → f W → Property
        rightInterchange (apply → f) (fmap apply → x) y =
            (f <$> (x <*> y)) === ((f .) <$> x <*> y)

        leftNullification ∷ Fun W W → Fun W W → f W → f W → Property
        leftNullification (apply → f) (apply → g) m n =
            ((f <$> m) *> (g <$> n)) === (g <$> (m *> n))

        rightNullification ∷ Fun W W → Fun W W → f W → f W → Property
        rightNullification (apply → f) (apply → g) m n =
            ((f <$> m) <* (g <$> n)) === (f <$> (m <* n))


applyLaws' ∷ RunEvals → TestTree
applyLaws' (runEvaluation', runEvaluation'', _, _) =
    testGroup
        "Applicative Laws"
        [ testLaw composition "Composition" "(.) <$> u <*> v <*> w = u <*> (v <*> w)"
        , testLaw leftInterchange "Left Interchange" "x <*> (f <$> y) = (. f) <$> x <*> y"
        , testLaw rightInterchange "Right Interchange" "f <$> (x <*> y) = (f .) <$> x <*> y"
        , testLaw leftNullification "Left Nullification" "(mf <$> m) *> (nf <$> n) = nf <$> (m *> n)"
        , testLaw rightNullification "Right Nullification" "(mf <$> m) <* (nf <$> n) = mf <$> (m <* n)"
        ]
    where
        composition
            ∷ ENV
            → Blind (Evaluation ENV (Fun W W))
            → Blind (Evaluation ENV (Fun W W))
            → Blind (Evaluation ENV W)
            → Property
        composition w (fmap apply . getBlind → x) (fmap apply . getBlind → y) (Blind z) =
            runEvaluation' w ((.) <$> x <*> y <*> z) ==== runEvaluation' w (x <*> (y <*> z))

        leftInterchange ∷ ENV → Fun W W → Blind (Evaluation ENV (Fun W W)) → Blind (Evaluation ENV W) → Property
        leftInterchange w (apply → f) (fmap apply . getBlind → x) (Blind y) =
            runEvaluation' w (x <*> (f <$> y)) ==== runEvaluation' w ((. f) <$> x <*> y)

        rightInterchange
            ∷ ENV → Fun W (Blind (Evaluation ENV W)) → Blind (Evaluation ENV (Fun W W)) → Blind (Evaluation ENV W) → Property
        rightInterchange w (fmap getBlind . apply → f) (fmap apply . getBlind → x) (Blind y) =
            let go ∷ Evaluation ENV (Evaluation ENV W) → IO W
                go e = join $ runEvaluation' w <$> runEvaluation'' w e
            in  go (f <$> (x <*> y)) ==== go ((f .) <$> x <*> y)

        leftNullification ∷ ENV → Fun W W → Fun W W → Blind (Evaluation ENV W) → Blind (Evaluation ENV W) → Property
        leftNullification w (apply → f) (apply → g) (Blind m) (Blind n) =
            runEvaluation' w ((f <$> m) *> (g <$> n)) ==== runEvaluation' w (g <$> (m *> n))

        rightNullification ∷ ENV → Fun W W → Fun W W → Blind (Evaluation ENV W) → Blind (Evaluation ENV W) → Property
        rightNullification w (apply → f) (apply → g) (Blind m) (Blind n) =
            runEvaluation' w ((f <$> m) <* (g <$> n)) ==== runEvaluation' w (f <$> (m <* n))


bindLaws
    ∷ ∀ m
     . ( Arbitrary (m W)
       , Arbitrary (m (m W))
       , Arbitrary (m (m (m W)))
       , Arbitrary (m (Fun W W))
       , Monad m
       , Eq (m W)
       , Show (m W)
       , Show (m (m W))
       , Show (m (m (m W)))
       , Show (m (Fun W W))
       )
    ⇒ TestTree
bindLaws =
    testGroup
        "Bind Laws"
        [ testLaw defJoin "Definition of join" "join === (>>= id)"
        , testLaw defBind "Definition of bind" "m >>= f === join (fmap f m)"
        , testLaw defApply "Definition of apply" "f <*> x === f >>= (<$> x)"
        , testLaw associativity1 "Associativity I" "(m >>= f) >>= g === m >>= (\\x -> f x >>= g)"
        , testLaw associativity2 "Associativity II" "join . join === join . mmap join"
        ]
    where
        defJoin ∷ m (m W) → Property
        defJoin x =
            join x === (>>= id) x

        defBind ∷ Fun W (m W) → m W → Property
        defBind (apply → f) x =
            (x >>= f) === join (fmap f x)

        defApply ∷ m (Fun W W) → m W → Property
        defApply (fmap apply → f) x =
            (f <*> x) === (f >>= (<$> x))

        associativity1 ∷ Fun W (m W) → Fun W (m W) → m W → Property
        associativity1 (apply → f) (apply → g) x =
            ((x >>= f) >>= g) === (x >>= (\a → f a >>= g))

        associativity2 ∷ m (m (m W)) → Property
        associativity2 x =
            (join . join) x === (join . fmap join) x


bindLaws' ∷ RunEvals → TestTree
bindLaws' (runEvaluation', _, _, _) =
    testGroup
        "Bind Laws"
        [ testLaw defJoin "Definition of join" "join === (>>= id)"
        , testLaw defBind "Definition of bind" "m >>= f === join (fmap f m)"
        , testLaw defApply "Definition of apply" "f <*> x === f >>= (<$> x)"
        , testLaw associativity1 "Associativity I" "(m >>= f) >>= g === m >>= (\\x -> f x >>= g)"
        , testLaw associativity2 "Associativity II" "join . join === join . mmap join"
        ]
    where
        defJoin ∷ ENV → Blind (Evaluation ENV (Evaluation ENV W)) → Property
        defJoin w (Blind x) =
            runEvaluation' w (join x) ==== runEvaluation' w ((>>= id) x)

        defBind ∷ ENV → Fun W (Blind (Evaluation ENV W)) → Blind (Evaluation ENV W) → Property
        defBind w (fmap getBlind . apply → f) (Blind x) =
            runEvaluation' w (x >>= f) ==== runEvaluation' w (join (fmap f x))

        defApply ∷ ENV → Blind (Evaluation ENV (Fun W W)) → Blind (Evaluation ENV W) → Property
        defApply w (fmap apply . getBlind → f) (Blind x) =
            runEvaluation' w (f <*> x) ==== runEvaluation' w (f >>= (<$> x))

        associativity1
            ∷ ENV → Fun W (Blind (Evaluation ENV W)) → Fun W (Blind (Evaluation ENV W)) → Blind (Evaluation ENV W) → Property
        associativity1 w (fmap getBlind . apply → f) (fmap getBlind . apply → g) (Blind x) =
            runEvaluation' w ((x >>= f) >>= g) ==== runEvaluation' w (x >>= (\a → f a >>= g))

        associativity2 ∷ ENV → Blind (Evaluation ENV (Evaluation ENV (Evaluation ENV W))) → Property
        associativity2 w (Blind x) =
            runEvaluation' w ((join . join) x) ==== runEvaluation' w ((join . fmap join) x)


equalityLaws
    ∷ ∀ a
     . ( Arbitrary a
       , Eq a
       , Show a
       )
    ⇒ TestTree
equalityLaws =
    testGroup
        "Equality Laws"
        [ testLaw negation "Negation" "x /= y ==> not (x == y)"
        , testLaw symmetry "Symmetry" "x /= y ==> y /= x"
        , testLaw transitivity "Transitivity" "x == y && y == z ==> x == z"
        , testLaw refexivity "Reflexivity" "x == x"
        ]
    where
        negation ∷ a → a → Property
        negation x y =
            x /= y ==> not (x == y)

        symmetry ∷ a → a → Property
        symmetry x y =
            x /= y ==> y =/= x

        transitivity ∷ a → a → a → Property
        transitivity x y z =
            not (x == y && y == z) .||. x == z

        refexivity ∷ a → Property
        refexivity x =
            x === x


normalFormDataLaws
    ∷ ∀ a
     . ( Arbitrary a
       , NFData a
       , Show a
       )
    ⇒ TestTree
normalFormDataLaws =
    testGroup
        "NFData Laws"
        [ testLaw finiteReduction "Finiteness" "rnf x =/= _|_"
        ]
    where
        finiteReduction ∷ a → Property
        finiteReduction x =
            rnf x === ()


orderingLaws
    ∷ ∀ a
     . ( Arbitrary a
       , Ord a
       , Show a
       )
    ⇒ TestTree
orderingLaws =
    testGroup
        "Ordering Laws"
        [ testLaw symmetry "Symmetry" "x >= y ==> y <= x"
        , testLaw transitivity1 "Transitive I" "x < y && y < z ==> x < z"
        , testLaw transitivity2 "Transitive II" "x > y && y > z ==> x > z"
        ]
    where
        symmetry ∷ a → a → Bool
        symmetry lhs rhs =
            case (lhs `compare` rhs, rhs `compare` lhs) of
                (EQ, EQ) → True
                (GT, LT) → True
                (LT, GT) → True
                _ → False

        transitivity1 ∷ a → a → a → Property
        transitivity1 x y z =
            (x < y && y < z) ==> x < z

        transitivity2 ∷ a → a → a → Property
        transitivity2 x y z =
            (x > y && y > z) ==> x > z


semigroupLaws
    ∷ ∀ a
     . ( Arbitrary a
       , Eq a
       , Semigroup a
       , Show a
       )
    ⇒ TestTree
semigroupLaws =
    testGroup
        "Semigroup Laws"
        [ testLaw semigroupAssociativity "Associativity" "x <> (y <> z) === (x <> y) <> z"
        ]
    where
        semigroupAssociativity ∷ a → a → a → Property
        semigroupAssociativity x y z =
            (x <> (y <> z)) === ((x <> y) <> z)


semigroupLaws' ∷ RunEvals → TestTree
semigroupLaws' (runEvaluation', _, _, _) =
    testGroup
        "Semigroup Laws"
        [ testLaw semigroupAssociativity "Associativity" "x <> (y <> z) === (x <> y) <> z"
        ]
    where
        semigroupAssociativity ∷ ENV → Blind (Evaluation ENV W) → Blind (Evaluation ENV W) → Blind (Evaluation ENV W) → Property
        semigroupAssociativity w (Blind x) (Blind y) (Blind z) =
            runEvaluation' w (x <> (y <> z)) ==== runEvaluation' w ((x <> y) <> z)


showProperties
    ∷ ∀ a
     . ( Arbitrary a
       , Show a
       )
    ⇒ TestTree
showProperties =
    testGroup
        "Show Laws"
        [ testLaw finiteString "Finiteness" "rnf (show x) =/= _|_"
        , testLaw nonNullString "Non-null" "not . null . show"
        ]
    where
        finiteString ∷ a → Property
        finiteString x =
            (rnf . show) x === ()

        nonNullString ∷ a → Bool
        nonNullString =
            not . null . show


foldableLaws
    ∷ ∀ f
     . ( Arbitrary (f W)
       , Foldable f
       , Show (f W)
       )
    ⇒ TestTree
foldableLaws =
    testGroup
        "Foldable Laws"
        [ testLaw
            testFoldlFoldMap
            "Dual Endomorphism"
            "foldl' f z t === appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z"
        , testLaw
            testFoldrFoldMap
            "Fold-map Endomorphism"
            "foldr f z t === appEndo (foldMap (Endo . f) t ) z"
        , testLaw
            testFoldr
            "Fold-right List Equivelency"
            "foldr f z === foldr f z . toList"
        , testLaw
            testFoldl
            "Fold-right List Equivelency"
            "foldl' f z === foldl' f z . toList"
        , testLaw
            testFoldr1
            "Non-empty Fold-right List Equivelency"
            "foldr1 f === foldr1 f . toList"
        , testLaw
            testFoldl1
            "Non-empty Fold-left List Equivelency"
            "foldl1 f === foldl1 f . toList"
        , testLaw
            testNull
            "Zero-length Nullability Implication"
            "null === (0 ==) . length"
        , testLaw
            testLength
            "Length List Equivelency"
            "length === length . toList"
        , testLaw
            testInclusionConsistency
            "Inclusion Consistency"
            "elem e =/= notElem e"
        , testLaw
            testMax
            "Max Fold-map Equivelency"
            "maximum === getMax . foldMap Max"
        , testLaw
            testMin
            "Min Fold-map Equivelency"
            "minimum === getMin . foldMap Min"
        , testLaw
            testSum
            "Sum Fold-map Equivelency"
            "sum === getSum . foldMap Sum"
        , testLaw
            testProduct
            "Product Fold-map Equivelency"
            "product === getProduct . foldMap Product"
        , testLaw
            testFirst
            "First Fold-map Equivelency"
            "head . toList === getFirst . foldMap First"
        , testLaw
            testLast
            "Last Fold-map Equivelency"
            "last . toList === getLast . foldMap Last"
        , testLaw
            testAll
            "All Fold-map Equivelency"
            "all f === getAll . foldMap (All . f)"
        , testLaw
            testAny
            "Any Fold-map Equivelency"
            "any f === getAny . foldMap (Any . f)"
        ]
    where
        testFoldrFoldMap ∷ Fun (W, W) W → W → f W → Property
        testFoldrFoldMap (applyFun2 → f) z x =
            foldr f z x === appEndo (foldMap (Endo . f) x) z

        testFoldlFoldMap ∷ Fun (W, W) W → W → f W → Property
        testFoldlFoldMap (applyFun2 → f) z x =
            foldl' f z x === appEndo (getDual (foldMap (Dual . Endo . flip f) x)) z

        testFoldr ∷ Fun (W, W) W → W → f W → Property
        testFoldr (applyFun2 → f) z x =
            foldr f z x === (foldr f z . toList) x

        testFoldl ∷ Fun (W, W) W → W → f W → Property
        testFoldl (applyFun2 → f) z x =
            foldl' f z x === (foldl' f z . toList) x

        testFoldr1 ∷ Fun (W, W) W → f W → Property
        testFoldr1 (applyFun2 → f) x =
            (not . null) x ==> foldr1 f x === (foldr1 f . toList) x

        testFoldl1 ∷ Fun (W, W) W → f W → Property
        testFoldl1 (applyFun2 → f) x =
            (not . null) x ==> foldl1 f x === (foldl1 f . toList) x

        testNull ∷ f W → Property
        testNull x =
            null x === ((0 ==) . length) x

        testLength ∷ f W → Property
        testLength x =
            length x === (length . toList) x

        testInclusionConsistency ∷ (W, f W) → Property
        testInclusionConsistency (e, x) =
            elem e x =/= notElem e x

        testMax ∷ f W → Property
        testMax x =
            (not . null) x ==>
                maximum x
                    === (getMax . foldMap Max) x

        testMin ∷ f W → Property
        testMin x =
            (not . null) x ==>
                minimum x
                    === (getMin . foldMap Min) x

        testSum ∷ f W → Property
        testSum x =
            sum x === (getSum . foldMap Sum) x

        testProduct ∷ f W → Property
        testProduct x =
            product x === (getProduct . foldMap Product) x

        testFirst ∷ f W → Property
        testFirst x = case toList x of
            [] → discard
            y : _ → Just y === (fmap getFirst . foldMap (Just . First)) x

        testLast ∷ f W → Property
        testLast x =
            (not . null) x ==>
                (Just . last . toList) x
                    === (fmap getLast . foldMap (Just . Last)) x

        testAll ∷ Fun W Bool → f W → Property
        testAll (apply → f) x =
            all f x === (getAll . foldMap (All . f)) x

        testAny ∷ Fun W Bool → f W → Property
        testAny (apply → f) x =
            any f x === (getAny . foldMap (Any . f)) x


traversableLaws
    ∷ ∀ f
     . ( Arbitrary (f W)
       , Eq (f Bool)
       , Eq (f W)
       , Traversable f
       , Show (f W)
       , Show (f Bool)
       )
    ⇒ TestTree
traversableLaws =
    testGroup
        "Traversable Laws"
        [ testLaw naturality "Naturality" "t . traverse f === traverse (t . f)"
        , testLaw identity "Identity" "traverse Identity === Identity"
        , testLaw composition "Composition" "traverse (Compose . fmap g . f) === Compose . fmap (traverse g) . traverse f"
        , testLaw equality "Definition Equality" "traverse === mapM"
        ]
    where
        naturality ∷ Fun W [W] → f W → Property
        naturality (apply → f) x =
            (headMay . traverse f) x === traverse (headMay . f) x
            where
                headMay ∷ [a] → Maybe a
                headMay [] = Nothing
                headMay (a : _) = Just a

        identity ∷ f W → Property
        identity x =
            traverse Identity x === Identity x

        composition ∷ Fun W (Either W Bool) → Fun Bool (Maybe W) → f W → Property
        composition (apply → f) (apply → g) x =
            traverse (Compose . fmap g . f) x === (Compose . fmap (traverse g) . traverse f) x

        equality ∷ Fun W (Maybe Bool) → f W → Property
        equality (apply → f) x =
            traverse f x === mapM f x


testLaw ∷ (Testable a) ⇒ a → String → String → TestTree
testLaw f lawName lawExpression = testGroup lawName [testProperty lawExpression f]


-- | Like '/=', but prints a counterexample when it fails.
infix 4 =/=


(=/=) ∷ (Eq a, Show a) ⇒ a → a → Property
(=/=) x y = counterexample (show x <> " == " <> show y) (x /= y)
