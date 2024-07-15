{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Smart constructor methods for producing a tranistion cost matrix.
The 'TransitionMatrix.Diagnosis.TransitionMeasureDiagnosis' provides
a metadata describing the structure of the tranistion cost matrix.
-}
module TransitionMatrix.Diagnosis.Discretization (
    adaptiveDiscretization,
    ErrorFromDiscretization (..),
) where

import Control.DeepSeq
import Control.Monad.ST (runST)
import Data.Bifunctor
import Data.Bits
import Data.Either (isRight)
import Data.Foldable
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Ord
import Data.Ratio
import Data.Semigroup (Arg (..))
import Data.Set (Set)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as V
import Data.Vector.Generic ((!?))
import Data.Vector.Storable qualified as VS
import GHC.Exts qualified as GHC (fromList, toList)
import GHC.Generics (Generic)
import GHC.IsList (IsList (Item))
import Layout.Compact.Symbols (DiscretizedResolution)
import Math.NumberTheory.Logarithms
import Math.NumberTheory.Roots
import Measure.Unit.SymbolCount
import Numeric.Natural
import Numeric.Search.Range (searchFromTo)
import TransitionMatrix.Diagnosis.Error


{- |
Summary statistics describing the amount of numerical error incurred by the
matrix discretization process.
-}
data ErrorFromDiscretization
    = ErrorFromDiscretization
    { discretizationErrrorTotal ∷ Rational
    , discretizationErrrorMean ∷ Rational
    , discretizationErrrorSTD ∷ Rational
    , discretizationErrrorMin ∷ Rational
    , discretizationErrrorMax ∷ Rational
    }
    deriving stock (Eq, Ord, Generic, Show)
    deriving anyclass (NFData)


data FactorizedRational = FactorizedRational [(Natural, Word)] [(Natural, Word)]


instance Show FactorizedRational where
    show (FactorizedRational numFactors denFactors) =
        let renderFactor ∷ (Natural, Word) → String
            renderFactor (p, 1) = show p
            renderFactor (p, e) = show p <> "^" <> show e
            renderComponent = List.intercalate " × " . fmap renderFactor
        in  unwords
                [ renderComponent numFactors
                , "//"
                , renderComponent denFactors
                ]


{- |
The perturbation super-position of a value, by a given delta.
-}
data PerturbationValue = PerturbationValue Word Natural
    deriving stock (Eq, Ord, Show)


{- |
A data-type for representing a potential perturbation choice.
-}
data PerturbationChoice
    = PChoice
    { _pGCD ∷ Natural
    , _pElements ∷ NonEmpty Natural
    }
    deriving stock (Eq, Ord, Show)


perturbedValues ∷ PerturbationValue → NonEmpty Natural
perturbedValues (PerturbationValue _ 0) = 0 :| []
perturbedValues (PerturbationValue delta x) =
    let minMinus ∷ Natural → Word → Natural
        minMinus a b = fromInteger . max 0 $ toInteger a - toInteger b

        extraRange = lowerRange <> upperRange
        lowerRange = reverse $ (x `minMinus`) <$> [1 .. delta]
        upperRange = (x +) . fromIntegral <$> [1 .. delta]
    in  NE.sort $ x :| extraRange


perturbedChoice ∷ Natural → PerturbationChoice → PerturbationChoice
perturbedChoice a (PChoice currGCD (c :| cs)) =
    let nextGCD = gcd' a currGCD
    in  PChoice nextGCD $ a :| c : cs


perturbGCD
    ∷ Word
    → PerturbationValue
    → NonEmpty PerturbationChoice
    → NonEmpty PerturbationChoice
perturbGCD keep p choices =
    let nextChoices ∷ NonEmpty PerturbationChoice
        nextChoices = liftA2 perturbedChoice (perturbedValues p) choices
    in  filterPerturbationBy keep approxMeasure nextChoices


approxMeasure ∷ PerturbationChoice → Natural
approxMeasure (PChoice pGCD _) = pGCD


errorDiscretizingTo ∷ (Foldable f, Foldable t, Real a, Real b) ⇒ f a → t b → ErrorFromDiscretization
errorDiscretizingTo original adjusted =
    let ratOriginal = toRational <$> toList original
        ratAdjusted = toRational <$> toList adjusted

        diff ∷ Rational → Rational → Rational
        diff x y = max x y - min x y

        diffSquared ∷ Rational → Rational → Rational
        diffSquared x y = let d = diff x y in d * d

        differences = zipWith diff ratOriginal ratAdjusted

        sampleCount = fromIntegral $ length differences

        errTotal = sum differences

        errMean = errTotal / sampleCount

        num = sum $ (`diffSquared` errMean) <$> differences

        den = sampleCount - 1
        var = num / den

        varNum = numerator var
        varDen = denominator var

        errStandardDev = integerSquareRoot varNum % integerSquareRoot varDen
    in  ErrorFromDiscretization
            { discretizationErrrorTotal = errTotal
            , discretizationErrrorMean = errMean
            , discretizationErrrorSTD = errStandardDev
            , discretizationErrrorMax = maximum differences
            , discretizationErrrorMin = minimum differences
            }


getPerturbedGCDError
    ∷ (Foldable f, Functor f, IsList (f Natural), Item (f Natural) ~ Natural)
    ⇒ Word → Word → f Natural → (ErrorFromDiscretization, Natural, f Natural)
getPerturbedGCDError keep delta vals =
    let (finalGCD, factoredVals) = getPerturbedGCD keep delta vals
        perturbedVals = (finalGCD *) <$> factoredVals
    in  (vals `errorDiscretizingTo` perturbedVals, finalGCD, factoredVals)


filterPerturbationBy
    ∷ (Ord o)
    ⇒ Word
    → (PerturbationChoice → o)
    → NonEmpty PerturbationChoice
    → NonEmpty PerturbationChoice
filterPerturbationBy keep f = NE.fromList . NE.take (fromEnum keep) . NE.sortWith (Down . f)


{- |
Find the perturbed GCD of the list and return the GCD along with the /perturbed/ values after having the GCD factored out.
-}
getPerturbedGCD ∷ (IsList f, Item f ~ Natural) ⇒ Word → Word → f → (Natural, f)
getPerturbedGCD keep delta ns = case GHC.toList ns of
    [] → (1, ns)
    x : xs → case xs of
        [] → (x, GHC.fromList [1])
        y : zs →
            let pX = PerturbationValue delta x
                pY = PerturbationValue delta y

                gcdChoice a b =
                    let v = gcd' a b
                    in  PChoice v $ b :| [a]

                gcdCrossProduct = do
                    vX ← perturbedValues pX
                    vY ← perturbedValues pY
                    pure $ gcdChoice vX vY

                gcdChoices = filterPerturbationBy keep approxMeasure gcdCrossProduct

                perturb' = flip (perturbGCD keep)

                PChoice bestGCD bestValues :| _ =
                    filterPerturbationBy keep _pGCD
                        . foldl' perturb' gcdChoices
                        $ PerturbationValue delta <$> zs
            in  (bestGCD, GHC.fromList . toList $ (`div` bestGCD) <$> NE.reverse bestValues)


selectBestPerturbation
    ∷ ∀ a
     . (Ord a)
    ⇒ SymbolCount
    → V.Vector a
    → Word
    → Word
    → V.Vector Natural
    → Either
        (DiagnosisFailure a)
        (Rational, ErrorFromDiscretization, Natural, V.Vector Natural)
selectBestPerturbation dim originalValues keep deltaMax vals =
    let deltaRange ∷ V.Vector Word
        deltaRange = V.fromListN (fromEnum deltaMax) [1 .. deltaMax]

        findOverflows ∷ V.Vector Natural → V.Vector a
        findOverflows = getOverflowValues dim originalValues

        tagAndSortDeltaErrors
            ∷ V.Vector (ErrorFromDiscretization, Natural, V.Vector Natural)
            → V.Vector (Arg (Rational, Word) (ErrorFromDiscretization, Natural, V.Vector Natural))
        tagAndSortDeltaErrors =
            let compareDeltas
                    ∷ Word
                    → (ErrorFromDiscretization, Natural, V.Vector Natural)
                    → Arg (Rational, Word) (ErrorFromDiscretization, Natural, V.Vector Natural)
                compareDeltas i x@(err, factor, _) =
                    let weightedError = discretizationErrrorTotal err / fromIntegral factor
                    in  Arg (weightedError, i) x

                sortVec ∷ ∀ x. (Ord x) ⇒ V.Vector x → V.Vector x
                sortVec vec = runST $ do
                    v ← V.thaw vec
                    V.sort v
                    V.freeze v
            in  sortVec . V.zipWith compareDeltas deltaRange

        -- Step 1:
        -- For each Δ, what is the Δ-pertubation and the Δ-pertubation error?
        deltaErrorData ∷ V.Vector (ErrorFromDiscretization, Natural, V.Vector Natural)
        deltaErrorData =
            let perturbΔ δ = getPerturbedGCDError keep δ vals
            in  perturbΔ <$> deltaRange

        -- Step 2:
        -- Filter from the list those Δ-pertubations whose values overflow.
        deltasWithoutOverflow ∷ V.Vector (ErrorFromDiscretization, Natural, V.Vector Natural)
        deltasWithoutOverflow =
            let perturbationDoesNotOverflow
                    ∷ (ErrorFromDiscretization, Natural, V.Vector Natural)
                    → Bool
                perturbationDoesNotOverflow (_, _, perturbationValues) =
                    V.null $ findOverflows perturbationValues
            in  V.filter perturbationDoesNotOverflow deltaErrorData

        -- Step 3:
        -- From the list of Δ-perturbations which do not contain any overflowed values,
        -- tag each perturbation with its weighted error and Δ value.
        -- Then sort the perturbations in ascending order of weighted error.
        --
        -- /Note:/ If all Δ-perturbations contained an overflow value,
        -- then the list is empty and the sort incurs no cost!
        deltasSortedByError
            ∷ V.Vector (Arg (Rational, Word) (ErrorFromDiscretization, Natural, V.Vector Natural))
        deltasSortedByError = tagAndSortDeltaErrors deltasWithoutOverflow
    in  case deltasSortedByError !? 0 of
            -- Step 4(a):
            -- If all Δ-perturbations contain one or more overflow values,
            -- then we return as a failure state a Diagnosis Failure containing
            -- the Δ-perturbation which had the minimal weighted error.
            Nothing →
                let Arg _ (_, _, bestPertubation) = V.head $ tagAndSortDeltaErrors deltaErrorData
                    overflows = findOverflows bestPertubation
                in  Left $ discretizationFailure (ValueOverflow dim) overflows
            -- Step 4(b):
            -- When there exists at least one Δ-perturbations without any overflow values,
            -- then select the Δ-perturbations with the least weighted error.
            Just (Arg (weight, _) (err, factor, newVals)) → Right (weight, err, factor, newVals)


truncateDiscretized
    ∷ (Functor f)
    ⇒ f Natural
    -- ^ Values to be truncated
    → Word
    -- ^ Truncate ε bits from values
    → (Natural, f Natural)
truncateDiscretized nats ε =
    let -- Truncate ε bits from each number
        truncVal ∷ Natural → Natural
        truncVal = (`div` expVal)

        expVal ∷ Natural
        expVal = 1 `shiftL` fromEnum ε
    in  (expVal, truncVal <$> nats)


{- |
__Desiderata:__ For each given number to fit within @Limit@ defined as:

  * @Limit@:
      1. A 15-bit word /if and only if/ the symbol count is less than 9
      2. A 31-bit word /otherwise/

Computes the number of bits to trucnate from each number such that the number
would less than or equal to @Limit@ when truncated.
-}
truncationExponents ∷ (Functor f) ⇒ SymbolCount → f Natural → f Word
truncationExponents dim =
    let limit ∷ Natural
        limit = snd $ dicretizedValueBounds dim

        naturalLog2' ∷ Natural → Word
        naturalLog2' n = case n `div` limit of
            0 → 0
            m → fromIntegral $ naturalLog2 m
    in  fmap naturalLog2'


discretizeWithMaximumTruncationOf
    ∷ ∀ a
     . (Real a)
    ⇒ SymbolCount
    → V.Vector a
    → V.Vector Natural
    → Word
    -- ^ ε
    → Word
    -- ^ δ
    → Either
        (DiagnosisFailure a)
        (Rational, ErrorFromDiscretization, Natural, V.Vector Natural)
discretizeWithMaximumTruncationOf dim@(SymbolCount n) originalValues oversizedValues ε δ =
    let (truncationCoefficient, almostValues) =
            truncateDiscretized oversizedValues ε

        -- Twice the dim permits a sufficient number of candidates to draw from.
        candidatePoolSize =
            let r = integerSquareRoot n
            in  max 8 $ r + r

        mergeCoefficients
            ∷ (Rational, ErrorFromDiscretization, Natural, V.Vector Natural)
            → (Rational, ErrorFromDiscretization, Natural, V.Vector Natural)
        mergeCoefficients (a, b, c, d) = (a, b, truncationCoefficient * c, d)

        best = selectBestPerturbation dim originalValues candidatePoolSize δ almostValues
    in  mergeCoefficients <$> best


discretizeOversizedValues
    ∷ ∀ a
     . (Real a)
    ⇒ SymbolCount
    → Rational
    → V.Vector a
    → V.Vector Natural
    → Either
        (DiagnosisFailure a)
        (Rational, ErrorFromDiscretization, VS.Vector DiscretizedResolution)
discretizeOversizedValues dim exactCoefficient originalValues oversizedValues =
    let exponents = truncationExponents dim oversizedValues
        𝑚 = maximum exponents
        𝑛 = case toList $ V.filter (> 0) exponents of
            [] → 0
            x : xs → minimum $ x :| xs

        εδDiscretize ε =
            let -- Let 𝑚 be the maximum possible truncation exponent.
                -- Let β = 𝑚 - ε representing the excess bits of precision.
                -- Let δ = max { 0, 1 + 2^β }
                --
                -- We allow ε to range from ε ∈ [ 1 ... 𝑚 ].
                -- It is important to start varying ε at 1 and not 0 because
                -- when ε = 0 no truncation occurs. We entered this function
                -- specifically because we must truncate values, so skip the
                -- base case where we would do no truncation.
                --
                -- For all 𝑥 ∈ 𝙓, consider the perturbation superposition of 𝑥 ± δ.
                -- Since we have β bits extra of precision, we select δ ∈ [2, 17],
                -- which creates a numeric superposition range of  [ 𝑥 - δ, x + δ ]
                -- with cardinality from 5 to 35. The cardinality of the perturbation
                -- superposition will be called the "δ-slack values" and denoted as ω.
                --
                -- Suppose we are given the following ε-sequence over its range:
                --         ε ∈ [ 1 ... 𝑚 ]
                --
                -- Then we would derivive the corresponing sequences:
                --   • β-sequence denoting the bits of extra precision
                --         β ∈ [ β, β-1, ...,  5,  4,  3, 2, 1 ]
                --   • δ-sequence defining the value superposition range
                --         δ ∈ [ 17, 17, ..., 17,  9,  5, 3, 2 ]
                --   • ω-sequence of "δ-slack values"
                --         ω ∈ [ 35, 35, ..., 35, 19, 11, 7, 5 ]
                --
                -- If for all 𝑥 ∈ 𝙓, ε bits of 𝑥 are truncated, and β bits of precision
                -- remain in excess of the discretization resolution, then there are 2^β
                -- values under consideration for each 𝑥 ∈ 𝙓. By varying over the all the
                -- superpositions of 𝑥 ± δ ∈ 𝙓, we consider all possibel "overlapping windows"
                -- of perturbed 𝑥 values which might be cleanly discretized by factoring out
                -- some integer 𝑖 ≥ 2^β which for all 𝑥 ∈ 𝙓, can be factored out from one
                -- of the perturbed values of 𝑥 ± δ. With 𝑖 factored out of each perturbed
                -- value, the selected values will be within the discretization resolution.
                --
                -- Without bounding δ, the pertubation superposition cardinality ω will
                -- grow exponentially, and consequently, will require exponential runtime
                -- when 𝑚 is large and the ε-values are small. Since ε-value necessarily
                -- start small (at the value 1), this is untenible and δ must be bounded.
                --
                -- By bounding δ, we ensure runtime efficiency. However, we also limit the
                -- pertubation superposition to a subspace such in which ω < 2^β. This
                -- results in considering only ω values for each 𝑥 ∈ 𝙓 rather than some
                -- valuegreater than 2^β. Hence our insurance of runtime efficiency comes
                -- at the cost of potentially not finding a suitible pertubation with a
                -- sufficient GCD to remove the β bits of excess precision.
                --
                -- The solution to this is to let ε vary over the aforementioned range of
                -- ε ∈ [ 1 ... 𝑚 ] and then select the least ε for which a suitible
                -- pertubation is found. This can be done with a binary search.
                δ ∷ Word
                δ = min 17 . succ $ 1 `shiftL` fromEnum (𝑚 - ε)
            in  discretizeWithMaximumTruncationOf dim originalValues oversizedValues ε δ

        mostValidPertubation =
            let validTruncation = isRight . εδDiscretize
            in  εδDiscretize . product $ searchFromTo validTruncation 𝑛 𝑚

        finalizePerturbation
            ∷ (Rational, ErrorFromDiscretization, Natural, V.Vector Natural)
            → (Rational, ErrorFromDiscretization, VS.Vector DiscretizedResolution)
        finalizePerturbation (_, _, pertubedCoefficient, perturbationValues) =
            let combinedCoefficient = exactCoefficient * toRational pertubedCoefficient

                adjustedValues = (combinedCoefficient *) . toRational <$> perturbationValues

                scaledError = originalValues `errorDiscretizingTo` adjustedValues

                clampedVals = clampDiscretizedValues perturbationValues
            in  (combinedCoefficient, scaledError, clampedVals)
    in  finalizePerturbation <$> mostValidPertubation


attemptExactDiscretization
    ∷ ∀ a
     . (Real a)
    ⇒ SymbolCount
    → V.Vector a
    → (Rational, Either (V.Vector Natural) (VS.Vector DiscretizedResolution))
attemptExactDiscretization dim originalValues =
    let -- Step 1.
        -- Convert all values to /positive/ Rational values
        rationalInputs ∷ V.Vector Rational
        rationalInputs = abs . toRational <$> originalValues

        α ∷ Rational
        rationalValues ∷ V.Vector Rational
        (α, rationalValues) =
            let leastNonZero ∷ Rational
                leastNonZero =
                    let least ∷ Rational → Rational → Rational
                        least 0 x = x
                        least x 0 = x
                        least x y = min x y
                    in  foldl1 least rationalInputs

                greatestCell ∷ Rational
                greatestCell = maximum rationalInputs

                (a, normalized)
                    | leastNonZero < 1 && 1 <= greatestCell =
                        (leastNonZero, (/ leastNonZero) <$> rationalInputs)
                    | otherwise = (1, rationalInputs)
            in  (a, normalized)

        -- Step 2.
        -- Let β be the Least Common Multiple (LCM) of all denominators
        -- Taking the maximum of 1 and the resulting fold ensures that
        -- if the fold seed is 0, the result is still 1.
        β ∷ Integer
        β = max 1 . foldl1 lcm' $ denominator <$> rationalValues

        -- Step 3.
        -- Multiply all values by β to integerize them.
        intermediateValues ∷ V.Vector Natural
        intermediateValues =
            let extract ∷ Rational → Natural
                extract = fromInteger . numerator
            in  extract . ((β % 1) *) <$> rationalValues

        -- Step 4.
        -- Let γ be the Greatest Common Divisor (GCD) of all
        -- Taking the maximum of 1 and the resulting fold ensures that
        -- if the fold seed is 0, the result is still 1.
        γ = max 1 $ foldl1 gcd' intermediateValues

        -- Step 5.
        -- Divide all integer values by γ.
        -- This division is "safe," i.e. there are no truncated remainders.
        prospectiveValues = (`div` γ) <$> intermediateValues

        -- Step 6.
        -- Check for values which overflowed the representation time.
        overflowValues ∷ V.Vector a
        overflowValues = getOverflowValues dim originalValues prospectiveValues

        coefficient ∷ Rational
        coefficient = α * (toInteger γ % β)

        exactnessContext
            | V.null overflowValues = Right $ clampDiscretizedValues prospectiveValues
            | otherwise = Left prospectiveValues
    in  (coefficient, exactnessContext)


{- |
Attempt to discretize the matrix values.
-}
adaptiveDiscretization
    ∷ ∀ a
     . (Real a)
    ⇒ SymbolCount
    → V.Vector a
    → Either
        (DiagnosisFailure a)
        (Rational, Maybe ErrorFromDiscretization, VS.Vector DiscretizedResolution)
adaptiveDiscretization dim@(SymbolCount sCount) originalValues =
    let d ∷ Int
        d = fromEnum sCount

        negativeValues ∷ V.Vector a
        negativeValues =
            let negative ∷ (a, b) → Bool
                negative (x, _) = x < 0

                filtration ∷ V.Vector (a, Rational) → V.Vector a
                filtration = fmap fst . V.filter negative
            in  filtration . V.zip originalValues $ toRational <$> originalValues

        negativeFailure ∷ DiagnosisFailure a
        negativeFailure = discretizationFailure ValueNegative negativeValues

        finalizeApproximation
            ∷ Either
                (DiagnosisFailure a)
                (Rational, ErrorFromDiscretization, VS.Vector DiscretizedResolution)
            → Either
                (DiagnosisFailure a)
                (Rational, Maybe ErrorFromDiscretization, VS.Vector DiscretizedResolution)
        finalizeApproximation =
            let combineNegative ∷ DiagnosisFailure a → DiagnosisFailure a
                combineNegative
                    | V.null negativeValues = id
                    | otherwise = (<>) negativeFailure

                liftSuccess
                    ∷ (Rational, ErrorFromDiscretization, VS.Vector DiscretizedResolution)
                    → (Rational, Maybe ErrorFromDiscretization, VS.Vector DiscretizedResolution)
                liftSuccess (a, b, c) = (a, Just b, c)
            in  bimap combineNegative liftSuccess

        (exactCoefficient, context) = attemptExactDiscretization dim originalValues
    in  -- Check if the input can be discrettized exactly.
        case context of
            -- If all values are zero, do not attempt discretization
            _
                | all (== 0) originalValues →
                    Right (1, Nothing, VS.generate (d * d) $ const 0)
            -- If all values were successfully discretized,
            -- then check for negative values
            Right exactValues → case negativeValues !? 0 of
                Just _ → Left negativeFailure
                Nothing → Right (exactCoefficient, Nothing, exactValues)
            -- If some discretized values would overflow,
            -- then attempt approximated discretization
            Left oversizedValues →
                let discretized =
                        discretizeOversizedValues
                            dim
                            exactCoefficient
                            originalValues
                            oversizedValues
                in  -- Again, check for negative values
                    case (negativeValues !? 0, discretized) of
                        -- If negative values exist,
                        -- and the approximated discretization was a success,
                        -- report the negative values as an error
                        (Just _, Right _) → Left negativeFailure
                        -- Otherwise, simply combine any exising errors in the finalizer
                        (_, approximation) → finalizeApproximation approximation


discretizationFailure ∷ ∀ a. (Ord a) ⇒ (Set a → DiagnosisError a) → V.Vector a → DiagnosisFailure a
discretizationFailure build =
    let vectorToSet ∷ V.Vector a → Set a
        vectorToSet = GHC.fromList . toList

        failure ∷ Set a → DiagnosisFailure a
        failure = makeDiagnosisFailure . pure . build
    in  failure . vectorToSet


clampDiscretizedValues ∷ V.Vector Natural → VS.Vector DiscretizedResolution
clampDiscretizedValues = VS.convert . fmap fromIntegral


getOverflowValues ∷ SymbolCount → V.Vector a → V.Vector Natural → V.Vector a
getOverflowValues dim originalValues =
    let upperVal ∷ Natural
        upperVal = snd $ dicretizedValueBounds dim

        overflow ∷ (a, Natural) → Bool
        overflow (_, x) = x > upperVal
    in  fmap fst . V.filter overflow . V.zip originalValues


{- |
GCD which ignores 0 values
-}
gcd' ∷ Natural → Natural → Natural
gcd' x 0 = x
gcd' 0 y = y
gcd' x y = gcd x y


{- |
LCD which ignores 0 values
-}
lcm' ∷ Integer → Integer → Integer
lcm' x 0 = x
lcm' 0 y = y
lcm' x y = lcm x y
