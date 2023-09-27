module TransitionMatrix (
    -- * Specialized Representation
    TransitionMatrix (),
    TransitionMeasureDiagnosis (..),

    -- * Special Constructors
    discreteCrossGap,
    discreteMetric,
    linearNorm,

    -- * General Constructors
    fromList,
    fromColumns,
    fromRows,
    generate,
) where

import TransitionMatrix.Diagnosis

