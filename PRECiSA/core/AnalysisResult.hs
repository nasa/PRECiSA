-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.

module AnalysisResult where

import AbsPVSLang
import AbstractDomain (Conditions)
import Common.ControlFlow (ControlFlow)
import Common.DecisionPath (LDecisionPath)
import Kodiak.Runner (KodiakResult)
import RelativeError (RelError(..))

-- | Per-path facts drawn from the abstract semantics, before any Kodiak run.
--   'piErrExpr' is the RAW 'eExpr' from the 'ACeb': 'initAExpr' has NOT been
--   applied, and must be applied exactly once downstream.
data PathInput = PathInput
  { piConds     :: Conditions
  , piPath      :: LDecisionPath
  , piFlow      :: ControlFlow
  , piErrExpr   :: AExpr
  , piFpExprs   :: [FAExpr]
  , piRealExprs :: [AExpr]
  } deriving Show

-- | The relative error outcome for one path. Three-way on purpose: a Kodiak
--   FAILURE is not a bound and must never be presented as one, and it must
--   also be distinguishable from the feature simply being switched off.
data RelErrorResult
  = RelErrorOff             -- ^ --relative-error was not given
  | RelErrorFailed String   -- ^ Kodiak failed; the message. NOT a bound.
  | RelErrorBound RelError  -- ^ a sound bound (possibly 'RelInfinite')
  deriving (Show, Eq)

-- | Classify the outcome of 'RelativeError.computeRelError'. A 'Left' is a
--   Kodiak failure and stays a failure: it must never be reported as a bound.
toRelErrorResult :: Either String RelError -> RelErrorResult
toRelErrorResult (Left msg)  = RelErrorFailed msg
toRelErrorResult (Right rel) = RelErrorBound rel

-- | Everything computed for one decision path of one result field.
data PathResult = PathResult
  { prConds     :: Conditions
  , prPath      :: LDecisionPath
  , prFlow      :: ControlFlow
  , prKodiak    :: KodiakResult   -- ^ absolute error bound
  , prErrExpr   :: AExpr          -- ^ symbolic absolute error expression
  , prFpExprs   :: [FAExpr]
  , prRealExprs :: [AExpr]        -- ^ alternative real results for this path
  , prRelError  :: RelErrorResult
  } deriving Show

-- | Analysis results for one function.
data FunResult = FunResult
  { frName   :: String
  , frType   :: PVSType
  , frArgs   :: [Arg]
  , frFields :: [(ResultField, [PathResult])]
  } deriving Show

-- | One line of the analysis report.
data FunSummary = FunSummary
  { fsName        :: String
  , fsField       :: ResultField
  , fsStable      :: Double
  , fsUnstable    :: Maybe Double
  , fsRelStable   :: RelErrorResult
  , fsRelUnstable :: RelErrorResult
  } deriving Show

-- | Aggregate the relative error over a set of paths.
--
--   A FAILURE on any path wins: if even one path could not be bounded, the
--   function has not been bounded, and reporting the best of the others would
--   overstate what was established. 'RelInfinite' then dominates finite
--   bounds, and otherwise the maximum is taken.
worstRelError :: [PathResult] -> RelErrorResult
worstRelError = foldr (combine . prRelError) RelErrorOff
  where
    -- A failure anywhere wins, and the leftmost message is the one kept.
    combine (RelErrorFailed msg) _                    = RelErrorFailed msg
    combine _                    (RelErrorFailed msg) = RelErrorFailed msg
    combine RelErrorOff          acc                  = acc
    combine (RelErrorBound rel)  RelErrorOff          = RelErrorBound rel
    combine (RelErrorBound rel)  (RelErrorBound acc)  = RelErrorBound (worse rel acc)

    worse RelInfinite   _             = RelInfinite
    worse _             RelInfinite   = RelInfinite
    worse (RelFinite a) (RelFinite b) = RelFinite (max a b)
