-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module RelativeError where

-- Design rationale
-- ================
--
-- Relative error is bounded by maximizing the RATIO E(x) / abs(r(x)) in one
-- Kodiak run, where E is the absolute error expression and r the program's
-- exact real-valued result.
--
-- Why the ratio and not max E / min abs(r).  Branch-and-bound evaluates
-- numerator and denominator on the SAME sub-box, so their correlation
-- survives.  Bounding them independently finds the numerator's maximum and the
-- denominator's minimum at opposite ends of the input box and loses orders of
-- magnitude: for x / abs(x*x) over x in [1,1000], whose true supremum is 1,
-- the ratio gives [1.0, 16.6] at depth 7 and exactly [1.0, 1.0] at depth 14,
-- while the decoupled quotient gives 1000.
--
-- The bound is a TOTAL function into [0, +infinity].  It is 0 when the
-- absolute error is identically zero, and +infinity when the divisor's
-- interval enclosure contains zero.  +infinity is a sound bound, not a
-- failure.
--
-- A Kodiak FAILURE is not a bound.  'RelError' deliberately has no
-- constructor for it and 'computeRelError' returns 'Either String RelError',
-- so a tool failure -- an unsupported constructor, a domain violation such as
-- sqrt of a negative interval, an allocation failure -- cannot be presented as
-- a proved result on any output path.  Kodiak throws for 45 distinct
-- conditions; only the two whose message ends "division by an interval that
-- contains zero" mean +infinity.
--
-- Nothing here tries to PREDICT Kodiak.  Two attempts were made and both were
-- wrong, in the same way:
--
--   * Predicting the enclosure.  Kodiak's division guard tests the divisor's
--     interval ENCLOSURE, not its true minimum, and the two differ.  For
--     q(x) = x*x - x + 1 on [0,1], Kodiak's own minimization proves
--     min abs(q) = 0.75 > 0, yet evaluating 1/abs(q) still aborts because the
--     enclosure at the top box is [0,2].  So a positive floor on the
--     denominator does not make division safe, and computing one buys nothing.
--
--   * Predicting the constant folding.  Kodiak folds far more aggressively
--     than 'simplAExpr' (Abs(val) -> val, -(val) -> val, empty polynomial ->
--     val 0, transcendentals on literals).  'x - x' still reaches
--     real_create_division as a literal zero divisor even though the PRECiSA
--     AST is a subtraction.  'literalDenomContainsZero' below is therefore an
--     optimisation only; the real defence is asking Kodiak and catching the
--     answer.
--
-- Catching the answer needs C++.  Kodiak signals these conditions by throwing,
-- and the exception escapes extern "C" into the FFI and reaches
-- std::terminate, killing the process; no Haskell handler can intercept it.
-- Kodiak is left unmodified and PRECiSA owns a shim (cbits/kodiak_shim.cpp)
-- that catches at its own boundary and returns status codes.  After a non-Ok
-- status the MinMaxSystem is dead: its bounds must not be read, because the
-- getters throw too.
--
-- The ABSOLUTE path goes through the same shim.  It is not a relative-error
-- concern, but it shares 'maximizeAndReadBounds' with this one, so the two
-- cannot hand Kodiak different systems; the difference is only what they do
-- with a non-Ok status.  Relative error reports it and carries on with the
-- absolute analysis intact; absolute error re-throws it as a Haskell exception
-- ('KodiakBuildFailed' or 'KodiakMaximizeFailed'), which is a catchable
-- failure rather than the SIGABRT the raw entry points used to produce.
--
-- So does the FUNCTION-CALL ABSTRACTION path, which encloses a callee's body
-- with Kodiak's minmax rather than maximizing it.  It has its own shim entry
-- point and its own exception ('KodiakMinMaxFailed'), but it shares
-- 'setupSystem' with the other two, so all three configure Kodiak identically;
-- an abstraction run at a different depth or precision would bound a different
-- quantity than the analysis it feeds.  Every min-max entry point PRECiSA
-- calls is now guarded.  So is the PAVER ('Kodiak.Paver', reached only with
-- --paving), a separate evaluator with its own shim entry points and its own
-- exceptions ('KodiakPaveFailed', 'KodiakSavePavingFailed'); it has nothing to
-- do with relative error but shared the same crash.
--
-- The PVS certificate states the bound MULTIPLICATIVELY,
-- abs(fp - r) <= rel * abs(r), never as a quotient.  Division in PVS emits a
-- TCC requiring a nonzero divisor, and discharging it would require proving
-- the real result bounded away from zero -- reintroducing the floor this
-- design exists without.  The multiplicative form has no TCC, the
-- zero-error case reads 0 <= 0, and +infinity is simply no lemma emitted.
--
-- Known gap: 'unfoldFunCallInCeb' rewrites 'conds' and 'eExpr' but never
-- 'rExprs', so with --unfold-fun-calls off a path containing a function call
-- still carries 'EFun', which the Kodiak translation cannot handle.  Such
-- paths report a failure rather than a bound.  It fails softly, leaving the
-- absolute analysis intact.

import AbsPVSLang
import AbsSpecLang
import Control.Exception (SomeAsyncException, SomeException, catch, evaluate,
                          fromException, throwIO)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Kodiak.Kodiak (KodiakStatus(..))
import Kodiak.Paver (SearchParameters(..))
import Kodiak.Runner
import Operators

-- | A sound over-approximation of |r - fp| / |r| over the input ranges.
--
--   'RelInfinite' is a sound bound, not a failure: it means the divisor's
--   interval enclosure contains zero, so no finite ratio can be certified.
--   A Kodiak *failure* is deliberately NOT representable here — that is an
--   analysis error, not a relative error value, and callers must keep the two
--   apart.
data RelError = RelFinite Double | RelInfinite
  deriving (Show, Eq)

-- | Classify Kodiak's 'maximumUpperBound' for the ratio expression.
--   A non-finite value cannot bound anything, so it degrades to 'RelInfinite'.
classifyRatio :: Double -> RelError
classifyRatio ub
  | isNaN ub      = RelInfinite
  | isInfinite ub = RelInfinite
  | otherwise     = RelFinite ub

-- | True when the (already simplified) error expression is identically zero,
--   as it is for 'TInt' results where 'initErrorMark' maps the error mark to
--   'Int 0'. Such paths have relative error 0 and need no Kodiak run.
isZeroError :: EExpr -> Bool
isZeroError (Int 0) = True
isZeroError (Rat 0) = True
isZeroError _       = False

-- | The expression whose maximum over the input box bounds the relative error.
--   Numerator and denominator are maximized together so Kodiak's
--   branch-and-bound evaluates them on the same sub-box, preserving their
--   correlation; bounding them separately loses orders of magnitude.
ratioExpr :: EExpr -> [AExpr] -> EExpr
ratioExpr _   []  = error "ratioExpr: no real expression for this path."
ratioExpr err [r] = ratio err r
ratioExpr err rs  = MaxErr (map (ratio err) rs)

ratio :: EExpr -> AExpr -> EExpr
ratio err r = BinaryOp DivOp err (UnaryOp AbsOp r)

-- | True when a real-result alternative is syntactically a LITERAL whose value
--   interval contains zero.
--
--   THIS IS AN OPTIMISATION ONLY. IT IS NOT THE SAFETY MECHANISM, AND IT IS NOT
--   COMPLETE. It exists purely to skip a pointless Kodiak round-trip for the
--   obvious case.
--
--   It cannot be the safety mechanism because it inspects the PRECiSA AST,
--   whereas Kodiak's construction-time divisor check (Real.cpp:214,
--   @e2.isVal() && e2.val().contains(0)@) fires on the KODIAK term, after
--   Kodiak's own constant folding -- which is far more aggressive than
--   'simplAExpr': @Abs(val)@, @-(val)@, an empty polynomial such as @x - x@,
--   and every transcendental applied to a literal all fold to a value. No
--   enumeration of PRECiSA constructors can predict that.
--
--   The actual defence is 'Kodiak.Runner.createDivision', which builds every
--   division through the guarded 'Kodiak.Kodiak.realCreateDivision' and
--   re-throws a construction failure as a catchable
--   'Kodiak.Runner.KodiakBuildFailed'. 'computeRelError' turns that into
--   'RelInfinite' whatever shape the divisor had.
literalDenomContainsZero :: AExpr -> Bool
literalDenomContainsZero (Int n) = n == 0
literalDenomContainsZero (Rat q) = q == 0
literalDenomContainsZero (Interval lb ub) = lb <= 0 && 0 <= ub
literalDenomContainsZero _       = False

-- | Reduce a rendered exception to its FIRST LINE, trimmed.
--
--   WHY, and not merely for tidiness: this string is USER FACING -- it is the
--   whole value of the JSON field @relativeStableErrorFailure@ that the VSCode
--   extension displays. 'show' on an 'ErrorCall' raised by 'error' appends the
--   'HasCallStack' backtrace, so without this the field carried
--   @"CallStack (from HasCallStack): error, called at src\/Kodiak\/Runner.hs:504:20 ..."@.
--   A source file and line number is noise to whoever reads the field, and it
--   ROTS SILENTLY: nothing recomputes it, so the moment 'Kodiak.Runner' gains
--   or loses a line the message points at the wrong place. The embedded
--   newlines are their own problem for a consumer parsing a JSON string field.
--
--   Only the RENDERING changes. A message that is already one line -- every
--   'Kodiak.Kodiak.KodiakError' from the shim, e.g.
--   @"Kodiak (eval): sqrt expects a nonnegative interval"@ -- comes back
--   unchanged, so no diagnostic detail Kodiak itself reported is lost.
oneLineMessage :: String -> String
oneLineMessage = trim . takeWhile (/= '\n')
  where trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Bound the relative error of one decision path.
--
--   The error expression MUST be the same processed expression the absolute
--   error run uses (after 'initAExpr', 'simplAExpr', and 'replaceFunCallErr'
--   when function calls are not unfolded). Bounding a differently processed
--   expression would certify a different quantity than the absolute
--   certificate.
--
--   Returns 'Left' for a Kodiak FAILURE, which is an analysis error and must
--   never be reported as a relative error value. Only 'KodiakDivByZero'
--   becomes 'RelInfinite'.
--
--   THE RELATIVE PATH FAILS SOFT. Relative error is opt-in and must never sink
--   an otherwise-successful absolute analysis, so ANY Haskell exception raised
--   while bounding the ratio is turned into a 'Left' rather than being allowed
--   to kill the process. This matters because the REAL result expressions are
--   handed over raw: unlike the error expression, they are never rewritten by
--   'AbstractSemantics.unfoldFunCallInCeb' (which touches only @conds@ and
--   @eExpr@, never @rExprs@), and they never pass through
--   'initErrorMark'/'simplAExpr'. They can therefore still contain
--   constructors 'Kodiak.Runner.run'' has no case for -- 'EFun' above all, but
--   also 'ArrayElem', 'RMap', 'RFold', 'RForLoop', 'FromFloat', 'Prec', 'RIte',
--   'RListIte', 'Infinity', 'RUnstWarning', and @BinaryOp PowOp _ (Int n)@ for
--   @n \/= 2@ -- whose catch-all raises an 'ErrorCall'. In particular, a
--   program with FUNCTION CALLS yields 'Left' here (surfaced as
--   'AnalysisResult.RelErrorFailed') whenever @--unfold-fun-calls@ is off,
--   because @rExprs@ still holds the un-unfolded 'EFun'. Teaching the unfolder
--   or the runner about those constructors is a separate design question; not
--   aborting the run is not.
computeRelError :: SearchParameters -> FunName -> [VarBind]
                -> EExpr -> [AExpr] -> IO (Either String RelError)
computeRelError searchParams fname varBinds err reals
  | isZeroError err = return $ Right (RelFinite 0)
  -- A path with no real result has nothing to divide by; that is an analysis
  -- error, not an unbounded ratio, and it must not abort the whole run.
  | null reals = return $ Left "no real expression for this path"
  -- Fast path only: skips a Kodiak round-trip for an obviously zero divisor.
  -- Kodiak's guarded division is what actually keeps the process alive.
  | any literalDenomContainsZero reals = return $ Right RelInfinite
  | otherwise = maximizeRatio `catch` softFail
  where
    -- The result is FORCED before it leaves the handler's scope. 'run'' builds
    -- its catch-all 'error' lazily, and a 'catch' around a computation that
    -- merely RETURNS a thunk cannot catch that thunk blowing up at the print
    -- site, long after the handler is gone.
    maximizeRatio = do
      result <- runMaximizeGuarded kodiakInput
      forceRelResult $ case result of
        Right kr                 -> Right $ classifyRatio (maximumUpperBound kr)
        Left KodiakDivByZero     -> Right RelInfinite
        Left (KodiakError msg)   -> Left msg
        Left KodiakOk            -> error $ "computeRelError: the guarded Kodiak API "
                                         ++ "returned Left KodiakOk, which cannot happen."

    -- Every synchronous exception becomes a failure MESSAGE on the existing
    -- 'AnalysisResult.RelErrorFailed' channel, kept to one line by
    -- 'oneLineMessage' because the channel ends up in user-facing JSON. An
    -- asynchronous exception (a Ctrl-C, a timeout) is not ours to swallow and
    -- is re-thrown unchanged.
    softFail :: SomeException -> IO (Either String RelError)
    softFail e =
      case fromException e :: Maybe SomeAsyncException of
        Just asyncExc -> throwIO asyncExc
        Nothing       -> return $ Left ("relative error unavailable: "
                                        ++ oneLineMessage (show e))

    kodiakInput = KI { kiName       = fname ++ "_rel"
                     , kiExpression = ratioExpr err reals
                     , kiBindings   = varBinds
                     , kiMaxDepth   = maximumDepth searchParams
                     , kiPrecision  = minimumPrecision searchParams
                     }

-- | Force a 'computeRelError' result to the point where no bottom can still be
--   hiding in it, so that a 'Control.Exception.catch' wrapping its production
--   actually sees the exception. Both payloads are flat, so seq'ing the field
--   of whichever constructor is present is full forcing.
forceRelResult :: Either String RelError -> IO (Either String RelError)
forceRelResult r = evaluate (go r)
  where
    go v@(Left msg)              = length msg `seq` v
    go v@(Right RelInfinite)     = v
    go v@(Right (RelFinite ub))  = ub `seq` v
