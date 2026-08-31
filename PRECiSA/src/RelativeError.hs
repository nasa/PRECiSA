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
-- exact real-valued result.  When Kodiak will not perform that division, a
-- FALLBACK bounds abs(r) from below instead and divides the absolute bound by
-- the floor; see "The fallback" below.
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
-- absolute error is identically zero, and +infinity when NEITHER route gives a
-- finite ratio: the divisor's interval enclosure contains zero, so Kodiak will
-- not divide, and no positive floor on abs(r) could be proved either.
-- +infinity is a sound bound, not a failure.
--
-- A Kodiak FAILURE is not a bound.  'RelError' deliberately has no
-- constructor for it and 'computeRelError' returns 'Either String RelError',
-- so a tool failure -- an unsupported constructor, a domain violation such as
-- sqrt of a negative interval, an allocation failure -- cannot be presented as
-- a proved result on any output path.  Kodiak throws for 45 distinct
-- conditions; only the two whose message ends "division by an interval that
-- contains zero" are not failures -- those are the ones that hand over to the
-- fallback.
--
-- The FALLBACK.  Kodiak's division guard tests the divisor's interval
-- ENCLOSURE on the current box, and it fires on the TOP box, before
-- branch-and-bound subdivides anything, so raising --max-depth cannot rescue
-- the ratio.  But the enclosure is not the true minimum: when a variable occurs
-- several times in r, interval dependency widens the enclosure far past the
-- range of values r actually takes, and the widened enclosure can straddle zero
-- for an r that is nowhere near it.  kepler0 with every input in [4, 6.36] is
-- the clean case -- the naive enclosure of r is [-93.93, 93.93], while the true
-- minimum of abs(r) is 20.86.
--
-- Minimizing abs(r) involves no division, so it cannot trip the guard, and,
-- unlike the guard, it DOES profit from subdivision -- subdividing is exactly
-- what breaks up the interval dependency.  So on a division refusal,
-- 'computeRelError' minimizes 'absExpr' of each alternative real result, takes
-- the smallest of the floors (the bound must hold whichever alternative the
-- path realizes), and reports maxE / d for a floor d > 0, where maxE is the
-- absolute bound the same path's absolute run produced.  Sound because
-- abs(r - fp) <= maxE and abs(r) >= d > 0 bound the numerator ABOVE and the
-- denominator BELOW, and both inequalities push the quotient up, so maxE / d
-- dominates the quotient at every point of the box ('relFromFloor').
--
-- The floor is a branch-and-bound result, so its quality tracks --max-depth and
-- the fallback only pays off above the default.  Measured floors: kepler0 gets
-- 0.0 at depth 7 and 20.2 at depth 14; kepler1 3.71 at depth 14; kepler2 0.0 at
-- depths 7 and 14, 5.33 at 18, 91.19 at 22.  At the default depth 7 all three
-- still report +infinity, and that is expected, not a bug to work around: no
-- new flag exists for the fallback, it uses the --max-depth and --precision the
-- rest of the analysis uses, so the floor is proved over exactly the box and at
-- exactly the resolution the reported bound covers.
--
-- maxE / d is formed over 'Rational' and converted to 'Double' rounding UP
-- ('safeQuotient').  Hardware division rounds to nearest and so can land one
-- ulp BELOW the exact quotient, and a value below the exact quotient is not a
-- bound -- the same constant is emitted as the PVS lemma's rel.
--
-- Nothing here tries to PREDICT Kodiak.  Two attempts were made and both were
-- wrong, in the same way:
--
--   * Predicting the enclosure.  Kodiak's division guard tests the divisor's
--     interval ENCLOSURE, not its true minimum, and the two differ.  For
--     q(x) = x*x - x + 1 on [0,1], Kodiak's own minimization proves
--     min abs(q) = 0.75 > 0, yet evaluating 1/abs(q) still aborts because the
--     enclosure at the top box is [0,2].  So a positive floor on the
--     denominator does not make DIVISION safe, and no test on the PRECiSA AST
--     can decide in advance whether Kodiak will divide.  (Such a floor is worth
--     computing, though -- just not for that.  It bounds the ratio WITHOUT
--     dividing, which is the fallback described above; what does not work is
--     using it to predict or unlock the ratio run.)
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
-- TCC requiring a nonzero divisor, and discharging it would mean proving the
-- real result bounded away from zero inside PVS.  The multiplicative form has
-- no TCC, the zero-error case reads 0 <= 0, and +infinity is simply no lemma
-- emitted.  This is also why the fallback needs NO new lemma and no
-- denominator lemma: the obligation is already division-free and is discharged
-- by E <= maxE and abs(r) >= d giving rel * abs(r) >= maxE >= E, so only the
-- constant differs from a ratio-derived certificate.
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
import Numeric.IEEE (succIEEE)
import Operators

-- | A sound over-approximation of |r - fp| / |r| over the input ranges.
--
--   'RelInfinite' is a sound bound, not a failure: it means neither route to a
--   ratio worked: the divisor's interval enclosure contains zero, so Kodiak
--   will not divide, AND no positive lower bound on @abs(r)@ could be proved
--   either, so neither 'ratioExpr' nor 'relFromFloor' yields a finite ratio.
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

-- | The expression a FALLBACK run minimizes: the magnitude of one real-result
--   alternative.
--
--   It contains NO DIVISION, which is the whole point -- Kodiak's
--   construction- and evaluation-time guards fire only on division, so
--   minimizing this cannot trip them however close to zero the enclosure of
--   @r@ comes. And unlike the guard, a minimization BENEFITS from
--   branch-and-bound: subdividing the box breaks up the interval dependency
--   that made the naive enclosure straddle zero in the first place.
absExpr :: AExpr -> EExpr
absExpr = UnaryOp AbsOp

-- | The smallest 'Double' that is greater than or equal to the EXACT quotient
--   @n / d@.
--
--   ROUNDING UP IS MANDATORY FOR SOUNDNESS, and this is why the quotient is not
--   simply @n / d@ in 'Double'. The value returned here is reported as a bound
--   on the relative error and is emitted as the constant @rel@ of the PVS
--   lemma @abs(fp - r) <= rel * abs(r)@. Hardware division rounds to NEAREST,
--   so it can round DOWN, producing a constant strictly below the real quotient
--   @maxE / d@ -- and a bound that is one ulp too small is not a bound. The
--   exact quotient is therefore formed over 'Rational', where no rounding
--   happens at all, and only the final conversion to 'Double' is rounded, in
--   the one direction that keeps the inequality: upwards.
--
--   Overflow needs no special case: 'fromRational' gives an infinity, which
--   'classifyRatio' turns into 'RelInfinite' -- still sound, just uninformative.
safeQuotient :: Double -> Double -> Double
safeQuotient n d = roundUpFromRational (toRational n / toRational d)

-- | Convert a 'Rational' to the smallest 'Double' not less than it.
--
--   'fromRational' rounds to nearest and so may land BELOW the exact value;
--   when it does, the next representable 'Double' above is the smallest one
--   that is not, and 'succIEEE' is exactly that step. A non-finite result is
--   returned as is: an infinity already dominates the exact value, and there is
--   no exact value to compare a NaN against.
roundUpFromRational :: Rational -> Double
roundUpFromRational q
  | isNaN nearest || isInfinite nearest = nearest
  | toRational nearest >= q             = nearest
  | otherwise                           = succIEEE nearest
  where
    nearest = fromRational q :: Double

-- | The relative bound implied by an absolute bound @maxE@ on the error and a
--   proven floor @d@ on the magnitude of the exact result.
--
--   SOUNDNESS. On every point of the box @abs(r - fp) <= maxE@ and
--   @abs(r) >= d > 0@, so
--
--   > abs(r - fp) / abs(r) <= maxE / d
--
--   -- the numerator is bounded above and the denominator below, and both
--   inequalities push the quotient UP, so the quotient of the two extremes
--   dominates the quotient at every point. 'safeQuotient' then makes sure the
--   floating-point rendering of @maxE / d@ does not undo that.
--
--   A floor that is not STRICTLY positive proves nothing about the quotient, so
--   it yields 'RelInfinite'; so does a floor that is not finite, which is not a
--   floor at all but a sign that the minimization returned nonsense.
relFromFloor :: Double -> Double -> RelError
relFromFloor maxE d
  | not (isFiniteDouble d) || d <= 0 = RelInfinite
  | not (isFiniteDouble maxE)        = RelInfinite
  | otherwise                        = classifyRatio (safeQuotient maxE d)

-- | True for a 'Double' that is neither a NaN nor an infinity.
isFiniteDouble :: Double -> Bool
isFiniteDouble x = not (isNaN x) && not (isInfinite x)

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
--   @maxE@ MUST be the absolute error bound the same path's absolute run
--   produced -- the 'Kodiak.Runner.maximumUpperBound' of that run, the very
--   number the report and the certificate carry -- because the fallback below
--   divides it by a floor on the exact result, and dividing a DIFFERENT
--   absolute bound would certify a relative bound for a quantity nothing else
--   in the analysis mentions. It is only read on the fallback path; when the
--   ratio maximization succeeds, the bound comes from Kodiak and @maxE@ is
--   unused.
--
--   Returns 'Left' for a Kodiak FAILURE, which is an analysis error and must
--   never be reported as a relative error value. 'KodiakDivByZero' is not a
--   failure: it triggers the denominator-floor fallback below, which minimizes
--   'absExpr' of each real result and either proves a positive floor -- turned
--   into a bound by 'relFromFloor' -- or settles for 'RelInfinite'.
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
                -> EExpr -> Double -> [AExpr] -> IO (Either String RelError)
computeRelError searchParams fname varBinds err maxE reals
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
      case result of
        Right kr               -> forceRelResult $ Right
                                    $ classifyRatio (maximumUpperBound kr)
        -- Not a failure and no longer the end of the road: Kodiak would not
        -- divide, so try the division-free route instead.
        Left KodiakDivByZero   -> denominatorFloorBound >>= forceRelResult
        Left (KodiakError msg) -> forceRelResult $ Left msg
        Left KodiakOk          -> forceRelResult
                                    $ error $ "computeRelError: the guarded Kodiak API "
                                           ++ "returned Left KodiakOk, which cannot happen."

    -- The FALLBACK, reached only when Kodiak refused to divide.
    --
    -- Kodiak's division guard tests the divisor's interval ENCLOSURE on the
    -- current box, and it fires on the TOP box, before branch-and-bound
    -- subdivides -- so a deeper search cannot rescue the ratio. Minimizing
    -- @abs(r)@ has no division in it, cannot trip the guard, and DOES profit
    -- from subdivision, because subdividing is what breaks up the interval
    -- dependency that made the enclosure straddle zero. With a proven floor
    -- @d > 0@ on @abs(r)@, @maxE / d@ bounds the relative error (see
    -- 'relFromFloor').
    --
    -- One minimization per ALTERNATIVE real result, and the floor is the
    -- MINIMUM of theirs: the bound has to hold whichever alternative the path
    -- actually realizes, so the smallest floor is the only sound choice. A
    -- minimization that fails itself contributes no floor, and without a floor
    -- for every alternative there is nothing sound to divide by, so the whole
    -- fallback settles for 'RelInfinite' -- never for a 'Left', because
    -- Kodiak's refusal to divide was not an analysis failure and must not be
    -- reported as one.
    denominatorFloorBound = do
      floors <- mapM minimizeAbsReal reals
      return $ Right $ case sequence floors of
        Nothing -> RelInfinite
        Just ds -> relFromFloor maxE (minimum ds)

    minimizeAbsReal r = do
      result <- runMinMaxGuarded (minMaxInput r)
      return $ case result of
        Right kmm -> Just (kmmoMinimumLowerBound kmm)
        Left _    -> Nothing

    -- The same box, depth and precision as the ratio run, so the floor is
    -- proved over exactly the region the reported bound covers.
    minMaxInput r = KodiakMinMaxInput
                     { kmmiName       = fname ++ "_rel_denominator"
                     , kmmiExpression = absExpr r
                     , kmmiBindings   = varBinds
                     , kmmiMaxDepth   = maximumDepth searchParams
                     , kmmiPrecision  = minimumPrecision searchParams
                     }

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
