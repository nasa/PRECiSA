-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


{-# LANGUAGE MultiParamTypeClasses  #-}

module Kodiak.Runner where

import Kodiak.Kodiak
import Kodiak.Runnable
import AbsPVSLang
import AbsSpecLang
import Numeric (fromRat)
import Control.Exception (Exception,throw,throwIO,try,AssertionFailed(..))
import Control.Monad ((>=>),foldM)
import Data.Maybe(fromMaybe)
import Foreign.C

data KodiakInput = KI { kiName :: String,
                        kiExpression :: EExpr,
                        kiBindings :: [VarBind],
                        kiMaxDepth :: CUInt,
                        kiPrecision :: CUInt }
  deriving Show

variableMapFromBinds :: [VarBind] -> VariableMap
variableMapFromBinds binds = VMap $ zip variableNames [(0::CUInt)..]
  where
    variableNames = map (\(VarBind s field _ _ _) -> cVarName s field) binds

data KodiakResult = KR { maximumLowerBound :: Double,
                         maximumUpperBound :: Double
                       } deriving Show

-- | A Kodiak expression could not be BUILT: 'realCreateDivision' reported a
--   construction-time failure while the term was being assembled.
--
--   Kodiak signals such failures by throwing a C++ exception, which kills the
--   process outright; re-throwing the decoded status as a Haskell exception
--   turns that abort into something callers can actually catch (see
--   'runMaximizeGuarded').
newtype KodiakBuildFailed = KodiakBuildFailed KodiakStatus
  deriving Show

instance Exception KodiakBuildFailed

-- | A Kodiak min-max system could not be MAXIMIZED, or its bounds could not be
--   read back afterwards: an EVALUATION-time failure, thrown by Kodiak from
--   inside the branch-and-bound loop long after the expression was built.
--
--   Division by an interval enclosure containing zero is the common case, and
--   it is not decidable at construction time -- the top box can enclose zero
--   for a divisor that is nowhere zero. Callers that can carry on without a
--   bound should use 'runMaximizeGuarded', which returns the status instead.
newtype KodiakMaximizeFailed = KodiakMaximizeFailed KodiakStatus

-- | Written out rather than derived because this string is what PRECiSA prints
--   when the exception reaches the top level: GHC's default handler SHOWS the
--   exception, so 'show' is the user-facing message and has to read like one.
instance Show KodiakMaximizeFailed where
  show (KodiakMaximizeFailed status) =
    "Kodiak failed while maximizing the error expression: " ++ describe status
    where
      describe KodiakOk          = "reported success (should not happen)"
      describe KodiakDivByZero   = "division by an interval that contains zero"
      describe (KodiakError msg) = msg

instance Exception KodiakMaximizeFailed

-- | A Kodiak min-max system could not be MINIMIZED AND MAXIMIZED, or its
--   bounds could not be read back afterwards.
--
--   The min-max sibling of 'KodiakMaximizeFailed', and deliberately a separate
--   type rather than a reuse of it: this failure comes from enclosing a
--   CALLEE'S REAL-VALUED BODY over a caller-supplied box during function-call
--   error abstraction, not from maximizing an error expression, so it needs its
--   own message to be diagnosable, and a caller that wants to tolerate one of
--   the two operations failing must be able to name which.
newtype KodiakMinMaxFailed = KodiakMinMaxFailed KodiakStatus

-- | Written out rather than derived for the same reason as
--   'Show' 'KodiakMaximizeFailed': GHC's top handler SHOWS the exception, so
--   this string is the user-facing message.
instance Show KodiakMinMaxFailed where
  show (KodiakMinMaxFailed status) =
    "Kodiak failed while enclosing an expression (min-max): " ++ describe status
    where
      describe KodiakOk          = "reported success (should not happen)"
      describe KodiakDivByZero   = "division by an interval that contains zero"
      describe (KodiakError msg) = msg

instance Exception KodiakMinMaxFailed

-- | Create the min-max system described by a 'KodiakInput', configure it, bind
--   its variables, and build its expression.
--
--   Shared by every path that hands Kodiak a system -- 'run' and
--   'runMaximizeGuarded' for the maximize side, 'minmaxAndReadBounds' for the
--   min-max side -- so they cannot drift: they must hand Kodiak the same
--   system, at the same precision and depth, for the relative bound to certify
--   the same quantity as the absolute one and for the function-call
--   abstraction to enclose the quantity the main analysis bounds.
--
--   Throws 'KodiakBuildFailed' if the expression cannot be built.
setupSystem :: KodiakInput -> IO (PMinMaxSystem, PReal)
setupSystem kInput =
  do
    let sysName       = kiName kInput
        errorExpr     = kiExpression kInput
        varRanges     = kiBindings kInput
        variableMap   = variableMapFromBinds varRanges
        thisPrecision = kiPrecision kInput
        thisMaxDepth  = kiMaxDepth kInput
    cName <- newCString sysName
    pSys <- minmax_system_create cName
    minmax_system_set_maxdepth pSys thisMaxDepth
    minmax_system_set_precision pSys (negate (fromInteger $ toInteger thisPrecision))
    mapM_ (`run` pSys) varRanges
    pExpr <- run errorExpr variableMap
    return (pSys, pExpr)

-- | Build the system, maximize it, and read its bounds back -- every step
--   through Kodiak's guarded API, so a Kodiak C++ exception becomes a status
--   instead of a @std::terminate@.
--
--   This is the WHOLE body of both 'run' and 'runMaximizeGuarded'; they differ
--   only in what they do with a non-'KodiakOk' status. Keeping it in one place
--   is not tidiness: the two must hand Kodiak the same system, at the same
--   precision and depth, and read the same bounds, for the relative bound to
--   certify the same quantity as the absolute one.
--
--   Build failures arrive as a thrown 'KodiakBuildFailed' rather than as a
--   'Left', because they are thrown from deep inside the expression
--   translation in 'createDivision'; each caller decides whether to catch it.
--
--   Resource note: this inherits the leaks of 'setupSystem' -- the 'newCString'
--   for the system name and for each 'VarBind', the strings and
--   'real_create_variable' nodes built per variable, and the
--   'minmax_system_create' that is never destroyed. Being guarded does NOT make
--   it resource-clean.
maximizeAndReadBounds :: KodiakInput -> IO (Either KodiakStatus KodiakResult)
maximizeAndReadBounds kInput =
  do
    (pSys, pExpr) <- setupSystem kInput
    maximized <- maximizeGuarded pSys pExpr
    case maximized of
      -- On a non-'KodiakOk' status the system is dead and its bounds must not
      -- be read, so return before touching them.
      Left status -> return (Left status)
      Right ()    -> readBounds pSys
  where
    -- The upper bound is read FIRST and never discarded because of the lower
    -- one: 'lb_of_max' guards on a non-empty @min_point_@ while 'ub_of_max'
    -- guards on a non-empty point set (MinMax.cpp:54,60), so the lower bound
    -- can fail in a state where the upper bound is perfectly sound. The upper
    -- bound is what a maximization is for, so the lower one is best-effort.
    readBounds pSys = do
      eUb <- maximumUpperBoundGuarded pSys
      case eUb of
        Left status -> return (Left status)
        Right ub -> do
          eLb <- maximumLowerBoundGuarded pSys
          -- Any value below the maximum is a sound lower bound, so an
          -- unreadable one degrades to -infinity rather than to a failure.
          let lb = either (const (-1/0)) (fromRational . toRational) eLb
          return $ Right KR { maximumLowerBound = lb,
                              maximumUpperBound = fromRational (toRational ub) }

-- | The absolute-error path: a bound is the only acceptable outcome, so a
--   Kodiak failure is re-thrown.
--
--   It is re-thrown as a HASKELL exception ('KodiakBuildFailed' from the build,
--   'KodiakMaximizeFailed' from the maximization or the bound getters) rather
--   than left to abort the process, which is what the raw
--   'minmax_system_maximize' and the raw bound getters used to do: a divisor
--   whose enclosure contains zero killed PRECiSA with SIGABRT out of the middle
--   of an analysis. The type is unchanged and every input that produced a bound
--   before still produces the same bound.
instance KodiakRunnable KodiakInput () KodiakResult where
  run kInput _ =
    maximizeAndReadBounds kInput
      >>= either (throwIO . KodiakMaximizeFailed) return

-- | The relative-error path: maximize like 'run', but report Kodiak's failures
--   as a status instead of throwing, so a missing relative bound can leave the
--   absolute analysis intact.
--
--   This catches both the exceptions thrown while BUILDING the expression
--   ('KodiakBuildFailed') and the statuses returned while maximizing it or
--   reading its bounds.
runMaximizeGuarded :: KodiakInput -> IO (Either KodiakStatus KodiakResult)
runMaximizeGuarded kInput =
  do
    result <- try (maximizeAndReadBounds kInput)
    case result of
      Left (KodiakBuildFailed status) -> return (Left status)
      Right statusOrBounds            -> return statusOrBounds

newtype VariableMap = VMap [(String,CUInt)] deriving Show

data KodiakMinMaxInput
  = KodiakMinMaxInput
  { kmmiName :: String
  , kmmiExpression :: EExpr
  , kmmiBindings :: [VarBind]
  , kmmiMaxDepth :: CUInt
  , kmmiPrecision :: CUInt
  }
  deriving Show

data KodiakMinMaxResult
  = KodiakMinMaxResult
  { kmmoMinimumLowerBound :: Double
  , kmmoMaximumUpperBound :: Double
  } deriving Show

-- | The same five fields as a 'KodiakInput', so that the min-max path can go
--   through the one 'setupSystem' instead of building a second, drifting copy
--   of it. The two records exist separately only because they are consumed by
--   different 'KodiakRunnable' instances; the system they describe is the same.
minMaxInputAsKodiakInput :: KodiakMinMaxInput -> KodiakInput
minMaxInputAsKodiakInput kInput =
  KI { kiName       = kmmiName kInput,
       kiExpression = kmmiExpression kInput,
       kiBindings   = kmmiBindings kInput,
       kiMaxDepth   = kmmiMaxDepth kInput,
       kiPrecision  = kmmiPrecision kInput }

-- | Build the system, minimize AND maximize it in one run, and read back the
--   minimum's lower bound and the maximum's upper bound -- every step through
--   Kodiak's guarded API, so a Kodiak C++ exception becomes a status instead
--   of a @std::terminate@.
--
--   The min-max counterpart of 'maximizeAndReadBounds', sharing its
--   'setupSystem' so the enclosure the function-call abstraction computes is
--   taken over the very same system, depth and precision as the bound the main
--   analysis reports.
--
--   Build failures arrive as a thrown 'KodiakBuildFailed', as in
--   'maximizeAndReadBounds'.
--
--   Resource note: inherits the leaks of 'setupSystem'; see
--   'maximizeAndReadBounds'.
minmaxAndReadBounds :: KodiakInput -> IO (Either KodiakStatus KodiakMinMaxResult)
minmaxAndReadBounds kInput =
  do
    (pSys, pExpr) <- setupSystem kInput
    minmaxed <- minmaxGuarded pSys pExpr
    case minmaxed of
      -- On a non-'KodiakOk' status the system is dead and its bounds must not
      -- be read, so return before touching them.
      Left status -> return (Left status)
      Right ()    -> readBounds pSys
  where
    -- Both bounds are load-bearing here: the caller uses them as an interval
    -- enclosure of the expression, so neither may be degraded to an infinity
    -- the way 'maximizeAndReadBounds' degrades its best-effort lower bound. An
    -- unreadable bound is reported as a failure rather than widened into a
    -- vacuous enclosure that would silently loosen every bound derived from it.
    --
    -- Read in the order the unguarded code read them, lower bound first, and
    -- stop at the first failure rather than poking a system that has already
    -- refused once.
    readBounds pSys = do
      eLb <- minimumLowerBoundGuarded pSys
      case eLb of
        Left status -> return (Left status)
        Right lb -> do
          eUb <- maximumUpperBoundGuarded pSys
          case eUb of
            Left status -> return (Left status)
            Right ub ->
              return $ Right KodiakMinMaxResult
                { kmmoMinimumLowerBound = fromRational (toRational lb),
                  kmmoMaximumUpperBound = fromRational (toRational ub) }

-- | The function-call error abstraction's path: an enclosure is the only
--   acceptable outcome, so a Kodiak failure is re-thrown as the Haskell
--   exception 'KodiakMinMaxFailed' (or 'KodiakBuildFailed' from the build)
--   rather than left to abort the process, which is what the raw
--   'minmax_system_minmax' and the raw bound getters used to do: a callee whose
--   body divided by an enclosure containing zero killed PRECiSA with SIGABRT
--   from inside 'FunctionCallErrorAbstraction'. The type is unchanged and every
--   input that produced an enclosure before still produces the same enclosure.
instance KodiakRunnable KodiakMinMaxInput () KodiakMinMaxResult where
  run kInput _ =
    minmaxAndReadBounds (minMaxInputAsKodiakInput kInput)
      >>= either (throwIO . KodiakMinMaxFailed) return

-- | The min-max sibling of 'runMaximizeGuarded': enclose the expression like
--   the 'KodiakRunnable' instance above, but report Kodiak's failures as a
--   status instead of throwing, so a caller that can carry on without an
--   enclosure is left standing.
--
--   Used by 'RelativeError.computeRelError' to MINIMIZE @abs(r)@ when the
--   relative-error ratio itself could not be maximized. That run must not be
--   able to abort the process either -- it is a second, opportunistic attempt
--   after a first one already failed -- so it goes through the same guarded API,
--   and, exactly like 'runMaximizeGuarded', it catches both the exception thrown
--   while BUILDING the expression ('KodiakBuildFailed') and the statuses
--   returned while enclosing it or reading its bounds.
runMinMaxGuarded :: KodiakMinMaxInput -> IO (Either KodiakStatus KodiakMinMaxResult)
runMinMaxGuarded kInput =
  do
    result <- try (minmaxAndReadBounds (minMaxInputAsKodiakInput kInput))
    case result of
      Left (KodiakBuildFailed status) -> return (Left status)
      Right statusOrBounds            -> return statusOrBounds

lookup' :: String -> VariableMap -> CUInt
lookup' str (VMap mappings) = fromMaybe (error $ "lookup': tried to search \"" ++ str ++ "\" in \"" ++ show mappings ++ "\"")
                                        (lookup str mappings)

rat2interval :: Rational -> (CDouble, CDouble)
rat2interval rat |  toRational ratDouble == rat = (ratDouble, ratDouble)
                 | (toRational ratDouble  < rat) && (toRational next >= rat)  = (ratDouble, next)
                 | (toRational ratDouble  > rat) && (toRational prev <= rat)  = (prev, ratDouble)
                 | otherwise = error $ "rat2interval failed with this values:"
                                ++ "\n\trat: " ++ show rat
                                ++ "\n\tratDouble: " ++ show ratDouble
                                ++ "\n\tprev: " ++ show prev
                                ++ "\n\tnext: " ++ show next
    where
        ratDouble = fromRat rat :: CDouble
        next = nextDouble rat
        prev = prevDouble rat
      --  next = nextUp' ratDouble
      --  prev = nextDown' ratDouble

instance KodiakRunnable AExpr VariableMap PReal where
  run err m = do
    kodiakVariables <- mapM createKodiakVariable variableToNumberMap
    run' err kodiakVariables
      where
        VMap variableToNumberMap = m

        createKodiakVariable (vName,varId) = do
             cName <- newCString vName
             pVariable <- real_create_variable varId cName
             return (vName,pVariable)

        createKodiakLocalVariable vName = do
             cName <- newCString vName
             real_create_local_variable cName

        runBinaryOperator l r vmap kodiakFunction = do
          pl <- run' l vmap
          pr <- run' r vmap
          kodiakFunction pl pr

        -- Kodiak refuses at CONSTRUCTION time to divide by a term its own
        -- constant folding has reduced to a value interval containing zero,
        -- and it refuses by throwing a C++ exception that aborts the process.
        -- The guarded builder decodes that into a status, which becomes a
        -- catchable 'KodiakBuildFailed'. EVERY division this runner builds
        -- goes through here, the two 'HalfUlp' cases included.
        createDivision pl pr =
          realCreateDivision pl pr >>= either (throwIO . KodiakBuildFailed) return

        runUnaryOperator e vmap kodiakFunction = do
          p <- run' e vmap
          kodiakFunction p

        runBinaryErrorOperator l le r re vmap kodiakFunction = do
          pl  <- run' l vmap
          ple <- run' le vmap
          pr  <- run' r vmap
          pre <- run' re vmap
          kodiakFunction pl ple pr pre

        runUnaryErrorOperator l le vmap kodiakFunction = do
          pl  <- run' l vmap
          ple <- run' le vmap
          kodiakFunction pl ple

        run' (Int i) vmap = run' (Rat $ toRational i) vmap
        run' (Rat r) _ = interval_create lb ub >>= real_create_value
          where (lb,ub) = rat2interval r
        run' (Interval lb ub) _ =
          if lblb > ubub
            then error ("Invalid Interval [" ++ show lb ++ "," ++ show ub ++ "]")
            else interval_create lblb ubub >>= real_create_value
          where
            (lblb,_) = rat2interval lb
            (_,ubub) = rat2interval ub
        run' (Var _ x)              vmap = run' (RealMark x ResValue) vmap
        run' (ListElem _ x _)       vmap = run' (RealMark x ResValue) vmap
        run' (TupleElem _ x idx)    vmap = run' (RealMark x (ResTupleIndex idx)) vmap
        run' (RecordElem _ x field) vmap = run' (RealMark x (ResRecordField field)) vmap
        run' (RealMark x field) vmap = case lookup (cVarName x field) vmap of
                                  Just pVar -> return pVar
                                  Nothing -> createKodiakLocalVariable x
        run' (ErrorMark {}) _ = throw (AssertionFailed "ErrorMark should not be used") >> return undefined
        run' (RLet defs body) vmap = run' body vmap >>= \pBody -> foldM f pBody (reverse defs)
            where
                f pBody (LetElem{letVar = name, letExpr = expr}) =
                    do
                        cName <- newCString name
                        pDefs <- run' expr vmap
                        real_create_letin cName pDefs pBody

        run' (BinaryOp AddOp l r) vmap = runBinaryOperator l r vmap real_create_addition
        run' (BinaryOp SubOp l r) vmap = runBinaryOperator l r vmap real_create_subtraction
        run' (BinaryOp MulOp l r) vmap = runBinaryOperator l r vmap real_create_multiplication
        run' (BinaryOp DivOp l r) vmap = runBinaryOperator l r vmap createDivision
        run' (BinaryOp PowOp l (Int 2)) vmap = runBinaryOperator l l vmap real_create_multiplication

        run' (UnaryOp AbsOp   e) vmap = runUnaryOperator e vmap real_create_absolute_value
        run' (UnaryOp SqrtOp  e) vmap = runUnaryOperator e vmap real_create_sqrt
        run' (UnaryOp NegOp   e) vmap = runUnaryOperator e vmap real_create_negation
        run' (UnaryOp LnOp    e) vmap = runUnaryOperator e vmap real_create_elogarithm
        run' (UnaryOp ExpoOp  e) vmap = runUnaryOperator e vmap real_create_eexponent
        run' (UnaryOp SinOp   e) vmap = runUnaryOperator e vmap real_create_sine
        run' (UnaryOp AsinOp  e) vmap = runUnaryOperator e vmap real_create_arcsine
        run' (UnaryOp CosOp   e) vmap = runUnaryOperator e vmap real_create_cosine
        run' (UnaryOp AcosOp  e) vmap = runUnaryOperator e vmap real_create_arccosine
        run' (UnaryOp TanOp   e) vmap = runUnaryOperator e vmap real_create_tangent
        run' (UnaryOp AtanOp  e) vmap = runUnaryOperator e vmap real_create_arctangent
        run' (UnaryOp FloorOp e) vmap = runUnaryOperator e vmap real_create_floor
        -- Divisor is the literal 2, so the construction-time guard cannot
        -- fire here. Built through 'createDivision' regardless, so that every
        -- division in this runner goes through the guarded builder and no
        -- reader has to check which ones are exceptions.
        run' (HalfUlp e FPDouble) vmap =
          do
           dulp <- runUnaryOperator e vmap real_create_double_ulp
           ptwo <- run' (Rat 2) vmap
           createDivision dulp ptwo
        run' (HalfUlp e FPSingle) vmap =
          do
           sulp <- runUnaryOperator e vmap real_create_single_ulp
           ptwo <- run' (Rat 2) vmap
           createDivision sulp ptwo
        run' (HalfUlp e (ArrayOf _ FPDouble)) vmap =
          do
           dulp <- runUnaryOperator e vmap real_create_double_ulp
           ptwo <- run' (Rat 2) vmap
           createDivision dulp ptwo
        run' (ErrBinOp AddOp FPDouble r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_error_addition
        run' (ErrBinOp SubOp FPDouble r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_error_subtraction
        run' (ErrSubSternenz FPDouble r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_error_subtraction --_Sternenz
        run' (ErrBinOp MulOp FPDouble r1 e1 r2 e2) vmap
          | (Int 2) <- r1 = run' e2 vmap
          | (Int 2) <- r2 = run' e1 vmap
          | otherwise = runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_error_multiplication
        run' (ErrBinOp DivOp FPDouble r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_error_division
        run' (ErrBinOp PowOp FPDouble r1 e1 (Int 2) _) vmap =
          runBinaryErrorOperator r1 e1 r1 e1 vmap real_create_error_multiplication
        run' (ErrRat rat) vmap = run' (Rat rat) vmap
        run' (ErrMulPow2L FPDouble i e) vmap = do pe <- run' e vmap
                                                  real_create_error_power_of_two_multiplication (fromInteger i) pe
        run' (ErrMulPow2R FPDouble i e) vmap = do pe <- run' e vmap
                                                  real_create_error_power_of_two_multiplication (fromInteger i) pe
        run' (ErrUnOp NegOp  FPDouble _ e) vmap = run' e vmap
        run' (ErrUnOp AbsOp  FPDouble _ e) vmap = run' e vmap
        run' (ErrUnOp ExpoOp FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_eexponent
        run' (ErrUnOp LnOp   FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_elogarithm
        run' (ErrUnOp SinOp  FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_sine
        run' (ErrUnOp AsinOp FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_arcsine
        run' (ErrUnOp CosOp  FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_cosine
        run' (ErrUnOp AcosOp FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_arccosine
        run' (ErrUnOp TanOp  FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_tangent
        run' (ErrUnOp AtanOp FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_arctangent
        run' (ErrUnOp FloorOp FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_floor
        run' (ErrFloorNoRound  FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_floor_tight
        run' (ErrUnOp SqrtOp FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_sqrt
        --
        run' (ErrBinOp AddOp FPSingle r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_single_error_addition
        run' (ErrBinOp SubOp FPSingle r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_single_error_subtraction
        run' (ErrSubSternenz FPSingle r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_single_error_subtraction --_Sternenz
        run' (ErrBinOp MulOp FPSingle r1 e1 r2 e2) vmap
          | (Int 2) <- r1 = run' e2 vmap
          | (Int 2) <- r2 = run' e1 vmap
          | otherwise = runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_single_error_multiplication
        run' (ErrBinOp DivOp FPSingle r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_single_error_division
        run' (ErrBinOp _   TInt _ _ _ _) vmap = run' (Rat 0) vmap
        run' (ErrMulPow2L FPSingle i e) vmap = do
          pe <- run' e vmap
          real_create_single_error_power_of_two_multiplication (fromInteger i) pe
        run' (ErrMulPow2R FPSingle i e) vmap = do
          pe <- run' e vmap
          real_create_single_error_power_of_two_multiplication (fromInteger i) pe
        run' (ErrUnOp NegOp  FPSingle _ e) vmap = run' e vmap
        run' (ErrUnOp AbsOp  FPSingle _ e) vmap = run' e vmap
        run' (ErrUnOp ExpoOp FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_eexponent
        run' (ErrUnOp LnOp FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_elogarithm
        run' (ErrUnOp SinOp FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_sine
        run' (ErrUnOp CosOp FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_cosine
        run' (ErrUnOp AtanOp FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_arctangent
        run' (ErrUnOp FloorOp FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_floor
        run' (ErrFloorNoRound FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_floor_tight
        run' (ErrUnOp SqrtOp FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_sqrt
        run' (ErrUnOp  _ TInt _ _)     vmap = run' (Rat 0) vmap
        --
        run' (Min es) vmap = let
          buildVector = do pVector <- real_vector_create
                           mapM_ (flip run' vmap >=> real_vector_add pVector) es
                           return pVector
          in if length es > 1
             then buildVector >>= real_create_minimum
             else run' (head es) vmap
        run' (Max es) vmap = let
          buildVector = do pVector <- real_vector_create
                           mapM_ (flip run' vmap >=> real_vector_add pVector) es
                           return pVector
          in if length es > 1
             then buildVector >>= real_create_maximum
             else run' (head es) vmap
        run' (MaxErr es) vmap = run' (Max es) vmap
        run' (ErrCast TInt FPDouble _ _) vmap = run' (Rat 0) vmap
        run' (ErrCast TInt FPSingle _ _) vmap = run' (Rat 0) vmap
        run' e _ = error $ "KodiakRunnable instance for AExpr, VariableMap and PReal undefined for " ++ show e


instance KodiakRunnable BExpr VariableMap PBool where
  run = runBExpr

runBExpr :: BExpr -> VariableMap -> IO PBool
runBExpr BTrue            _ = bool_create_true
runBExpr BFalse           _ = bool_create_false
runBExpr (Not bexp)    vmap = runBExpr bexp vmap >>= bool_create_not
runBExpr (Or lhs rhs)  vmap = do
  pLHS <- runBExpr lhs vmap
  pRHS <- runBExpr rhs vmap
  bool_create_or pLHS pRHS
runBExpr (And lhs rhs) vmap = do
  pLHS <- runBExpr lhs vmap
  pRHS <- runBExpr rhs vmap
  bool_create_and pLHS pRHS
runBExpr (Rel Eq lhs rhs)  vmap = do
  pLHS <- run lhs vmap
  pRHS <- run rhs vmap
  bool_create_equal_to pLHS pRHS
runBExpr (Rel Neq lhs rhs) vmap = do
  pLHS <- run lhs vmap
  pRHS <- run rhs vmap
  pEq <- bool_create_equal_to pLHS pRHS
  bool_create_not pEq
runBExpr (Rel Lt lhs rhs)  vmap = do
  pLHS <- run lhs vmap
  pRHS <- run rhs vmap
  bool_create_less_than pLHS pRHS
runBExpr (Rel LtE lhs rhs) vmap = do
  pLHS <- run lhs vmap
  pRHS <- run rhs vmap
  bool_create_less_than_or_equal_to pLHS pRHS
runBExpr (Rel Gt lhs rhs)  vmap = do
  pLHS <- run lhs vmap
  pRHS <- run rhs vmap
  bool_create_greater_than pLHS pRHS
runBExpr (Rel GtE lhs rhs) vmap = do
  pLHS <- run lhs vmap
  pRHS <- run rhs vmap
  bool_create_greater_than_or_equal_to pLHS pRHS
runBExpr bexpr _ = error $ "Boolean expression not supported by Kodiak: " ++ show bexpr
