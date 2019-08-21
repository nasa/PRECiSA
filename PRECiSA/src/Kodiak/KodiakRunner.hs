-- Notices:
--
-- Copyright 2017 United States Government as represented by the Administrator of the National Aeronautics and Space Administration.
-- All Rights Reserved.
--
-- Disclaimers:
--
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED,
-- IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT
-- SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS,
-- HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
--
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT,
-- ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING
-- FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH
-- MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


{-# LANGUAGE MultiParamTypeClasses  #-}

module Kodiak.KodiakRunner where

import Kodiak.Kodiak
import Kodiak.KodiakRunnable
import AbsPVSLang
import AbsSpecLang
import FPrec
import Numeric (fromRat)
import Control.Exception (throw,AssertionFailed(..))
import Control.Monad ((>=>))
import Data.Maybe(fromMaybe)
import Foreign.C

data KodiakInput = KI { name :: String,
                        expression :: EExpr,
                        bindings :: [VarBind],
                        maxDepth :: CUInt,
                        precision :: CUInt }

variableMapFromBinds :: [VarBind] -> VariableMap
variableMapFromBinds binds = VMap $ zip variableNames [(0::CUInt)..]
  where variableNames = map (\(VarBind s _ _ _) -> s) binds

fromLBoundToRational :: LBound -> Rational
fromLBoundToRational (LBInt i) = toRational i
fromLBoundToRational (LBDouble r) = toRational r
fromLBoundToRational LInf = error "not implemented for LInf TODO: Refactor"

fromUBoundToRational :: UBound -> Rational
fromUBoundToRational (UBInt i) = toRational i
fromUBoundToRational (UBDouble r) = toRational r
fromUBoundToRational UInf = error "not implemented for UInf TODO: Refactor"

instance KodiakRunnable VarBind PMinMaxSystem () where
  run (VarBind x _ lowerBound upperBound) pSys =
    do
      cName <- newCString x
      pLb <- interval_create cLowerBound cLowerBound
      pUb <- interval_create cUpperBound cUpperBound
      minmax_system_register_variable pSys cName pLb pUb
        where
          cLowerBound = fromRational $ fromLBoundToRational lowerBound
          cUpperBound = fromRational $ fromUBoundToRational upperBound

instance KodiakRunnable KodiakInput () KodiakResult where
  run kInput _ =
    do
      let sysName       = name kInput
          errorExpr     = expression kInput
          varRanges     = bindings kInput
          variableMap   = variableMapFromBinds varRanges
          thisPrecision = precision kInput
          thisMaxDepth  = maxDepth kInput
      cName <- newCString sysName
      pSys <- minmax_system_create cName
      minmax_system_set_maxdepth pSys thisMaxDepth
      minmax_system_set_precision pSys (negate (fromInteger $ toInteger thisPrecision))
      mapM_ (`run` pSys) varRanges
      pExpr <- run errorExpr variableMap
      minmax_system_maximize pSys pExpr
      lb <- minmax_system_maximum_lower_bound pSys >>= fmap (fromRational . toRational) . return
      ub <- minmax_system_maximum_upper_bound pSys >>= fmap (fromRational . toRational) . return
      return $ KR { maximumLowerBound = lb,
                    maximumUpperBound = ub }

newtype VariableMap = VMap [(String,CUInt)] deriving Show

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

        createKodiakVariable (varName,varId) = do
             cName <- newCString varName
             pVariable <- real_create_variable varId cName
             return (varName,pVariable)

        runBinaryOperator l r vmap kodiakFunction = do
          pl <- run' l vmap
          pr <- run' r vmap
          kodiakFunction pl pr

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

        run' (Add l r) vmap = runBinaryOperator l r vmap real_create_addition
        run' (Sub l r) vmap = runBinaryOperator l r vmap real_create_subtraction
        run' (Mul l r) vmap = runBinaryOperator l r vmap real_create_multiplication
        run' (Div l r) vmap = runBinaryOperator l r vmap real_create_division
        run' (Int i) vmap = run' (Rat $ toRational i) vmap
        run' (Rat r) _ = interval_create lb ub >>= real_create_value
          where (lb,ub) = rat2interval r
        run' (Var _ x) vmap = run' (RealMark x) vmap
        run' (RealMark x) vmap = case lookup x vmap of
                                      Just pVar -> return pVar
                                      Nothing -> throw (AssertionFailed $ "TODO: RealMark " ++ x ++ " name not found") >> return undefined
        run' (ErrorMark _ _) _ = throw (AssertionFailed "ErrorMark should not be used") >> return undefined
        run' (Abs   e) vmap = runUnaryOperator e vmap real_create_absolute_value
        run' (Sqrt  e) vmap = runUnaryOperator e vmap real_create_sqrt
        run' (Neg   e) vmap = runUnaryOperator e vmap real_create_negation
        run' (Ln    e) vmap = runUnaryOperator e vmap real_create_elogarithm
        run' (Expo  e) vmap = runUnaryOperator e vmap real_create_eexponent
        run' (Sin   e) vmap = runUnaryOperator e vmap real_create_sine
        run' (Cos   e) vmap = runUnaryOperator e vmap real_create_cosine
        run' (ATan  e) vmap = runUnaryOperator e vmap real_create_arctangent
        run' (Floor e) vmap = runUnaryOperator e vmap real_create_floor
        run' (HalfUlp e FPDouble) vmap =
          do
           dulp <- runUnaryOperator e vmap real_create_double_ulp
           ptwo <- run' (Rat 2) vmap
           real_create_division dulp ptwo
        run' (HalfUlp e FPSingle) vmap =
          do
           sulp <- runUnaryOperator e vmap real_create_single_ulp
           ptwo <- run' (Rat 2) vmap
           real_create_division sulp ptwo
        run' (ErrAdd FPDouble r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_error_addition
        run' (ErrSub FPDouble r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_error_subtraction
        run' (ErrMul FPDouble r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_error_multiplication
        run' (ErrDiv FPDouble r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_error_division
        run' (ErrNeg FPDouble r e) vmap =
          runUnaryErrorOperator r e vmap real_create_error_negation
        run' (ErrRat rat) vmap = run' (Rat rat) vmap
        run' (ErrMulPow2L FPDouble i e) vmap = do pe <- run' e vmap
                                                  real_create_error_power_of_two_multiplication (fromInteger i) pe
        run' (ErrMulPow2R FPDouble i e) vmap = do pe <- run' e vmap
                                                  real_create_error_power_of_two_multiplication (fromInteger i) pe
        run' (ErrExpo   FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_eexponent
        run' (ErrLn     FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_elogarithm
        run' (ErrSin    FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_sine
        run' (ErrCos    FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_cosine
        run' (ErrAtan   FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_arctangent
        run' (ErrAtanT  FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_arctangent_tight
        run' (ErrFloor  FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_floor
        run' (ErrFloor0 FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_floor_tight
        run' (ErrSqrt   FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_sqrt
        --
        run' (ErrAdd FPSingle r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_single_error_addition
        run' (ErrSub FPSingle r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_single_error_subtraction
        run' (ErrMul FPSingle r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_single_error_multiplication
        run' (ErrDiv FPSingle r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_single_error_division
        run' (ErrNeg FPSingle r e) vmap =
          runUnaryErrorOperator r e vmap real_create_single_error_negation
        run' (ErrMulPow2L FPSingle i e) vmap = do
          pe <- run' e vmap
          real_create_single_error_power_of_two_multiplication (fromInteger i) pe
        run' (ErrMulPow2R FPSingle i e) vmap = do
          pe <- run' e vmap
          real_create_single_error_power_of_two_multiplication (fromInteger i) pe
        run' (ErrExpo   FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_eexponent
        run' (ErrLn     FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_elogarithm
        run' (ErrSin    FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_sine
        run' (ErrCos    FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_cosine
        run' (ErrAtan   FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_arctangent
        run' (ErrAtanT  FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_arctangent_tight
        run' (ErrFloor  FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_floor
        run' (ErrFloor0 FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_floor_tight
        run' (ErrSqrt   FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_sqrt
        --
        run' (ErrAdd TInt _ _ _ _) vmap = run' (Rat 0) vmap
        run' (ErrSub TInt _ _ _ _) vmap = run' (Rat 0) vmap
        run' (ErrMul TInt _ _ _ _) vmap = run' (Rat 0) vmap
        run' (ErrDiv TInt _ _ _ _) vmap = run' (Rat 0) vmap
        run' (ErrMod TInt _ _ _ _) vmap = run' (Rat 0) vmap
        run' (ErrNeg TInt _ _)     vmap = run' (Rat 0) vmap
        run' (ErrAbs TInt _ _)     vmap = run' (Rat 0) vmap
        run' (MaxErr es) vmap = let
          buildVector = do pVector <- real_vector_create
                           mapM_ (flip run' vmap >=> real_vector_add pVector) es
                           return pVector
          in if length es > 1
             then buildVector >>= real_create_maximum
             else run' (head es) vmap
        run' e _ = error $ "KodiakRunnable instance for AExpr, VariableMap and PReal undefined for " ++ show e


instance KodiakRunnable BExpr VariableMap PBool where
  run = runBExpr

runBExpr :: BExpr -> VariableMap -> IO PBool
runBExpr BTrue            _ = bool_create_true
runBExpr BFalse           _ = bool_create_false
runBExpr (Not bexp)    vmap = runBExpr bexp vmap >>= bool_create_not
runBExpr (Or lhs rhs)  vmap = do pLHS <- runBExpr lhs vmap
                                 pRHS <- runBExpr rhs vmap
                                 bool_create_or pLHS pRHS
runBExpr (And lhs rhs) vmap = do pLHS <- runBExpr lhs vmap
                                 pRHS <- runBExpr rhs vmap
                                 bool_create_and pLHS pRHS
runBExpr (Eq lhs rhs)  vmap = do pLHS <- run lhs vmap
                                 pRHS <- run rhs vmap
                                 bool_create_equal_to pLHS pRHS
runBExpr (Neq lhs rhs) vmap = do pLHS <- run lhs vmap
                                 pRHS <- run rhs vmap
                                 pEq <- bool_create_equal_to pLHS pRHS
                                 bool_create_not pEq
runBExpr (Lt lhs rhs)  vmap = do pLHS <- run lhs vmap
                                 pRHS <- run rhs vmap
                                 bool_create_less_than pLHS pRHS
runBExpr (LtE lhs rhs) vmap = do pLHS <- run lhs vmap
                                 pRHS <- run rhs vmap
                                 bool_create_less_than_or_equal_to pLHS pRHS
runBExpr (Gt lhs rhs)  vmap = do pLHS <- run lhs vmap
                                 pRHS <- run rhs vmap
                                 bool_create_greater_than pLHS pRHS
runBExpr (GtE lhs rhs) vmap = do pLHS <- run lhs vmap
                                 pRHS <- run rhs vmap
                                 bool_create_greater_than_or_equal_to pLHS pRHS
--runBExpr _ _ = undefined
