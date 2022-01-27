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
import PVSTypes
import Numeric (fromRat)
import Control.Exception (throw,AssertionFailed(..))
import Control.Monad ((>=>),foldM)
import Data.Maybe(fromMaybe)
import Foreign.C
import Operators

data KodiakInput = KI { kiName :: String,
                        kiExpression :: EExpr,
                        kiBindings :: [VarBind],
                        kiMaxDepth :: CUInt,
                        kiPrecision :: CUInt }

data KodiakResult = KR { maximumLowerBound :: Double,
                         maximumUpperBound :: Double
                       } deriving Show

variableMapFromBinds :: [VarBind] -> VariableMap
variableMapFromBinds binds = VMap $ zip variableNames [(0::CUInt)..]
  where variableNames = map (\(VarBind s _ _ _) -> s) binds

instance KodiakRunnable KodiakInput () KodiakResult where
  run kInput _ =
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
        run' (Var _ x) vmap = run' (RealMark x) vmap
        run' (RealMark x) vmap = case lookup x vmap of
                                  Just pVar -> return pVar
                                  Nothing -> createKodiakLocalVariable x
        run' (ErrorMark _ _) _ = throw (AssertionFailed "ErrorMark should not be used") >> return undefined
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
        run' (BinaryOp DivOp l r) vmap = runBinaryOperator l r vmap real_create_division
        run' (BinaryOp PowOp l (Int 2)) vmap = runBinaryOperator l l vmap real_create_multiplication

        run' (UnaryOp AbsOp   e) vmap = runUnaryOperator e vmap real_create_absolute_value
        run' (UnaryOp SqrtOp  e) vmap = runUnaryOperator e vmap real_create_sqrt
        run' (UnaryOp NegOp   e) vmap = runUnaryOperator e vmap real_create_negation
        run' (UnaryOp LnOp    e) vmap = runUnaryOperator e vmap real_create_elogarithm
        run' (UnaryOp ExpoOp  e) vmap = runUnaryOperator e vmap real_create_eexponent
        run' (UnaryOp SinOp   e) vmap = runUnaryOperator e vmap real_create_sine
        run' (UnaryOp AsinOp   e) vmap = runUnaryOperator e vmap real_create_arcsine
        run' (UnaryOp CosOp   e) vmap = runUnaryOperator e vmap real_create_cosine
        run' (UnaryOp AcosOp   e) vmap = runUnaryOperator e vmap real_create_arccosine
        run' (UnaryOp TanOp  e) vmap = runUnaryOperator e vmap real_create_tangent
        run' (UnaryOp AtanOp  e) vmap = runUnaryOperator e vmap real_create_arctangent
        run' (UnaryOp FloorOp e) vmap = runUnaryOperator e vmap real_create_floor
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
        run' (ErrBinOp AddOp FPDouble r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_error_addition
        run' (ErrBinOp SubOp FPDouble r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_error_subtraction
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
        run' (ErrUnOp NegOp   _     FPDouble _ e) vmap = run' e vmap
        run' (ErrUnOp AbsOp   _     FPDouble _ e) vmap = run' e vmap
        run' (ErrUnOp ExpoOp  False FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_eexponent
        run' (ErrUnOp LnOp    False FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_elogarithm
        run' (ErrUnOp SinOp   False FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_sine
        run' (ErrUnOp AsinOp   False FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_arcsine
        run' (ErrUnOp CosOp   False FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_cosine
        run' (ErrUnOp AcosOp   False FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_arccosine
        run' (ErrUnOp TanOp  False FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_tangent
        run' (ErrUnOp AtanOp  False FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_arctangent
        run' (ErrUnOp AtanOp  True  FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_arctangent_tight
        run' (ErrUnOp FloorOp False FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_floor
        run' (ErrUnOp FloorOp True  FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_floor_tight
        run' (ErrUnOp SqrtOp  False FPDouble r e) vmap = runUnaryErrorOperator r e vmap real_create_error_sqrt
        --
        run' (ErrBinOp AddOp FPSingle r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_single_error_addition
        run' (ErrBinOp SubOp FPSingle r1 e1 r2 e2) vmap =
          runBinaryErrorOperator r1 e1 r2 e2 vmap real_create_single_error_subtraction
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
        run' (ErrUnOp NegOp   _     FPSingle _ e) vmap = run' e vmap
        run' (ErrUnOp AbsOp   _     FPSingle _ e) vmap = run' e vmap
        run' (ErrUnOp ExpoOp  False FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_eexponent
        run' (ErrUnOp LnOp    False FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_elogarithm
        run' (ErrUnOp SinOp   False FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_sine
        run' (ErrUnOp CosOp   False FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_cosine
        run' (ErrUnOp AtanOp  False FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_arctangent
        run' (ErrUnOp AtanOp  True  FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_arctangent_tight
        run' (ErrUnOp FloorOp False FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_floor
        run' (ErrUnOp FloorOp True  FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_floor_tight
        run' (ErrUnOp SqrtOp  False FPSingle r e) vmap = runUnaryErrorOperator r e vmap real_create_single_error_sqrt
        run' (ErrUnOp  _ _ TInt _ _)     vmap = run' (Rat 0) vmap
        --
        run' (MaxErr es) vmap = let
          buildVector = do pVector <- real_vector_create
                           mapM_ (flip run' vmap >=> real_vector_add pVector) es
                           return pVector
          in if length es > 1
             then buildVector >>= real_create_maximum
             else run' (head es) vmap
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
