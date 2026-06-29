{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Kodiak.ErrorComputation (
    computeAllErrorsInKodiakMap
  , expandArrays
  , expandArrayArguments
) where

import AbsPVSLang
import AbsSpecLang
import AbstractDomain
import AbstractSemantics
import Common.ControlFlow
import Common.DecisionPath
import Data.Generics.Uniplate.Data (transformM)
import FunctionCallErrorAbstraction
import qualified Kodiak.Paver as KP
import Kodiak.Runnable
import Kodiak.Runner
import Utils (snd4, trd4, frt4)

import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as Map

type FieldErrorResult = (Conditions, LDecisionPath, ControlFlow, KodiakResult, AExpr, [FAExpr], [AExpr])

computeAllErrorsInKodiakMap ::
  Bool
  -> [Decl]
  -> SemanticConfiguration
  -> Interpretation
  -> Spec
  -> KP.SearchParameters
  -> IO [(String
         ,PVSType
         ,[Arg]
         ,[(ResultField, [FieldErrorResult])])]
computeAllErrorsInKodiakMap unfoldFunCalls' decls config interp (Spec specBinds) searchParams =
  mapM (runFunction unfoldFunCalls' decls config interp specBinds searchParams) functionNames
  where
    declInterps = Map.filter isNumericalInterp interp
    functionNames = Map.keys declInterps

runFunction :: Bool -> [Decl] -> SemanticConfiguration -> Interpretation -> [SpecBind] -> KP.SearchParameters -> String -> IO (String, PVSType, [Arg], [(ResultField, [FieldErrorResult])])
runFunction unfoldFunCalls' decls config interp specBinds searchParams fname = do
  let funInfo = fromMaybe errorMsg $ Map.lookup fname interp
  let fprec = snd4 funInfo
  let args = trd4 funInfo
  let fSem = frt4 funInfo
  let fields = Map.keys fSem
  results <- mapM (runFunField unfoldFunCalls' decls config interp specBinds searchParams fname fSem) fields
  return (fname, fprec, args, results)
  where
    errorMsg = error $ "computeAllErrorsInKodiakMap: function " ++ fname ++ " not found."

runFunField :: Bool -> [Decl] -> SemanticConfiguration -> Interpretation -> [SpecBind] -> KP.SearchParameters -> String -> Map.Map ResultField [ACeb] -> ResultField -> IO (ResultField, [FieldErrorResult])
runFunField unfoldFunCalls' decls config interp specBinds searchParams fname sem field = do
  let funErrExprs = fromMaybe errorMsgField (Map.lookup field sem)
  let functionErrorExpressionsMap = map aceb2PathFlowErrorTuple funErrExprs
  fieldResults <- mapM (runErrorExpression unfoldFunCalls' decls config interp specBinds searchParams fname) functionErrorExpressionsMap
  return (field, fieldResults)
  where
    errorMsgField = error $ "runFunction: function " ++ show fname ++ " not found in input bound specification."

runErrorExpression :: Bool -> [Decl] -> SemanticConfiguration -> Interpretation -> [SpecBind] -> KP.SearchParameters -> String -> (Conditions, LDecisionPath, ControlFlow, EExpr, [FAExpr], [AExpr]) -> IO FieldErrorResult
runErrorExpression unfoldFunCalls' decls config interp specBinds searchParams fname (conditions, path, flow, err, fpes, res) = do
  ki <- buildKodiakInput unfoldFunCalls' decls config interp specBinds searchParams fname err
  result <- run ki ()
  return (conditions, path, flow, result, initAExpr err, fpes, res)

buildKodiakInput :: Bool -> [Decl] -> SemanticConfiguration -> Interpretation -> [SpecBind] -> KP.SearchParameters -> String -> EExpr -> IO KodiakInput
buildKodiakInput unfoldFunCalls' decls config interp specBinds searchParams fname err = do
  let binds = fromJust $ findInSpec fname specBinds
  let functionBindingsMap = map (\(SpecBind f b) -> (f,b)) $ expandArrayArguments specBinds
  errExpr <- if unfoldFunCalls'
             then return $ simplAExpr $ initAExpr err
             else case findInDecls fname decls of
               Just (_,_,AExprBody funBody) -> do
                 let locVars = localVarsWithType funBody
                 replaceFunCallErr True config interp emptyEnv locVars binds $ simplAExpr $ initAExpr err
               _ -> error $ "[computeAllErrorsInKodiakMap.runFunField] Function " ++ fname ++ " not found."
  errExpr' <- expandArrays errExpr
  return $ KI { kiName = fname,
                kiExpression = errExpr',
                kiBindings = fromMaybe (error $ "runFunction: function " ++ show fname ++ " not found.")
                                     (lookup fname functionBindingsMap),
                kiMaxDepth  = KP.maximumDepth searchParams,
                kiPrecision = KP.minimumPrecision searchParams
              }

aceb2PathFlowErrorTuple :: ACeb -> (Conditions, LDecisionPath, ControlFlow, EExpr, [FAExpr], [AExpr])
aceb2PathFlowErrorTuple aceb =
  (conds aceb, decisionPath aceb, cFlow aceb, fromJust $ eExpr aceb, fDeclRes $ fpExprs aceb, rDeclRes $ rExprs aceb)

expandArrayArguments :: [SpecBind] -> [SpecBind]
expandArrayArguments = map expandSpecBind
  where
    expandSpecBind (SpecBind f vbs) = SpecBind f (concatMap expandVarBind vbs)

    expandVarBind (VarBind var resField (ArrayOf n ty) lb ub) =
      [VarBind (projName i var) resField ty lb ub | i <- [0 .. n - 1]]
    expandVarBind vb = [vb]

expandArrays :: AExpr -> IO AExpr
expandArrays = transformM expandArrays'

expandArrays' :: AExpr -> IO AExpr
expandArrays' (ArrayElem ty var [Int idx]) = do
  return $ Var ty (projName idx var)
expandArrays' (ArrayElem ty var idxs) = do
  error $ "[expandArrays'] ArrayElem should not be used: " ++ show (ArrayElem ty var idxs)
expandArrays' (ErrBinOp (ArrayAddOp n) FPDouble r1 e1 r2 e2) = expandArrayAddOp (toInteger n) r1 e1 r2 e2
expandArrays' (ErrBinOp (ArrayDotOp n) FPDouble r1 e1 r2 e2) = expandArrayDotOp (toInteger n) r1 e1 r2 e2
expandArrays' (HalfUlp (RealMark var ResValue) (ArrayOf _ FPDouble)) = do
  let maxRM = RealMark (projName 0 var) ResValue
  return $ HalfUlp maxRM FPDouble
expandArrays' other = return other

expandArrayAddOp :: Integer -> AExpr -> AExpr -> AExpr -> AExpr -> IO AExpr
expandArrayAddOp n r1 e1 r2 e2
  | n == 0 = error "not reachable"
  | otherwise = return $
      ErrBinOp
        AddOp
        FPDouble
        (r1 `mkProj` 0)
        e1
        (r2 `mkProj` 0)
        e2

expandArrayDotOp :: Integer -> AExpr -> AExpr -> AExpr -> AExpr -> IO AExpr
expandArrayDotOp n r1 e1 r2 e2
  | n == 0 = error "not reachable"
  | otherwise =
      do
        (_res,err) <- expandArrayDotOp' 0 (n - 1) r1 e1 r2 e2
        return err

expandArrayDotOp' :: Integer -> Integer -> AExpr -> AExpr -> AExpr -> AExpr -> IO (AExpr,AExpr)
expandArrayDotOp' idx maxIdx r1 e1 r2 e2
  | idx == maxIdx
      = return
          ( BinaryOp MulOp (r1 `mkProj` idx) (r2 `mkProj` idx)
          , ErrBinOp MulOp FPDouble (r1 `mkProj` idx) e1 (r2 `mkProj` idx) e2 )
  | idx < maxIdx = do
      (r1', e1') <- expandArrayDotOp' idx       idx    r1 e1 r2 e2
      (r2', e2') <- expandArrayDotOp' (idx + 1) maxIdx r1 e1 r2 e2
      return
        ( BinaryOp AddOp r1' r2'
        , ErrBinOp
            AddOp
            FPDouble
            r1'
            e1'
            r2'
            e2' )
  | otherwise = error $ "[expandArrayDotOp'] not implemented. idx: " ++ show idx ++ ", maxIdx: " ++ show maxIdx ++ ", r1: " ++ show r1 ++ ", e1: " ++ show e1 ++ ", r2: " ++ show r2 ++ ", e2: " ++ show e2

mkProj :: AExpr -> Integer -> AExpr
mkProj e i
  | i < 0 = error $ "[mkProj] cannot accept negative indexes: " ++ show i
  | RealMark var ResValue <- e = RealMark (projName i var) ResValue
  | Var (ArrayOf n FPDouble) var <- e, i < n = Var FPDouble (projName i var)
  | BinaryOp (ArrayAddOp _) e1 e2 <- e =
      let e1' = mkProj e1 i
          e2' = mkProj e2 i
      in BinaryOp AddOp e1' e2'
  | otherwise = error $ "[mkProj] not implemented for e: " ++ show e ++ ", and i: " ++ show i

projName :: Integer -> String -> String
projName i n = n ++ "_array_idx_" ++ show i