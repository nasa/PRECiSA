{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Kodiak.ErrorComputation (
    computeAllErrorsInKodiakMap
) where

import AbsPVSLang
import AbsSpecLang
import AbstractDomain
import AbstractSemantics
import Common.ControlFlow
import Common.DecisionPath
import FunctionCallErrorAbstraction
import qualified Kodiak.Paver as KP
import Kodiak.Runnable
import Kodiak.Runner
import Utils (snd4, trd4, frt4)

import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as Map

import Debug.Trace

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
  let functionBindingsMap = map (\(SpecBind f b) -> (f,b)) specBinds
  errExpr <- if unfoldFunCalls'
             then return $ simplAExpr $ initAExpr err
             else case findInDecls fname decls of
               Just (_,_,AExprBody funBody) -> do
                 let locVars = localVarsWithType funBody
                 replaceFunCallErr True config interp emptyEnv locVars binds $ simplAExpr $ initAExpr err
               _ -> error $ "[computeAllErrorsInKodiakMap.runFunField] Function " ++ fname ++ " not found."
  !_ <- trace ("errExpr: " ++ show errExpr) $ return ()
  !_ <- trace ("functionBindingsMap: " ++ show functionBindingsMap) $ return ()
  return $ KI { kiName = fname,
                kiExpression = errExpr,
                kiBindings = fromMaybe (error $ "runFunction: function " ++ show fname ++ " not found.")
                                     (lookup fname functionBindingsMap),
                kiMaxDepth  = KP.maximumDepth searchParams,
                kiPrecision = KP.minimumPrecision searchParams
              }

aceb2PathFlowErrorTuple :: ACeb -> (Conditions, LDecisionPath, ControlFlow, EExpr, [FAExpr], [AExpr])
aceb2PathFlowErrorTuple aceb =
  (conds aceb, decisionPath aceb, cFlow aceb, fromJust $ eExpr aceb, fDeclRes $ fpExprs aceb, rDeclRes $ rExprs aceb)
