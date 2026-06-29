-- Notices:
--
-- Copyright 2025 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module PVSio.ErrorComputation (
    computeAllErrorsInPVSioMap
  , PVSioFieldErrorResult
) where

import AbsPVSLang
import AbsSpecLang
import AbstractDomain
import AbstractSemantics
import Common.ControlFlow
import Common.DecisionPath
import FunctionCallErrorAbstraction
import qualified Kodiak.Paver as KP
import qualified PVSio.Runner as PVSio
import Utils (snd4, trd4, frt4)

import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as Map

type PVSioFieldErrorResult = (Conditions, LDecisionPath, ControlFlow, PVSio.PVSioResult, AExpr, [FAExpr], [AExpr])

computeAllErrorsInPVSioMap ::
  Bool
  -> [Decl]
  -> SemanticConfiguration
  -> Interpretation
  -> Spec
  -> KP.SearchParameters
  -> IO [(String
         ,PVSType
         ,[Arg]
         ,[(ResultField, [PVSioFieldErrorResult])])]
computeAllErrorsInPVSioMap unfoldFunCalls' decls config interp (Spec specBinds) searchParams =
  mapM (runFunction unfoldFunCalls' decls config interp specBinds searchParams) functionNames
  where
    declInterps = Map.filter isNumericalInterp interp
    functionNames = Map.keys declInterps

runFunction :: Bool -> [Decl] -> SemanticConfiguration -> Interpretation -> [SpecBind] -> KP.SearchParameters -> String -> IO (String, PVSType, [Arg], [(ResultField, [PVSioFieldErrorResult])])
runFunction unfoldFunCalls' decls config interp specBinds searchParams fname = do
  let funInfo = fromMaybe errorMsg $ Map.lookup fname interp
  let fprec = snd4 funInfo
  let args = trd4 funInfo
  let fSem = frt4 funInfo
  let fields = Map.keys fSem
  results <- mapM (runFunField unfoldFunCalls' decls config interp specBinds searchParams fname fSem) fields
  return (fname, fprec, args, results)
  where
    errorMsg = error $ "computeAllErrorsInPVSioMap: function " ++ fname ++ " not found."

runFunField :: Bool -> [Decl] -> SemanticConfiguration -> Interpretation -> [SpecBind] -> KP.SearchParameters -> String -> Map.Map ResultField [ACeb] -> ResultField -> IO (ResultField, [PVSioFieldErrorResult])
runFunField unfoldFunCalls' decls config interp specBinds searchParams fname sem field = do
  let funErrExprs = fromMaybe errorMsgField (Map.lookup field sem)
  let functionErrorExpressionsMap = map aceb2PathFlowErrorTuple funErrExprs
  fieldResults <- mapM (runErrorExpression unfoldFunCalls' decls config interp specBinds searchParams fname) functionErrorExpressionsMap
  return (field, fieldResults)
  where
    errorMsgField = error $ "runFunction: function " ++ show fname ++ " not found in input bound specification."

runErrorExpression :: Bool -> [Decl] -> SemanticConfiguration -> Interpretation -> [SpecBind] -> KP.SearchParameters -> String -> (Conditions, LDecisionPath, ControlFlow, EExpr, [FAExpr], [AExpr]) -> IO PVSioFieldErrorResult
runErrorExpression unfoldFunCalls' decls config interp specBinds searchParams fname (conditions, path, flow, err, fpes, res) = do
  pvi <- buildPVSioInput unfoldFunCalls' decls config interp specBinds searchParams fname err
  result <- PVSio.runPVSioComputation pvi
  return (conditions, path, flow, result, initAExpr err, fpes, res)

buildPVSioInput :: Bool -> [Decl] -> SemanticConfiguration -> Interpretation -> [SpecBind] -> KP.SearchParameters -> String -> EExpr -> IO PVSio.PVSioInput
buildPVSioInput unfoldFunCalls' decls config interp specBinds _searchParams fname err = do
  let binds = fromJust $ findInSpec fname specBinds
  -- For PVSio, we do NOT expand arrays - keep them as array types
  let functionBindingsMap = map (\(SpecBind f b) -> (f,b)) specBinds
  errExpr <- if unfoldFunCalls'
             then return $ simplAExpr $ initAExpr err
             else case findInDecls fname decls of
               Just (_,_,AExprBody funBody) -> do
                 let locVars = localVarsWithType funBody
                 replaceFunCallErr True config interp emptyEnv locVars binds $ simplAExpr $ initAExpr err
               _ -> error $ "[computeAllErrorsInPVSioMap.runFunField] Function " ++ fname ++ " not found."
  -- For PVSio, we do NOT expand arrays in the expression
  return $ PVSio.PVI { PVSio.pviName = fname,
                       PVSio.pviExpression = errExpr,
                       PVSio.pviBindings = fromMaybe (error $ "runFunction: function " ++ show fname ++ " not found.")
                                                   (lookup fname functionBindingsMap)
                     }

aceb2PathFlowErrorTuple :: ACeb -> (Conditions, LDecisionPath, ControlFlow, EExpr, [FAExpr], [AExpr])
aceb2PathFlowErrorTuple aceb =
  (conds aceb, decisionPath aceb, cFlow aceb, fromJust $ eExpr aceb, fDeclRes $ fpExprs aceb, rDeclRes $ rExprs aceb)
