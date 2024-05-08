-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module FunctionCallErrorAbstraction where

import Control.Monad
import Data.Maybe (fromJust,fromMaybe)
import Data.Either (isLeft)
import Data.Bifunctor
import qualified Data.Map as Map
import Foreign.C.String
import Foreign.C.Types (CUInt)
import System.IO (hFlush,stdout)
import GHC.IO.Handle (hFlush)

import AbsPVSLang
import AbsSpecLang
import AbstractDomain
import AbstractSemantics
import Data.Maybe(fromMaybe)
import Kodiak.Runnable
import Kodiak.Kodiak
import Kodiak.Runner
import qualified Kodiak.Expression as K
import Common.DecisionPath
import Common.TypesUtils
import Common.ControlFlow
import Common.DecisionPath
import Foreign.C
import PVSTypes
import Operators
import Translation.Float2Real
import Translation.Real2Float

type AssumeStability = Bool


roundOffError :: SemanticConfiguration
        -> CUInt
        -> CUInt
        -> [VarBind]
        -> Interpretation
        -> Env ACebS
        -> FAExpr
        -> IO Double
roundOffError config maximumDepth minimumPrecision varBinds interp env ae =
  do
    let aeSem = stmSem ae interp env config (LDP []) []
    let aeSemMergedACeb = mergeACebFold aeSem
    let acebAExpr = initErrAceb aeSemMergedACeb
    let errExpr = simplAExpr $ initAExpr $ fromMaybe (error "roError: unexpected argument.") $
                                                     eExpr (unfoldFunCallInCeb interp acebAExpr)

    expr <- replaceFunCallErr True config interp env [] varBinds errExpr
    let kodiakInput = KI { kiName = "error",
                           kiExpression = expr,
                           kiBindings = varBinds,
                           kiMaxDepth = maximumDepth,
                           kiPrecision = minimumPrecision
                           }
    err <- maximumUpperBound <$> run kodiakInput ()
    return err


aexprEnclosure :: Bool -> SemanticConfiguration
                -> Interpretation
                -> Env ACebS -> CUInt -> CUInt -> [VarBind] -> AExpr -> IO (Double,Double)
aexprEnclosure withError config interp env maximumDepth minimumPrecision varBinds aexpr = do
  expr <- replaceFunCallErr withError config interp env [] varBinds aexpr
  (KodiakMinMaxResult lb ub) <- run  KodiakMinMaxInput {
        kmmiName = show aexpr ++ "enclosure",
        kmmiExpression = expr,
        kmmiBindings = varBinds,
        kmmiMaxDepth  = maximumDepth,
        kmmiPrecision = minimumPrecision
      } ()
  return (lb,ub)


faexprEnclosure :: SemanticConfiguration
                -> Interpretation
                -> Env ACebS
                -> CUInt
                -> CUInt
                -> [VarBind]
                -> FAExpr
                -> IO (Double,Double)
faexprEnclosure conf interp env maximumDepth minimumPrecision varBinds faexpr = do
  (lb,ub) <- aexprEnclosure False conf interp env maximumDepth minimumPrecision varBinds (fae2real faexpr)
  e <- roundOffError conf maximumDepth minimumPrecision varBinds interp env faexpr
  return (lb - e,ub + e)

enclosure2binding :: Arg -> (Double,Double) -> VarBind
enclosure2binding (Arg x t) (lb,ub) =  VarBind x ResValue t (LBDouble $ toRational lb) (UBDouble $ toRational ub)

replaceFunCallErr :: Bool
                  -> SemanticConfiguration
                  -> Interpretation
                  -> Env ACebS
                  -> [(VarName,PVSType,FAExpr)]
                  -> [VarBind]
                  -> AExpr
                  -> IO AExpr
replaceFunCallErr withError config interp env locVars callerFunBinds e@(ErrFun fun fp field funargs) =
  do
    let fpargs = map (addLocVarsFAExpr locVars) funargs
    fpargsEnclosures <- mapM (faexprEnclosure config interp env 2 7 callerFunBinds) fpargs
    let binds = zipWith enclosure2binding formalArgs fpargsEnclosures
    errorExpr <- replaceFunCallErr withError config interp env locVars binds
                 $ initAExpr $ maxRoundOffErrorStable $ fromJust $ Map.lookup field sem
    let ki =  KI {
         kiName = "error",
         kiExpression = errorExpr,
         kiBindings = binds,
         kiMaxDepth  = 7,
         kiPrecision = 14
       }
    result <- run ki ()
    let res = maximumUpperBound result
    return $ Rat $ toRational $ res
    where
      (_,_,formalArgs,sem) = fromJust $ Map.lookup fun interp
      (_,acebs) = case Map.lookup field sem of
          Nothing -> error $ "field: " ++ show field ++ " not found in semantic item: " ++ show sem
          Just acebs -> (concatMap (rDeclRes . rExprs) acebs,acebs)

replaceFunCallErr withError config interp env locVars callerFunBinds e@(EFun fun field t args)
  = do
    fpargsEnclosures <- mapM (aexprEnclosure withError config interp env 2 7 callerFunBinds) args
    let binds = zipWith enclosure2binding formalArgs fpargsEnclosures
    funRExprs' <- mapM (replaceFunCallErr withError config interp env locVars binds) funRExprs
    minRes <- run (kInput (Min funRExprs') binds) ()
    maxRes <- run (kInput (Max funRExprs') binds) ()
    errorExpr <- replaceFunCallErr withError config interp env locVars binds
                 $ initAExpr $ maxRoundOffError $ fromJust $ Map.lookup field sem
    let r_lb = toRational $ kmmoMinimumLowerBound minRes
    let r_ub = toRational $ kmmoMaximumUpperBound maxRes
    if withError
      then do
        let ki =  KI {
         kiName = "error",
         kiExpression = errorExpr,
         kiBindings = binds,
         kiMaxDepth  = 7,
         kiPrecision = 14
        }
        e <- maximumUpperBound <$> run ki ()
        return $ Interval (r_lb - (toRational e)) (r_ub + (toRational e))
      else
        return $ Interval (r_lb) (r_ub)

  where
    (_,fp,formalArgs,sem) = fromJust $ Map.lookup fun interp

    funRExprs = case Map.lookup field sem of
      Nothing -> error $ "field: " ++ show field ++ " not found in semantic item: " ++ show sem
      Just acebs -> concatMap (rDeclRes . rExprs) acebs
    kInput e bs =
      KodiakMinMaxInput {
        kmmiName = "function " ++ show fun,
        kmmiExpression = e,
        kmmiBindings = bs ,
        kmmiMaxDepth  = 2,
        kmmiPrecision = 7
      }

replaceFunCallErr withError config interp env locVars callerFunBinds (BinaryOp op ae1 ae2)
  = BinaryOp op <$> replaceFunCallErr withError config interp env locVars callerFunBinds ae1
                <*> replaceFunCallErr withError config interp env locVars callerFunBinds ae2
replaceFunCallErr withError config interp env locVars callerFunBinds (UnaryOp op ae)
  = UnaryOp op <$> replaceFunCallErr withError config interp env locVars callerFunBinds ae
replaceFunCallErr withError config interp env locVars callerFunBinds (ArrayElem t size v aes)
  = ArrayElem t size v <$> mapM (replaceFunCallErr withError config interp env locVars callerFunBinds) aes
replaceFunCallErr withError config interp env locVars callerFunBinds (ListElem t x ae)
  = ListElem t x <$> replaceFunCallErr withError config interp env locVars callerFunBinds ae
replaceFunCallErr withError config interp env locVars callerFunBinds (Min aes)
  = Min <$> mapM (replaceFunCallErr withError config interp env locVars callerFunBinds) aes
replaceFunCallErr withError config interp env locVars callerFunBinds (Max aes)
  = Max <$> mapM (replaceFunCallErr withError config interp env locVars callerFunBinds) aes
replaceFunCallErr withError config interp env locVars callerFunBinds (RLet letElems ae)
  = RLet <$> mapM replaceFunCallLetElem letElems <*> replaceFunCallErr withError config interp env locVars callerFunBinds ae
    where
      replaceFunCallLetElem letElem = do
        newLetExpr <- replaceFunCallErr withError config interp env locVars callerFunBinds (letExpr letElem)
        return letElem{letExpr = newLetExpr}
replaceFunCallErr withError config interp env locVars callerFunBinds (RIte be ae1 ae2)
  = RIte be <$> replaceFunCallErr withError config interp env locVars callerFunBinds ae1
            <*> replaceFunCallErr withError config interp env locVars callerFunBinds ae2
replaceFunCallErr withError config interp env locVars callerFunBinds (RListIte thenExprs elseExpr)
  = RListIte <$> mapM replaceFunCallErrSnd thenExprs
             <*> replaceFunCallErr withError config interp env locVars callerFunBinds elseExpr
    where
      replaceFunCallErrSnd (be,ae) = do
        ae' <- replaceFunCallErr withError config interp env locVars callerFunBinds ae
        return (be,ae')
replaceFunCallErr withError config interp env locVars callerFunBinds (RForLoop t idxInit idxEnd accInit idx acc body)
  = do
      idxInit' <- replaceFunCallErr withError config interp env locVars callerFunBinds idxInit
      idxEnd' <- replaceFunCallErr withError config interp env locVars callerFunBinds idxEnd
      accInit' <- replaceFunCallErr withError config interp env locVars callerFunBinds accInit
      body' <- replaceFunCallErr withError config interp env locVars callerFunBinds body
      return $ RForLoop t idxInit' idxEnd' accInit' idx acc body'
replaceFunCallErr withError config interp env locVars callerFunBinds (ErrFma t ae1 ee1 ae2 ee2 ae3 ee3)
  = ErrFma t <$> replaceFunCallErr withError config interp env locVars callerFunBinds ae1
             <*> replaceFunCallErr withError config interp env locVars callerFunBinds ee1
             <*> replaceFunCallErr withError config interp env locVars callerFunBinds ae2
             <*> replaceFunCallErr withError config interp env locVars callerFunBinds ee2
             <*> replaceFunCallErr withError config interp env locVars callerFunBinds ae3
             <*> replaceFunCallErr withError config interp env locVars callerFunBinds ee3
replaceFunCallErr withError config interp env locVars callerFunBinds (ErrBinOp op t ae1 ee1 ae2 ee2)
  = ErrBinOp op t <$> replaceFunCallErr withError config interp env locVars callerFunBinds ae1
                  <*> replaceFunCallErr withError config interp env locVars callerFunBinds ee1
                  <*> replaceFunCallErr withError config interp env locVars callerFunBinds ae2
                  <*> replaceFunCallErr withError config interp env locVars callerFunBinds ee2
replaceFunCallErr withError config interp env locVars callerFunBinds (ErrUnOp op t ae ee)
  = ErrUnOp op t <$> replaceFunCallErr withError config interp env locVars callerFunBinds ae
                 <*> replaceFunCallErr withError config interp env locVars callerFunBinds ee
replaceFunCallErr withError config interp env locVars callerFunBinds (ErrCast t1 t2 ae ee)
  = ErrCast t1 t2 <$> replaceFunCallErr withError config interp env locVars callerFunBinds ae
                  <*> replaceFunCallErr withError config interp env locVars callerFunBinds ee
replaceFunCallErr withError config interp env locVars callerFunBinds (ErrMulPow2R t i ee)
  = ErrMulPow2R t i <$> replaceFunCallErr withError config interp env locVars callerFunBinds ee
replaceFunCallErr withError config interp env locVars callerFunBinds (ErrMulPow2L t i ee)
  = ErrMulPow2L t i <$> replaceFunCallErr withError config interp env locVars callerFunBinds ee
replaceFunCallErr withError config interp env locVars callerFunBinds (ErrSubSternenz t ae1 ee1 ae2 ee2)
  = ErrSubSternenz t <$> replaceFunCallErr withError config interp env locVars callerFunBinds ae1
                     <*> replaceFunCallErr withError config interp env locVars callerFunBinds ae1
                     <*> replaceFunCallErr withError config interp env locVars callerFunBinds ae2
                     <*> replaceFunCallErr withError config interp env locVars callerFunBinds ee2
replaceFunCallErr withError config interp env locVars callerFunBinds (ErrFloorNoRound t ae ee)
  = ErrFloorNoRound t <$> replaceFunCallErr withError config interp env locVars callerFunBinds ae
                      <*> replaceFunCallErr withError config interp env locVars callerFunBinds ee
replaceFunCallErr withError config interp env locVars callerFunBinds (HalfUlp ae t)
  = do
      expr <- replaceFunCallErr withError config interp env locVars callerFunBinds ae
      return $ HalfUlp expr t
replaceFunCallErr withError config interp env locVars callerFunBinds (MaxErr aes)
  = MaxErr <$> mapM (replaceFunCallErr withError config interp env locVars callerFunBinds) aes
replaceFunCallErr _ config _ _ _ _ aexpr = return aexpr

