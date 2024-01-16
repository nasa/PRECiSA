-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module SMT.GenerateSMTModel where

import AbstractSemantics
import AbstractDomain
import AbsPVSLang
import AbsSpecLang
import PVSTypes
import Control.Monad.State
import Data.List(find,zip4)
import Data.Maybe(mapMaybe,fromMaybe)
import Kodiak.Runnable
import Kodiak.Runner
import Common.DecisionPath
import Utils
import Translation.Float2Real
import Operators
import Common.TypesUtils
import Foreign.C
import FreshVariables

data SMTmodel = SMT [(VarName, PVSType)] [VarName] Conditions

genSMTConstraints :: CUInt -> CUInt -> Conditions -> [VarBind] -> IO (Conditions, BExpr, [VarName], [(VarName, PVSType)], [VarName])
genSMTConstraints maximumDepth minimumPrecision (Conds cs) inputs = do
    (errorConstraints, newErrVars) <- generateErrorConstraints maximumDepth minimumPrecision aexprMap fpexprMap inputs
    return (Conds $ newCond, errorConstraints, newRVars, newFPVars, newErrVars)
    where
        aexprMap  = snd finalRState
        newRVars  = map fst (snd finalRState)
        fpexprMap = snd finalFPState
        newFPVars = map (\(var, _, fp) -> (var,fp)) (snd finalFPState)
        newCond = map toCond (zip4 newRealPathCond newFpPathCond newRealCond newFpCond)
        (newRealPathCond, _) =  runState (mapM (replaceFreshVarInBExpr  . realPathCond) cs) (0,[])
        (newFpPathCond, _) =  runState (mapM (replaceFreshVarInFBExpr . fpPathCond) cs) (0,[])
        (newRealCond, finalRState) =  runState (mapM (replaceFreshVarInBExpr  . realCond) cs) (0,[])
        (newFpCond, finalFPState) =  runState (mapM (replaceFreshVarInFBExpr . fpCond) cs) (0,[])

generateErrorConstraints :: CUInt -> CUInt -> [(VarName, AExpr)] -> [(VarName,  FAExpr, PVSType)] -> [VarBind] -> IO (BExpr,[VarName])
generateErrorConstraints maximumDepth minimumPrecision [] _ inputs = do
    bExprInput <- mapM (generateErrorConstraintInput maximumDepth minimumPrecision) inputs
    -- eliminate the occurencies of true
    return (And (foldlWithDefault And BTrue (map fst bExprInput))
                  (foldlWithDefault And BTrue (map (buildErrAssignConstraints . snd) bExprInput))
             , map (fst . snd) bExprInput)
generateErrorConstraints maximumDepth minimumPrecision aexprMap fpexprMap inputs = do
    bExprs <- mapM (\(rVar,fVar,fpAExpr) -> generateErrorConstraint maximumDepth minimumPrecision rVar fVar fpAExpr inputs) triples
    bExprInput <- mapM (generateErrorConstraintInput maximumDepth minimumPrecision) inputs
    -- eliminate the occurencies of true
    return (And (And (foldlWithDefault And BTrue (map fst bExprs)) (foldlWithDefault And BTrue (map fst bExprInput)))
                 (And (foldlWithDefault And BTrue (map (buildErrAssignConstraints . snd) bExprs))
                      (foldlWithDefault And BTrue (map (buildErrAssignConstraints . snd) bExprInput)))
            , map (fst . snd) bExprs ++ map (fst . snd) bExprInput)
    where
        getRealVar faexpr = fst <$> find ((== fae2real faexpr) . snd) aexprMap
        triples = mapMaybe (\ (fvar, fpexp, _) -> getRealVar fpexp >>= \ x -> return (x, fvar, fpexp)) fpexprMap

buildErrAssignConstraints :: Real a => (VarName, a) -> BExpr
buildErrAssignConstraints (e, roError) = Rel Eq (Var Real e) (Rat $ toRational roError)

generateErrorConstraintInput :: CUInt -> CUInt -> VarBind -> IO (BExpr,(VarName, Double))
generateErrorConstraintInput  maximumDepth minimumPrecision range@(VarBind x fp _ _) = do
    roError <- computeErrorAExpr  maximumDepth minimumPrecision (FVar fp x) [range]
    let nameErrVar = "Err_"++x
    return (Rel GtE (Var Real nameErrVar) (UnaryOp AbsOp $ BinaryOp SubOp (RealMark x) (Var Real x)), (nameErrVar, roError))

generateErrorConstraint :: CUInt -> CUInt -> VarName -> VarName -> FAExpr -> [VarBind] -> IO (BExpr,(VarName, Double))
generateErrorConstraint maximumDepth minimumPrecision rVar fVar faexpr inputs = do
    roError <- computeErrorAExpr maximumDepth minimumPrecision faexpr inputs
    let ev = "Err_"++fVar
    return (generateErrorConstraint' rVar fVar (getPVSType faexpr) ev, (ev, roError))

generateErrorConstraint' :: VarName -> VarName -> PVSType -> VarName -> BExpr
generateErrorConstraint' rVar fVar fp ev = Rel GtE (Var Real ev) (UnaryOp AbsOp $ BinaryOp SubOp (FromFloat fp (FVar fp fVar)) (Var Real rVar))

computeErrorAExpr :: CUInt -> CUInt -> FAExpr -> [VarBind] -> IO Double
computeErrorAExpr maximumDepth minimumPrecision ae varBind = maximumUpperBound <$> run kodiakInput ()
  where
    semConf = SemConf {improveError = False
                      ,assumeTestStability = False
                      ,mergeUnstables = True
                      ,unfoldFunCalls = False }
    sem = map initErrAceb $ stmSem ae emptyInterpretation emptyEnv semConf root []
    errorExpr = MaxErr (map (fromMaybe (error "computeErrorAExpr: unexpected argument.") . eExpr) sem)
    kodiakInput = KI { kiName = ""
                     , kiExpression = errorExpr
                     , kiBindings = varBind
                     , kiMaxDepth = maximumDepth
                     , kiPrecision = minimumPrecision}
