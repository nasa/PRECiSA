module SMT.GenerateSMTModel where

import AbstractSemantics
import AbstractDomain
import AbsPVSLang
import AbsSpecLang
import FPrec
import Control.Monad.State
import Data.List(find)
import Data.Maybe(mapMaybe)
import Kodiak.KodiakRunnable
import Kodiak.KodiakRunner
import Common.DecisionPath
import Utils
import Translation.Float2Real

type ReplaceFPState = State (Int,[(VarName, FAExpr, FPrec)])
type ReplaceRState  = State (Int,[(VarName,  AExpr)])

data SMTmodel = SMT [(VarName, FPrec)] [VarName] Conditions

baseFPVarName :: String
baseFPVarName = "Temp_"

baseRVarNameTemp :: String
baseRVarNameTemp  = "Real_Temp_"

genSMTConstraints :: Conditions -> [VarBind] -> IO (Conditions, BExpr, [VarName], [(VarName, FPrec)], [VarName])
genSMTConstraints (Cond cs) inputs = do
    (errorConstraints, newErrVars) <- generateErrorConstraints aexprMap fpexprMap inputs
    return (Cond $ zip newBe newFbe, errorConstraints, newRVars, newFPVars, newErrVars)
    where
        aexprMap  = snd finalRState
        newRVars  = map fst (snd finalRState)
        fpexprMap = snd finalFPState
        newFPVars = map (\(var, _, fp) -> (var,fp)) (snd finalFPState)
        (newFbe, finalFPState) =  runState (mapM (replaceFreshVarInFBExpr . snd) cs) (0,[])
        ( newBe,  finalRState) =  runState (mapM (replaceFreshVarInBExpr  . fst) cs) (0,[])

generateErrorConstraints :: [(VarName, AExpr)] -> [(VarName,  FAExpr, FPrec)] -> [VarBind] -> IO (BExpr,[VarName])
generateErrorConstraints [] _ inputs = do
    bExprInput <- mapM generateErrorConstraintInput inputs
    -- eliminate the occurencies of true
    return (And (foldlWithDefault And BTrue (map fst bExprInput))
                  (foldlWithDefault And BTrue (map (buildErrAssignConstraints . snd) bExprInput))
             , map (fst . snd) bExprInput)
generateErrorConstraints aexprMap fpexprMap inputs = do
    bExprs <- mapM (\(rVar,fVar,fpAExpr) -> generateErrorConstraint rVar fVar fpAExpr inputs) triples
    bExprInput <- mapM generateErrorConstraintInput inputs
    -- eliminate the occurencies of true
    return (And (And (foldlWithDefault And BTrue (map fst bExprs)) (foldlWithDefault And BTrue (map fst bExprInput)))
                 (And (foldlWithDefault And BTrue (map (buildErrAssignConstraints . snd) bExprs))
                      (foldlWithDefault And BTrue (map (buildErrAssignConstraints . snd) bExprInput)))
            , map (fst . snd) bExprs ++ map (fst . snd) bExprInput)
    where
        getRealVar faexpr = fst <$> find ((== fae2real faexpr) . snd) aexprMap
        triples = mapMaybe (\ (fvar, fpexp, _) -> getRealVar fpexp >>= \ x -> return (x, fvar, fpexp)) fpexprMap
         
buildErrAssignConstraints :: Real a => (VarName, a) -> BExpr
buildErrAssignConstraints (e, roError) = Eq (Var Real e) (Rat $ toRational roError)

generateErrorConstraintInput :: VarBind -> IO (BExpr,(VarName, Double))
generateErrorConstraintInput range@(VarBind x fp _ _) = do
    roError <- computeErrorAExpr (FVar fp x) [range]
    let errVarName = "Err_"++x
    return (GtE (Var Real errVarName) (Abs $ Sub (RealMark x) (Var Real x)), (errVarName, roError))

generateErrorConstraint :: VarName -> VarName -> FAExpr -> [VarBind] -> IO (BExpr,(VarName, Double))
generateErrorConstraint rVar fVar faexpr inputs = do
    roError <- computeErrorAExpr faexpr inputs
    let ev = "Err_"++fVar
    return (generateErrorConstraint' rVar fVar (getFPrec faexpr) ev, (ev, roError))

generateErrorConstraint' :: VarName -> VarName -> FPrec -> VarName -> BExpr
generateErrorConstraint' rVar fVar fp ev = GtE (Var Real ev) (Abs $ Sub (f2r(FVar fp fVar)) (Var Real rVar))
    where
        f2r = case fp of
                FPSingle -> StoR
                FPDouble -> DtoR
                _ -> error "generateErrorConstraint': unexpected value."

computeErrorAExpr :: FAExpr -> [VarBind] -> IO Double
computeErrorAExpr ae varBind = maximumUpperBound <$> run kodiakInput ()
    where
        sem = map initErrAceb $ aexprSem ae emptyInterpretation emptyEnv root ""
        errorExpr = MaxErr (map eExpr sem)
        kodiakInput = KI { name = "", expression = errorExpr, bindings = varBind, maxDepth = maxDepth', precision = precision'}
        maxDepth' = 7
        precision' = 14

genFreshVar :: AExpr -> ReplaceRState AExpr
genFreshVar ae = do
    (counter,varMap) <- get
    let freshVar = baseRVarNameTemp ++ show counter
    let newMap = (freshVar, ae):varMap
    put (counter+1,newMap)
    return $ Var Real freshVar

replaceFreshVarInBExpr :: BExpr -> ReplaceRState BExpr
replaceFreshVarInBExpr BTrue = return BTrue
replaceFreshVarInBExpr BFalse = return BFalse
replaceFreshVarInBExpr (Or be1 be2) = do
    be1' <- replaceFreshVarInBExpr be1
    be2' <- replaceFreshVarInBExpr be2
    return $ uncurry Or (be1',be2')
replaceFreshVarInBExpr (And be1 be2) = do
    be1' <- replaceFreshVarInBExpr be1
    be2' <- replaceFreshVarInBExpr be2
    return $ uncurry And (be1',be2')
replaceFreshVarInBExpr (Not be) = do
    be' <- replaceFreshVarInBExpr be
    return $ Not be'
replaceFreshVarInBExpr (Eq ae1 ae2) = do
    ae1' <- replaceFreshVarInAExpr ae1
    ae2' <- replaceFreshVarInAExpr ae2
    return $ uncurry Eq (ae1',ae2')
replaceFreshVarInBExpr (Neq ae1 ae2) = do
    ae1' <- replaceFreshVarInAExpr ae1
    ae2' <- replaceFreshVarInAExpr ae2
    return $ uncurry Neq (ae1',ae2')
replaceFreshVarInBExpr (Lt ae1 ae2) = do
    ae1' <- replaceFreshVarInAExpr ae1
    ae2' <- replaceFreshVarInAExpr ae2
    return $ uncurry Lt (ae1',ae2')
replaceFreshVarInBExpr (LtE ae1 ae2) = do
    ae1' <- replaceFreshVarInAExpr ae1
    ae2' <- replaceFreshVarInAExpr ae2
    return $ uncurry LtE (ae1',ae2')
replaceFreshVarInBExpr (Gt ae1 ae2) = do
    ae1' <- replaceFreshVarInAExpr ae1
    ae2' <- replaceFreshVarInAExpr ae2
    return $ uncurry Gt (ae1',ae2')
replaceFreshVarInBExpr (GtE ae1 ae2) = do
    ae1' <- replaceFreshVarInAExpr ae1
    ae2' <- replaceFreshVarInAExpr ae2
    return $ uncurry GtE (ae1',ae2')

replaceFreshVarInAExpr :: AExpr -> ReplaceRState AExpr
replaceFreshVarInAExpr ae@(Int _) = return ae
replaceFreshVarInAExpr ae@(Rat _) = return ae
replaceFreshVarInAExpr ae@(Var _ _) = return ae
replaceFreshVarInAExpr ae@(RealMark _) = return ae
replaceFreshVarInAExpr ae@(ErrorMark _ _) = return ae
replaceFreshVarInAExpr ae@(StoR (FVar _ _)) = return ae
replaceFreshVarInAExpr ae@(DtoR (FVar _ _)) = return ae
replaceFreshVarInAExpr ae@(StoR (FInt _)) = return ae
replaceFreshVarInAExpr ae@(DtoR (FInt _)) = return ae
replaceFreshVarInAExpr ae@(Neg (Int _))   = return ae
replaceFreshVarInAExpr ae@(Neg (Rat _))   = return ae
replaceFreshVarInAExpr ae         =
    do  (_,varMap) <- get
        case find ((== ae) . snd) varMap of
            Nothing            -> genFreshVar ae
            Just (var,_) -> return $ Var Real var

genFreshFVar :: FAExpr -> FPrec -> ReplaceFPState FAExpr
genFreshFVar ae fp = do
    (counter,varMap) <- get
    let freshVar = baseFPVarName ++ show counter
    let newMap = (freshVar, ae, fp):varMap
    put (counter+1,newMap)
    return $ FVar fp freshVar

replaceFreshVarInFBExpr :: FBExpr -> ReplaceFPState FBExpr
replaceFreshVarInFBExpr FBTrue  = return FBTrue
replaceFreshVarInFBExpr FBFalse = return FBFalse
replaceFreshVarInFBExpr (FOr be1 be2) = do
    be1' <- replaceFreshVarInFBExpr be1
    be2' <- replaceFreshVarInFBExpr be2
    return $ uncurry FOr (be1',be2')
replaceFreshVarInFBExpr (FAnd be1 be2) = do
    be1' <- replaceFreshVarInFBExpr be1
    be2' <- replaceFreshVarInFBExpr be2
    return $ uncurry FAnd (be1',be2')
replaceFreshVarInFBExpr (FNot be) = do
    be' <- replaceFreshVarInFBExpr be
    return $ FNot be'
replaceFreshVarInFBExpr (FEq ae1 ae2) = do
    ae1' <- replaceFreshVarInFAExpr ae1
    ae2' <- replaceFreshVarInFAExpr ae2
    return $ uncurry FEq (ae1',ae2')
replaceFreshVarInFBExpr (FNeq ae1 ae2) = do
    ae1' <- replaceFreshVarInFAExpr ae1
    ae2' <- replaceFreshVarInFAExpr ae2
    return $ uncurry FNeq (ae1',ae2')
replaceFreshVarInFBExpr (FLt ae1 ae2) = do
    ae1' <- replaceFreshVarInFAExpr ae1
    ae2' <- replaceFreshVarInFAExpr ae2
    return $ uncurry FLt (ae1',ae2')
replaceFreshVarInFBExpr (FLtE ae1 ae2) = do
    ae1' <- replaceFreshVarInFAExpr ae1
    ae2' <- replaceFreshVarInFAExpr ae2
    return $ uncurry FLtE (ae1',ae2')
replaceFreshVarInFBExpr (FGt ae1 ae2) = do
    ae1' <- replaceFreshVarInFAExpr ae1
    ae2' <- replaceFreshVarInFAExpr ae2
    return $ uncurry FGt (ae1',ae2')
replaceFreshVarInFBExpr (FGtE ae1 ae2) = do
    ae1' <- replaceFreshVarInFAExpr ae1
    ae2' <- replaceFreshVarInFAExpr ae2
    return $ uncurry FGtE (ae1',ae2')
replaceFreshVarInFBExpr (IsValid ae) = do
    ae' <- replaceFreshVarInFAExpr ae
    return $ IsValid ae'

replaceFreshVarInFAExpr :: FAExpr -> ReplaceFPState FAExpr
replaceFreshVarInFAExpr ae@(FInt _)    = return ae
replaceFreshVarInFAExpr ae@(FCnst _ _) = return ae
replaceFreshVarInFAExpr ae@(FVar _ _)    = return ae
replaceFreshVarInFAExpr ae@(RtoS (Var _ _)) = return ae
replaceFreshVarInFAExpr ae@(RtoD (Var _ _)) = return ae
replaceFreshVarInFAExpr ae@(RtoS (Int _)) = return ae
replaceFreshVarInFAExpr ae@(RtoD (Int _)) = return ae
replaceFreshVarInFAExpr ae@(RtoS (Rat _)) = return ae
replaceFreshVarInFAExpr ae@(RtoD (Rat _)) = return ae
replaceFreshVarInFAExpr ae@(FNeg _ (FInt _))   = return ae
replaceFreshVarInFAExpr ae@(FNeg _ (RtoS (Int _)))   = return ae
replaceFreshVarInFAExpr ae@(FNeg _ (RtoD (Int _)))   = return ae
replaceFreshVarInFAExpr ae@(FNeg _ (RtoS (Rat _)))   = return ae
replaceFreshVarInFAExpr ae@(FNeg _ (RtoD (Rat _)))   = return ae
replaceFreshVarInFAExpr ae =
    do  (_,varMap) <- get
        case find ((== ae) . second) varMap of
            Nothing            -> genFreshFVar ae fp
            Just (var,_,_) -> return $ FVar fp var
    where
        fp = getFPrec ae
        second (_,t,_) = t

