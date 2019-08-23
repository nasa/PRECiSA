-- Notices:
--
-- Copyright 2019 United States Government as represented by the Administrator of the National Aeronautics and Space Administration.
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
-- FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIE≠NT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH
-- MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.

module Transformation where

import AbsPVSLang
import FPrec
import AbstractSemantics
import Control.Monad.State
import Data.Maybe (fromJust)
import Utils

type ErrVarEnv = [(VarName,FAExpr,FBExpr)]

data FreshErrVar = FreshErrVar { env :: ErrVarEnv,
                                 count :: Int }
  deriving (Show)
  
type ErrVarInterp = [(FunName, FreshErrVar)]

findInErrVarInterp :: FunName -> ErrVarInterp -> FreshErrVar
findInErrVarInterp f interp = fromJust $ lookup f interp

updateErrVarInterp :: FunName -> FreshErrVar -> ErrVarInterp -> ErrVarInterp
updateErrVarInterp f newFreshErrVar = map replaceFreshErrVar
  where
    replaceFreshErrVar (g,freshErrVar) | f == g    = (g,newFreshErrVar)
                                       | otherwise = (g,freshErrVar)

listErrEnvVars :: Ord a => [(a, b, c)] -> [a]
listErrEnvVars envFreshVars = elimDuplicates $ map fst3 envFreshVars


transformProgramSymb :: [Decl] -> [(Decl, ErrVarEnv)]
transformProgramSymb decls = map (makePair interp') decls' 
  where
    (decls',interp') = runState (mapM (transformDeclSymb decls) decls) initState
    initState = map initStateDecl decls
    initStateDecl (Decl _ f _ _) = (f, FreshErrVar { env = [], count = 0 })
    makePair errInterp decl@(Decl  _ f _ _) = (decl, env (findInErrVarInterp f errInterp))
  
transformDeclSymb :: [Decl] -> Decl -> State ErrVarInterp Decl
transformDeclSymb decls (Decl fp f args stm) = do
  transformedSmt <- transformStmSymb decls f FBTrue stm
  currentState <- get
  let freshVars = findInErrVarInterp f currentState
  let envFreshVars = env freshVars
  let errVars = listErrEnvVars envFreshVars
  return $ Decl fp f (args ++ map errVar2Arg errVars) transformedSmt

errVar2Arg :: VarName -> Arg
errVar2Arg x = Arg x FPDouble

validityCheckFunCall :: [FAExpr] -> FBExpr
validityCheckFunCall [] = error "validityCheckFunCall: unexpected empty list."
validityCheckFunCall aes = listFAnd $ map IsValid aes

transformStmSymb :: [Decl] -> FunName -> FBExpr -> Stm -> State ErrVarInterp Stm
transformStmSymb decls f be (Let x fp faexpr stm) = do
  transformedStm <- transformStmSymb decls f be stm
  transformedAExpr <- transformAExprSymb decls f be faexpr
  let transfomedLet = Let x fp transformedAExpr transformedStm
  case funCallListFAExpr transformedAExpr of
    []  -> return transfomedLet
    fcs -> return $ Ite (validityCheckFunCall fcs) transfomedLet UnstWarning

transformStmSymb decls f be (Ite fbexpr thenStm elseStm) = do
  thenStmTran <- transformStmSymb decls f (simplFAnd be fbexpr)  thenStm
  elseStmTran <- transformStmSymb decls f (simplFAnd be (FNot fbexpr)) elseStm
  if noRoundOffErrorIn fbexpr
    then return $ Ite fbexpr thenStmTran elseStmTran
  else do
    bePlus  <- betaPlusVar  f be fbexpr --(FAnd be       fbexpr) 
    beMinus <- betaMinusVar f be fbexpr --(FAnd be (FNot fbexpr)) 
    return $ Ite bePlus thenStmTran (Ite beMinus elseStmTran UnstWarning)

transformStmSymb decls f be (ListIte listThen elseStm) = do
  transformedListThen <- makeNewListThen listThen [] 
  if all (noRoundOffErrorIn . fst) listThen
  then do 
    listThenTran <- mapM (transformStmSymb decls f be . snd) listThen
    elseStmTran <- transformStmSymb decls f be elseStm
    return $ ListIte (zip (map fst listThen) listThenTran) elseStmTran
  else
    return $ ListIte transformedListThen UnstWarning
  where

      makeNewListThen :: [(FBExpr, Stm)] -> [FBExpr] -> State ErrVarInterp [(FBExpr, Stm)]
  --    makeNewListThen [] []  = error "makeNewListThen: "
      makeNewListThen [] listElseBes = do
          transformedElseSmt <- transformStmSymb decls f be elseStm
          newlistElseBes <- mapM (betaMinusVar f be) listElseBes
          return [(listFAnd newlistElseBes, transformedElseSmt)]
      makeNewListThen ((thenBe,stm):rest) listElseBes = do
          newThenBranch <- makeNewThenBranch thenBe listElseBes stm
          newListThen <- if noRoundOffErrorIn thenBe
                            then makeNewListThen rest listElseBes
                            else makeNewListThen rest (thenBe:listElseBes) 
          return (newThenBranch:newListThen)

      makeNewThenBranch :: FBExpr -> [FBExpr] -> Stm -> State ErrVarInterp (FBExpr, Stm)
      makeNewThenBranch thenBe listElseBes stm = do
          newlistElseBes <- mapM (betaMinusVar f be) listElseBes
          transformedStm <- transformStmSymb decls f be stm
          if noRoundOffErrorIn thenBe
          then
            return (foldl FAnd thenBe newlistElseBes, transformedStm)
          else do
            bePlus <- betaPlusVar f be thenBe
            return (foldl FAnd bePlus newlistElseBes, transformedStm)

transformStmSymb decls f be (StmExpr ae) = do
  transformedAExpr <- transformAExprSymb decls f be ae
  case funCallListFAExpr transformedAExpr of
    []  -> return $ StmExpr transformedAExpr
    fcs -> return $ Ite (validityCheckFunCall fcs) (StmExpr transformedAExpr) UnstWarning

transformStmSymb decls f be (ForLoop retType startIdx endIdx initValueAcc idx acc forBody) = do
  transformedForBody <- transformStmSymb decls f be forBody
  -- case funCallListStm transformedForBody of
    -- []  ->
  return $ ForLoop retType startIdx endIdx initValueAcc idx acc transformedForBody
    -- fcs -> return $ ForLoop retType startIdx endIdx initValueAcc idx acc (Ite (validityCheckFunCall fcs) transformedForBody UnstWarning)

transformStmSymb _ _ _ UnstWarning = return UnstWarning


transformAExprSymb :: [Decl] -> FunName -> FBExpr -> FAExpr -> State ErrVarInterp FAExpr
transformAExprSymb decls g be (FEFun f fp actArgs) =  do
  currentStateEnv  <- get
  let fCurrentState = findInErrVarInterp f currentStateEnv
  let (_,formArgs,stm) = findInDecls f decls
  let locVars = localVars stm
  newErrArgs <- mapM (generateErrVarArg g be . argsBindFAExpr formArgs actArgs . replaceLocVarsFix locVars . snd3) (env fCurrentState)
  return $ FEFun f fp (actArgs++newErrArgs)

transformAExprSymb decls g be (FIAdd ae1 ae2) = do
  ae1' <- transformAExprSymb decls g be ae1
  ae2' <- transformAExprSymb decls g be ae2
  return $ FIAdd ae1' ae2'

transformAExprSymb decls g be (FISub ae1 ae2) = do
  ae1' <- transformAExprSymb decls g be ae1
  ae2' <- transformAExprSymb decls g be ae2
  return $ FISub ae1' ae2'

transformAExprSymb decls g be (FIMul ae1 ae2) = do
  ae1' <- transformAExprSymb decls g be ae1
  ae2' <- transformAExprSymb decls g be ae2
  return $ FIMul ae1' ae2'

transformAExprSymb decls g be (FIDiv ae1 ae2) = do
  ae1' <- transformAExprSymb decls g be ae1
  ae2' <- transformAExprSymb decls g be ae2
  return $ FIDiv ae1' ae2'

transformAExprSymb decls g be (FItDiv ae1 ae2) = do
  ae1' <- transformAExprSymb decls g be ae1
  ae2' <- transformAExprSymb decls g be ae2
  return $ FItDiv ae1' ae2'

transformAExprSymb decls g be (FIMod ae1 ae2) = do
  ae1' <- transformAExprSymb decls g be ae1
  ae2' <- transformAExprSymb decls g be ae2
  return $ FIMod ae1' ae2'

transformAExprSymb decls g be (FItMod ae1 ae2) = do
  ae1' <- transformAExprSymb decls g be ae1
  ae2' <- transformAExprSymb decls g be ae2
  return $ FItMod ae1' ae2'

transformAExprSymb decls g be (FIExp ae1 ae2) = do
  ae1' <- transformAExprSymb decls g be ae1
  ae2' <- transformAExprSymb decls g be ae2
  return $ FIExp ae1' ae2' 

transformAExprSymb decls g be (FIPow ae1 ae2) = do
  ae1' <- transformAExprSymb decls g be ae1
  ae2' <- transformAExprSymb decls g be ae2
  return $ FIPow ae1' ae2'

transformAExprSymb decls g be (FINeg ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ FINeg ae1'

transformAExprSymb decls g be (FIAbs ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ FIAbs ae1'

transformAExprSymb decls g be (FFma fp ae1 ae2 ae3) = do
  ae1' <- transformAExprSymb decls g be ae1
  ae2' <- transformAExprSymb decls g be ae2
  ae3' <- transformAExprSymb decls g be ae3
  return $ FFma fp ae1' ae2' ae3'

transformAExprSymb decls g be (FAdd fp ae1 ae2) = do
  ae1' <- transformAExprSymb decls g be ae1
  ae2' <- transformAExprSymb decls g be ae2
  return $ FAdd fp ae1' ae2'

transformAExprSymb decls g be (FSub fp ae1 ae2) = do
  ae1' <- transformAExprSymb decls g be ae1
  ae2' <- transformAExprSymb decls g be ae2
  return $ FSub fp ae1' ae2'

transformAExprSymb decls g be (FMul fp ae1 ae2) = do
  ae1' <- transformAExprSymb decls g be ae1
  ae2' <- transformAExprSymb decls g be ae2
  return $ FMul fp ae1' ae2'

transformAExprSymb decls g be (FDiv fp ae1 ae2) = do
  ae1' <- transformAExprSymb decls g be ae1
  ae2' <- transformAExprSymb decls g be ae2
  return $ FDiv fp ae1' ae2'

transformAExprSymb decls g be (FPow fp ae1 ae2) = do
  ae1' <- transformAExprSymb decls g be ae1
  ae2' <- transformAExprSymb decls g be ae2
  return $ FPow fp ae1' ae2'

transformAExprSymb decls g be (FMod fp ae1 ae2) = do
  ae1' <- transformAExprSymb decls g be ae1
  ae2' <- transformAExprSymb decls g be ae2
  return $ FMod fp ae1' ae2'

transformAExprSymb decls g be (FNeg fp ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ FNeg fp ae1'

transformAExprSymb decls g be (FFloor fp ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ FFloor fp ae1'

transformAExprSymb decls g be (FSqrt fp ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ FSqrt fp ae1'

transformAExprSymb decls g be (FAbs fp ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ FAbs fp ae1'

transformAExprSymb decls g be (FSin fp ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ FSin fp ae1'

transformAExprSymb decls g be (FCos fp ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ FCos fp ae1'

transformAExprSymb decls g be (FTan fp ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ FTan fp ae1'

transformAExprSymb decls g be (FAcos fp ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ FAcos fp ae1'

transformAExprSymb decls g be (FAsin fp ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ FAsin fp ae1'

transformAExprSymb decls g be (FAtan fp ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ FAtan fp ae1'

transformAExprSymb decls g be (FLn fp ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ FLn fp ae1'

transformAExprSymb decls g be (FExpo fp ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ FExpo fp ae1'

transformAExprSymb decls g be (StoD ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ StoD ae1'

transformAExprSymb decls g be (DtoS ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ DtoS ae1'

transformAExprSymb decls g be (ItoD ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ ItoD ae1'

transformAExprSymb decls g be (ItoS ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ ItoS ae1'

transformAExprSymb decls g be (Value ae1) = do
  ae1' <- transformAExprSymb decls g be ae1
  return $ Value ae1'

transformAExprSymb _ _ _ ae  = return ae

replaceLocVarsFix :: [(VarName, FAExpr)] -> FAExpr -> FAExpr
replaceLocVarsFix locVars ae =
  if ae == ae' then ae
  else replaceLocVarsFix locVars ae'
  where
    ae' = replaceLocVars locVars ae

replaceLocVars :: [(VarName, FAExpr)] -> FAExpr -> FAExpr
replaceLocVars locVars = argsBindFAExpr (map ((`Arg` FPDouble) . fst) locVars) (map snd locVars)

generateErrVarArg :: FunName -> FBExpr -> FAExpr -> State ErrVarInterp FAExpr
generateErrVarArg f be ae = do
  currentStateEnv <- get 
  let currentState = findInErrVarInterp f currentStateEnv
  let (freshName,newEnv,newCount) = generateVarName ae (env currentState) (count currentState) be
  put $ updateErrVarInterp f (FreshErrVar { env = newEnv, count = newCount}) currentStateEnv
  return $ FVar FPDouble freshName

generateVarName :: FAExpr -> [(VarName, FAExpr, FBExpr)] -> Int -> FBExpr -> (String, ErrVarEnv,Int)
generateVarName ae environment counter be =
  case findInEnv (equivModuloIndex ae) environment of
  --case (lookup ae (map swap env)) of
    Just (v, ae',_)  -> if ae == ae' then (v,environment,counter) else (v,environment++[(v, ae, be)],counter)
    -- TODO when be' and be are different keep the most restrictive one?
    Nothing -> (freshName, environment++[(freshName, ae, be)], counter + 1)
    where
      freshName = "E_"++ show counter
      -- TODO: all the calls with array are considered as if was the same var
      --lookup' (FArrayElem fp v idx) [] = Nothing
      --lookup' (FArrayElem fp v idx) (((FArrayElem fp' v' idx'), ae):rest) | v == v' = Just ae
      --                                                                    | otherwise = lookup' (FArrayElem fp v idx) rest
      --lookup' a list = lookup a list                                         

findInEnv :: (FAExpr -> Bool) -> ErrVarEnv -> Maybe (VarName, FAExpr,FBExpr)
findInEnv _ [] = Nothing
findInEnv p ((ev, ae, be):environment) | p ae      = Just (ev, ae, be)
                                       | otherwise = findInEnv p environment

updateBetaState :: FunName -> FBExpr -> FAExpr -> State ErrVarInterp VarName
updateBetaState f pathCond ae1 = do
  currentStateEnv <- get 
  let currentState = findInErrVarInterp f currentStateEnv
  let (freshName,newEnv,newCount) = generateVarName ae1 (env currentState) (count currentState) pathCond
  put $ updateErrVarInterp f (FreshErrVar { env = newEnv, count = newCount}) currentStateEnv
  return freshName

betaPlusVar :: FunName -> FBExpr -> FBExpr -> State ErrVarInterp FBExpr
betaPlusVar f pathCond (FOr  be1 be2) = do
  be1' <- betaPlusVar f pathCond be1
  be2' <- betaPlusVar f pathCond be2
  return $ FOr be1' be2'

betaPlusVar f pathCond (FAnd be1 be2) = do
  be1' <- betaPlusVar f pathCond be1
  be2' <- betaPlusVar f pathCond be2
  return $ FAnd be1' be2'
betaPlusVar f pathCond (FNot be) = betaMinusVar f pathCond be

betaPlusVar f pathCond (FLt ae1 ae2) | isZeroExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  return $ FLt ae1 (FNeg FPDouble (FVar FPDouble freshName))

betaPlusVar f pathCond (FLt ae2 ae1) | isZeroExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  return $ FLt (FVar FPDouble freshName) ae1 

betaPlusVar f pathCond (FLtE ae1 ae2) | isZeroExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  return $ FLtE ae1 (FNeg FPDouble (FVar FPDouble freshName))

betaPlusVar f pathCond (FLtE ae2 ae1) | isZeroExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  return $ FLtE (FVar FPDouble freshName) ae1

betaPlusVar f pathCond (FGt  ae1 ae2) | isZeroExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  return $ FGt ae1 (FVar FPDouble freshName)

betaPlusVar f pathCond (FGt ae2 ae1)| isZeroExpr ae2  = do
  freshName <- updateBetaState f pathCond ae1
  return $ FGt (FNeg FPDouble (FVar FPDouble freshName)) ae1

betaPlusVar f pathCond (FGtE ae1 ae2) | isZeroExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  return $ FGtE ae1 (FVar FPDouble freshName)

betaPlusVar f pathCond (FGtE ae2 ae1) | isZeroExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  return $ FGtE (FNeg FPDouble (FVar FPDouble freshName)) ae1

betaPlusVar _ _ FBTrue  = return FBTrue
betaPlusVar _ _ FBFalse = return FBFalse
betaPlusVar _ _ ae = error $ "betaPlusVar: not implemented yet for " ++ show ae


betaMinusVar :: FunName -> FBExpr -> FBExpr -> State ErrVarInterp FBExpr
betaMinusVar f pathCond (FOr  be1 be2) = do
    be1' <- betaMinusVar f pathCond be1
    be2' <- betaMinusVar f pathCond be2
    return $ FAnd be1' be2'

betaMinusVar f pathCond (FAnd be1 be2) = do
    be1' <- betaMinusVar f pathCond be1
    be2' <- betaMinusVar f pathCond be2
    return $ FOr be1' be2'

betaMinusVar f pathCond (FNot be) = betaPlusVar f pathCond be

betaMinusVar f pathCond (FLt  ae1 ae2) | isZeroExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  return $ FGtE ae1 (FVar FPDouble freshName)

betaMinusVar f pathCond (FLt ae2 ae1) | isZeroExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  return $ FGtE (FNeg FPDouble (FVar FPDouble freshName)) ae1

betaMinusVar f pathCond (FLtE ae1 ae2) | isZeroExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  return $ FGt ae1 (FVar FPDouble freshName)

betaMinusVar f pathCond (FLtE ae2 ae1) | isZeroExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  return $ FGt (FNeg FPDouble (FVar FPDouble freshName)) ae1

betaMinusVar f pathCond (FGt  ae1 ae2) | isZeroExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  return $ FLtE ae1 (FNeg FPDouble (FVar FPDouble freshName))

betaMinusVar f pathCond (FGt ae2 ae1) | isZeroExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  return $ FLtE (FVar FPDouble freshName) ae1

betaMinusVar f pathCond (FGtE ae1 ae2) | isZeroExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  return $ FLt ae1 (FNeg FPDouble (FVar FPDouble freshName))

betaMinusVar f pathCond (FGtE ae2 ae1) | isZeroExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  return $ FLt (FVar FPDouble freshName) ae1

betaMinusVar _ _ FBTrue  = return FBFalse
betaMinusVar _ _ FBFalse = return FBTrue
betaMinusVar _ _ be = error $ "betaMinus niy for "++ show be
