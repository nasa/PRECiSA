-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.

module Transformation where

import AbsPVSLang
import PVSTypes
import AbstractSemantics
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.List (isSuffixOf)
import Utils
import Operators
import Common.TypesUtils
import AbsSpecLang
import Translation.Float2Real (fae2real')

type ErrVarEnv = [(VarName,FAExpr,FBExpr)]

data FreshErrVar = FreshErrVar { env :: ErrVarEnv,
                                 localEnv :: LocalEnv,
                                 count :: Int }
  deriving (Show)

data TransformationState = TransState { freshErrVar :: FreshErrVar
                                      , forExprMap :: [(FAExpr,AExpr)] }
  deriving (Show)

type FunMap a = [(FunName, a)]
type TranStateInterp = FunMap TransformationState

suffixTauPlus :: FunName
suffixTauPlus  = "_tauplus"

suffixTauMinus :: FunName
suffixTauMinus = "_tauminus"

isTauPlus :: PredAbs -> Bool
isTauPlus TauPlus = True
isTauPlus _       = False

isTauMinus :: PredAbs -> Bool
isTauMinus TauMinus = True
isTauMinus _        = False

tauPlusName :: FunName -> FunName
tauPlusName name = name ++ suffixTauPlus

tauMinusName :: FunName -> FunName
tauMinusName name = name ++ suffixTauMinus

origDeclName :: FunName -> FunName
origDeclName f | suffixTauPlus  `isSuffixOf` f = take (length f - length suffixTauPlus)  f
               | suffixTauMinus `isSuffixOf` f = take (length f - length suffixTauMinus) f
               | otherwise = f

findInTranStateInterp :: FunName -> TranStateInterp -> TransformationState
findInTranStateInterp f interp = fromMaybe
                                 (error $ "findFreshErrsInTranStateInterp: function" ++ show f ++ " not found " ++ show interp)
                                 (lookup f interp)

findFreshErrsInTranStateInterp :: FunName -> TranStateInterp -> FreshErrVar
findFreshErrsInTranStateInterp f interp = freshErrVar $ fromMaybe
                               (error $ "findFreshErrsInTranStateInterp: function" ++ show f ++ " not found in " ++ show interp)
                               (lookup f interp)

findOrigFunInProg :: FunName -> [Decl] -> Maybe Decl
findOrigFunInProg f decls =
  case findInProg f decls of
    Just d -> Just d
    Nothing -> findInProg (origDeclName f) decls

findOrigFunInRealProg :: FunName -> [RDecl] -> Maybe RDecl
findOrigFunInRealProg f decls =
  case findInRealProg f decls of
    Just d -> Just d
    Nothing -> findInRealProg (origDeclName f) decls

lookupOrigFun :: FunName -> [(FunName, a)] -> Maybe a
lookupOrigFun f list =
  case lookup f list of
    Just e -> Just e
    Nothing -> lookup (origDeclName f) list

findOrigFunInSpec :: FunName -> [SpecBind] -> Maybe [VarBind]
findOrigFunInSpec f list =
  case findInSpec f list of
    Just e -> Just e
    Nothing -> findInSpec (origDeclName f) list

updateFreshErrVars :: FunName -> FreshErrVar -> TranStateInterp -> TranStateInterp
updateFreshErrVars f newFreshErrVar st = let res = map replaceFreshErrVar st in res
  where
    replaceFreshErrVar (g,transSt) | f == g    = (g, transSt { freshErrVar = newFreshErrVar} )
                                   | otherwise = (g, transSt)

addToForExprMap :: FunName
                 -> AExpr
                 -> FAExpr
                 -> TranStateInterp
                 -> TranStateInterp
addToForExprMap f realFor tranFor = map addForExprPair
  where
    addForExprPair (g,transSt) | f == g    = (g, transSt { forExprMap = (tranFor,realFor):(forExprMap transSt)} )
                               | otherwise = (g, transSt)

declareLocalVars :: LocalEnv -> FAExpr -> FAExpr
declareLocalVars locEnv fae = if null locVars then fae else Let locVars fae
  where
    locVars = localVarsInExpr locEnv fae

localVarsInExpr :: LocalEnv -> FAExpr -> [(VarName, PVSType, FAExpr)]
localVarsInExpr locEnv fae = filter (\(v,_,_) -> v `elem` (localVarsInExpr' (varNameList fae) locEnv [])) locEnv
  where
    localVarsInExpr' :: [VarName] -> [(VarName, PVSType, FAExpr)] -> [VarName] -> [VarName]
    localVarsInExpr' [] _ acc = acc
    localVarsInExpr' (x:xs) locEnv' acc = maybe (localVarsInExpr' xs locEnv' acc)
                                           (\expr -> localVarsInExpr' (xs++(varNameList expr)) locEnv' (x:acc))
                                           (localEnvLookup x locEnv')

localEnvLookup :: VarName -> [(VarName, PVSType, FAExpr)] -> Maybe FAExpr
localEnvLookup _ [] = Nothing
localEnvLookup var ((x,_,expr):rest) | var == x = Just expr
                                     | otherwise = localEnvLookup var rest

transformProgramSymb :: RProgram -> [Decl] -> [(Decl, ErrVarEnv, LocalEnv, [(FAExpr,AExpr)])]
transformProgramSymb rprog decls =  map (makePair interp') tauDecls
  where
    (decls',interp') = runState (mapM (transformDeclSymb rprog decls) decls) initState
    tauDecls = concat decls'
    initState = concatMap initStateDecl decls
    initStateDecl decl = [(declName decl
                         ,TransState { freshErrVar = FreshErrVar { env = [], count = 0, localEnv = [] },
                                       forExprMap = [] })]
    makePair tranStateInterp decl = (decl, env $ freshErrVar tranState, localEnv $ freshErrVar tranState, forExprMap tranState)
      where
        tranState = findInTranStateInterp (declName decl) tranStateInterp

transformDeclSymb :: RProgram -> [Decl] -> Decl -> State TranStateInterp [Decl]
transformDeclSymb rprog decls (Decl _ fp f args stm) = do
  transformedSmt <- transformStmSymb rprog decls f FBTrue True stm
  currentState <- get
  let freshVars = findFreshErrsInTranStateInterp f currentState
  let errVars = map errVar2Arg $ listFst3 $ env freshVars
  return [Decl True fp f (args++errVars) transformedSmt]

transformDeclSymb rprog decls (Pred isTrans _ f args expr) = do
  betaPlusBExpr   <- betaBExprStm rprog True  decls f FBTrue True expr
  betaMinusBExpr  <- betaBExprStm rprog False decls f FBTrue True expr
  currentState <- get
  let freshVars = findFreshErrsInTranStateInterp f currentState
  let errVars = map errVar2Arg $ listFst3 $ env freshVars
  if betaPlusBExpr == betaMinusBExpr
    then return [Pred isTrans Original f (args++errVars) betaPlusBExpr]
    else do
      let tauEnv  = (f, TransState {freshErrVar = freshVars, forExprMap = []})
      put $ tauEnv:currentState
      return [Pred isTrans TauPlus  f (args++errVars) betaPlusBExpr
             ,Pred isTrans TauMinus f (args++errVars) betaMinusBExpr]

errVar2Arg :: VarName -> Arg
errVar2Arg x = Arg x FPDouble

validityCheckFunCall :: [FAExpr] -> [FBExpr] -> FBExpr
validityCheckFunCall  []  [] = error "validityCheckFunCall: unexpected empty list."
validityCheckFunCall aes  [] = listFAnd $ map IsValid aes
validityCheckFunCall  [] bes = listFAnd $ map BIsValid bes
validityCheckFunCall aes bes = FAnd (listFAnd $ map IsValid aes) (listFAnd $ map BIsValid bes)

checkFunCallValidity :: [FAExpr] -> [FBExpr] -> FAExpr -> FAExpr
checkFunCallValidity [] [] expr = expr
checkFunCallValidity fcs preds expr = Ite (validityCheckFunCall fcs preds) expr UnstWarning

checkFunCallValidityBExprStm :: [FAExpr] -> [FBExpr] -> FBExprStm -> FBExprStm
checkFunCallValidityBExprStm [] [] expr = expr
checkFunCallValidityBExprStm fcs preds expr = BIte (validityCheckFunCall fcs preds) expr BUnstWarning

returnBExprWithValidityCheck :: RProgram -> FBExprStm -> Bool ->  State TranStateInterp FBExprStm
returnBExprWithValidityCheck rprog be check = do
  let funCallListWithConds = funCallListFBExprStmWithConds rprog be
  let predListWithConds = predCallListFBExprStmWithCond rprog be
  if check
    then return $ checkFunCallValidityBExprStm funCallListWithConds predListWithConds be
    else return be

returnAExprWithValidityCheck :: RProgram -> FAExpr -> Bool -> State TranStateInterp FAExpr
returnAExprWithValidityCheck rprog ae check =  do
  let funCallList = funCallListFAExprWithConds rprog ae
  let predList = predCallListFAExprWithConds rprog ae
  if check
    then return $ checkFunCallValidity funCallList predList ae
    else return ae

addToLocalEnv :: FLetElem -> LocalEnv -> LocalEnv
addToLocalEnv letElem@(x,t,ae) locEnv | letElem `elem` locEnv = locEnv
                                      | x `elem` (map fst3 locEnv) = error "addToLocalEnv: adding an existing variable to the local environment."
                                      | otherwise = locEnv++[(x,t,ae)]

transformLetElem :: RProgram -> [Decl] -> FunName -> FBExpr -> Bool -> FLetElem -> State TranStateInterp FLetElem
transformLetElem rprog decls f be _ (x,t,faexpr) = do
  transformedAExpr <- transformStmSymb rprog decls f be False faexpr
  currentStateEnv <- get
  let currentState = findFreshErrsInTranStateInterp f currentStateEnv
  let newCurrentState = currentState { localEnv = addToLocalEnv (x,t,faexpr) (localEnv currentState) }
  let newFreshErrVars = updateFreshErrVars f newCurrentState currentStateEnv
  put newFreshErrVars
  return (x,t,transformedAExpr)

transformStmSymb :: RProgram -> [Decl] -> FunName -> FBExpr -> Bool -> FAExpr -> State TranStateInterp FAExpr
transformStmSymb _ _ _ _ _ (Let [] _) = error $ "transformStmSymb: Something went wrong, Let statments with empty list of assignments."

transformStmSymb rprog decls f be check (Let (letElem:rest) stm) = do
  transformedLetElem <- transformLetElem rprog decls f be check letElem
  let funCallListLetElem = funCallListFAExprWithConds rprog (exprFLetElem transformedLetElem)
  let predListLetElem    = predCallListFAExprWithConds rprog (exprFLetElem transformedLetElem)
  transformedStm <- transformStmSymb rprog decls f be check (if null rest then stm else (Let rest stm))
  if check
    then return $ checkFunCallValidity funCallListLetElem predListLetElem (Let [transformedLetElem] transformedStm)
    else return $ Let [transformedLetElem] transformedStm

transformStmSymb rprog decls f be check (Ite fbexpr thenStm elseStm) = do
  thenStmTran <- transformStmSymb rprog decls f (simplFAnd be fbexpr) check thenStm
  elseStmTran <- transformStmSymb rprog decls f (simplFAnd be (FNot fbexpr)) check  elseStm
  if noRoundOffErrorIn fbexpr
    then do
      let transformedAExpr = Ite fbexpr thenStmTran elseStmTran
      returnAExprWithValidityCheck rprog transformedAExpr check
    else do
      bePlus  <- betaPlusVar  rprog f be decls fbexpr
      beMinus <- betaMinusVar rprog f be decls fbexpr
      let transformedAExpr = Ite bePlus thenStmTran (Ite beMinus elseStmTran UnstWarning)
      returnAExprWithValidityCheck rprog transformedAExpr check

transformStmSymb rprog decls f be check (ListIte listThen elseStm) = do
  transformedListThen <- makeNewListThen listThen []
  if all (noRoundOffErrorIn . fst) listThen
    then do
      listThenTran <- mapM (transformStmSymb rprog decls f be check . snd) listThen
      elseStmTran <- transformStmSymb rprog decls f be check elseStm
      let transformedAExpr = ListIte (zip (map fst listThen) listThenTran) elseStmTran
      returnAExprWithValidityCheck rprog transformedAExpr check
    else do
      let transformedAExpr = ListIte transformedListThen UnstWarning
      returnAExprWithValidityCheck rprog transformedAExpr check
  where
      makeNewListThen :: [(FBExpr, FAExpr)] -> [FBExpr] -> State TranStateInterp [(FBExpr, FAExpr)]
      makeNewListThen [] listElseBes = do
          transformedElseSmt <- transformStmSymb rprog decls f be check elseStm
          newlistElseBes <- mapM (betaMinusVar rprog f be decls) listElseBes
          return [(listFAnd newlistElseBes, transformedElseSmt)]
      makeNewListThen ((thenBe,stm):rest) listElseBes = do
          newThenBranch <- makeNewThenBranch thenBe listElseBes stm
          newListThen <- if noRoundOffErrorIn thenBe
                            then makeNewListThen rest listElseBes
                            else makeNewListThen rest (thenBe:listElseBes)
          return (newThenBranch:newListThen)

      makeNewThenBranch :: FBExpr -> [FBExpr] -> FAExpr -> State TranStateInterp (FBExpr, FAExpr)
      makeNewThenBranch thenBe listElseBes stm = do
          newlistElseBes <- mapM (betaMinusVar rprog f be decls) listElseBes
          transformedStm <- transformStmSymb rprog decls f be check stm
          if noRoundOffErrorIn thenBe
          then
            return (foldl FAnd thenBe newlistElseBes, transformedStm)
          else do
            bePlus <- betaPlusVar rprog f be decls thenBe
            return (foldl FAnd bePlus newlistElseBes, transformedStm)

transformStmSymb rprog decls f be check forOrig@(ForLoop retType startIdx endIdx initValueAcc idx acc forBody) = do
  transformedForBody <- transformStmSymb rprog decls f be check forBody
  currentState <- get
  let forTrans = ForLoop retType startIdx endIdx initValueAcc idx acc transformedForBody
  put $ addToForExprMap f (fae2real' forOrig) forTrans currentState
  return $ forTrans

transformStmSymb _ _ _ _ _ UnstWarning = return UnstWarning

transformStmSymb rprog decls g be _ (FEFun isTrans f fp actArgs) =  do
  currentStateEnv  <- get
  let fCurrentState = findFreshErrsInTranStateInterp f currentStateEnv
  let (_,formArgs,body) = fromMaybe (error $ "transformStmSymb: function " ++ show f ++ " not found.") (findInDecls f decls)
  let locVars = case body of
                  Left stm -> localVars stm
                  Right _ -> []
  newErrArgs <- mapM (generateErrVarArg g be . argsBindFAExpr formArgs actArgs . replaceLocVarsFix locVars . snd3) (env fCurrentState)
  return $ FEFun isTrans f fp (actArgs++newErrArgs)

transformStmSymb rprog decls g be check (BinaryFPOp op fp ae1 ae2) = do
  ae1' <- transformStmSymb rprog decls g be False ae1
  ae2' <- transformStmSymb rprog decls g be False ae2
  let transformedAExpr = BinaryFPOp op fp ae1' ae2'
  let funCallList = funCallListFAExprWithConds rprog transformedAExpr
  let predList = predCallListFAExprWithConds rprog transformedAExpr
  if check
  then return $ checkFunCallValidity funCallList predList transformedAExpr
  else return transformedAExpr

transformStmSymb rprog decls g be check (UnaryFPOp op fp ae1) = do
  ae1' <- transformStmSymb rprog decls g be False ae1
  let transformedAExpr = UnaryFPOp op fp ae1'
  let funCallList = funCallListFAExprWithConds rprog transformedAExpr
  let predList = predCallListFAExprWithConds rprog transformedAExpr
  if check
  then return $ checkFunCallValidity funCallList predList transformedAExpr
  else return transformedAExpr

transformStmSymb rprog decls g be check (FFma fp ae1 ae2 ae3) = do
  ae1' <- transformStmSymb rprog decls g be False ae1
  ae2' <- transformStmSymb rprog decls g be False ae2
  ae3' <- transformStmSymb rprog decls g be False ae3
  let transformedAExpr = FFma fp ae1' ae2' ae3'
  let funCallList = funCallListFAExprWithConds rprog transformedAExpr
  let predList = predCallListFAExprWithConds rprog transformedAExpr
  if check
  then return $ checkFunCallValidity funCallList predList transformedAExpr
  else return transformedAExpr

transformStmSymb rprog decls g be check (TypeCast fp1 fp2 ae1) = do
  ae1' <- transformStmSymb rprog decls g be False ae1
  let transformedAExpr = TypeCast fp1 fp2 ae1'
  let funCallList = funCallListFAExprWithConds rprog transformedAExpr
  let predList = predCallListFAExprWithConds rprog transformedAExpr
  if check
  then return $ checkFunCallValidity funCallList predList transformedAExpr
  else return transformedAExpr

transformStmSymb rprog decls g be check (Value ae1) = do
  ae1' <- transformStmSymb rprog decls g be False ae1
  let transformedAExpr = Value ae1'
  let funCallList = funCallListFAExprWithConds rprog transformedAExpr
  let predList = predCallListFAExprWithConds rprog transformedAExpr
  if check
  then return $ checkFunCallValidity funCallList predList transformedAExpr
  else return transformedAExpr

transformStmSymb _ _ _ _ _ ae = return ae


betaBExprStm :: RProgram -> Bool -> [Decl] -> FunName -> FBExpr -> Bool -> FBExprStm -> State TranStateInterp FBExprStm
betaBExprStm _ _ _ _ _ _  BUnstWarning = return BUnstWarning

betaBExprStm rprog True  decls f _ check (BExpr expr) = do
  transformedExpr <- betaPlusVar rprog f FBTrue decls expr
  returnBExprWithValidityCheck rprog (BExpr transformedExpr) check

betaBExprStm rprog False decls f _ check (BExpr expr) = do
  transformedExpr <- betaMinusVar rprog f FBTrue decls expr
  returnBExprWithValidityCheck rprog (BExpr transformedExpr) check

betaBExprStm _ _ _ _ _ _  (BLet [] _) = error $ "betaBExprStm: BLet statement with no variable binding."

betaBExprStm rprog isTPlus decls f pathCond check (BLet (letElem:rest) stm) = do
  transformedLetElem <- transformLetElem rprog decls f pathCond check letElem
  let funCallListLetElem = funCallListFAExprWithConds rprog  (exprFLetElem transformedLetElem)
  let predListLetElem    = predCallListFAExprWithConds rprog (exprFLetElem transformedLetElem)
  transformedStm     <- betaBExprStm rprog isTPlus decls f pathCond check (if null rest then stm else (BLet rest stm))
  if check
    then return $ checkFunCallValidityBExprStm funCallListLetElem predListLetElem (BLet [transformedLetElem] transformedStm)
    else return $ BLet [transformedLetElem] transformedStm

betaBExprStm rprog isTPlus decls f be check stm@(BIte fbexpr thenStm elseStm) = do
  thenStmTran <- betaBExprStm rprog isTPlus decls f (simplFAnd be fbexpr)        check thenStm
  elseStmTran <- betaBExprStm rprog isTPlus decls f (simplFAnd be (FNot fbexpr)) check elseStm
  if noRoundOffErrorIn fbexpr
    then do
      let transformedBExpr = BIte fbexpr thenStmTran elseStmTran
      returnBExprWithValidityCheck rprog transformedBExpr check
    else do
      bePlus  <- betaPlusVar rprog f be decls fbexpr
      beMinus <- betaMinusVar rprog f be decls fbexpr
      let transformedBExpr = BIte bePlus thenStmTran (BIte beMinus elseStmTran BUnstWarning)
      returnBExprWithValidityCheck rprog transformedBExpr check

betaBExprStm rprog isTPlus decls f pathCond check (BListIte listThen elseStm) =  do
  transformedListThen <- makeNewListThen listThen []
  if all (noRoundOffErrorIn . fst) listThen
    then do
      listThenTran <- mapM (betaBExprStm rprog isTPlus decls f pathCond check . snd) listThen
      elseStmTran <- betaBExprStm rprog isTPlus decls f pathCond check elseStm
      let transformedBExpr = BListIte (zip (map fst listThen) listThenTran) elseStmTran
      returnBExprWithValidityCheck rprog transformedBExpr check
    else do
      let transformedBExpr = BListIte transformedListThen BUnstWarning
      returnBExprWithValidityCheck rprog transformedBExpr check
  where
      makeNewListThen :: [(FBExpr, FBExprStm)] -> [FBExpr] -> State TranStateInterp [(FBExpr, FBExprStm)]
      makeNewListThen [] listElseBes = do
          transformedElseSmt <- betaBExprStm rprog isTPlus decls f pathCond check elseStm
          newlistElseBes <- mapM (betaMinusVar rprog f pathCond decls) listElseBes
          return [(listFAnd newlistElseBes, transformedElseSmt)]
      makeNewListThen ((thenBe,stm):rest) listElseBes = do
          newThenBranch <- makeNewThenBranch thenBe listElseBes stm
          newListThen <- if noRoundOffErrorIn thenBe
                            then makeNewListThen rest listElseBes
                            else makeNewListThen rest (thenBe:listElseBes)
          return (newThenBranch:newListThen)

      makeNewThenBranch :: FBExpr -> [FBExpr] -> FBExprStm -> State TranStateInterp (FBExpr, FBExprStm)
      makeNewThenBranch thenBe listElseBes stm = do
          newlistElseBes <- mapM (betaMinusVar rprog f pathCond decls) listElseBes
          transformedStm <- betaBExprStm rprog isTPlus decls f pathCond check stm
          if noRoundOffErrorIn thenBe
          then
            return (foldl FAnd thenBe newlistElseBes, transformedStm)
          else do
            bePlus <- betaPlusVar rprog f pathCond decls thenBe
            return (foldl FAnd bePlus newlistElseBes, transformedStm)

replaceLocVarsFix :: [(VarName, FAExpr)] -> FAExpr -> FAExpr
replaceLocVarsFix locVars ae =
  if ae == ae' then ae
  else replaceLocVarsFix locVars ae'
  where
    ae' = replaceLocVars locVars ae

replaceLocVars :: [(VarName, FAExpr)] -> FAExpr -> FAExpr
replaceLocVars locVars = argsBindFAExpr (map ((`Arg` FPDouble) . fst) locVars) (map snd locVars)

generateErrVarArg :: FunName -> FBExpr -> FAExpr -> State TranStateInterp FAExpr
generateErrVarArg f be ae = do
  currentStateEnv <- get
  let currentState = findFreshErrsInTranStateInterp f currentStateEnv
  let locEnv = localEnv currentState
  let (freshName,newEnv,newCount) = generateVarName (declareLocalVars locEnv ae) (env currentState) (count currentState) be
  put $ updateFreshErrVars f (FreshErrVar { env = newEnv, count = newCount, localEnv = locEnv}) currentStateEnv
  return $ FVar FPDouble freshName

generateVarName :: FAExpr -> [(VarName, FAExpr, FBExpr)] -> Int -> FBExpr -> (String, ErrVarEnv,Int)
generateVarName ae environment counter be =
  case findInEnv ae environment of
    Just (v, ae',_)  -> (v,environment,counter)
    Nothing -> (freshName, environment++[(freshName, ae, be)], counter + 1)
    where
      freshName = "E_"++ show counter
      -- TODO: all the calls with array are considered as if was the same var
      --lookup' (FArrayElem fp v idx) [] = Nothing
      --lookup' (FArrayElem fp v idx) (((FArrayElem fp' v' idx'), ae):rest) | v == v' = Just ae
      --                                                                    | otherwise = lookup' (FArrayElem fp v idx) rest
      --lookup' a list = lookup a list

findInEnv :: FAExpr  -> ErrVarEnv -> Maybe (VarName, FAExpr,FBExpr)
findInEnv _ [] = Nothing
findInEnv ae ((ev, ae', be):environment) | ae == ae'      = Just (ev, ae, be)
                                         | otherwise = findInEnv ae environment

updateBetaState :: FunName -> FBExpr -> FAExpr -> State TranStateInterp VarName
updateBetaState f pathCond ae1 = do
  currentStateEnv <- get
  let fCurrentState = findFreshErrsInTranStateInterp f currentStateEnv
  let locEnv = localEnv fCurrentState
  let (freshName,newEnv,newCount) = generateVarName (declareLocalVars locEnv ae1) (env fCurrentState) (count fCurrentState) pathCond
  put $ updateFreshErrVars f (FreshErrVar { env = newEnv, count = newCount, localEnv = locEnv}) currentStateEnv
  return freshName

betaPlusVar :: RProgram -> FunName -> FBExpr -> [Decl] -> FBExpr -> State TranStateInterp FBExpr
betaPlusVar _ _ _ _ FBTrue  = return FBTrue
betaPlusVar _ _ _ _ FBFalse = return FBFalse

betaPlusVar rprog f pathCond decls (FOr  be1 be2) = do
  be1' <- betaPlusVar rprog f pathCond decls be1
  be2' <- betaPlusVar rprog f pathCond decls be2
  return $ FOr be1' be2'

betaPlusVar rprog f pathCond decls (FAnd be1 be2) = do
  be1' <- betaPlusVar rprog f pathCond decls be1
  be2' <- betaPlusVar rprog f pathCond decls be2
  return $ FAnd be1' be2'

betaPlusVar rprog f pathCond decls (FNot be) = betaMinusVar rprog f pathCond decls be

betaPlusVar rprog f pathCond decls (FRel Lt ae1 ae2) | isZeroFAExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  ae1Trans <- transformStmSymb rprog decls f pathCond False ae1
  return $ FRel Lt ae1Trans (UnaryFPOp NegOp FPDouble (FVar FPDouble freshName))

betaPlusVar rprog f pathCond decls (FRel Lt ae2 ae1) | isZeroFAExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  ae1Trans <- transformStmSymb rprog decls f pathCond False ae1
  return $ FRel Lt (FVar FPDouble freshName) ae1Trans

betaPlusVar rprog f pathCond decls (FRel LtE ae1 ae2) | isZeroFAExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  ae1Trans <- transformStmSymb rprog decls f pathCond False ae1
  return $ FRel LtE ae1Trans (UnaryFPOp NegOp FPDouble (FVar FPDouble freshName))

betaPlusVar rprog f pathCond decls (FRel LtE ae2 ae1) | isZeroFAExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  ae1Trans <- transformStmSymb rprog decls f pathCond False ae1
  return $ FRel LtE (FVar FPDouble freshName) ae1Trans

betaPlusVar rprog f pathCond decls (FRel Gt ae1 ae2) | isZeroFAExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  ae1Trans <- transformStmSymb rprog decls f pathCond False ae1
  return $ FRel Gt ae1Trans (FVar FPDouble freshName)

betaPlusVar rprog f pathCond decls (FRel Gt ae2 ae1)| isZeroFAExpr ae2  = do
  freshName <- updateBetaState f pathCond ae1
  ae1Trans <- transformStmSymb rprog  decls f pathCond False ae1
  return $ FRel Gt (UnaryFPOp NegOp FPDouble (FVar FPDouble freshName)) ae1Trans

betaPlusVar rprog f pathCond decls (FRel GtE ae1 ae2) | isZeroFAExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  ae1Trans <- transformStmSymb rprog  decls f pathCond False ae1
  return $ FRel GtE ae1Trans (FVar FPDouble freshName)

betaPlusVar rprog f pathCond decls (FRel GtE ae2 ae1) | isZeroFAExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  ae1Trans <- transformStmSymb rprog decls f pathCond False ae1
  return $ FRel GtE (UnaryFPOp NegOp FPDouble (FVar FPDouble freshName)) ae1Trans

betaPlusVar rprog f pathCond decls (FRel Eq ae1 ae2) | isIntFAExpr ae1 && isIntFAExpr ae2 = do
  ae1Trans <- transformStmSymb rprog decls f pathCond False ae1
  ae2Trans <- transformStmSymb rprog decls f pathCond False ae2
  return $ FRel Eq ae1Trans ae2Trans

betaPlusVar rprog f pathCond decls (FRel Eq ae1 ae2) = betaPlusVar rprog f pathCond decls (FAnd (FRel LtE ae1 ae2) (FRel GtE ae1 ae2))

betaPlusVar rprog f pathCond decls (FRel Neq ae1 ae2) = betaPlusVar rprog f pathCond decls (FOr (FRel Gt ae1 ae2) (FRel Lt ae1 ae2))

betaPlusVar rprog g pathCond decls (FEPred isTrans _ f args) = do
  currentStateEnv  <- get
  let fCurrentState = findFreshErrsInTranStateInterp f currentStateEnv
  let (_,formArgs,body) = fromMaybe (error $ "betaPlusVar: function " ++ show f ++ " not found.") (findInDecls f decls)
  let locVars = case body of
                  Left stm -> localVars stm
                  Right _ -> []
  newErrArgs <- mapM (generateErrVarArg g pathCond . argsBindFAExpr formArgs args . replaceLocVarsFix locVars . snd3) (env fCurrentState)
  return $ FEPred isTrans TauPlus f (args ++ newErrArgs)

betaPlusVar _ _ _ _ ae = error $ "betaPlusVar: not implemented yet for " ++ show ae ++ "."


betaMinusVar :: RProgram -> FunName -> FBExpr -> [Decl] -> FBExpr -> State TranStateInterp FBExpr
betaMinusVar _ _ _ _ FBTrue  = return FBFalse
betaMinusVar _ _ _ _ FBFalse = return FBTrue

betaMinusVar rprog f pathCond decls (FOr  be1 be2) = do
    be1' <- betaMinusVar rprog f pathCond decls be1
    be2' <- betaMinusVar rprog f pathCond decls be2
    return $ FAnd be1' be2'

betaMinusVar rprog f pathCond decls (FAnd be1 be2) = do
    be1' <- betaMinusVar rprog f pathCond decls be1
    be2' <- betaMinusVar rprog f pathCond decls be2
    return $ FOr be1' be2'

betaMinusVar rprog f pathCond decls (FNot be) = betaPlusVar rprog f pathCond decls be

betaMinusVar rprog f pathCond decls (FRel Lt  ae1 ae2) | isZeroFAExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  ae1Trans <- transformStmSymb rprog decls f pathCond False ae1
  return $ FRel GtE ae1Trans (FVar FPDouble freshName)

betaMinusVar rprog f pathCond decls (FRel Lt ae2 ae1) | isZeroFAExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  ae1Trans <- transformStmSymb rprog decls f pathCond False ae1
  return $ FRel GtE (UnaryFPOp NegOp FPDouble (FVar FPDouble freshName)) ae1Trans

betaMinusVar rprog f pathCond decls (FRel LtE ae1 ae2) | isZeroFAExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  ae1Trans <- transformStmSymb rprog decls f pathCond False ae1
  return $ FRel Gt ae1Trans (FVar FPDouble freshName)

betaMinusVar rprog f pathCond decls (FRel LtE ae2 ae1) | isZeroFAExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  ae1Trans <- transformStmSymb rprog decls f pathCond False ae1
  return $ FRel Gt (UnaryFPOp NegOp FPDouble (FVar FPDouble freshName)) ae1Trans

betaMinusVar rprog f pathCond decls (FRel Gt  ae1 ae2) | isZeroFAExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  ae1Trans <- transformStmSymb rprog decls f pathCond False ae1
  return $ FRel LtE ae1Trans (UnaryFPOp NegOp FPDouble (FVar FPDouble freshName))

betaMinusVar rprog f pathCond decls (FRel Gt ae2 ae1) | isZeroFAExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  ae1Trans <- transformStmSymb rprog decls f pathCond False ae1
  return $ FRel LtE (FVar FPDouble freshName) ae1Trans

betaMinusVar rprog f pathCond decls (FRel GtE ae1 ae2) | isZeroFAExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  ae1Trans <- transformStmSymb rprog decls f pathCond False ae1
  return $ FRel Lt ae1Trans (UnaryFPOp NegOp FPDouble (FVar FPDouble freshName))

betaMinusVar rprog f pathCond decls (FRel GtE ae2 ae1) | isZeroFAExpr ae2 = do
  freshName <- updateBetaState f pathCond ae1
  ae1Trans <- transformStmSymb rprog decls f pathCond False ae1
  return $ FRel Lt (FVar FPDouble freshName) ae1Trans

betaMinusVar rprog f pathCond decls (FRel Eq ae1 ae2) | isIntFAExpr ae1 && isIntFAExpr ae2 = do
  ae1Trans <- transformStmSymb rprog decls f pathCond False ae1
  ae2Trans <- transformStmSymb rprog decls f pathCond False ae2
  return $ FRel Eq ae1Trans ae2Trans

betaMinusVar rprog f pathCond decls (FRel Eq ae1 ae2) = betaMinusVar rprog f pathCond decls (FAnd (FRel LtE ae1 ae2) (FRel GtE ae1 ae2))

betaMinusVar rprog f pathCond decls (FRel Neq ae1 ae2) = betaMinusVar rprog f pathCond decls (FOr (FRel Gt ae1 ae2) (FRel Lt ae1 ae2))

betaMinusVar rprog g pathCond decls (FEPred isTrans _ f args) = do
  currentStateEnv  <- get
  let fCurrentState = findFreshErrsInTranStateInterp f currentStateEnv
  let (_,formArgs,body) = fromMaybe (error $ "betaMinusVar: function " ++ show f ++ "not found.") (findInDecls f decls)
  let locVars = case body of
                  Left  stm -> localVars stm
                  Right _ -> []
  newErrArgs <- mapM (generateErrVarArg g pathCond . argsBindFAExpr formArgs args . replaceLocVarsFix locVars . snd3) (env fCurrentState)
  return $ FEPred isTrans TauMinus f (args ++ newErrArgs)

betaMinusVar _ _ _ _ be = error $ "betaMinus niy for "++ show be
