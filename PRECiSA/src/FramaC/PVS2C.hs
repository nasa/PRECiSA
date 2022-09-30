-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module FramaC.PVS2C where

import Prelude
import AbstractSemantics
import AbstractDomain
import AbsPVSLang
import Common.TypesUtils
import Common.TypeConversions
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Either (isLeft, fromLeft, isRight, fromRight)
import Data.Bifunctor (bimap)
import qualified FramaC.Types as C
import qualified FramaC.CLang as C
import qualified FramaC.ACSLTypes as ACSL
import qualified FramaC.ACSLlang as ACSL
import FramaC.GenerateACSL (expr2acsl)
import FramaC.Types (HasConditionals)
import Operators
import PVSTypes
import Translation.Float2Real

resultVar :: String
resultVar = "res"

args2C :: Arg -> C.Arg
args2C (Arg x fp) = C.Arg (fprec2type fp) x

args2CwithType :: C.Type -> Arg -> C.Arg
args2CwithType t (Arg x _) = C.Arg t x

resultNone ::  C.Type -> C.Stm
resultNone t = C.VarAssign t resultVar (C.None t)

auxVarName :: Int -> String
auxVarName n = "aux_" ++ show n

removeValue :: C.AExpr -> C.AExpr
removeValue (C.Value expr) = expr
removeValue expr = expr

removeValueBExpr :: C.BExpr -> C.BExpr
removeValueBExpr (C.BValue expr) = expr
removeValueBExpr expr = expr

declareVar :: HasConditionals -> FAExpr -> Int -> C.Stm
declareVar hasConds expr@(FEFun _ f _ _) n
  | hasConds f = C.VarDeclAssign (C.MaybeStruct (fprec2type $ getPVSType expr)) (auxVarName n)
                 (removeValue $ aexpr2inlineC hasConds expr)
  | otherwise = C.VarDeclAssign (fprec2type $ getPVSType expr) (auxVarName n)
                 (aexpr2inlineC hasConds expr)

declareBoolVar ::  HasConditionals ->  FBExpr -> Int -> C.Stm
declareBoolVar hasConds expr@(FEPred _ _ f _) n
  | hasConds f = C.VarDeclAssignBool (C.MaybeStruct C.Boolean) (auxVarName n)
                 (removeValueBExpr $ bexpr2C hasConds expr)
  | otherwise =  C.VarDeclAssignBool C.Boolean (auxVarName n) (bexpr2C hasConds expr)

predAbsSuffix :: PredAbs -> String
predAbsSuffix Original = ""
predAbsSuffix TauPlus = "_tauplus"
predAbsSuffix TauMinus = "_tauminus"

decl2C :: [(AExpr,AExpr)]
          -> [(FAExpr,AExpr)]
          -> Interpretation
          -> Decl
          -> HasConditionals
          -> C.Decl
decl2C forListExpr forMap interp (Decl _ fp f args expr) hasConds =
  C.Decl returnType (f++"_fp") (map args2C args) (resDecl:stmList++[returnRes])
  where
    (stmList,_) = runState (aexpr2C f hasConds forListExpr forMap (fprec2type fp) interp emptyEnv expr) 0
    returnType = if (hasConds f) then fprec2MaybeType fp else fprec2type fp
    resDecl   = C.VarDecl returnType resultVar
    returnRes = C.Return (C.Var returnType resultVar)
    printReturnType TInt     = "_int"
    printReturnType t        = error $ "decl2C: unexpected return type " ++ show t

decl2C _ _ _ Pred{} _ = error "decl2C: Expected numerical function declaration but a predicate."

pred2C ::
  HasConditionals ->
  Interpretation ->
  Decl ->
  C.Decl
pred2C hasConds interp (Pred _ predAbs f args expr) =
  C.Decl returnType (f ++ (predAbsSuffix predAbs) ++ "_fp") (map args2C args) bodyWithValidityCheck
  where
    returnType = if (hasConds f) then (C.MaybeStruct C.Boolean) else C.Boolean
    resDecl = C.VarDecl returnType resultVar
    (stmList, _) = runState (bexprStm2C f hasConds interp expr) 0
    returnRes = C.Return (C.Var C.Boolean resultVar)
    bodyWithValidityCheck = resDecl : stmList ++ [returnRes]
pred2C _ _ Decl {} = error "pred2C: Expected predicate but got a numerical function declaration."

letElem2C :: HasConditionals -> Interpretation -> FLetElem -> ([C.Stm],Env [ACeb]) -> ([C.Stm],Env [ACeb])
letElem2C hasConds interp (x,t,expr) (listVarAssign, env) =
  (C.VarDeclAssign (fprec2type t) x (aexpr2inlineC hasConds expr):listVarAssign, env')
  where
    semx = exprSemantics interp env expr
    env' = insertEnv x semx env

aexpr2C :: FunName
        -> HasConditionals
        -> [(AExpr,AExpr)]
        -> [(FAExpr,AExpr)]
        -> C.Type
        -> Interpretation
        -> Env AbstractDomain.ACebS
        -> FAExpr
        -> State Int [C.Stm]

aexpr2C f hasConds forListExpr forMap t interp env (Let listExpr body) = do
  let (listVarAssign,env') = foldr (letElem2C hasConds interp) ([],env) listExpr
  bodyStm <- aexpr2C f hasConds forListExpr forMap t interp env' body
  return $ listVarAssign++bodyStm

aexpr2C f hasConds forListExpr forMap t interp env (Ite bexpr thenExpr elseExpr) = do
  let callList = funCallListFBExpr bexpr
  callListVars <- generateAuxVarList callList
  let predList = predCallListFBExpr bexpr
  predListVars <- generateAuxVarList predList
  thenStm  <- aexpr2C f hasConds forListExpr forMap t interp env
                     (replaceCallsInAExpr hasConds callListVars predListVars thenExpr)
  elseStm  <- aexpr2C f hasConds forListExpr forMap t interp env
                     (replaceCallsInAExpr hasConds callListVars predListVars elseExpr)
  return $ map (uncurry $ declareVar hasConds) callListVars
           ++
           map (uncurry $ declareBoolVar hasConds) predListVars
           ++
           [C.Ite (bexpr2C hasConds (replaceCallsInBExpr hasConds callListVars predListVars bexpr)) thenStm elseStm]

aexpr2C f hasConds forListExpr forMap t interp env (ListIte listThen elseExpr) = do
  let guards = map fst listThen
  let callList = concatMap funCallListFBExpr guards
  callListVars <- generateAuxVarList callList
  let predList = concatMap predCallListFBExpr guards
  predListVars <- generateAuxVarList predList
  thenListStm <- mapM (aexpr2C f hasConds forListExpr forMap t interp env . replaceCallsInAExpr hasConds callListVars predListVars . snd) listThen
  elseStm <- aexpr2C f hasConds forListExpr forMap t interp env (replaceCallsInAExpr hasConds callListVars  predListVars elseExpr)
  let guardsC = map (bexpr2C hasConds . replaceCallsInBExpr hasConds callListVars predListVars) guards
  return $ map (uncurry $ declareVar hasConds) callListVars
           ++
           map (uncurry $ declareBoolVar hasConds) predListVars
           ++
           [C.ListIte (zip guardsC thenListStm) elseStm]

aexpr2C f hasConds forListExpr forMap t interp env for@(ForLoop fp n0 n initAcc idx acc forBody) = do
  forBodyStm <- aexpr2C f hasConds forListExpr forMap t interp env forBody
  let accType  = fprec2type fp
  let forBody' =  [C.Ite (C.IsValid (C.Var accType resultVar)) forBodyStm [C.VarAssign t resultVar (C.None accType)]]
  return [ C.VarDecl C.Int idx
         , C.VarDeclAssign accType acc (aexpr2inlineC hasConds initAcc)
         , C.VarAssign t resultVar (C.Some accType (Left $ C.Var accType acc))
         , generateForLoopACSL forMap idx acc (type2acsl accType) n n0 for forListExpr interp env forBody
         , C.ForLoop idx (aexpr2inlineC hasConds n0) (aexpr2inlineC hasConds n) forBody']

aexpr2C _ _ _ _ t _ _ UnstWarning = return [resultNone t]

aexpr2C f hasConds _ _ t _ _ expr
    | hasConds f = return [C.VarAssign t resultVar (C.Some t (Left returnExpr))]
    | otherwise  = return [C.VarAssign t resultVar returnExpr ]
  where
    returnExpr = aexpr2inlineC hasConds expr

aexpr2inlineC :: HasConditionals -> FAExpr -> C.AExpr
aexpr2inlineC _ (FInt  i)               = C.IntCnst i
aexpr2inlineC _ (ToFloat _ (Int n))     = C.IntCnst n
aexpr2inlineC _ (ToFloat FPSingle (Rat rat)) = C.FPCnst SinglePrec rat
aexpr2inlineC _ (ToFloat FPDouble (Rat rat)) = C.FPCnst SinglePrec rat
aexpr2inlineC _ (FCnst FPSingle rat)    = C.FPCnst SinglePrec rat
aexpr2inlineC _ (FCnst FPDouble rat)    = C.FPCnst DoublePrec rat
aexpr2inlineC _ (FVar  fp x)            = C.Var  (fprec2type fp) x
aexpr2inlineC _ (StructVar fp x)        = C.Var  (fprec2MaybeType fp) x
aexpr2inlineC hasConds (FArrayElem fp _ v idx) = C.ArrayElem (fprec2type fp) v (aexpr2inlineC hasConds idx)
aexpr2inlineC hasConds (FEFun _ f fp args) = (if (hasConds f) then C.Value else id) $ C.EFun f (fprec2type fp) (map (aexpr2inlineC hasConds) args)
aexpr2inlineC hasConds (Value expr)            = C.Value (aexpr2inlineC hasConds expr)
aexpr2inlineC hasConds (TypeCast fp1 fp2 expr) = C.TypeCast (fprec2type fp1) (fprec2type fp2) (aexpr2inlineC hasConds expr)
aexpr2inlineC hasConds (BinaryFPOp op fp expr1 expr2) = C.BinaryOp op (fprec2type fp) (aexpr2inlineC hasConds expr1)
                                                                             (aexpr2inlineC hasConds expr2)
aexpr2inlineC hasConds (UnaryFPOp  op fp expr)        = C.UnaryOp  op (fprec2type fp) (aexpr2inlineC hasConds expr)
aexpr2inlineC hasConds (FMin exprs) = C.Min (map (aexpr2inlineC hasConds) exprs)
aexpr2inlineC hasConds (FMax exprs) = C.Max (map (aexpr2inlineC hasConds) exprs)
aexpr2inlineC hasConds (Ite be exprThen exprElse) =  C.IteExpr (bexpr2C hasConds be) (aexpr2inlineC hasConds exprThen) (aexpr2inlineC hasConds exprElse)
aexpr2inlineC _ ae = error $ "aexpr2inlineC not defined for " ++ show ae



bexprStm2C :: FunName -> HasConditionals -> Interpretation -> FBExprStm -> State Int [C.Stm]
bexprStm2C f hasConds interp (BLet listElem body) = do
  let (listVarAssign,_) = foldr (letElem2C hasConds interp) ([],emptyEnv) listElem
  bodyStm <- bexprStm2C f hasConds interp body
  return $ listVarAssign++bodyStm

bexprStm2C f hasConds interp (BIte bexpr thenExpr elseExpr) = do
  let callList = funCallListFBExpr bexpr
  callListVars <- generateAuxVarList callList
  let predList = predCallListFBExpr bexpr
  predListVars <- generateAuxVarList predList
  thenStm  <- bexprStm2C f hasConds interp (replaceCallsInBExprStm hasConds callListVars predListVars thenExpr)
  elseStm  <- bexprStm2C f hasConds interp (replaceCallsInBExprStm hasConds callListVars predListVars elseExpr)
  return $ map (uncurry $ declareVar hasConds) callListVars
           ++
           map (uncurry $ declareBoolVar hasConds) predListVars
           ++
           [C.Ite (bexpr2C hasConds (replaceCallsInBExpr hasConds callListVars predListVars bexpr)) thenStm elseStm]

bexprStm2C f hasConds interp (BListIte listThen elseExpr) = do
  let guards = map fst listThen
  let callList = concatMap funCallListFBExpr guards
  callListVars <- generateAuxVarList callList
  let predList = concatMap predCallListFBExpr guards
  predListVars <- generateAuxVarList predList
  thenListStm <- mapM (bexprStm2C f hasConds interp . replaceCallsInBExprStm hasConds callListVars predListVars . snd) listThen
  elseStm     <- bexprStm2C f hasConds interp (replaceCallsInBExprStm hasConds callListVars predListVars elseExpr)
  let guardsC = map (bexpr2C hasConds . replaceCallsInBExpr hasConds callListVars predListVars) guards
  return $ map (uncurry $ declareVar hasConds) callListVars
           ++
           map (uncurry $ declareBoolVar hasConds) predListVars
           ++
           [C.ListIte (zip guardsC thenListStm) elseStm]

bexprStm2C f hasConds _ (BExpr expr) = return [(if (hasConds f)
      then C.VarAssign C.Boolean resultVar (C.Some C.Boolean (Right $ bexpr2C hasConds expr))
      else C.VarAssignBool resultVar (bexpr2C hasConds expr))]

bexprStm2C _ _ _ BUnstWarning = return [resultNone C.Boolean]

bexpr2C :: HasConditionals -> FBExpr -> C.BExpr
bexpr2C _ FBTrue  = C.BTrue
bexpr2C _ FBFalse = C.BFalse
bexpr2C hasConds (FNot be)      = C.Not (bexpr2C hasConds be)
bexpr2C hasConds (FOr  be1 be2) = C.Or  (bexpr2C hasConds be1) (bexpr2C hasConds be2)
bexpr2C hasConds (FAnd be1 be2) = C.And (bexpr2C hasConds be1) (bexpr2C hasConds be2)
bexpr2C hasConds (FRel rel ae1 ae2) = C.Rel rel (aexpr2inlineC hasConds ae1) (aexpr2inlineC hasConds ae2)
bexpr2C hasConds (IsValid ae@(FEFun _ _ _ _)) = C.IsValid (aexpr2inlineC hasConds ae)
bexpr2C hasConds (BIsValid (FEPred _ Original f args)) = C.BIsValid (C.FEPred (f++"_fp") (map (aexpr2inlineC hasConds) args))
bexpr2C hasConds (BIsValid (FEPred _ TauPlus  f args)) = C.BIsValid (C.FEPred (f++"_tauplus_bool") (map (aexpr2inlineC hasConds) args))
bexpr2C hasConds (BIsValid (FEPred _ TauMinus f args)) = C.BIsValid (C.FEPred (f++"_tauminus_bool") (map (aexpr2inlineC hasConds) args))
bexpr2C hasConds (IsValid ae)    = C.IsValid (aexpr2inlineC hasConds ae)
bexpr2C hasConds (BIsValid ae)   = C.BIsValid (bexpr2C hasConds ae)
bexpr2C hasConds (BValue ae)     = C.BValue (bexpr2C hasConds ae)
bexpr2C hasConds (FEPred _ Original f args) = C.FEPred (f++"_fp") (map (aexpr2inlineC hasConds) args)
bexpr2C hasConds (FEPred _ TauPlus  f args) = C.FEPred (f++"_tauplus_bool")  (map (aexpr2inlineC hasConds) args)
bexpr2C hasConds (FEPred _ TauMinus f args) = C.FEPred (f++"_tauminus_bool") (map (aexpr2inlineC hasConds) args)
bexpr2C hasConds (BStructVar x) = C.BVar  (C.MaybeStruct  C.Boolean) x

generateForLoopACSL :: [(FAExpr, AExpr)]
                    -> VarName
                    -> VarName
                    -> ACSL.Type
                    -> FAExpr
                    -> FAExpr
                    -> FAExpr
                    -> [(AExpr, AExpr)]
                    -> Interpretation
                    -> Env AbstractDomain.ACebS
                    -> FAExpr
                    -> C.Stm
generateForLoopACSL forMap idx acc accType n n0 for forRecFuns interp env forBody =
  C.ACSL [ ACSL.LoopAssigns  [idx, acc, "res"]
         , ACSL.LoopInvariant invariant
         , ACSL.LoopVariant   variant ]
  where
    n0ACSL  = expr2acsl $ fae2real n0
    nACSL   = expr2acsl $ fae2real n
    realFor = fromMaybe (error $ "aexpr2C: real for expression" ++ show for ++ " not found in \n" ++ show forMap ++ ".")
                                    (lookup for forMap)
    funCall = expr2acsl $ fromMaybe (error $ "aexpr2C: for expression = " ++ show realFor
                                    ++ "\n not found in \n forRecFuns = " ++ show forRecFuns ++ ".") $
                (lookup realFor forRecFuns)
    symbROError = if isIntFAExpr forBody
                    then ACSL.IntCnst 0
                    else expr2acsl $ symbolicErrorStable interp env (prevIteration idx forBody)
    variant   = ACSL.BinaryOp SubOp nACSL (ACSL.Var ACSL.Int idx)
    invariant = generateForLoopInvariant n0ACSL nACSL idx acc accType funCall symbROError

prevIteration :: VarName -> FAExpr -> FAExpr
prevIteration idx = replaceInFAExpr (const Nothing) (replaceIdxWithPrev idx)

replaceIdxWithPrev :: String -> FAExpr -> Maybe FAExpr
replaceIdxWithPrev index (FVar TInt i) | index == i  = Just $ BinaryFPOp SubOp TInt (FVar TInt index) (FInt 1)
                                       | otherwise = Nothing
replaceIdxWithPrev _ _ = Nothing

generateForLoopInvariant :: ACSL.AExpr
                         -> ACSL.AExpr
                         -> VarName
                         -> VarName
                         -> ACSL.Type
                         -> ACSL.AExpr
                         -> ACSL.AExpr
                         -> ACSL.Pred
generateForLoopInvariant n0ACSL nACSL idx acc accType funCall roErr =
  ACSL.Implies (ACSL.PredBExpr $ ACSL.And
               (ACSL.Between n0ACSL (ACSL.Var ACSL.Int idx) nACSL)
               (ACSL.Rel Gt (ACSL.Var ACSL.Int idx) n0ACSL))
               (ACSL.PredBExpr $ ACSL.Rel LtE (ACSL.UnaryOp AbsOp $ ACSL.BinaryOp SubOp (ACSL.Var accType acc) funCall) roErr)

generateAuxVarList :: [a] -> State Int [(a, Int)]
generateAuxVarList callList = do
    currentState <- get
    let list = zip callList [currentState ..]
    put (currentState + (length callList))
    return list


replaceCallsInAExpr :: (FunName -> Bool) -> [(FAExpr,Int)] -> [(FBExpr,Int)] -> FAExpr -> FAExpr
replaceCallsInAExpr hasConds [] [] expr = expr
replaceCallsInAExpr hasConds callList predList funCall@(FEFun _ f fp _) =
  case lookup funCall callList of
    Just n  -> if hasConds f then (Value $ StructVar fp $ auxVarName n) else (FVar fp $ auxVarName n)
    Nothing -> funCall
replaceCallsInAExpr hasConds _ _ ae@(FInt  _)           = ae
replaceCallsInAExpr hasConds _ _ ae@(FCnst _ _)         = ae
replaceCallsInAExpr hasConds _ _ ae@(FVar  _ _)         = ae
replaceCallsInAExpr hasConds _ _ ae@(StructVar  _ _)    = ae
replaceCallsInAExpr hasConds _ _ ae@(ToFloat _ (Int _)) = ae
replaceCallsInAExpr hasConds _ _ ae@(ToFloat _ (Rat _)) = ae
replaceCallsInAExpr hasConds callList predList (Value ae) = Value (replaceCallsInAExpr hasConds callList predList ae)
replaceCallsInAExpr hasConds callList predList (FMin aes) = FMin (map (replaceCallsInAExpr hasConds callList predList) aes)
replaceCallsInAExpr hasConds callList predList (FMax aes) = FMax (map (replaceCallsInAExpr hasConds callList predList) aes)
replaceCallsInAExpr hasConds callList predList (FFma fp ae1 ae2 ae3) = FFma fp (replaceCallsInAExpr hasConds callList predList ae1)
                                                             (replaceCallsInAExpr hasConds callList predList ae2)
                                                             (replaceCallsInAExpr hasConds callList predList ae3)
replaceCallsInAExpr hasConds callList predList (BinaryFPOp op fp ae1 ae2) = BinaryFPOp op fp (replaceCallsInAExpr hasConds callList predList ae1)
                                                                           (replaceCallsInAExpr hasConds callList predList ae2)
replaceCallsInAExpr hasConds callList predList (UnaryFPOp  op fp ae1) = UnaryFPOp  op fp (replaceCallsInAExpr hasConds callList predList ae1)
replaceCallsInAExpr hasConds callList predList (TypeCast fp1 fp2 ae)      = TypeCast fp1 fp2 (replaceCallsInAExpr hasConds callList predList ae)
replaceCallsInAExpr hasConds callList predList (Let letElems stm) = Let (map replaceCallsInLetElem letElems)
                                                      (replaceCallsInAExpr hasConds callList predList stm)
  where
    replaceCallsInLetElem (x,t,ae) = (x,t,replaceCallsInAExpr hasConds callList predList ae)
replaceCallsInAExpr hasConds callList predList (Ite be stmThen stmElse) = Ite (replaceCallsInBExpr hasConds callList predList be)
                                                            (replaceCallsInAExpr hasConds callList predList stmThen)
                                                            (replaceCallsInAExpr hasConds callList predList stmElse)
replaceCallsInAExpr hasConds callList predList (ListIte listThen stmElse) = ListIte (map replaceCallsInListThen listThen)
                                                                  (replaceCallsInAExpr hasConds callList predList stmElse)
  where
    replaceCallsInListThen (be,stm) = (replaceCallsInBExpr hasConds callList predList be
                                      ,replaceCallsInAExpr hasConds callList predList stm)
replaceCallsInAExpr hasConds callList predList (ForLoop fp idxStart idxEnd initAcc idx acc forBody) = ForLoop fp idxStart idxEnd initAcc idx acc (replaceCallsInAExpr hasConds callList predList forBody)
replaceCallsInAExpr _ _ _ UnstWarning = UnstWarning

replaceCallsInAExpr _ _ _ ae = error $ "replaceCallsInAExpr not defined for " ++ show ae

replaceCallsInBExprStm :: (FunName -> Bool) -> [(FAExpr,Int)] -> [(FBExpr,Int)] -> FBExprStm -> FBExprStm
replaceCallsInBExprStm hasConds callList predList (BLet letElems body) = BLet (map replaceCallsLetElem letElems) (replaceCallsInBExprStm hasConds callList predList body)
  where
    replaceCallsLetElem (x,t,ae) = (x,t,replaceCallsInAExpr hasConds callList predList ae)
replaceCallsInBExprStm hasConds callList predList (BIte be stmThen stmElse) = BIte (replaceCallsInBExpr hasConds callList predList be)
                                                                 (replaceCallsInBExprStm hasConds callList predList stmThen)
                                                                 (replaceCallsInBExprStm hasConds callList predList stmElse)
replaceCallsInBExprStm hasConds callList predList (BListIte listThen stmElse) = BListIte (map replaceCallsInListThen listThen)
                                                                       (replaceCallsInBExprStm hasConds callList predList stmElse)
  where
    replaceCallsInListThen (be,stm) = (replaceCallsInBExpr hasConds callList predList be, replaceCallsInBExprStm hasConds callList predList stm)
replaceCallsInBExprStm hasConds callList predList (BExpr be) = BExpr $ replaceCallsInBExpr hasConds callList predList be
replaceCallsInBExprStm _ _ _ BUnstWarning = BUnstWarning

replaceCallsInBExpr :: (FunName -> Bool) -> [(FAExpr,Int)] -> [(FBExpr,Int)] -> FBExpr -> FBExpr
replaceCallsInBExpr _ [] [] bexpr = bexpr
replaceCallsInBExpr _ _ _ FBTrue  = FBTrue
replaceCallsInBExpr _ _ _ FBFalse = FBFalse
replaceCallsInBExpr _ _ _ v@(BStructVar _) = v
replaceCallsInBExpr hasConds callList predList call@FEPred{} =
  case lookup call predList of
    Just n  -> BValue $ BStructVar $ auxVarName n
    Nothing -> call
replaceCallsInBExpr hasConds callList predList (BIsValid (call@FEPred{})) =
  case lookup call predList of
    Just n  -> BIsValid $ BStructVar $ auxVarName n
    Nothing -> call
replaceCallsInBExpr hasConds callList predList (FOr  be1 be2) = FOr  (replaceCallsInBExpr hasConds callList predList be1) (replaceCallsInBExpr hasConds callList predList be2)
replaceCallsInBExpr hasConds callList predList (FAnd be1 be2) = FAnd (replaceCallsInBExpr hasConds callList predList be1) (replaceCallsInBExpr hasConds callList predList be2)
replaceCallsInBExpr hasConds callList predList (FNot be)      = FNot (replaceCallsInBExpr hasConds callList predList be)
replaceCallsInBExpr hasConds callList predList (FRel rel ae1 ae2) = FRel rel (replaceCallsInAExpr hasConds callList predList ae1) (replaceCallsInAExpr hasConds callList predList ae2)
replaceCallsInBExpr hasConds callList predList (BIsValid be) = BIsValid $ replaceCallsInBExpr hasConds callList predList be
replaceCallsInBExpr hasConds callList predList (IsValid  ae) = IsValid  $ replaceCallsInAExpr hasConds callList predList ae
replaceCallsInBExpr hasConds callList predList (BValue   be) = BValue   $ replaceCallsInBExpr hasConds callList predList be


numFunName :: Maybe PredAbs -> String -> String
numFunName predAbs f = (symbFunName predAbs f) ++ "_num"

symbFunName :: Maybe PredAbs -> String -> String
symbFunName (Just TauPlus)  f = f ++ "_tauplus"
symbFunName (Just TauMinus) f = f ++ "_tauminus"
symbFunName _ f = f


generateNumericFunction :: HasConditionals -> Maybe PredAbs -> PVSType -> String -> [Arg] -> [Arg] -> [(VarName, Double)] -> C.Decl
generateNumericFunction hasConds predAbs fp f args errArgs numErrExprs =
  C.Decl (if (hasConds f) then C.MaybeStruct t else t) (numFunName predAbs f) (map args2C args) [C.Return $ C.EFun (symbFunName predAbs f) t (actArgs++numErrArgs)]
  where
    t = fprec2type fp
    actArgs = map (aexpr2inlineC hasConds . arg2var) args
    numErrArgs = map (\(Arg var _) -> C.ErrorCnst $ fromMaybe (error $ "generateNumericFunction: error variable " ++ show var ++" not found.") (lookup var numErrExprs)) errArgs
