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
import qualified FramaC.Clang as C
import qualified FramaC.ACSLTypes as ACSL
import qualified FramaC.ACSLlang as ACSL 
import FramaC.GenerateACSL (expr2acsl)
import Operators
import PVSTypes
import Translation.Float2Real

resultVar :: String
resultVar = "res"

args2C :: Arg -> C.Arg
args2C (Arg x fp) = C.Arg (fprec2type fp) x

args2CwithType :: C.Type -> Arg -> C.Arg
args2CwithType t (Arg x _) = C.Arg t x

resultSome :: C.Type -> FAExpr -> C.Stm
resultSome t expr = C.VarAssign resultVar (C.Some t (Left $ aexpr2inlineC expr))

resultSomeBool :: FBExpr -> C.Stm
resultSomeBool expr = C.VarAssign resultVar (C.Some C.Boolean (Right $ bexpr2C expr))

resultNone ::  C.Type -> C.Stm
resultNone t = C.VarAssign resultVar (C.None t)

structVarName :: Int -> String
structVarName n = "aux_" ++ show n

declareStructVar :: (Either FAExpr FBExpr,Int) -> C.Stm
declareStructVar (Left (Value expr), n) =
  C.VarDeclAssign (C.MaybeStruct (fprec2type $ getPVSType expr)) (structVarName n) (aexpr2inlineCstruct expr)
declareStructVar (Left expr, n) =
  C.VarDeclAssign (C.MaybeStruct (fprec2type $ getPVSType expr)) (structVarName n) (aexpr2inlineCstruct expr)
declareStructVar (Right expr, n) =
  C.VarDeclAssignBool (C.MaybeStruct C.Boolean) (structVarName n) (bexpr2C expr)

predAbsSuffix :: PredAbs -> String
predAbsSuffix Original = ""
predAbsSuffix TauPlus = "_tauplus"
predAbsSuffix TauMinus = "_tauminus"

decl2C :: [(AExpr,AExpr)]
          -> [(FAExpr,AExpr)]
          -> Interpretation
          -> Decl 
          -> C.Decl
decl2C forListExpr forMap interp (Decl _ fp f args expr) =
  C.Decl returnType (f++printReturnType fp) (map args2C args) (resDecl:stmList++[returnRes])
  where
    (stmList,_) = runState (aexpr2C forListExpr forMap (fprec2type fp) interp emptyEnv expr) 0
    returnType = fprec2MaybeType fp
    resDecl   = C.VarDecl returnType resultVar
    returnRes = C.Return (C.Var returnType resultVar)
    printReturnType FPSingle = "_single"
    printReturnType FPDouble = "_double"
    printReturnType TInt     = "_int"
    printReturnType t        = error $ "decl2C: unexpected return type " ++ show t

decl2C _ _ _ Pred{} = error "decl2C: Expected numerical function declaration but a predicate."

pred2C :: Interpretation
       -> Decl 
       -> C.Decl
pred2C interp (Pred _ predAbs f args expr) =
  C.Decl (C.MaybeStruct C.Boolean) (f++(predAbsSuffix predAbs)++"_bool") (map args2C args) bodyWithValidityCheck
  where
    resDecl   = C.VarDecl (C.MaybeStruct C.Boolean) resultVar
    (stmList,_) = runState (bexprStm2C interp expr) 0
    returnRes = C.Return (C.Var C.Boolean resultVar)
    bodyWithValidityCheck = resDecl:stmList++[returnRes]
pred2C _ Decl{} = error "pred2C: Expected predicate but got a numerical function declaration."

letElem2C :: Interpretation -> FLetElem -> ([C.Stm],Env [ACeb]) -> ([C.Stm],Env [ACeb])
-- letElem2C interp (x,_,expr@(FEFun _ _ t _)) (listVarAssign, env) = 
--   (C.VarDeclAssign (fprec2type t) x (C.Value $ aexpr2inlineC expr):listVarAssign, env')
--   where
--     semx = exprSemantics interp env expr
--     env' = insertEnv x semx env
letElem2C interp (x,t,expr) (listVarAssign, env) =
  (C.VarDeclAssign (fprec2type t) x (aexpr2inlineC expr):listVarAssign, env')
  where
    semx = exprSemantics interp env expr
    env' = insertEnv x semx env

aexpr2C :: [(AExpr,AExpr)]
        -> [(FAExpr,AExpr)]
        -> C.Type
        -> Interpretation
        -> Env AbstractDomain.ACebS
        -> FAExpr
        -> State Int [C.Stm]

aexpr2C forListExpr forMap t interp env (Let listExpr body) = do
  let (listVarAssign,env') = foldr (letElem2C interp) ([],env) listExpr
  bodyStm <- aexpr2C forListExpr forMap t interp env' body
  return $ listVarAssign++bodyStm

aexpr2C forListExpr forMap t interp env (Ite bexpr thenExpr elseExpr) = do
  callList <- validityCheckList bexpr
  thenStm  <- aexpr2C forListExpr forMap t interp env (replaceCallsInAExpr callList thenExpr)
  elseStm  <- aexpr2C forListExpr forMap t interp env (replaceCallsInAExpr callList elseExpr)
  return $ map declareStructVar callList
           ++
           [C.Ite (bexpr2C (replaceCallsInBExpr callList bexpr)) thenStm elseStm]

aexpr2C forListExpr forMap t interp env (ListIte listThen elseExpr) = do
  let guards = map fst listThen
  callListMap <- mapM validityCheckList guards
  let callList = concat callListMap
  thenListStm <- mapM (aexpr2C forListExpr forMap t interp env . replaceCallsInAExpr callList . snd) listThen
  elseStm     <- aexpr2C forListExpr forMap t interp env (replaceCallsInAExpr callList elseExpr)
  let guardsC = map (bexpr2C . replaceCallsInBExpr callList) guards
  return $ map declareStructVar callList ++ [C.ListIte (zip guardsC thenListStm) elseStm]

aexpr2C forListExpr forMap t interp env for@(ForLoop fp n0 n initAcc idx acc forBody) = do
  forBodyStm <- aexpr2C forListExpr forMap t interp env forBody
  let accType  = fprec2type fp
  let forBody' =  [C.Ite (C.IsValid (C.Var accType resultVar)) forBodyStm [C.VarAssign resultVar (C.None accType)]]
  return [ C.VarDecl C.Int idx
         , C.VarDeclAssign accType acc (aexpr2inlineC initAcc)
         , C.VarAssign resultVar (C.Some accType (Left $ C.Var accType acc))
         , generateForLoopACSL forMap idx acc (type2acsl accType) n n0 for forListExpr interp env forBody
         , C.ForLoop idx (aexpr2inlineC n0) (aexpr2inlineC n) forBody']

aexpr2C _ _ t _ _ UnstWarning = return [resultNone t]
aexpr2C _ _ t _ _ expr        = return [resultSome t expr]

aexpr2inlineC :: FAExpr -> C.AExpr
aexpr2inlineC (FInt  i)               = C.IntCnst i
aexpr2inlineC (ToFloat _ (Int n))     = C.IntCnst n
aexpr2inlineC (ToFloat FPSingle (Rat rat)) = C.FPCnst SinglePrec rat
aexpr2inlineC (ToFloat FPDouble (Rat rat)) = C.FPCnst SinglePrec rat
aexpr2inlineC (FCnst FPSingle rat)    = C.FPCnst SinglePrec rat
aexpr2inlineC (FCnst FPDouble rat)    = C.FPCnst DoublePrec rat
aexpr2inlineC (FVar  fp x)            = C.Var  (fprec2type fp) x
aexpr2inlineC (StructVar fp x)        = C.Var  (fprec2MaybeType fp) x
aexpr2inlineC (FArrayElem fp _ v idx) = C.ArrayElem (fprec2type fp) v (aexpr2inlineC idx)
aexpr2inlineC (FEFun _ f fp args)       = C.Value $ C.EFun f (fprec2type fp) (map aexpr2inlineC args)
aexpr2inlineC (Value expr)            = C.Value (aexpr2inlineC expr)
aexpr2inlineC (TypeCast fp1 fp2 expr) = C.TypeCast (fprec2type fp1) (fprec2type fp2) (aexpr2inlineC expr)
aexpr2inlineC (BinaryFPOp op fp expr1 expr2) = C.BinaryOp op (fprec2type fp) (aexpr2inlineC expr1) (aexpr2inlineC expr2)
aexpr2inlineC (UnaryFPOp  op fp expr)        = C.UnaryOp  op (fprec2type fp) (aexpr2inlineC expr)
aexpr2inlineC (FMin exprs) = C.Min (map aexpr2inlineC exprs)
aexpr2inlineC (FMax exprs) = C.Max (map aexpr2inlineC exprs)
aexpr2inlineC (Ite be exprThen exprElse) =  C.IteExpr (bexpr2C be) (aexpr2inlineC exprThen) (aexpr2inlineC exprElse)
aexpr2inlineC ae = error $ "aexpr2inlineC not defined for " ++ show ae

aexpr2inlineCstruct :: FAExpr -> C.AExpr
aexpr2inlineCstruct (FEFun _ f fp args) = C.EFun f (fprec2type fp) (map aexpr2inlineC args)
aexpr2inlineCstruct (StructVar fp x)   = C.Var  (fprec2MaybeType fp) x
aexpr2inlineCstruct ae = error $ "aexpr2inlineCstruct: " ++ show ae ++ " is not a function with type maybeStruct."

bexprStm2C :: Interpretation -> FBExprStm -> State Int [C.Stm]
bexprStm2C interp (BLet listElem body) = do
  let (listVarAssign,_) = foldr (letElem2C interp) ([],emptyEnv) listElem
  bodyStm <- bexprStm2C interp body
  return $ listVarAssign++bodyStm

bexprStm2C interp (BIte bexpr thenExpr elseExpr) = do
  callList <- validityCheckList bexpr
  thenStm  <- bexprStm2C interp (replaceCallsInBExprStm callList thenExpr)
  elseStm  <- bexprStm2C interp (replaceCallsInBExprStm callList elseExpr)
  return $ map declareStructVar callList
           ++
           [C.Ite (bexpr2C (replaceCallsInBExpr callList bexpr)) thenStm elseStm]

bexprStm2C interp (BListIte listThen elseExpr) = do
  let guards = map fst listThen
  callListMap <- mapM validityCheckList guards
  let callList = concat callListMap
  thenListStm <- mapM (bexprStm2C interp . replaceCallsInBExprStm callList . snd) listThen
  elseStm     <- bexprStm2C interp (replaceCallsInBExprStm callList elseExpr)
  let guardsC = map (bexpr2C . replaceCallsInBExpr callList) guards
  return $ map declareStructVar callList
           ++
           [C.ListIte (zip guardsC thenListStm) elseStm]

bexprStm2C _ (BExpr be) = return [resultSomeBool be]

bexprStm2C _ BUnstWarning = return [resultNone C.Boolean]

bexpr2C :: FBExpr -> C.BExpr
bexpr2C FBTrue  = C.BTrue
bexpr2C FBFalse = C.BFalse
bexpr2C (FNot be)      = C.Not (bexpr2C be)
bexpr2C (FOr  be1 be2) = C.Or  (bexpr2C be1) (bexpr2C be2)
bexpr2C (FAnd be1 be2) = C.And (bexpr2C be1) (bexpr2C be2)
bexpr2C (FRel rel ae1 ae2) = C.Rel rel (aexpr2inlineC ae1) (aexpr2inlineC ae2)
bexpr2C (IsValid ae@(FEFun _ _ _ _))    = C.IsValid (aexpr2inlineCstruct ae)
bexpr2C (BIsValid (FEPred _ Original f args)) = C.BIsValid (C.FEPred (f++"_bool") (map aexpr2inlineC args))
bexpr2C (BIsValid (FEPred _ TauPlus  f args)) = C.BIsValid (C.FEPred (f++"_tauplus_bool") (map aexpr2inlineC args))
bexpr2C (BIsValid (FEPred _ TauMinus f args)) = C.BIsValid (C.FEPred (f++"_tauminus_bool") (map aexpr2inlineC args))
bexpr2C (IsValid ae)    = C.IsValid (aexpr2inlineC ae)
bexpr2C (BIsValid ae)   = C.BIsValid (bexpr2C ae)
bexpr2C (BValue ae)     = C.BValue (bexpr2C ae)
bexpr2C (FEPred _ Original f args) = C.FEPred (f++"_bool") (map aexpr2inlineC args)
bexpr2C (FEPred _ TauPlus  f args) = C.FEPred (f++"_tauplus_bool") (map aexpr2inlineC args)
bexpr2C (FEPred _ TauMinus f args) = C.FEPred (f++"_tauminus_bool") (map aexpr2inlineC args)
bexpr2C (BStructVar x)             = C.BVar  (C.MaybeStruct  C.Boolean) x

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

validityCheckListBinOp :: FBExpr -> FBExpr -> State Int [(Either FAExpr FBExpr, Int)]
validityCheckListBinOp be1 be2 = do
  listBe1 <- validityCheckList be1
  listBe2 <- validityCheckList be2
  return $ listBe1 ++ listBe2

validityCheckList :: FBExpr -> State Int [(Either FAExpr FBExpr,Int)]
validityCheckList (FOr  be1 be2) = validityCheckListBinOp be1 be2
validityCheckList (FAnd be1 be2) = validityCheckListBinOp be1 be2
validityCheckList (FNot be)      = validityCheckList be
validityCheckList (IsValid ae)   = do
  currentState <- get
  put (currentState + 1)
  return [(Left ae,currentState)]
validityCheckList (BIsValid ae)   = do
  currentState <- get
  put (currentState + 1)
  return [(Right ae,currentState)]
validityCheckList            _ = return []

replaceCallsInAExpr :: [(Either FAExpr FBExpr,Int)] -> FAExpr -> FAExpr
replaceCallsInAExpr [] expr = expr
replaceCallsInAExpr callList call@(FEFun _ _ fp _) =
  case lookup call funCalls of
    Just n  -> Value $ StructVar fp $ structVarName n
    Nothing -> call
  where
    funCalls = map (bimap (fromLeft $ error "replaceCallsInAExpr: unexpected predicate call.") id) (filter (isLeft . fst) callList)
replaceCallsInAExpr _ ae@(FInt  _)           = ae
replaceCallsInAExpr _ ae@(FCnst _ _)         = ae
replaceCallsInAExpr _ ae@(FVar  _ _)         = ae
replaceCallsInAExpr _ ae@(StructVar  _ _)         = ae
replaceCallsInAExpr _ ae@(ToFloat _ (Int _)) = ae
replaceCallsInAExpr _ ae@(ToFloat _ (Rat _)) = ae
replaceCallsInAExpr callList (Value ae)      = Value (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (FMin aes) = FMin (map (replaceCallsInAExpr callList) aes)
replaceCallsInAExpr callList (FMax aes) = FMax (map (replaceCallsInAExpr callList) aes)
replaceCallsInAExpr callList (FFma fp ae1 ae2 ae3) = FFma fp (replaceCallsInAExpr callList ae1)
                                                             (replaceCallsInAExpr callList ae2)
                                                             (replaceCallsInAExpr callList ae3)
replaceCallsInAExpr callList (BinaryFPOp op fp ae1 ae2) = BinaryFPOp op fp (replaceCallsInAExpr callList ae1)
                                                                           (replaceCallsInAExpr callList ae2)
replaceCallsInAExpr callList (UnaryFPOp  op fp ae1    ) = UnaryFPOp  op fp (replaceCallsInAExpr callList ae1)
replaceCallsInAExpr callList (TypeCast fp1 fp2 ae)      = TypeCast fp1 fp2 (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (Let letElems stm) = Let (map replaceCallsInLetElem letElems) 
                                                      (replaceCallsInAExpr callList stm)
  where
    replaceCallsInLetElem (x,t,ae) = (x,t,replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (Ite be stmThen stmElse) = Ite (replaceCallsInBExpr callList be) 
                                                            (replaceCallsInAExpr callList stmThen)
                                                            (replaceCallsInAExpr callList stmElse)
replaceCallsInAExpr callList (ListIte listThen stmElse) = ListIte (map replaceCallsInListThen listThen)
                                                                  (replaceCallsInAExpr callList stmElse)
  where
    replaceCallsInListThen (be,stm) = (replaceCallsInBExpr callList be, replaceCallsInAExpr callList stm)
replaceCallsInAExpr callList (ForLoop fp idxStart idxEnd initAcc idx acc forBody) = ForLoop fp idxStart idxEnd initAcc idx acc (replaceCallsInAExpr callList forBody)
replaceCallsInAExpr _ UnstWarning = UnstWarning

replaceCallsInAExpr _ ae = error $ "replaceCallsInAExpr not defined for " ++ show ae

replaceCallsInBExprStm :: [(Either FAExpr FBExpr,Int)] -> FBExprStm -> FBExprStm
replaceCallsInBExprStm callList (BLet letElems body) = BLet (map replaceCallsLetElem letElems) (replaceCallsInBExprStm callList body)
  where
    replaceCallsLetElem (x,t,ae) = (x,t,replaceCallsInAExpr callList ae)
replaceCallsInBExprStm callList (BIte be stmThen stmElse) = BIte (replaceCallsInBExpr callList be)
                                                                 (replaceCallsInBExprStm callList stmThen)
                                                                 (replaceCallsInBExprStm callList stmElse)
replaceCallsInBExprStm callList (BListIte listThen stmElse) = BListIte (map replaceCallsInListThen listThen)
                                                                       (replaceCallsInBExprStm callList stmElse)
  where
    replaceCallsInListThen (be,stm) = (replaceCallsInBExpr callList be, replaceCallsInBExprStm callList stm)
replaceCallsInBExprStm callList (BExpr be) = BExpr $ replaceCallsInBExpr callList be
replaceCallsInBExprStm _ BUnstWarning = BUnstWarning

replaceCallsInBExpr :: [(Either FAExpr FBExpr,Int)] -> FBExpr -> FBExpr
replaceCallsInBExpr [] bexpr = bexpr
replaceCallsInBExpr _ FBTrue  = FBTrue
replaceCallsInBExpr _ FBFalse = FBFalse
replaceCallsInBExpr _ v@(BStructVar _) = v
replaceCallsInBExpr callList call@FEPred{} = 
  case lookup call predCalls of
    Just n  -> BValue $ BStructVar $ structVarName n
    Nothing -> call
  where
    predCalls = map (bimap (fromRight $ error "replaceCallsInAExpr: unexpected function call.") id) (filter (isRight . fst) callList)
replaceCallsInBExpr callList (BIsValid (call@FEPred{})) =
  case lookup call predCalls of
    Just n  -> BIsValid $ BStructVar $ structVarName n
    Nothing -> BIsValid $ call
  where
    predCalls = map (bimap (fromRight $ error "replaceCallsInAExpr: unexpected function call.") id) (filter (isRight . fst) callList)
replaceCallsInBExpr callList (FOr  be1 be2) = FOr  (replaceCallsInBExpr callList be1) (replaceCallsInBExpr callList be2)
replaceCallsInBExpr callList (FAnd be1 be2) = FAnd (replaceCallsInBExpr callList be1) (replaceCallsInBExpr callList be2)
replaceCallsInBExpr callList (FNot be)      = FNot (replaceCallsInBExpr callList be)
replaceCallsInBExpr callList (FRel rel ae1 ae2) = FRel rel (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInBExpr callList (BIsValid be) = BIsValid $ replaceCallsInBExpr callList be
replaceCallsInBExpr callList (IsValid  ae) = IsValid  $ replaceCallsInAExpr callList ae
replaceCallsInBExpr callList (BValue   be) = BValue   $ replaceCallsInBExpr callList be


numFunName :: Maybe PredAbs -> String -> String
numFunName predAbs f = (symbFunName predAbs f) ++ "_num"

symbFunName :: Maybe PredAbs -> String -> String
symbFunName (Just TauPlus)  f = f ++ "_tauplus"
symbFunName (Just TauMinus) f = f ++ "_tauminus"
symbFunName _ f = f


generateNumericFunction :: Maybe PredAbs -> PVSType -> String -> [Arg] -> [Arg] -> [(VarName, Double)] -> C.Decl
generateNumericFunction predAbs fp f args errArgs numErrExprs =
  C.Decl (C.MaybeStruct t) (numFunName predAbs f) (map args2C args) [C.Return $ C.EFun (symbFunName predAbs f) t (actArgs++numErrArgs)]
  where
    t = fprec2type fp
    actArgs = map (aexpr2inlineC . arg2var) args
    numErrArgs = map (\(Arg var _) -> C.ErrorCnst $ fromMaybe (error $ "generateNumericFunction: error variable " ++ show var ++" not found.") (lookup var numErrExprs)) errArgs
