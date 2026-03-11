-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Frontend.PVS.MapRealPVSLangAST where

import Frontend.PVS.AbsRawPVSLang
import qualified Frontend.PVS.AbsRawPVSLang as Raw
import AbsPVSLang
import Data.Maybe(fromMaybe)
import ErrM
import qualified Operators as Op
import Frontend.PVS.ParRawPVSLang
import Frontend.PVS.LexRawPVSLang
import Frontend.PVS.MapPVSLangAST(isBExpr,raw2FPType,toErrM,retTypeFun,raw2Args, TypeContext)

parseFileToRealProgram :: FilePath -> IO (Err RProgram)
parseFileToRealProgram src_filename = fmap parseRealProgram (readFile src_filename)

parseRealProgram :: String -> Err RProgram
parseRealProgram str =
  do
    rawParsedRealProg <- rawparserRealPVS str
    return $ raw2RealProg rawParsedRealProg

type VarTypeEnv = [(String, PVSType)]
type FunTypeEnv = [(String, PVSType)]

raw2Id :: Raw.Id -> VarName
raw2Id (Raw.Id x) = x

raw2RealProg :: Raw.Program -> AbsPVSLang.RProgram
raw2RealProg pgm
  | Raw.Prog _ listDecl _ <- pgm
    = let listDecl' = nonTypeDeclarations listDecl in
      raw2Decsl tc (map retTypeFun' listDecl') listDecl'
  | Raw.ProgImp _ _ listDecl _ <- pgm
    = let listDecl' = nonTypeDeclarations listDecl in
      raw2Decsl tc (map retTypeFun' listDecl') listDecl'
  where
    isTypeSynonym :: Raw.Decl -> Bool
    isTypeSynonym (DeclTypeAlias _ _) = True
    isTypeSynonym _ = False

    nonTypeDeclarations :: [Raw.Decl] -> [Raw.Decl]
    nonTypeDeclarations = filter (not . isTypeSynonym)

    retTypeFun' = retTypeFun tc

    tc = []

raw2Decsl :: TypeContext -> FunTypeEnv -> [Raw.Decl] -> [AbsPVSLang.RDecl]
raw2Decsl tc fenv = map (raw2Decl tc fenv)

raw2Decl :: TypeContext -> FunTypeEnv -> Raw.Decl -> AbsPVSLang.RDecl
raw2Decl tc fenv (DeclConstant f (TypeSimple (Id "bool")) expr)
  = RPred (raw2Id f) [] (raw2BExprStm tc [] fenv expr)

raw2Decl tc fenv (DeclFunction f rawArgs (TypeSimple (Id "bool")) expr)
  = RPred (raw2Id f) args (raw2BExprStm tc env fenv expr)
  where
    args = raw2Args tc rawArgs
    env = map mapArg2Pair args

raw2Decl tc fenv (DeclConstant f fptype@(TypeRecord _) stm)
  = RCollDecl (raw2FPType tc fptype) (raw2Id f) [] (raw2RCollExpr tc [] fenv stm)

raw2Decl tc fenv (DeclFunction f rawArgs fptype@(TypeRecord _) stm)
  = RCollDecl (raw2FPType tc fptype) (raw2Id f) args (raw2RCollExpr tc env fenv stm)
  where
    args = raw2Args tc rawArgs
    env  = map mapArg2Pair args

raw2Decl tc fenv (DeclConstant f fptype@(TypeTuple _) stm)
  = RCollDecl (raw2FPType tc fptype) (raw2Id f) [] (raw2RCollExpr tc [] fenv stm)

raw2Decl tc fenv (DeclFunction f rawArgs fptype@(TypeTuple _) stm)
  = RCollDecl (raw2FPType tc fptype) (raw2Id f) args (raw2RCollExpr tc env fenv stm)
  where
    args = raw2Args tc rawArgs
    env  = map mapArg2Pair args

raw2Decl tc fenv (DeclConstant f t stm)
  = RDecl (raw2FPType tc t) (raw2Id f) [] (raw2AExpr tc [] fenv stm)

raw2Decl tc fenv (DeclFunction f rawArgs t stm)
  = RDecl (raw2FPType tc t) (raw2Id f) args (raw2AExpr tc env fenv stm)
  where
    args = raw2Args tc rawArgs
    env = map mapArg2Pair args

raw2Decl _ _ decl = error $ "[raw2Decl] unexpected argument: " ++ show decl

raw2Elsif :: TypeContext -> VarTypeEnv -> FunTypeEnv -> Raw.ElsIf -> (AbsPVSLang.BExpr, AbsPVSLang.AExpr)
raw2Elsif tc env fenv (ElsIf fbexpr stm) = (raw2BExpr tc env fenv fbexpr, raw2AExpr tc env fenv stm)

raw2BElsif :: TypeContext -> VarTypeEnv -> FunTypeEnv -> Raw.ElsIf -> (AbsPVSLang.BExpr, AbsPVSLang.BExprStm)
raw2BElsif tc env fenv (ElsIf fbexpr stm) = (raw2BExpr tc env fenv fbexpr, raw2BExprStm tc env fenv stm)

raw2LetElem :: TypeContext -> VarTypeEnv -> FunTypeEnv -> Raw.LetElem -> AbsPVSLang.LetElem
raw2LetElem tc env fenv (Raw.LetElem x rawExpr)
  | isIntAExpr expr = AbsPVSLang.LetElem {letVar = raw2Id x, letType = TInt, letExpr = expr}
  | otherwise         = AbsPVSLang.LetElem {letVar = raw2Id x, letType = Real, letExpr = expr}
  where
    expr = raw2AExpr tc env fenv rawExpr
raw2LetElem tc env fenv (LetElemType x t rawExpr) = AbsPVSLang.LetElem {letVar  = raw2Id x
                                                                       ,letType = raw2FPType tc t
                                                                       ,letExpr = raw2AExpr tc env fenv rawExpr}

raw2BinOp :: TypeContext -> VarTypeEnv -> FunTypeEnv -> Raw.Expr -> Raw.Expr -> Op.BinOp -> AbsPVSLang.AExpr
raw2BinOp tc env fenv fae1 fae2 op = AbsPVSLang.BinaryOp op ae1 ae2
  where
    ae1 = raw2AExpr tc env fenv fae1
    ae2 = raw2AExpr tc env fenv fae2

raw2RecordElem :: TypeContext -> VarTypeEnv -> FunTypeEnv -> Raw.RecordElem
                  -> (RecordField, Either AbsPVSLang.AExpr AbsPVSLang.BExpr)
raw2RecordElem tc env fenv (Raw.RecordElem (Raw.Id field) expr)
  | isBExpr expr = (field, Right $ raw2BExpr tc env fenv expr)
  | otherwise    = (field, Left  $ raw2AExpr tc env fenv expr)

raw2TupleExpr :: TypeContext -> VarTypeEnv -> FunTypeEnv -> Raw.Expr -> Either AbsPVSLang.AExpr AbsPVSLang.BExpr
raw2TupleExpr tc env fenv expr | isBExpr expr = Right $ raw2BExpr tc env fenv expr
                            | otherwise    = Left  $ raw2AExpr tc env fenv expr

raw2RCollExpr :: TypeContext -> VarTypeEnv -> FunTypeEnv -> Raw.Expr -> AbsPVSLang.CollAExpr
raw2RCollExpr tc env fenv (Raw.With array idx newValue)
  = RArrayUpdate (raw2RCollExpr tc env fenv array) (raw2AExpr tc env fenv idx) (raw2AExpr tc env fenv newValue)

raw2RCollExpr tc env fenv (Raw.Let letElems stm)
  = RCLet letList (raw2RCollExpr tc newenv fenv stm)
  where
    (newenv,letList) = foldl aux_fold (env,[]) letElems
    aux_fold (accEnv,elems) letElem = (env',elems ++ [newLetElem])
      where
        newLetElem = raw2LetElem tc accEnv fenv letElem
        env' = (letVar newLetElem, letType newLetElem):accEnv

raw2RCollExpr tc env fenv (Raw.If be thenSmt elseStm)
  = RCIte (raw2BExpr tc env fenv be) (raw2RCollExpr tc env fenv thenSmt)
                                     (raw2RCollExpr tc env fenv elseStm)

raw2RCollExpr tc env fenv (Raw.ListIf be stmThen listElsif elseStm)
  = RCListIte ((raw2BExpr tc env fenv be,raw2RCollExpr tc env fenv stmThen) : map raw2CollElsif listElsif) (raw2RCollExpr tc env fenv elseStm)
    where
      raw2CollElsif (ElsIf fbexpr stm) = (raw2BExpr tc env fenv fbexpr, raw2RCollExpr tc env fenv stm)

raw2RCollExpr tc env fenv (Raw.RecordExpr recordElems)
  = AbsPVSLang.RRecordExpr $ map (raw2RecordElem tc env fenv) recordElems

raw2RCollExpr tc env fenv (Raw.TupleExpr exprs)
  = AbsPVSLang.RTupleExpr $ map (raw2TupleExpr tc env fenv) exprs

raw2RCollExpr _ env fenv (Raw.ExprId (Raw.Id i))
  = case lookup i fenv of
      Just t@(Tuple _)  -> RCollFun i t []
      Just t@(Record _) -> RCollFun i t []
      Just t@(Array _ _) -> RCollFun i t []
      Just _  -> error $ "Identifier " ++ show i ++ "is not of data collection type."
      Nothing -> case lookup i env of
                    Just t@(Tuple  _) -> AbsPVSLang.RCollVar t i
                    Just t@(Record _) -> AbsPVSLang.RCollVar t i
                    Just t@(Array _ _) -> AbsPVSLang.RCollVar t i
                    Just _  -> error $ "Identifier " ++ show i ++ "is not of data collection type."
                    Nothing -> error $ "Identifier " ++ show i ++ "not found." ++ " in env: " ++ show env

raw2RCollExpr tc env fenv (Raw.Call (Raw.ExprId (Raw.Id f)) actArgs)
  = RCollFun f fp (map (raw2AExpr tc env fenv) actArgs)
    where
      fp = fromMaybe (error $ "raw2FAExpr: function " ++ show f ++ " not found.")
                    (lookup f fenv)

raw2RCollExpr _ _ _ fae = error $ "raw2RCollExpr: " ++ show fae ++ "is not of type data collection expression."

raw2AExpr :: TypeContext -> VarTypeEnv -> FunTypeEnv -> Raw.Expr -> AbsPVSLang.AExpr
raw2AExpr _ _ _ (Raw.Int      i)   = AbsPVSLang.Int i
raw2AExpr _ _ _ (Raw.Rat      d)   = AbsPVSLang.Rat (toRational d)

raw2AExpr _ env fenv (Raw.ExprId (Raw.Id i)) =
  case lookup i fenv of
    Just (Tuple _) -> error $ "Identifier " ++ show i ++ "is a tuple."
    Just (Record _) -> error $ "Identifier " ++ show i ++ "is a record."
    Just fp -> AbsPVSLang.EFun i ResValue fp []
    Nothing -> case lookup i env of
                  Just fp -> AbsPVSLang.Var fp i
                  Nothing -> error $ "Identifier " ++ show i ++ "not found." ++ " in env: " ++ show env

raw2AExpr _ _ _ (Raw.ExprNeg (Raw.Int i)) = AbsPVSLang.Int (-i)
raw2AExpr _ _ _ (Raw.ExprNeg (Raw.Rat d)) = AbsPVSLang.Rat (toRational (-d))
raw2AExpr tc env fenv (Raw.ExprNeg fae) = AbsPVSLang.UnaryOp Op.NegOp (raw2AExpr tc env fenv fae)

raw2AExpr tc env fenv (Raw.ExprAdd fae1 fae2) = raw2BinOp tc env fenv fae1 fae2 Op.AddOp
raw2AExpr tc env fenv (Raw.ExprSub fae1 fae2) = raw2BinOp tc env fenv fae1 fae2 Op.SubOp
raw2AExpr tc env fenv (Raw.ExprMul fae1 fae2) = raw2BinOp tc env fenv fae1 fae2 Op.MulOp
raw2AExpr tc env fenv (Raw.ExprDiv fae1 fae2) = raw2BinOp tc env fenv fae1 fae2 Op.DivOp
raw2AExpr tc env fenv (Raw.ExprPow fae1 fae2) = raw2BinOp tc env fenv fae1 fae2 Op.PowOp

raw2AExpr tc env fenv (Raw.Call (Raw.ExprId (Raw.Id f)) [fae])
  | f == "abs"    = AbsPVSLang.UnaryOp Op.AbsOp  (raw2AExpr tc env fenv fae)
  | f == "sqrt"   = AbsPVSLang.UnaryOp Op.SqrtOp (raw2AExpr tc env fenv fae)
  | f == "sin"    = AbsPVSLang.UnaryOp Op.SinOp  (raw2AExpr tc env fenv fae)
  | f == "cos"    = AbsPVSLang.UnaryOp Op.CosOp  (raw2AExpr tc env fenv fae)
  | f == "tan"    = AbsPVSLang.UnaryOp Op.TanOp  (raw2AExpr tc env fenv fae)
  | f == "asin"   = AbsPVSLang.UnaryOp Op.AsinOp (raw2AExpr tc env fenv fae)
  | f == "acos"   = AbsPVSLang.UnaryOp Op.AcosOp (raw2AExpr tc env fenv fae)
  | f == "atan"   = AbsPVSLang.UnaryOp Op.AtanOp (raw2AExpr tc env fenv fae)
  | f == "ln"     = AbsPVSLang.UnaryOp Op.LnOp   (raw2AExpr tc env fenv fae)
  | f == "exp"    = AbsPVSLang.UnaryOp Op.ExpoOp (raw2AExpr tc env fenv fae)

raw2AExpr tc env fenv (Raw.Call (Raw.ExprId (Raw.Id f)) [Raw.ExprId (Raw.Id listName),idx])
  | f == "nth"  = AbsPVSLang.ListElem fp listName (raw2AExpr tc env fenv idx)
  where
    t = fromMaybe (error errorMsg)
                  (lookup listName env)
    fp = case t of
            List t' -> t'
            _       -> error errorMsg
    errorMsg = "raw2AExpr: list " ++ show listName ++ " not found."

raw2AExpr _ env _ (Raw.Call (Raw.ExprId (Raw.Id f)) [Raw.ExprId (Raw.Id funName)
                                                           ,Raw.ExprId (Raw.Id listName)])
  | f == "map"  = AbsPVSLang.RMap fp funName listName
  where
    t = fromMaybe (error errorMsg)
                  (lookup listName env)
    fp = case t of
            List t' -> t'
            _       -> error errorMsg
    errorMsg = "raw2AExpr: list " ++ show listName ++ " not found."

raw2AExpr tc env fenv (Raw.Call (Raw.ExprId (Raw.Id f)) [Raw.ExprId (Raw.Id funName)
                                                               ,Raw.ExprId (Raw.Id listName)
                                                               ,Raw.Int n
                                                               ,baseCase])
  | f == "fold"  = AbsPVSLang.RFold fp funName listName n (raw2AExpr tc env fenv baseCase)
  where
    t = fromMaybe (error errorMsg)
                  (lookup listName env)
    fp = case t of
            List t' -> t'
            _       -> error errorMsg
    errorMsg = "raw2AExpr: list " ++ show listName ++ " not found."

raw2AExpr tc env fenv (Raw.Call (Raw.ExprId (Raw.Id f)) [fae1,fae2])
  | f == "add"  = raw2BinOp tc env fenv fae1 fae2 Op.AddOp
  | f == "sub"  = raw2BinOp tc env fenv fae1 fae2 Op.SubOp
  | f == "mul"  = raw2BinOp tc env fenv fae1 fae2 Op.MulOp
  | f == "div"  = raw2BinOp tc env fenv fae1 fae2 Op.DivOp
  | f == "mod"  = raw2BinOp tc env fenv fae1 fae2 Op.ModOp

raw2AExpr tc env fenv (Raw.Call (Raw.ExprId (Raw.Id f)) actArgs)
  = case lookup f fenv of
      Just fp -> AbsPVSLang.EFun f ResValue fp (map (raw2AExpr tc env fenv) actArgs)
      Nothing -> case lookup f env of
                   Just (Array _ t) -> ArrayElem t f (map (raw2AExpr tc env fenv) actArgs)
                   _ -> error $ "raw2AExpr: identifier " ++ show f ++ " not found."

raw2AExpr tc env fenv (TupleIndex callee idx)
  = case callee of
      Raw.ExprId (Raw.Id tuple) ->
        let t = fromMaybe (error $ "raw2AExpr: tuple " ++ show tuple ++ " not found.")
                          (lookup tuple env)
            fp = case t of
                   Tuple idxTypes -> idxTypes !! fromInteger (idx - 1)
                   _ -> error $ "raw2AExpr: " ++ show t ++ "is not a tuple type."
        in TupleElem fp tuple idx
      Raw.Call (Raw.ExprId (Raw.Id f)) args ->
        let t = fromMaybe (error $ "raw2AExpr: function " ++ show f ++ " not found.")
                          (lookup f fenv)
            fp = case t of
                   Tuple idxTypes -> idxTypes !! fromInteger (idx - 1)
                   _ -> error $ "raw2AExpr: " ++ show t ++ "is not a tuple type."
        in EFun f (ResTupleIndex idx) fp (map (raw2AExpr tc env fenv) args)
      _ -> error $ "raw2AExpr: unsupported callee in TupleIndex: " ++ show callee

raw2AExpr tc env fenv (RecordField callee (Id field))
  = case callee of
      Raw.ExprId (Raw.Id record) ->
        let t = fromMaybe (error $ "raw2AExpr: record " ++ show record ++ " not found.")
                          (lookup record env)
            fp = case t of
                   Record fieldTypes -> fromMaybe (error $ "raw2AExpr: record field " ++ show field ++ " not found.")
                                                  (lookup field fieldTypes)
                   _ -> error $ "raw2AExpr: " ++ show t ++ "is not a record type."
        in AbsPVSLang.RecordElem fp record field
      Raw.Call (Raw.ExprId (Raw.Id f)) args ->
        let t = fromMaybe (error $ "raw2AExpr: function " ++ show f ++ " not found.")
                          (lookup f fenv)
            fp = case t of
                   Record fieldTypes -> fromMaybe (error $ "raw2AExpr: record field " ++ show field ++ " not found.")
                                                  (lookup field fieldTypes)
                   _ -> error $ "raw2AExpr: " ++ show t ++ "is not a record type."
        in EFun f (ResRecordField field) fp (map (raw2AExpr tc env fenv) args)
      _ -> error $ "raw2AExpr: unsupported callee in RecordField: " ++ show callee

raw2AExpr tc env fenv (Raw.Let letElems stm)
  = RLet letList (raw2AExpr tc newenv fenv stm)
  where
    (newenv,letList) = foldl aux_fold (env,[]) letElems
    aux_fold  (accEnv,elems) letElem =  (env',elems ++ [newLetElem])
      where
        newLetElem = raw2LetElem tc accEnv fenv letElem
        env' = (letVar newLetElem, letType newLetElem):accEnv

raw2AExpr tc env fenv (Raw.For idxInit idxEnd initValueAcc (Lambda _ idx _subRangeLb _subRangeUb acc accType body))
  = RForLoop t idxName
             (raw2AExpr tc env fenv idxInit)
             (raw2AExpr tc env fenv idxEnd)
             accName
             (raw2AExpr tc env fenv initValueAcc)
             (raw2AExpr tc ((idxName,TInt):(accName,t):env) fenv body)
  where
    idxName = raw2Id idx
    accName = raw2Id acc
    t = raw2FPType tc accType

raw2AExpr tc env fenv (Raw.If be thenSmt elseStm)  = RIte (raw2BExpr tc env fenv be) (raw2AExpr tc env fenv thenSmt) (raw2AExpr tc env fenv elseStm)

raw2AExpr tc env fenv (Raw.ListIf be stmThen listElsif elseStm) =
    RListIte ((raw2BExpr tc env fenv be,raw2AExpr tc env fenv stmThen) : map (raw2Elsif tc env fenv) listElsif) (raw2AExpr tc env fenv elseStm)

raw2AExpr _ _ _ ae = error $ "Something went wrong: arithmetic expression expected but got " ++ show ae ++ "."

raw2BExprStm :: TypeContext -> VarTypeEnv -> FunTypeEnv -> Raw.Expr -> AbsPVSLang.BExprStm

raw2BExprStm tc env fenv (Raw.Let letElems stm)
  = RBLet letList (raw2BExprStm tc newenv fenv stm)
  where
    (newenv,letList) = foldl aux_fold (env,[]) letElems
    aux_fold (accEnv,elems) letElem =  (env',elems ++ [newLetElem])
      where
        newLetElem = raw2LetElem tc accEnv fenv letElem
        env' = (letVar newLetElem, letType newLetElem):accEnv

raw2BExprStm tc env fenv (Raw.If be thenSmt elseStm)
  = RBIte (raw2BExpr tc env fenv be) (raw2BExprStm tc env fenv thenSmt)
                                     (raw2BExprStm tc env fenv elseStm)

raw2BExprStm tc env fenv (Raw.ListIf be stmThen listElsif elseStm) =
    RBListIte ((raw2BExpr tc env fenv be,raw2BExprStm tc env fenv stmThen) : map (raw2BElsif tc env fenv) listElsif) (raw2BExprStm tc env fenv elseStm)

raw2BExprStm tc env fenv be = RBExpr $ raw2BExpr tc env fenv be


raw2BExpr :: TypeContext -> VarTypeEnv -> FunTypeEnv -> Raw.Expr -> AbsPVSLang.BExpr
raw2BExpr tc env fenv (Raw.Or  be1 be2) = AbsPVSLang.Or  (raw2BExpr tc env fenv be1) (raw2BExpr tc env fenv be2)
raw2BExpr tc env fenv (Raw.And be1 be2) = AbsPVSLang.And (raw2BExpr tc env fenv be1) (raw2BExpr tc env fenv be2)
raw2BExpr tc env fenv (Raw.Not be)      = AbsPVSLang.Not (raw2BExpr tc env fenv be)
raw2BExpr tc env fenv (Raw.Eq  ae1 ae2) = AbsPVSLang.Rel Op.Eq  (raw2AExpr tc env fenv ae1) (raw2AExpr tc env fenv ae2)
raw2BExpr tc env fenv (Raw.Neq ae1 ae2) = AbsPVSLang.Rel Op.Neq (raw2AExpr tc env fenv ae1) (raw2AExpr tc env fenv ae2)
raw2BExpr tc env fenv (Raw.Lt  ae1 ae2) = AbsPVSLang.Rel Op.Lt  (raw2AExpr tc env fenv ae1) (raw2AExpr tc env fenv ae2)
raw2BExpr tc env fenv (Raw.LtE ae1 ae2) = AbsPVSLang.Rel Op.LtE (raw2AExpr tc env fenv ae1) (raw2AExpr tc env fenv ae2)
raw2BExpr tc env fenv (Raw.Gt  ae1 ae2) = AbsPVSLang.Rel Op.Gt  (raw2AExpr tc env fenv ae1) (raw2AExpr tc env fenv ae2)
raw2BExpr tc env fenv (Raw.GtE ae1 ae2) = AbsPVSLang.Rel Op.GtE (raw2AExpr tc env fenv ae1) (raw2AExpr tc env fenv ae2)
raw2BExpr _ _   _     Raw.BTrue        = AbsPVSLang.BTrue
raw2BExpr _ _   _     Raw.BFalse       = AbsPVSLang.BFalse
raw2BExpr tc env fenv  (Call (ExprId (Id f)) args) =
  case lookup f fenv of
    Just Boolean -> AbsPVSLang.EPred f (map (raw2AExpr tc env fenv) args)
    Just _ -> error "raw2BExpr: Boolean function expected."
    Nothing -> error $ "raw2BExpr: something went wrong "++ show f ++ " is not a predicate."
raw2BExpr _ _ _ be = error $ "Something went wrong: boolean expression expected but got " ++ show be ++ "."


rawparserRealPVS :: String -> Err Raw.Program
rawparserRealPVS = toErrM . pProgram . tokens
