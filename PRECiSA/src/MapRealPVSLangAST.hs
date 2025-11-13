-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
{-# LANGUAGE ScopedTypeVariables #-}

module MapRealPVSLangAST
where

import AbsRawPVSLang
import AbsPVSLang
import Common.TypesUtils
import Data.Maybe(fromMaybe)
import ErrM
import qualified Operators as Op
import Parser.ParRawPVSLang
import Parser.LexRawPVSLang
import MapPVSLangAST(isBExpr,raw2FPType,toErrM,retTypeFun,raw2Args, TypeContext)

type VarTypeEnv = [(String, PVSType)]
type FunTypeEnv = [(String, PVSType)]

raw2Id :: AbsRawPVSLang.Id -> VarName
raw2Id (AbsRawPVSLang.Id x) = x

raw2RealProg :: AbsRawPVSLang.Program -> AbsPVSLang.RProgram
raw2RealProg pgm
  | AbsRawPVSLang.Prog _ listDecl _ <- pgm
    = let listDecl' = nonTypeDeclarations listDecl in
      raw2Decsl tc (map retTypeFun' listDecl') listDecl'
  | AbsRawPVSLang.ProgImp _ _ listDecl _ <- pgm
    = let listDecl' = nonTypeDeclarations listDecl in
      raw2Decsl tc (map retTypeFun' listDecl') listDecl'
  where
    isTypeSynonym :: AbsRawPVSLang.Decl -> Bool
    isTypeSynonym (DeclTypeAlias _ _) = True
    isTypeSynonym _ = False

    nonTypeDeclarations :: [AbsRawPVSLang.Decl] -> [AbsRawPVSLang.Decl]
    nonTypeDeclarations = filter (not . isTypeSynonym)

    retTypeFun' = retTypeFun tc

    tc = []

raw2Decsl :: TypeContext -> FunTypeEnv -> [AbsRawPVSLang.Decl] -> [AbsPVSLang.RDecl]
raw2Decsl tc fenv = map (raw2Decl tc fenv)

raw2Decl :: TypeContext -> FunTypeEnv -> AbsRawPVSLang.Decl -> AbsPVSLang.RDecl
raw2Decl tc fenv (DeclConstant f (TypeSimple (Id "bool")) expr)
  = RPred (raw2Id f) [] (raw2BExprStm tc [] fenv expr)

raw2Decl tc fenv (DeclFunction f rawArgs (TypeSimple (Id "bool")) expr)
  = RPred (raw2Id f) args (raw2BExprStm tc env fenv expr)
  where
    args = raw2Args tc rawArgs
    env = map mapArg2Pair args

raw2Decl tc fenv (DeclConstant f fptype@(TypeArray _ _) stm)
  = RCollDecl (raw2FPType tc fptype) (raw2Id f) [] (raw2RCollExpr tc [] fenv stm)

raw2Decl tc fenv (DeclFunction f rawArgs fptype@(TypeArray _ _) stm)
  = RCollDecl (raw2FPType tc fptype) (raw2Id f) args (raw2RCollExpr tc env fenv stm)
  where
    args = raw2Args tc rawArgs
    env  = map mapArg2Pair args

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

raw2Elsif :: TypeContext -> VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.ElsIf -> (AbsPVSLang.BExpr, AbsPVSLang.AExpr)
raw2Elsif tc env fenv (ElsIf fbexpr stm) = (raw2BExpr tc env fenv fbexpr, raw2AExpr tc env fenv stm)

raw2BElsif :: TypeContext -> VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.ElsIf -> (AbsPVSLang.BExpr, AbsPVSLang.BExprStm)
raw2BElsif tc env fenv (ElsIf fbexpr stm) = (raw2BExpr tc env fenv fbexpr, raw2BExprStm tc env fenv stm)

raw2LetElem :: TypeContext -> VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.LetElem -> AbsPVSLang.LetElem
raw2LetElem tc env fenv (AbsRawPVSLang.LetElem x rawExpr)
  | isIntAExpr expr = AbsPVSLang.LetElem {letVar = raw2Id x, letType = TInt, letExpr = expr}
  | otherwise         = AbsPVSLang.LetElem {letVar = raw2Id x, letType = Real, letExpr = expr}
  where
    expr = raw2AExpr tc env fenv rawExpr
raw2LetElem tc env fenv (LetElemType x t rawExpr) = AbsPVSLang.LetElem {letVar  = raw2Id x
                                                                       ,letType = raw2FPType tc t
                                                                       ,letExpr = raw2AExpr tc env fenv rawExpr}

raw2BinOp :: TypeContext -> VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> AbsRawPVSLang.Expr -> Op.BinOp -> AbsPVSLang.AExpr
raw2BinOp tc env fenv fae1 fae2 op = AbsPVSLang.BinaryOp op ae1 ae2
  where
    ae1 = raw2AExpr tc env fenv fae1
    ae2 = raw2AExpr tc env fenv fae2

raw2RecordElem :: TypeContext -> VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.RecordElem
                  -> (RecordField, Either AbsPVSLang.AExpr AbsPVSLang.BExpr)
raw2RecordElem tc env fenv (AbsRawPVSLang.RecordElem (AbsRawPVSLang.Id field) expr)
  | isBExpr expr = (field, Right $ raw2BExpr tc env fenv expr)
  | otherwise    = (field, Left  $ raw2AExpr tc env fenv expr)

raw2TupleExpr :: TypeContext -> VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> Either AbsPVSLang.AExpr AbsPVSLang.BExpr
raw2TupleExpr tc env fenv expr | isBExpr expr = Right $ raw2BExpr tc env fenv expr
                            | otherwise    = Left  $ raw2AExpr tc env fenv expr

raw2RCollExpr :: TypeContext -> VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> AbsPVSLang.CollAExpr
raw2RCollExpr tc env fenv (AbsRawPVSLang.With array idx newValue)
  = RArrayUpdate (raw2RCollExpr tc env fenv array) (raw2AExpr tc env fenv idx) (raw2AExpr tc env fenv newValue)

raw2RCollExpr tc env fenv (AbsRawPVSLang.Let letElems stm)
  = RCLet letList (raw2RCollExpr tc newenv fenv stm)
  where
    (newenv,letList) = foldl aux_fold (env,[]) letElems
    aux_fold (accEnv,elems) letElem = (env',elems ++ [newLetElem])
      where
        newLetElem = raw2LetElem tc accEnv fenv letElem
        env' = (letVar newLetElem, letType newLetElem):accEnv

raw2RCollExpr tc env fenv (AbsRawPVSLang.If be thenSmt elseStm)
  = RCIte (raw2BExpr tc env fenv be) (raw2RCollExpr tc env fenv thenSmt)
                                     (raw2RCollExpr tc env fenv elseStm)

raw2RCollExpr tc env fenv (AbsRawPVSLang.ListIf be stmThen listElsif elseStm)
  = RCListIte ((raw2BExpr tc env fenv be,raw2RCollExpr tc env fenv stmThen) : map raw2CollElsif listElsif) (raw2RCollExpr tc env fenv elseStm)
    where
      raw2CollElsif (ElsIf fbexpr stm) = (raw2BExpr tc env fenv fbexpr, raw2RCollExpr tc env fenv stm)

raw2RCollExpr tc env fenv (AbsRawPVSLang.RecordExpr recordElems)
  = AbsPVSLang.RRecordExpr $ map (raw2RecordElem tc env fenv) recordElems

raw2RCollExpr tc env fenv (AbsRawPVSLang.TupleExpr exprs)
  = AbsPVSLang.RTupleExpr $ map (raw2TupleExpr tc env fenv) exprs

raw2RCollExpr _ env fenv (AbsRawPVSLang.ExprId (AbsRawPVSLang.Id i))
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

raw2RCollExpr tc env fenv (AbsRawPVSLang.Call (AbsRawPVSLang.Id f) actArgs)
  = RCollFun f fp (map (raw2AExpr tc env fenv) actArgs)
    where
      fp = fromMaybe (error $ "raw2FAExpr: function " ++ show f ++ " not found.")
                    (lookup f fenv)

raw2RCollExpr _ _ _ fae = error $ "raw2RCollExpr: " ++ show fae ++ "is not of type data collection expression."

raw2AExpr :: TypeContext -> VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> AbsPVSLang.AExpr
raw2AExpr _ _ _ (AbsRawPVSLang.Int      i)   = AbsPVSLang.Int i
raw2AExpr _ _ _ (AbsRawPVSLang.Rat      d)   = AbsPVSLang.Rat (toRational d)

raw2AExpr _ env fenv (AbsRawPVSLang.ExprId (AbsRawPVSLang.Id i)) =
  case lookup i fenv of
    Just (Tuple _) -> error $ "Identifier " ++ show i ++ "is a tuple."
    Just (Record _) -> error $ "Identifier " ++ show i ++ "is a record."
    Just fp -> AbsPVSLang.EFun i ResValue fp []
    Nothing -> case lookup i env of
                  Just fp -> AbsPVSLang.Var fp i
                  Nothing -> error $ "Identifier " ++ show i ++ "not found." ++ " in env: " ++ show env

raw2AExpr _ _ _ (AbsRawPVSLang.ExprNeg (AbsRawPVSLang.Int i)) = AbsPVSLang.Int (-i)
raw2AExpr _ _ _ (AbsRawPVSLang.ExprNeg (AbsRawPVSLang.Rat d)) = AbsPVSLang.Rat (toRational (-d))
raw2AExpr tc env fenv (AbsRawPVSLang.ExprNeg fae) = AbsPVSLang.UnaryOp Op.NegOp (raw2AExpr tc env fenv fae)

raw2AExpr tc env fenv (AbsRawPVSLang.ExprAdd fae1 fae2) = raw2BinOp tc env fenv fae1 fae2 Op.AddOp
raw2AExpr tc env fenv (AbsRawPVSLang.ExprSub fae1 fae2) = raw2BinOp tc env fenv fae1 fae2 Op.SubOp
raw2AExpr tc env fenv (AbsRawPVSLang.ExprMul fae1 fae2) = raw2BinOp tc env fenv fae1 fae2 Op.MulOp
raw2AExpr tc env fenv (AbsRawPVSLang.ExprDiv fae1 fae2) = raw2BinOp tc env fenv fae1 fae2 Op.DivOp
raw2AExpr tc env fenv (AbsRawPVSLang.ExprPow fae1 fae2) = raw2BinOp tc env fenv fae1 fae2 Op.PowOp

raw2AExpr tc env fenv (AbsRawPVSLang.Call (AbsRawPVSLang.Id f) [fae])
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

raw2AExpr tc env fenv (AbsRawPVSLang.Call (AbsRawPVSLang.Id f) [AbsRawPVSLang.ExprId (AbsRawPVSLang.Id listName),idx])
  | f == "nth"  = AbsPVSLang.ListElem fp listName (raw2AExpr tc env fenv idx)
  where
    t = fromMaybe (error errorMsg)
                  (lookup listName env)
    fp = case t of
            List t' -> t'
            _       -> error errorMsg
    errorMsg = "raw2AExpr: list " ++ show listName ++ " not found."

raw2AExpr _ env _ (AbsRawPVSLang.Call (AbsRawPVSLang.Id f) [AbsRawPVSLang.ExprId (AbsRawPVSLang.Id funName)
                                                           ,AbsRawPVSLang.ExprId (AbsRawPVSLang.Id listName)])
  | f == "map"  = AbsPVSLang.RMap fp funName listName
  where
    t = fromMaybe (error errorMsg)
                  (lookup listName env)
    fp = case t of
            List t' -> t'
            _       -> error errorMsg
    errorMsg = "raw2AExpr: list " ++ show listName ++ " not found."

raw2AExpr tc env fenv (AbsRawPVSLang.Call (AbsRawPVSLang.Id f) [AbsRawPVSLang.ExprId (AbsRawPVSLang.Id funName)
                                                               ,AbsRawPVSLang.ExprId (AbsRawPVSLang.Id listName)
                                                               ,AbsRawPVSLang.Int n
                                                               ,baseCase])
  | f == "fold"  = AbsPVSLang.RFold fp funName listName n (raw2AExpr tc env fenv baseCase)
  where
    t = fromMaybe (error errorMsg)
                  (lookup listName env)
    fp = case t of
            List t' -> t'
            _       -> error errorMsg
    errorMsg = "raw2AExpr: list " ++ show listName ++ " not found."

raw2AExpr tc env fenv (AbsRawPVSLang.Call (AbsRawPVSLang.Id f) [fae1,fae2])
  | f == "add"  = raw2BinOp tc env fenv fae1 fae2 Op.AddOp
  | f == "sub"  = raw2BinOp tc env fenv fae1 fae2 Op.SubOp
  | f == "mul"  = raw2BinOp tc env fenv fae1 fae2 Op.MulOp
  | f == "div"  = raw2BinOp tc env fenv fae1 fae2 Op.DivOp
  | f == "mod"  = raw2BinOp tc env fenv fae1 fae2 Op.ModOp

raw2AExpr tc env fenv (AbsRawPVSLang.Call (AbsRawPVSLang.Id f) actArgs)
  = case lookup f fenv of
      Just fp -> AbsPVSLang.EFun f ResValue fp (map (raw2AExpr tc env fenv) actArgs)
      Nothing -> case lookup f env of
                   Just (Array _ t) -> ArrayElem t f (map (raw2AExpr tc env fenv) actArgs)
                   _ -> error $ "raw2AExpr: identifier " ++ show f ++ " not found."

raw2AExpr _ env _ (TupleIndex (Id tuple) idx) = TupleElem fp tuple idx
  where
    fp = case t of
           Tuple idxTypes -> idxTypes !! fromInteger (idx - 1)
           _ -> error $ "raw2AExpr: " ++ show t ++ "is not a tuple type."
    t = fromMaybe (error $ "raw2AExpr: tuple " ++ show tuple ++ " not found.")
                   (lookup tuple env)

raw2AExpr _ env _ (RecordField (Id record) (Id field)) = AbsPVSLang.RecordElem fp record field
  where
    fp = case t of
           Record fieldTypes -> fromMaybe (error $ "raw2AExpr: record field " ++ show field ++ " not found.")
                                          (lookup field fieldTypes)
           _ -> error $ "raw2AExpr: " ++ show t ++ "is not a record type."
    t = fromMaybe (error $ "raw2AExpr: record " ++ show record ++ " not found.")
                   (lookup record env)

raw2AExpr tc env fenv (TupleFunIndex (Id f) args idx)
  = EFun f (ResTupleIndex idx) fp (map (raw2AExpr tc env fenv) args)
  where
    fp = case t of
           Tuple idxTypes -> idxTypes !! fromInteger (idx - 1)
           _ -> error $ "raw2AExpr: " ++ show t ++ "is not a tuple type."
    t = fromMaybe (error $ "raw2AExpr: function " ++ show f ++ " not found.")
                   (lookup f fenv)

raw2AExpr tc env fenv (RecordFunField (Id f) args (Id field))
  = EFun f (ResRecordField field) fp (map (raw2AExpr tc env fenv) args)
  where
    fp = case t of
           Record fieldTypes -> fromMaybe (error $ "raw2AExpr: record field " ++ show field ++ " not found.")
                                          (lookup field fieldTypes)
           _ -> error $ "raw2AExpr: " ++ show t ++ "is not a record type."
    t = fromMaybe (error $ "raw2AExpr: function " ++ show f ++ " not found.")
                   (lookup f fenv)

raw2AExpr tc env fenv (AbsRawPVSLang.Let letElems stm)
  = RLet letList (raw2AExpr tc newenv fenv stm)
  where
    (newenv,letList) = foldl aux_fold (env,[]) letElems
    aux_fold  (accEnv,elems) letElem =  (env',elems ++ [newLetElem])
      where
        newLetElem = raw2LetElem tc accEnv fenv letElem
        env' = (letVar newLetElem, letType newLetElem):accEnv

raw2AExpr tc env fenv (AbsRawPVSLang.For idxInit idxEnd initValueAcc (Lambda _ idx _subRangeLb _subRangeUb acc accType body))
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

raw2AExpr tc env fenv (AbsRawPVSLang.If be thenSmt elseStm)  = RIte (raw2BExpr tc env fenv be) (raw2AExpr tc env fenv thenSmt) (raw2AExpr tc env fenv elseStm)

raw2AExpr tc env fenv (AbsRawPVSLang.ListIf be stmThen listElsif elseStm) =
    RListIte ((raw2BExpr tc env fenv be,raw2AExpr tc env fenv stmThen) : map (raw2Elsif tc env fenv) listElsif) (raw2AExpr tc env fenv elseStm)

raw2AExpr _ _ _ ae = error $ "Something went wrong: arithmetic expression expected but got " ++ show ae ++ "."

raw2BExprStm :: TypeContext -> VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> AbsPVSLang.BExprStm

raw2BExprStm tc env fenv (AbsRawPVSLang.Let letElems stm)
  = RBLet letList (raw2BExprStm tc newenv fenv stm)
  where
    (newenv,letList) = foldl aux_fold (env,[]) letElems
    aux_fold (accEnv,elems) letElem =  (env',elems ++ [newLetElem])
      where
        newLetElem = raw2LetElem tc accEnv fenv letElem
        env' = (letVar newLetElem, letType newLetElem):accEnv

raw2BExprStm tc env fenv (AbsRawPVSLang.If be thenSmt elseStm)
  = RBIte (raw2BExpr tc env fenv be) (raw2BExprStm tc env fenv thenSmt)
                                     (raw2BExprStm tc env fenv elseStm)

raw2BExprStm tc env fenv (AbsRawPVSLang.ListIf be stmThen listElsif elseStm) =
    RBListIte ((raw2BExpr tc env fenv be,raw2BExprStm tc env fenv stmThen) : map (raw2BElsif tc env fenv) listElsif) (raw2BExprStm tc env fenv elseStm)

raw2BExprStm tc env fenv be = RBExpr $ raw2BExpr tc env fenv be


raw2BExpr :: TypeContext -> VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> AbsPVSLang.BExpr
raw2BExpr tc env fenv (AbsRawPVSLang.Or  be1 be2) = AbsPVSLang.Or  (raw2BExpr tc env fenv be1) (raw2BExpr tc env fenv be2)
raw2BExpr tc env fenv (AbsRawPVSLang.And be1 be2) = AbsPVSLang.And (raw2BExpr tc env fenv be1) (raw2BExpr tc env fenv be2)
raw2BExpr tc env fenv (AbsRawPVSLang.Not be)      = AbsPVSLang.Not (raw2BExpr tc env fenv be)
raw2BExpr tc env fenv (AbsRawPVSLang.Eq  ae1 ae2) = AbsPVSLang.Rel Op.Eq  (raw2AExpr tc env fenv ae1) (raw2AExpr tc env fenv ae2)
raw2BExpr tc env fenv (AbsRawPVSLang.Neq ae1 ae2) = AbsPVSLang.Rel Op.Neq (raw2AExpr tc env fenv ae1) (raw2AExpr tc env fenv ae2)
raw2BExpr tc env fenv (AbsRawPVSLang.Lt  ae1 ae2) = AbsPVSLang.Rel Op.Lt  (raw2AExpr tc env fenv ae1) (raw2AExpr tc env fenv ae2)
raw2BExpr tc env fenv (AbsRawPVSLang.LtE ae1 ae2) = AbsPVSLang.Rel Op.LtE (raw2AExpr tc env fenv ae1) (raw2AExpr tc env fenv ae2)
raw2BExpr tc env fenv (AbsRawPVSLang.Gt  ae1 ae2) = AbsPVSLang.Rel Op.Gt  (raw2AExpr tc env fenv ae1) (raw2AExpr tc env fenv ae2)
raw2BExpr tc env fenv (AbsRawPVSLang.GtE ae1 ae2) = AbsPVSLang.Rel Op.GtE (raw2AExpr tc env fenv ae1) (raw2AExpr tc env fenv ae2)
raw2BExpr _ _   _     AbsRawPVSLang.BTrue        = AbsPVSLang.BTrue
raw2BExpr _ _   _     AbsRawPVSLang.BFalse       = AbsPVSLang.BFalse
raw2BExpr tc env fenv  (Call (Id f) args) =
  case lookup f fenv of
    Just Boolean -> AbsPVSLang.EPred f (map (raw2AExpr tc env fenv) args)
    Just _ -> error "raw2BExpr: Boolean function expected."
    Nothing -> error $ "raw2BExpr: something went wrong "++ show f ++ " is not a predicate."
raw2BExpr _ _ _ be = error $ "Something went wrong: boolean expression expected but got " ++ show be ++ "."


rawparserRealPVS :: String -> Err AbsRawPVSLang.Program
rawparserRealPVS = toErrM . pProgram . tokens
