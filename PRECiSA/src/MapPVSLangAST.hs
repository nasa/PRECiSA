-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module MapPVSLangAST
where

import AbsRawPVSLang
import AbsPVSLang
import Common.TypesUtils
import ErrM
import Numeric
import qualified Operators as Op
import Parser.ParRawPVSLang
import Parser.LexRawPVSLang
import Utils (fst3,snd3)
import Data.Maybe (fromMaybe)

type VarTypeEnv = [(String, PVSType)]
type FunTypeEnv = [(String, PVSType)]

isNumType :: AbsRawPVSLang.Type -> Bool
isNumType (TypeSimple (Id "int"))                  = True
isNumType (TypeSimple (Id "integer"))              = True
isNumType (TypeSimple (Id "single"))               = True
isNumType (TypeSimple (Id "unb_single"))           = True
isNumType (TypeSimple (Id "unb_pos_single"))       = True
isNumType (TypeSimple (Id "unb_nz_single"))        = True
isNumType (TypeSimple (Id "double"))               = True
isNumType (TypeSimple (Id "unb_double"))           = True
isNumType (TypeSimple (Id "unb_pos_double"))       = True
isNumType (TypeSimple (Id "unb_nz_double"))        = True
isNumType (TypeBelow _)                            = True
isNumType (ParametricTypeBi (Id "fixed_point") _ _)= True
isNumType _ = False

isIntType :: AbsRawPVSLang.Type -> Bool
isIntType (TypeSimple (Id "int"))     = True
isIntType (TypeSimple (Id "integer")) = True
isIntType (TypeBelow _)               = True
isIntType _                           = False

isRecordType :: AbsRawPVSLang.Type -> Bool
isRecordType (TypeRecord _) = True
isRecordType _ = False

isTupleType :: AbsRawPVSLang.Type -> Bool
isTupleType (TypeTuple _) = True
isTupleType _ = False

isBExpr :: AbsRawPVSLang.Expr -> Bool
isBExpr (AbsRawPVSLang.Or  _fbe1 _fbe2) = True
isBExpr (AbsRawPVSLang.And _fbe1 _fbe2) = True
isBExpr (AbsRawPVSLang.Not       _fbe) = True
isBExpr (AbsRawPVSLang.Eq  _fae1 _fae2) = True
isBExpr (AbsRawPVSLang.Neq _fae1 _fae2) = True
isBExpr (AbsRawPVSLang.Lt  _fae1 _fae2) = True
isBExpr (AbsRawPVSLang.LtE _fae1 _fae2) = True
isBExpr (AbsRawPVSLang.Gt  _fae1 _fae2) = True
isBExpr (AbsRawPVSLang.GtE _fae1 _fae2) = True
isBExpr AbsRawPVSLang.BTrue  = True
isBExpr AbsRawPVSLang.BFalse = True
isBExpr _ = False

namePVSTheory :: AbsRawPVSLang.Program -> String
namePVSTheory (Prog    (Id name) _ _) = name
namePVSTheory (ProgImp (Id name) _ _  _)   = name

raw2Prog :: AbsRawPVSLang.Program -> AbsPVSLang.Program
raw2Prog (AbsRawPVSLang.Prog     _   listDecl _) = raw2Decsl (map retTypeFun listDecl) listDecl
raw2Prog (AbsRawPVSLang.ProgImp  _ _ listDecl _) = raw2Decsl (map retTypeFun listDecl) listDecl

retTypeFun :: AbsRawPVSLang.Decl -> (String, PVSType)
retTypeFun (Decl0 (AbsRawPVSLang.Id f)   fp _) = (f, raw2FPType fp)
retTypeFun (DeclN (AbsRawPVSLang.Id f) _ fp _) = (f, raw2FPType fp)

rationalizeFP :: Show a => a -> Rational
rationalizeFP d = (fst . head $ readSigned readFloat $ show d) :: Rational

raw2Decsl :: FunTypeEnv -> [AbsRawPVSLang.Decl] -> [AbsPVSLang.Decl]
raw2Decsl fenv = map (raw2Decl fenv)

raw2Decl :: FunTypeEnv -> AbsRawPVSLang.Decl -> AbsPVSLang.Decl
raw2Decl fenv (Decl0 f (TypeSimple (Id "bool")) be)
  = Pred False Original (raw2Id f) [] (raw2FBExprStm [] fenv be)

raw2Decl fenv (DeclN f rawArgs (TypeSimple (Id "bool")) be)
  = Pred False Original (raw2Id f) args (raw2FBExprStm env fenv be)
  where
    args = raw2Args rawArgs
    env  = map mapArg2Pair args

raw2Decl fenv (Decl0 f fptype@(TypeArray _ _) stm)
  = CollDecl False (raw2FPType fptype) (raw2Id f) [] (raw2CollExpr [] fenv stm)

raw2Decl fenv (DeclN f rawArgs fptype@(TypeArray _ _) stm)
  = CollDecl False (raw2FPType fptype) (raw2Id f) args (raw2CollExpr env fenv stm)
  where
    args = raw2Args rawArgs
    env  = map mapArg2Pair args

raw2Decl fenv (Decl0 f fptype@(TypeRecord _) stm)
  = CollDecl False (raw2FPType fptype) (raw2Id f) [] (raw2CollExpr [] fenv stm)

raw2Decl fenv (DeclN f rawArgs fptype@(TypeRecord _) stm)
  = CollDecl False (raw2FPType fptype) (raw2Id f) args (raw2CollExpr env fenv stm)
  where
    args = raw2Args rawArgs
    env  = map mapArg2Pair args

raw2Decl fenv (Decl0 f fptype@(TypeTuple _) stm)
  = CollDecl False (raw2FPType fptype) (raw2Id f) [] (raw2CollExpr [] fenv stm)

raw2Decl fenv (DeclN f rawArgs fptype@(TypeTuple _) stm)
  = CollDecl False (raw2FPType fptype) (raw2Id f) args (raw2CollExpr env fenv stm)
  where
    args = raw2Args rawArgs
    env  = map mapArg2Pair args

raw2Decl fenv (Decl0 f fptype stm)
  = Decl False (raw2FPType fptype) (raw2Id f) [] (raw2FAExpr [] fenv stm)

raw2Decl fenv (DeclN f rawArgs fptype stm)
  = Decl False (raw2FPType fptype) (raw2Id f) args (raw2FAExpr env fenv stm)
  where
    args = raw2Args rawArgs
    env  = map mapArg2Pair args

raw2Elsif :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.ElsIf -> (AbsPVSLang.FBExpr, AbsPVSLang.FAExpr)
raw2Elsif env fenv (ElsIf fbexpr stm) = (raw2FBExpr env fenv fbexpr, raw2FAExpr env fenv stm)

raw2BinOp :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> AbsRawPVSLang.Expr -> Op.BinOp -> AbsPVSLang.FAExpr
raw2BinOp env fenv fae1 fae2 op = AbsPVSLang.BinaryFPOp op fp ae1 ae2
  where
    ae1 = raw2FAExpr env fenv fae1
    ae2 = raw2FAExpr env fenv fae2
    fp  = lubPVSType (getPVSType ae1) (getPVSType ae2)

realToFP :: PVSType -> AbsRawPVSLang.Expr -> AbsPVSLang.FAExpr
realToFP _  (AbsRawPVSLang.Int i) = AbsPVSLang.FInt i
realToFP fp (AbsRawPVSLang.Rat d) = AbsPVSLang.ToFloat fp $ AbsPVSLang.Rat $ rationalizeFP d
realToFP _ fae = error $ "realToFP: " ++ show fae ++ "is not of type rational."

intToFP :: PVSType -> AbsRawPVSLang.Expr -> AbsPVSLang.FAExpr
intToFP _ (AbsRawPVSLang.Int i) = AbsPVSLang.FInt i
intToFP _ fae = error $ "intToFP: " ++ show fae ++ "is not of type int."

raw2RecordElem :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.RecordElem
                  -> (RecordField, Either AbsPVSLang.FAExpr AbsPVSLang.FBExpr)
raw2RecordElem env fenv (AbsRawPVSLang.RecordElem (AbsRawPVSLang.Id field) expr)
  | isBExpr expr = (field, Right $ raw2FBExpr env fenv expr)
  | otherwise    = (field, Left  $ raw2FAExpr env fenv expr)

-- raw2RecordExpr :: VarTypeEnv -> FunTypeEnv -> (RecordField, AbsRawPVSLang.Expr)
--                   -> (RecordField, Either AbsPVSLang.FAExpr AbsPVSLang.FBExpr)
-- raw2RecordExpr env fenv (fieldName, expr) | isBExpr expr = (fieldName, Right $ raw2FBExpr env fenv expr)
--                                           | otherwise    = (fieldName, Left  $ raw2FAExpr env fenv expr)

raw2TupleExpr :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> Either AbsPVSLang.FAExpr AbsPVSLang.FBExpr
raw2TupleExpr env fenv expr | isBExpr expr = Right $ raw2FBExpr env fenv expr
                            | otherwise    = Left  $ raw2FAExpr env fenv expr

raw2CollExpr :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> AbsPVSLang.CollFAExpr
raw2CollExpr env fenv (AbsRawPVSLang.With array idx newValue)
  = ArrayUpdate (raw2CollExpr env fenv array) (raw2FAExpr env fenv idx) (raw2FAExpr env fenv newValue)

raw2CollExpr env fenv (AbsRawPVSLang.Let letElems stm)
  = CLet letList (raw2CollExpr newenv fenv stm)
  where
    (newenv,letList) = foldl aux_fold (env,[]) letElems
    aux_fold (accEnv,elems) letElem = (env',elems ++ [newLetElem])
      where
        newLetElem = raw2LetElem accEnv fenv letElem
        env' = (fst3 newLetElem, snd3 newLetElem):accEnv

raw2CollExpr env fenv (AbsRawPVSLang.If be thenSmt elseStm)
  = CIte (raw2FBExpr env fenv be) (raw2CollExpr env fenv thenSmt)
                                  (raw2CollExpr env fenv elseStm)

raw2CollExpr env fenv (AbsRawPVSLang.ListIf be stmThen listElsif elseStm)
  = CListIte ((raw2FBExpr env fenv be,raw2CollExpr env fenv stmThen) : map raw2CollElsif listElsif) (raw2CollExpr env fenv elseStm)
    where
      raw2CollElsif (ElsIf fbexpr stm) = (raw2FBExpr env fenv fbexpr, raw2CollExpr env fenv stm)

raw2CollExpr env fenv (AbsRawPVSLang.RecordExpr recordExprs)
  = AbsPVSLang.RecordExpr $ map (raw2RecordElem env fenv) recordExprs

raw2CollExpr env fenv (AbsRawPVSLang.TupleExpr exprs)
  = AbsPVSLang.TupleExpr $ map (raw2TupleExpr env fenv) exprs

raw2CollExpr env fenv (AbsRawPVSLang.ExprId (AbsRawPVSLang.Id i))
  = case lookup i fenv of
      Just t@(Tuple _)  -> CollFun False i t []
      Just t@(Record _) -> CollFun False i t []
      Just t@(Array _ _) -> CollFun False i t []
      Just _  -> error $ "Identifier " ++ show i ++ "is not of data collection type."
      Nothing -> case lookup i env of
                    Just t@(Tuple  _) -> AbsPVSLang.CollVar t i
                    Just t@(Record _) -> AbsPVSLang.CollVar t i
                    Just t@(Array _ _) -> AbsPVSLang.CollVar t i
                    Just _  -> error $ "Identifier " ++ show i ++ "is not of data collection type."
                    Nothing -> error $ "Identifier " ++ show i ++ "not found." ++ " in env: " ++ show env

raw2CollExpr env fenv (AbsRawPVSLang.Call (AbsRawPVSLang.Id f) actArgs)
  = CollFun False f fp (map (raw2FAExpr env fenv) actArgs)
    where
      fp = fromMaybe (error $ "raw2FAExpr: function " ++ show f ++ " not found.")
                    (lookup f fenv)

raw2CollExpr _ _ fae = error $ "raw2CollExpr: " ++ show fae ++ "is not of type data collection expression."

raw2FBExprStm :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> AbsPVSLang.FBExprStm
raw2FBExprStm env fenv (AbsRawPVSLang.Let letElems stm)
  = BLet letList (raw2FBExprStm newenv fenv stm)
  where
    (newenv,letList) = foldl aux_fold (env,[]) letElems
    aux_fold (accEnv,elems) letElem = (env',elems ++ [newLetElem])
      where
        newLetElem = raw2LetElem accEnv fenv letElem
        env' = (fst3 newLetElem, snd3 newLetElem):accEnv

raw2FBExprStm env fenv (AbsRawPVSLang.If be thenSmt elseStm)
  = BIte (raw2FBExpr env fenv be) (raw2FBExprStm env fenv thenSmt)
                                  (raw2FBExprStm env fenv elseStm)

raw2FBExprStm env fenv (AbsRawPVSLang.ListIf be stmThen listElsif elseStm)
  = BListIte ((raw2FBExpr env fenv be,raw2FBExprStm env fenv stmThen) : map (raw2BElsif env fenv) listElsif) (raw2FBExprStm env fenv elseStm)

raw2FBExprStm env fenv be = BExpr $ raw2FBExpr env fenv be

raw2BElsif :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.ElsIf -> (AbsPVSLang.FBExpr, AbsPVSLang.FBExprStm)
raw2BElsif env fenv (ElsIf fbexpr stm) = (raw2FBExpr env fenv fbexpr, raw2FBExprStm env fenv stm)

raw2FBExpr :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> AbsPVSLang.FBExpr
raw2FBExpr env fenv (AbsRawPVSLang.Or  fbe1 fbe2) = AbsPVSLang.FOr  (raw2FBExpr env fenv fbe1) (raw2FBExpr env fenv fbe2)
raw2FBExpr env fenv (AbsRawPVSLang.And fbe1 fbe2) = AbsPVSLang.FAnd (raw2FBExpr env fenv fbe1) (raw2FBExpr env fenv fbe2)
raw2FBExpr env fenv (AbsRawPVSLang.Not       fbe) = AbsPVSLang.FNot (raw2FBExpr env fenv fbe)
raw2FBExpr env fenv (AbsRawPVSLang.Eq  fae1 fae2) = AbsPVSLang.FRel Op.Eq  (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FBExpr env fenv (AbsRawPVSLang.Neq fae1 fae2) = AbsPVSLang.FRel Op.Neq (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FBExpr env fenv (AbsRawPVSLang.Lt  fae1 fae2) = AbsPVSLang.FRel Op.Lt  (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FBExpr env fenv (AbsRawPVSLang.LtE fae1 fae2) = AbsPVSLang.FRel Op.LtE (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FBExpr env fenv (AbsRawPVSLang.Gt  fae1 fae2) = AbsPVSLang.FRel Op.Gt  (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FBExpr env fenv (AbsRawPVSLang.GtE fae1 fae2) = AbsPVSLang.FRel Op.GtE (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FBExpr env fenv (AbsRawPVSLang.Call (AbsRawPVSLang.Id f) actArgs) = AbsPVSLang.FEPred False Original f (map (raw2FAExpr env fenv) actArgs)
raw2FBExpr _ _  AbsRawPVSLang.BTrue  = AbsPVSLang.FBTrue
raw2FBExpr _ _  AbsRawPVSLang.BFalse = AbsPVSLang.FBFalse
raw2FBExpr _ _  expr = error $ "raw2FBExpr: Boolean expression expected but got " ++ show expr ++ "."

raw2FAExpr :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> AbsPVSLang.FAExpr
raw2FAExpr _ _ (AbsRawPVSLang.Int n) = AbsPVSLang.FInt n

raw2FAExpr _ _ (AbsRawPVSLang.Rat d) = AbsPVSLang.ToFloat FPDouble $ AbsPVSLang.Rat $ rationalizeFP d

raw2FAExpr env fenv (AbsRawPVSLang.ExprId (AbsRawPVSLang.Id i)) =
  case lookup i fenv of
    Just (Tuple _) -> error $ "Identifier " ++ show i ++ "is a tuple."
    Just (Record _) -> error $ "Identifier " ++ show i ++ "is a record."
    Just fp -> AbsPVSLang.FEFun False i ResValue fp []
    Nothing -> case lookup i env of
                  Just fp -> AbsPVSLang.FVar fp i
                  Nothing -> error $ "Identifier " ++ show i ++ "not found." ++ " in env: " ++ show env

raw2FAExpr _ _ (AbsRawPVSLang.ExprNeg (AbsRawPVSLang.Int i)) = AbsPVSLang.FInt (-i)
raw2FAExpr _ _ (AbsRawPVSLang.ExprNeg (AbsRawPVSLang.Rat d)) = AbsPVSLang.ToFloat FPDouble
                                                               $ AbsPVSLang.Rat (rationalizeFP (-d))

raw2FAExpr _ _ (AbsRawPVSLang.ExprNeg (AbsRawPVSLang.Call (AbsRawPVSLang.Id "ItoD") [AbsRawPVSLang.Int i]))
  = AbsPVSLang.ToFloat FPDouble $ AbsPVSLang.Int (-i)
raw2FAExpr _ _ (AbsRawPVSLang.ExprNeg (AbsRawPVSLang.Call (AbsRawPVSLang.Id "ItoS") [AbsRawPVSLang.Int i]))
  = AbsPVSLang.ToFloat FPSingle $ AbsPVSLang.Int (-i)
raw2FAExpr _ _ (AbsRawPVSLang.ExprNeg (AbsRawPVSLang.Call (AbsRawPVSLang.Id "RtoD") [AbsRawPVSLang.Rat d]))
  = AbsPVSLang.ToFloat FPDouble $ AbsPVSLang.Rat (rationalizeFP (-d))
raw2FAExpr _ _ (AbsRawPVSLang.ExprNeg (AbsRawPVSLang.Call (AbsRawPVSLang.Id "RtoS") [AbsRawPVSLang.Rat d]))
  = AbsPVSLang.ToFloat FPSingle $ AbsPVSLang.Rat (rationalizeFP (-d))

raw2FAExpr env fenv (AbsRawPVSLang.ExprNeg fae) = AbsPVSLang.UnaryFPOp Op.NegOp fp (raw2FAExpr env fenv fae)
  where
    fp = getPVSType $ raw2FAExpr env fenv fae

raw2FAExpr env fenv (AbsRawPVSLang.ExprAdd fae1 fae2) = raw2BinOp env fenv fae1 fae2 Op.AddOp

raw2FAExpr env fenv (AbsRawPVSLang.ExprSub fae1 fae2) = raw2BinOp env fenv fae1 fae2 Op.SubOp

raw2FAExpr env fenv (AbsRawPVSLang.ExprMul fae1 fae2) = raw2BinOp env fenv fae1 fae2 Op.MulOp

raw2FAExpr env fenv (AbsRawPVSLang.ExprDiv fae1 fae2) = raw2BinOp env fenv fae1 fae2 Op.DivOp

raw2FAExpr env fenv (AbsRawPVSLang.ExprPow fae1 fae2) = raw2BinOp env fenv fae1 fae2 Op.PowOp

raw2FAExpr env fenv (AbsRawPVSLang.Call (AbsRawPVSLang.Id f) [fae])
  | f == "floor"  = AbsPVSLang.UnaryFPOp Op.FloorOp fp       (raw2FAExpr env fenv fae)
  | f == "Sfloor" = AbsPVSLang.UnaryFPOp Op.FloorOp FPSingle (raw2FAExpr env fenv fae)
  | f == "Dfloor" = AbsPVSLang.UnaryFPOp Op.FloorOp FPDouble (raw2FAExpr env fenv fae)
  | f == "abs"    = AbsPVSLang.UnaryFPOp Op.AbsOp   fp       (raw2FAExpr env fenv fae)
  | f == "Sabs"   = AbsPVSLang.UnaryFPOp Op.AbsOp   FPSingle (raw2FAExpr env fenv fae)
  | f == "Dabs"   = AbsPVSLang.UnaryFPOp Op.AbsOp   FPDouble (raw2FAExpr env fenv fae)
  | f == "sqrt"   = AbsPVSLang.UnaryFPOp Op.SqrtOp  fp       (raw2FAExpr env fenv fae)
  | f == "Ssqrt"  = AbsPVSLang.UnaryFPOp Op.SqrtOp  FPSingle (raw2FAExpr env fenv fae)
  | f == "Dsqrt"  = AbsPVSLang.UnaryFPOp Op.SqrtOp  FPDouble (raw2FAExpr env fenv fae)
  | f == "sin"    = AbsPVSLang.UnaryFPOp Op.SinOp   fp       (raw2FAExpr env fenv fae)
  | f == "Ssin"   = AbsPVSLang.UnaryFPOp Op.SinOp   FPSingle (raw2FAExpr env fenv fae)
  | f == "Dsin"   = AbsPVSLang.UnaryFPOp Op.SinOp   FPDouble (raw2FAExpr env fenv fae)
  | f == "cos"    = AbsPVSLang.UnaryFPOp Op.CosOp   fp       (raw2FAExpr env fenv fae)
  | f == "Scos"   = AbsPVSLang.UnaryFPOp Op.CosOp   FPSingle (raw2FAExpr env fenv fae)
  | f == "Dcos"   = AbsPVSLang.UnaryFPOp Op.CosOp   FPDouble (raw2FAExpr env fenv fae)
  | f == "tan"    = AbsPVSLang.UnaryFPOp Op.TanOp   fp       (raw2FAExpr env fenv fae)
  | f == "Stan"   = AbsPVSLang.UnaryFPOp Op.TanOp   FPSingle (raw2FAExpr env fenv fae)
  | f == "Dtan"   = AbsPVSLang.UnaryFPOp Op.TanOp   FPDouble (raw2FAExpr env fenv fae)
  | f == "asin"   = AbsPVSLang.UnaryFPOp Op.AsinOp  fp       (raw2FAExpr env fenv fae)
  | f == "Sasin"  = AbsPVSLang.UnaryFPOp Op.AsinOp  FPSingle (raw2FAExpr env fenv fae)
  | f == "Dasin"  = AbsPVSLang.UnaryFPOp Op.AsinOp  FPDouble (raw2FAExpr env fenv fae)
  | f == "acos"   = AbsPVSLang.UnaryFPOp Op.AcosOp  fp       (raw2FAExpr env fenv fae)
  | f == "Sacos"  = AbsPVSLang.UnaryFPOp Op.AcosOp  FPSingle (raw2FAExpr env fenv fae)
  | f == "Dacos"  = AbsPVSLang.UnaryFPOp Op.AcosOp  FPDouble (raw2FAExpr env fenv fae)
  | f == "atan"   = AbsPVSLang.UnaryFPOp Op.AtanOp  fp       (raw2FAExpr env fenv fae)
  | f == "Satan"  = AbsPVSLang.UnaryFPOp Op.AtanOp  FPSingle (raw2FAExpr env fenv fae)
  | f == "Datan"  = AbsPVSLang.UnaryFPOp Op.AtanOp  FPDouble (raw2FAExpr env fenv fae)
  | f == "ln"     = AbsPVSLang.UnaryFPOp Op.LnOp    fp       (raw2FAExpr env fenv fae)
  | f == "Sln"    = AbsPVSLang.UnaryFPOp Op.LnOp    FPSingle (raw2FAExpr env fenv fae)
  | f == "Dln"    = AbsPVSLang.UnaryFPOp Op.LnOp    FPDouble (raw2FAExpr env fenv fae)
  | f == "exp"    = AbsPVSLang.UnaryFPOp Op.ExpoOp  fp       (raw2FAExpr env fenv fae)
  | f == "Sexp"   = AbsPVSLang.UnaryFPOp Op.ExpoOp  FPSingle (raw2FAExpr env fenv fae)
  | f == "Dexp"   = AbsPVSLang.UnaryFPOp Op.ExpoOp  FPDouble (raw2FAExpr env fenv fae)
  | f == "Ineg"   = AbsPVSLang.UnaryFPOp Op.NegOp   TInt (raw2FAExpr env fenv fae)
  | f == "Sneg"   = AbsPVSLang.UnaryFPOp Op.NegOp   FPSingle (raw2FAExpr env fenv fae)
  | f == "Dneg"   = AbsPVSLang.UnaryFPOp Op.NegOp   FPDouble (raw2FAExpr env fenv fae)
  | f == "RtoS"   = realToFP FPSingle fae
  | f == "RtoD"   = realToFP FPDouble fae
  | f == "ItoS"   = intToFP  FPSingle fae
  | f == "ItoD"   = intToFP  FPDouble fae
  where
    fp = getPVSType $ raw2FAExpr env fenv fae

raw2FAExpr env fenv (AbsRawPVSLang.Call (AbsRawPVSLang.Id f) [AbsRawPVSLang.ExprId (AbsRawPVSLang.Id listName),idx])
  | f == "nth"  = AbsPVSLang.FListElem fp listName (raw2FAExpr env fenv idx)
  where
    t = fromMaybe (error errorMsg)
                  (lookup listName env)
    fp = case t of
            List t' -> t'
            _       -> error errorMsg
    errorMsg = "raw2FAExpr: list " ++ show listName ++ " not found."

raw2FAExpr env _fenv (AbsRawPVSLang.Call (AbsRawPVSLang.Id f) [AbsRawPVSLang.ExprId (AbsRawPVSLang.Id funName)
                                                             ,AbsRawPVSLang.ExprId (AbsRawPVSLang.Id listName)])
  | f == "map"  = AbsPVSLang.FMap fp funName listName
  where
    t = fromMaybe (error errorMsg)
                  (lookup listName env)
    fp = case t of
            List t' -> t'
            _       -> error errorMsg
    errorMsg = "raw2FAExpr: list " ++ show listName ++ " not found."

raw2FAExpr env fenv (AbsRawPVSLang.Call (AbsRawPVSLang.Id f) [AbsRawPVSLang.ExprId (AbsRawPVSLang.Id funName)
                                                             ,AbsRawPVSLang.ExprId (AbsRawPVSLang.Id listName)
                                                             ,AbsRawPVSLang.Int n
                                                             ,baseCase])
  | f == "fold"  = AbsPVSLang.FFold fp funName listName n (raw2FAExpr env fenv baseCase)
  where
    t = fromMaybe (error errorMsg)
                  (lookup listName env)
    fp = case t of
            List t' -> t'
            _       -> error errorMsg
    errorMsg = "raw2FAExpr: list " ++ show listName ++ " not found."

raw2FAExpr env fenv (AbsRawPVSLang.Call (AbsRawPVSLang.Id f) [fae1,fae2])
  | f == "add"  = raw2BinOp env fenv fae1 fae2 Op.AddOp
  | f == "Iadd" = AbsPVSLang.BinaryFPOp Op.AddOp TInt     ae1 ae2
  | f == "Sadd" = AbsPVSLang.BinaryFPOp Op.AddOp FPSingle ae1 ae2
  | f == "Dadd" = AbsPVSLang.BinaryFPOp Op.AddOp FPDouble ae1 ae2
  | f == "sub"  = raw2BinOp env fenv fae1 fae2 Op.SubOp
  | f == "Isub" = AbsPVSLang.BinaryFPOp Op.SubOp TInt     ae1 ae2
  | f == "Ssub" = AbsPVSLang.BinaryFPOp Op.SubOp FPSingle ae1 ae2
  | f == "Dsub" = AbsPVSLang.BinaryFPOp Op.SubOp FPDouble ae1 ae2
  | f == "mul"  = raw2BinOp env fenv fae1 fae2 Op.MulOp
  | f == "Imul" = AbsPVSLang.BinaryFPOp Op.MulOp TInt     ae1 ae2
  | f == "Smul" = AbsPVSLang.BinaryFPOp Op.MulOp FPSingle ae1 ae2
  | f == "Dmul" = AbsPVSLang.BinaryFPOp Op.MulOp FPDouble ae1 ae2
  | f == "div"  = raw2BinOp env fenv fae1 fae2 Op.DivOp
  | f == "Idiv" = AbsPVSLang.BinaryFPOp Op.DivOp TInt     ae1 ae2
  | f == "Sdiv" = AbsPVSLang.BinaryFPOp Op.DivOp FPSingle ae1 ae2
  | f == "Ddiv" = AbsPVSLang.BinaryFPOp Op.DivOp FPDouble ae1 ae2
  | f == "mod"  = raw2BinOp env fenv fae1 fae2 Op.ModOp
  | f == "Imod" = AbsPVSLang.BinaryFPOp Op.ModOp TInt     ae1 ae2
  | f == "Smod" = AbsPVSLang.BinaryFPOp Op.ModOp FPSingle ae1 ae2
  | f == "Dmod" = AbsPVSLang.BinaryFPOp Op.ModOp FPDouble ae1 ae2
  where
    ae1 = raw2FAExpr env fenv fae1
    ae2 = raw2FAExpr env fenv fae2

raw2FAExpr env fenv (AbsRawPVSLang.Call (AbsRawPVSLang.Id f) actArgs)
  = case lookup f fenv of
      Just fp -> AbsPVSLang.FEFun False f ResValue fp (map (raw2FAExpr env fenv) actArgs)
      Nothing -> case lookup f env of
                   Just (Array _argsType t) -> FArrayElem t f (map (raw2FAExpr env fenv) actArgs)
                   _ -> error $ "raw2FAExpr: identifier " ++ show f ++ " not found."

raw2FAExpr env fenv (AbsRawPVSLang.Let letElems stm)
  = AbsPVSLang.Let letList (raw2FAExpr newenv fenv stm)
  where
    (newenv,letList) = foldl aux_fold (env,[]) letElems
    aux_fold  (accEnv,elems) letElem = (env',elems ++ [newLetElem])
      where
        newLetElem = raw2LetElem accEnv fenv letElem
        env' = (fst3 newLetElem, snd3 newLetElem):accEnv

raw2FAExpr env fenv (If bexpr stmThen stmElse) =
    Ite (raw2FBExpr env fenv bexpr) (raw2FAExpr env fenv stmThen) (raw2FAExpr env fenv stmElse)

raw2FAExpr env fenv (ListIf bexpr stmThen listElsif stmElse) =
    ListIte ((raw2FBExpr env fenv bexpr,raw2FAExpr env fenv stmThen): map (raw2Elsif env fenv) listElsif) (raw2FAExpr env fenv stmElse)

raw2FAExpr env _fenv (TupleIndex (Id tuple) idx) = FTupleElem fp tuple idx
  where
    fp = case t of
           Tuple idxTypes -> idxTypes !! fromInteger (idx - 1)
           _ -> error $ "raw2FAExpr: " ++ show t ++ "is not a tuple type."
    t = fromMaybe (error $ "raw2FAExpr: tuple " ++ show tuple ++ " not found.")
                   (lookup tuple env)

raw2FAExpr env _fenv (RecordField (Id record) (Id field)) = FRecordElem fp record field
  where
    fp = case t of
           Record fieldTypes -> fromMaybe (error $ "raw2FAExpr: record field " ++ show field ++ " not found.")
                                          (lookup field fieldTypes)
           _ -> error $ "raw2FAExpr: " ++ show t ++ "is not a record type."
    t = fromMaybe (error $ "raw2FAExpr: record " ++ show record ++ " not found.")
                   (lookup record env)

raw2FAExpr env fenv (TupleFunIndex (Id f) args idx)
  = FEFun False f (ResTupleIndex idx) fp (map (raw2FAExpr env fenv) args)
  where
    fp = case t of
           Tuple idxTypes -> idxTypes !! fromInteger (idx - 1)
           _ -> error $ "raw2FAExpr: " ++ show t ++ "is not a tuple type."
    t = fromMaybe (error $ "raw2FAExpr: function " ++ show f ++ " not found.")
                   (lookup f fenv)

raw2FAExpr env fenv (RecordFunField (Id f) args (Id field))
  = FEFun False f (ResRecordField field) fp (map (raw2FAExpr env fenv) args)
  where
    fp = case t of
           Record fieldTypes -> fromMaybe (error $ "raw2FAExpr: record field " ++ show field ++ " not found.")
                                          (lookup field fieldTypes)
           _ -> error $ "raw2FAExpr: " ++ show t ++ "is not a record type."
    t = fromMaybe (error $ "raw2FAExpr: function " ++ show f ++ " not found.")
                   (lookup f fenv)

raw2FAExpr _ _ For{} = undefined

raw2FAExpr _ _ ae = error $ "raw2FAExpr: artihmetic expression expected but got " ++ show ae ++ "."

raw2LetElem :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.LetElem -> AbsPVSLang.FLetElem
raw2LetElem env fenv (AbsRawPVSLang.LetElem x   ae) = (raw2Id x, FPDouble, raw2FAExpr env fenv ae)
raw2LetElem env fenv (LetElemType           x t ae) = (raw2Id x, raw2FPType t,      raw2FAExpr env fenv ae)

raw2Args :: AbsRawPVSLang.Args -> [AbsPVSLang.Arg]
raw2Args (FArgs args) = concatMap raw2Arg args
raw2Args (FArgsNoType _) = error "Arguments have no type."

raw2Arg :: AbsRawPVSLang.Arg -> [AbsPVSLang.Arg]
raw2Arg (FArg xs t)         = map (raw2ArgWithType t) xs
raw2Arg (FArgGuard xs t _)  = map (raw2ArgWithType t) xs

raw2ArgWithType :: AbsRawPVSLang.Type -> AbsRawPVSLang.Id -> AbsPVSLang.Arg
raw2ArgWithType t x = AbsPVSLang.Arg (raw2Id x) (raw2FPType t)

raw2Id :: AbsRawPVSLang.Id -> VarName
raw2Id (AbsRawPVSLang.Id x) = x

raw2FieldDecls :: AbsRawPVSLang.FieldDecls -> (RecordField,PVSType)
raw2FieldDecls (FieldDecls (Id field) t) = (field, raw2FPType t)

raw2FPType :: AbsRawPVSLang.Type -> PVSType
raw2FPType (TypeSimple (Id "int"))            = TInt
raw2FPType (TypeSimple (Id "integer"))        = TInt
raw2FPType (TypeSimple (Id "single"))         = FPSingle
raw2FPType (TypeSimple (Id "unb_single"))     = FPSingle
raw2FPType (TypeSimple (Id "unb_pos_single")) = FPDouble
raw2FPType (TypeSimple (Id "unb_nz_single"))  = FPDouble
raw2FPType (TypeSimple (Id "double"))         = FPDouble
raw2FPType (TypeSimple (Id "unb_double"))     = FPDouble
raw2FPType (TypeSimple (Id "unb_pos_double")) = FPDouble
raw2FPType (TypeSimple (Id "unb_nz_double"))  = FPDouble
raw2FPType (TypeSimple (Id "bool"))           = Boolean
raw2FPType (TypeSimple (Id "real"))           = Real
raw2FPType (TypeSimple (Id "nnreal"))         = Real
raw2FPType (ParametricTypeBi _t _a _b) = error "Fixed-point numbers not supported yet."
raw2FPType (TypeBelow e) = Below (raw2FAExpr [] [] e)
raw2FPType (TypeRecord fieldDecls) = Record (map raw2FieldDecls fieldDecls)
raw2FPType (TypeTuple ts) = Tuple (map raw2FPType ts)
raw2FPType (TypeArray ts t) | all isIntType ts && isNumType t
  = Array (map raw2FPType ts) (raw2FPType t)
raw2FPType (TypeArray _ _) = error "This generic type of array is not supported yet."
raw2FPType (AbsRawPVSLang.TypeFun typeList retType)  = AbsPVSLang.TypeFun (map raw2FPType typeList) (raw2FPType retType)
raw2FPType (TypeFun2 typeList retType) = AbsPVSLang.TypeFun (map raw2FPType typeList) (raw2FPType retType)
raw2FPType (TypeList t) = List (raw2FPType t)
raw2FPType t = error $ "raw2FPType: type " ++ show t ++ " not recognized."

toErrM :: Either String a -> Err a
toErrM (Left e) = Bad e
toErrM (Right a) = Ok a

rawparserPVS :: String -> Err AbsRawPVSLang.Program
rawparserPVS = toErrM . pProgram . tokens
