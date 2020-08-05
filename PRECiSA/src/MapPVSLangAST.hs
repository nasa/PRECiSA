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
import Data.Maybe(fromMaybe)
import ErrM
import PVSTypes
import qualified Operators as Op
import Parser.ParRawPVSLang
import Parser.LexRawPVSLang
import Utils (fst3,snd3)

type VarTypeEnv = [(String, PVSType)]
type FunTypeEnv = [(String, PVSType)]

namePVSTheory :: AbsRawPVSLang.Program -> String
namePVSTheory (Prog    (Id name) _ _ _ _) = name
namePVSTheory (ProgImp (Id name) _ _ _)   = name

raw2Prog :: AbsRawPVSLang.Program -> AbsPVSLang.Program
raw2Prog (AbsRawPVSLang.Prog    _ _ _ listDecl _) = raw2Decsl (map retTypeFun listDecl) listDecl
raw2Prog (AbsRawPVSLang.ProgImp _ _ listDecl _)   = raw2Decsl (map retTypeFun listDecl) listDecl

retTypeFun :: AbsRawPVSLang.Decl -> (String, PVSType)
retTypeFun (Decl0 (AbsRawPVSLang.Id f)   fp _) = (f, raw2FPType fp)
retTypeFun (DeclN (AbsRawPVSLang.Id f) _ fp _) = (f, raw2FPType fp)

raw2Decsl :: FunTypeEnv -> [AbsRawPVSLang.Decl] -> [AbsPVSLang.Decl]
raw2Decsl fenv = map (raw2Decl fenv)

raw2Decl :: FunTypeEnv -> AbsRawPVSLang.Decl -> AbsPVSLang.Decl
raw2Decl fenv (DeclN f rawArgs FPtype_bool be) = Pred False Original (raw2Id f) args (raw2FBExprStm env fenv be)
  where
    args = raw2Args rawArgs
    env  = map mapArg2Pair args
raw2Decl fenv (Decl0 f FPtype_bool be)     = Pred False Original (raw2Id f) [] (raw2FBExprStm [] fenv be)
raw2Decl fenv (DeclN f rawArgs fptype stm) = Decl False (raw2FPType fptype) (raw2Id f) args (raw2FAExpr env fenv stm)
  where
    args = raw2Args rawArgs
    env  = map mapArg2Pair args
raw2Decl fenv (Decl0 f      fptype stm) = Decl False (raw2FPType fptype) (raw2Id f) [] (raw2FAExpr [] fenv stm)

raw2Elsif :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.ElsIf -> (AbsPVSLang.FBExpr, AbsPVSLang.FAExpr)
raw2Elsif env fenv (ElsIf fbexpr stm) = (raw2FBExpr env fenv fbexpr, raw2FAExpr env fenv stm)

raw2BinOp :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> AbsRawPVSLang.Expr -> Op.BinOp -> AbsPVSLang.FAExpr
raw2BinOp env fenv fae1 fae2 op = AbsPVSLang.BinaryFPOp op fp ae1 ae2 
  where
    ae1 = raw2FAExpr env fenv fae1
    ae2 = raw2FAExpr env fenv fae2
    fp  = lubPVSType (getPVSType ae1) (getPVSType ae2)

raw2FBExprStm :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> AbsPVSLang.FBExprStm 
raw2FBExprStm env fenv (AbsRawPVSLang.Let letElems stm)
  = BLet letList (raw2FBExprStm newenv fenv stm)
  where
    (newenv,letList) = foldr aux_fold (env,[]) letElems
    aux_fold letElem (accEnv,elems) = (env',elems ++ [newLetElem])
      where
        newLetElem = raw2LetElem accEnv fenv letElem
        env' = (fst3 newLetElem, snd3 newLetElem):accEnv

raw2FBExprStm env fenv (AbsRawPVSLang.If be thenSmt elseStm)
  = BIte (raw2FBExpr env fenv be) (raw2FBExprStm env fenv thenSmt)
                                  (raw2FBExprStm env fenv elseStm)

raw2FBExprStm env fenv (AbsRawPVSLang.ListIf be stmThen listElsif elseStm)
  = BListIte ((raw2FBExpr env fenv be,raw2FBExprStm env fenv stmThen) : map (raw2BElsif env fenv) listElsif) (raw2FBExprStm env fenv elseStm)

raw2FBExprStm _ _ be = error $ "raw2FBExprStm: Boolean expression expected but got " ++ show be ++ "."

raw2BElsif :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.ElsIf -> (AbsPVSLang.FBExpr, AbsPVSLang.FBExprStm)
raw2BElsif env fenv (ElsIf fbexpr stm) = (raw2FBExpr env fenv fbexpr, raw2FBExprStm env fenv stm)

raw2FBExpr :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> AbsPVSLang.FBExpr 
raw2FBExpr env fenv (AbsRawPVSLang.FOr  fbe1 fbe2) = AbsPVSLang.FOr  (raw2FBExpr env fenv fbe1) (raw2FBExpr env fenv fbe2)
raw2FBExpr env fenv (AbsRawPVSLang.FAnd fbe1 fbe2) = AbsPVSLang.FAnd (raw2FBExpr env fenv fbe1) (raw2FBExpr env fenv fbe2)
raw2FBExpr env fenv (AbsRawPVSLang.FNot       fbe) = AbsPVSLang.FNot (raw2FBExpr env fenv fbe)
raw2FBExpr env fenv (AbsRawPVSLang.FEq  fae1 fae2) = AbsPVSLang.FRel Op.Eq  (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FBExpr env fenv (AbsRawPVSLang.FNeq fae1 fae2) = AbsPVSLang.FRel Op.Neq (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FBExpr env fenv (AbsRawPVSLang.FLt  fae1 fae2) = AbsPVSLang.FRel Op.Lt  (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FBExpr env fenv (AbsRawPVSLang.FLtE fae1 fae2) = AbsPVSLang.FRel Op.LtE (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FBExpr env fenv (AbsRawPVSLang.FGt  fae1 fae2) = AbsPVSLang.FRel Op.Gt  (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FBExpr env fenv (AbsRawPVSLang.FGtE fae1 fae2) = AbsPVSLang.FRel Op.GtE (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FBExpr env fenv (AbsRawPVSLang.FCallN (AbsRawPVSLang.Id f) actArgs) = AbsPVSLang.FEPred False Original f (map (raw2FAExpr env fenv) actArgs)
raw2FBExpr   _    _  AbsRawPVSLang.FBTrue          = AbsPVSLang.FBTrue
raw2FBExpr   _    _  AbsRawPVSLang.FBFalse         = AbsPVSLang.FBFalse
raw2FBExpr   _    _  expr = error $ "raw2FBExpr: Boolean expression expected but got " ++ show expr ++ "."

raw2FAExpr :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> AbsPVSLang.FAExpr
raw2FAExpr env fenv (AbsRawPVSLang.ExprId (AbsRawPVSLang.Id i)) =
  case lookup i fenv of 
    Just fp -> AbsPVSLang.FEFun False i fp []
    Nothing -> case lookup i env of
                  Just fp -> AbsPVSLang.FVar fp i
                  Nothing -> error $ "Identifier " ++ show i ++ "not found."

raw2FAExpr env fenv (AbsRawPVSLang.FCallN (AbsRawPVSLang.Id f) actArgs) = AbsPVSLang.FEFun False f fp (map (raw2FAExpr env fenv) actArgs)
  where
    fp = fromMaybe (error $ "raw2FAExpr: function " ++ show f ++ " not found.")
                    (lookup f fenv)

raw2FAExpr _ _ (AbsRawPVSLang.FInt n) = AbsPVSLang.FInt n
raw2FAExpr _ _ (AbsRawPVSLang.RtoS d) = AbsPVSLang.ToFloat FPSingle $ AbsPVSLang.Rat (toRational d)
raw2FAExpr _ _ (AbsRawPVSLang.RtoD d) = AbsPVSLang.ToFloat FPDouble $ AbsPVSLang.Rat (toRational d) 
raw2FAExpr _ _ (AbsRawPVSLang.IntRtoS    i) = AbsPVSLang.ToFloat FPSingle $ AbsPVSLang.Int i
raw2FAExpr _ _ (AbsRawPVSLang.IntRtoD    i) = AbsPVSLang.ToFloat FPDouble $ AbsPVSLang.Int i
raw2FAExpr _ _ (AbsRawPVSLang.IntNegRtoS i) = AbsPVSLang.ToFloat FPSingle $ AbsPVSLang.Int (-i)
raw2FAExpr _ _ (AbsRawPVSLang.IntNegRtoD i) = AbsPVSLang.ToFloat FPDouble $ AbsPVSLang.Int (-i)
raw2FAExpr _ _ (AbsRawPVSLang.NegRtoS    d) = AbsPVSLang.ToFloat FPSingle $ AbsPVSLang.Rat (toRational (-d))
raw2FAExpr _ _ (AbsRawPVSLang.NegRtoD    d) = AbsPVSLang.ToFloat FPDouble $ AbsPVSLang.Rat (toRational (-d))

raw2FAExpr   _    _ (AbsRawPVSLang.ExprNeg (AbsRawPVSLang.FInt i)) = AbsPVSLang.FInt (-i)
raw2FAExpr   _    _ (AbsRawPVSLang.ExprNeg (AbsRawPVSLang.IntRtoD i))  = AbsPVSLang.ToFloat FPDouble $ AbsPVSLang.Int (-i)
raw2FAExpr   _    _ (AbsRawPVSLang.ExprNeg (AbsRawPVSLang.IntRtoS i))  = AbsPVSLang.ToFloat FPSingle $ AbsPVSLang.Int (-i)
raw2FAExpr   _    _ (AbsRawPVSLang.ExprNeg (AbsRawPVSLang.IntNegRtoD i))  = AbsPVSLang.ToFloat FPDouble $ AbsPVSLang.Int i
raw2FAExpr   _    _ (AbsRawPVSLang.ExprNeg (AbsRawPVSLang.IntNegRtoS i))  = AbsPVSLang.ToFloat FPSingle $ AbsPVSLang.Int i
raw2FAExpr   _    _ (AbsRawPVSLang.ExprNeg (AbsRawPVSLang.RtoD d))  = AbsPVSLang.ToFloat FPDouble $ AbsPVSLang.Rat (toRational (-d))
raw2FAExpr   _    _ (AbsRawPVSLang.ExprNeg (AbsRawPVSLang.RtoS d))  = AbsPVSLang.ToFloat FPSingle $ AbsPVSLang.Rat (toRational (-d))
raw2FAExpr   _    _ (AbsRawPVSLang.ExprNeg (AbsRawPVSLang.NegRtoD d))  = AbsPVSLang.ToFloat FPDouble $ AbsPVSLang.Rat (toRational d)
raw2FAExpr   _    _ (AbsRawPVSLang.ExprNeg (AbsRawPVSLang.NegRtoS d))  = AbsPVSLang.ToFloat FPSingle $ AbsPVSLang.Rat (toRational d)
raw2FAExpr env fenv (AbsRawPVSLang.ExprNeg fae) = AbsPVSLang.UnaryFPOp Op.NegOp fp (raw2FAExpr env fenv fae)
  where
    fp = getPVSType $ raw2FAExpr env fenv fae
 
raw2FAExpr env fenv (AbsRawPVSLang.SAdd fae1 fae2) = AbsPVSLang.BinaryFPOp Op.AddOp FPSingle (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.DAdd fae1 fae2) = AbsPVSLang.BinaryFPOp Op.AddOp FPDouble (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.IAdd fae1 fae2) = AbsPVSLang.BinaryFPOp Op.AddOp TInt     (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.SSub fae1 fae2) = AbsPVSLang.BinaryFPOp Op.SubOp FPSingle (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.DSub fae1 fae2) = AbsPVSLang.BinaryFPOp Op.SubOp FPDouble (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.ISub fae1 fae2) = AbsPVSLang.BinaryFPOp Op.SubOp TInt     (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.SMul fae1 fae2) = AbsPVSLang.BinaryFPOp Op.MulOp FPSingle (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.DMul fae1 fae2) = AbsPVSLang.BinaryFPOp Op.MulOp FPDouble (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.IMul fae1 fae2) = AbsPVSLang.BinaryFPOp Op.MulOp TInt     (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.SDiv fae1 fae2) = AbsPVSLang.BinaryFPOp Op.DivOp FPSingle (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.DDiv fae1 fae2) = AbsPVSLang.BinaryFPOp Op.DivOp FPDouble (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.IDiv fae1 fae2) = AbsPVSLang.BinaryFPOp Op.DivOp TInt     (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.SMod fae1 fae2) = AbsPVSLang.BinaryFPOp Op.ModOp FPSingle (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.DMod fae1 fae2) = AbsPVSLang.BinaryFPOp Op.ModOp FPDouble (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.IMod fae1 fae2) = AbsPVSLang.BinaryFPOp Op.ModOp TInt     (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)

raw2FAExpr env fenv (AbsRawPVSLang.ExprAdd fae1 fae2) = raw2BinOp env fenv fae1 fae2 Op.AddOp
raw2FAExpr env fenv (AbsRawPVSLang.ExprSub fae1 fae2) = raw2BinOp env fenv fae1 fae2 Op.SubOp
raw2FAExpr env fenv (AbsRawPVSLang.ExprMul fae1 fae2) = raw2BinOp env fenv fae1 fae2 Op.MulOp
raw2FAExpr env fenv (AbsRawPVSLang.ExprDiv fae1 fae2) = raw2BinOp env fenv fae1 fae2 Op.DivOp
raw2FAExpr env fenv (AbsRawPVSLang.ExprPow fae1 fae2) = raw2BinOp env fenv fae1 fae2 Op.PowOp

raw2FAExpr env fenv (AbsRawPVSLang.SNeg      fae)  = AbsPVSLang.UnaryFPOp Op.NegOp   FPSingle (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DNeg      fae)  = AbsPVSLang.UnaryFPOp Op.NegOp   FPDouble (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.INeg      fae)  = AbsPVSLang.UnaryFPOp Op.NegOp   TInt     (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SFloor    fae)  = AbsPVSLang.UnaryFPOp Op.FloorOp FPSingle (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DFloor    fae)  = AbsPVSLang.UnaryFPOp Op.FloorOp FPDouble (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SSqrt     fae)  = AbsPVSLang.UnaryFPOp Op.SqrtOp  FPSingle (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DSqrt     fae)  = AbsPVSLang.UnaryFPOp Op.SqrtOp  FPDouble (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SAbs      fae)  = AbsPVSLang.UnaryFPOp Op.AbsOp   FPSingle (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DAbs      fae)  = AbsPVSLang.UnaryFPOp Op.AbsOp   FPDouble (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.IAbs      fae)  = AbsPVSLang.UnaryFPOp Op.AbsOp   TInt     (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SSin      fae)  = AbsPVSLang.UnaryFPOp Op.SinOp   FPSingle (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DSin      fae)  = AbsPVSLang.UnaryFPOp Op.SinOp   FPDouble (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SCos      fae)  = AbsPVSLang.UnaryFPOp Op.CosOp   FPSingle (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DCos      fae)  = AbsPVSLang.UnaryFPOp Op.CosOp   FPDouble (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.STan      fae)  = AbsPVSLang.UnaryFPOp Op.TanOp   FPSingle (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DTan      fae)  = AbsPVSLang.UnaryFPOp Op.TanOp   FPDouble (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SAcos     fae)  = AbsPVSLang.UnaryFPOp Op.AcosOp  FPSingle (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DAcos     fae)  = AbsPVSLang.UnaryFPOp Op.AcosOp  FPDouble (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SAsin     fae)  = AbsPVSLang.UnaryFPOp Op.AsinOp  FPSingle (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DAsin     fae)  = AbsPVSLang.UnaryFPOp Op.AsinOp  FPDouble (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SAtan     fae)  = AbsPVSLang.UnaryFPOp Op.AtanOp  FPSingle (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DAtan     fae)  = AbsPVSLang.UnaryFPOp Op.AtanOp  FPDouble (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SLn       fae)  = AbsPVSLang.UnaryFPOp Op.LnOp    FPSingle (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DLn       fae)  = AbsPVSLang.UnaryFPOp Op.LnOp    FPDouble (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SExp      fae)  = AbsPVSLang.UnaryFPOp Op.ExpoOp  FPSingle (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DExp      fae)  = AbsPVSLang.UnaryFPOp Op.ExpoOp  FPDouble (raw2FAExpr env fenv fae)

raw2FAExpr env fenv (AbsRawPVSLang.Let letElems stm)
  = AbsPVSLang.Let letList (raw2FAExpr newenv fenv stm)
  where
    (newenv,letList) = foldr aux_fold (env,[]) letElems
    aux_fold letElem (accEnv,elems) =  (env',elems ++ [newLetElem])
      where
        newLetElem = raw2LetElem accEnv fenv letElem
        env' = (fst3 newLetElem, snd3 newLetElem):accEnv

raw2FAExpr env fenv (If bexpr stmThen stmElse) = 
    Ite (raw2FBExpr env fenv bexpr) (raw2FAExpr env fenv stmThen) (raw2FAExpr env fenv stmElse)

raw2FAExpr env fenv (ListIf bexpr stmThen listElsif stmElse) =
    ListIte ((raw2FBExpr env fenv bexpr,raw2FAExpr env fenv stmThen): map (raw2Elsif env fenv) listElsif) (raw2FAExpr env fenv stmElse)

raw2FAExpr _ _ AbsRawPVSLang.UnstWarning = AbsPVSLang.UnstWarning
raw2FAExpr _ _ For{} = undefined

raw2FAExpr _ _ ae = error $ "raw2FAExpr: artihmetic expression expected but got " ++ show ae ++ "."

raw2LetElem :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.LetElem -> AbsPVSLang.FLetElem
raw2LetElem env fenv (AbsRawPVSLang.LetElem x   ae) = (raw2Id x, PVSTypes.FPDouble, raw2FAExpr env fenv ae)
raw2LetElem env fenv (LetElemType           x t ae) = (raw2Id x, raw2FPType t,      raw2FAExpr env fenv ae)

raw2Args :: AbsRawPVSLang.Args -> [AbsPVSLang.Arg]
raw2Args (FArgs args) = concatMap raw2Arg args
raw2Args (FArgsNoType _) = error "Arguments have no type."

raw2Arg :: AbsRawPVSLang.Arg -> [AbsPVSLang.Arg]
raw2Arg (FArg xs t)         = map (raw2ArgWithType t) xs
raw2Arg (FArgSubrange xs _) = map (raw2ArgWithType FPtype_integer) xs
raw2Arg (FArgGuard xs t _)  = map (raw2ArgWithType t) xs

raw2ArgWithType :: FPtype -> AbsRawPVSLang.Id -> AbsPVSLang.Arg
raw2ArgWithType t x = AbsPVSLang.Arg (raw2Id x) (raw2FPType t)

raw2Id :: AbsRawPVSLang.Id -> VarName
raw2Id (AbsRawPVSLang.Id x) = x

raw2FPType :: AbsRawPVSLang.FPtype -> PVSType
raw2FPType FPtype_int            = TInt
raw2FPType FPtype_integer        = TInt
raw2FPType FPtype_unb_single     = FPSingle 
raw2FPType FPtype_unb_double     = FPDouble 
raw2FPType FPtype_unb_pos_single = FPSingle 
raw2FPType FPtype_unb_pos_double = FPDouble 
raw2FPType FPtype_unb_nz_single  = FPSingle 
raw2FPType FPtype_unb_nz_double  = FPDouble 
raw2FPType FPtype_bool           = Boolean
-- raw2FPType t = error $ "raw2FPType: unexpected type value " ++ show t

rawparserPVS :: String -> Err AbsRawPVSLang.Program
rawparserPVS = pProgram . tokens 
