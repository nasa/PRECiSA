-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
 
-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
 
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
  
  
module MapRealPVSLangAST
where

import AbsRawRealPVSLang
import AbsPVSLang
import Common.TypesUtils
import Data.Maybe(fromMaybe)
import ErrM
import PVSTypes
import qualified Operators as Op
import Parser.ParRawRealPVSLang
import Parser.LexRawRealPVSLang

type VarTypeEnv = [(String, PVSType)]
type FunTypeEnv = [(String, PVSType)]

namePVSRealTheory :: AbsRawRealPVSLang.Program -> String
namePVSRealTheory (Prog    (Id name) _ _ _ _) = name
namePVSRealTheory (ProgImp (Id name) _ _ _)   = name

rawparserRealPVS :: String -> Err AbsRawRealPVSLang.Program
rawparserRealPVS = pProgram . tokens

raw2Id :: AbsRawRealPVSLang.Id -> VarName
raw2Id (AbsRawRealPVSLang.Id x) = x

raw2FPType :: AbsRawRealPVSLang.Type -> PVSType
raw2FPType TypeInt       = TInt
raw2FPType TypeInteger   = TInt
raw2FPType TypeReal      = Real
raw2FPType TypePosNat    = TInt
raw2FPType (TypeBelow _) = TInt
raw2FPType TypeBool      = Boolean 
raw2FPType (TypeArrayInteger    t) = Array (raw2FPType t) Nothing
raw2FPType (TypeArrayInt        t) = Array (raw2FPType t) Nothing
raw2FPType (TypeArrayBelow (AbsRawRealPVSLang.Int n) t) = Array (raw2FPType t) (Just (ArraySizeInt n))
raw2FPType (TypeArrayBelow (AbsRawRealPVSLang.Var (AbsRawRealPVSLang.Id x)) t) = Array (raw2FPType t) (Just (ArraySizeVar x))
raw2FPType t = error $ "raw2FPType: unexpected value " ++ show t ++ "."

raw2RealProg :: AbsRawRealPVSLang.Program -> AbsPVSLang.RProgram
raw2RealProg (AbsRawRealPVSLang.Prog    _ _ _ listDecl _) = raw2Decsl (map retTypeFun listDecl) listDecl
raw2RealProg (AbsRawRealPVSLang.ProgImp _   _ listDecl _) = raw2Decsl (map retTypeFun listDecl) listDecl

retTypeFun :: AbsRawRealPVSLang.Decl -> (String, PVSType)
retTypeFun (Decl0   (AbsRawRealPVSLang.Id f)   t _) = (f, raw2FPType t)
retTypeFun (DeclN   (AbsRawRealPVSLang.Id f) _ t _) = (f, raw2FPType t)
retTypeFun (DeclRec (AbsRawRealPVSLang.Id f) _ t _) = (f, raw2FPType t)

raw2Decsl :: FunTypeEnv -> [AbsRawRealPVSLang.Decl] -> [AbsPVSLang.RDecl]
raw2Decsl fenv = map (raw2Decl fenv)

raw2Decl :: FunTypeEnv -> AbsRawRealPVSLang.Decl -> AbsPVSLang.RDecl
raw2Decl fenv (DeclN f rawArgs TypeBool expr) = RPred (raw2Id f) args (raw2BExprStm env fenv expr)
  where
    args = raw2Args rawArgs
    env = map mapArg2Pair args
raw2Decl fenv (Decl0 f TypeBool expr) = RPred (raw2Id f) [] (raw2BExprStm [] fenv expr)
raw2Decl fenv (DeclN   f rawArgs t stm) = RDecl (raw2FPType t) (raw2Id f) args (raw2AExpr env fenv stm)
  where
    args = raw2Args rawArgs
    env = map mapArg2Pair args
raw2Decl fenv (DeclRec f rawArgs t stm) = RDecl (raw2FPType t) (raw2Id f) args (raw2AExpr env fenv stm)
  where
    args = raw2Args rawArgs
    env = map mapArg2Pair args
raw2Decl fenv (Decl0   f      t stm) = RDecl (raw2FPType t) (raw2Id f) [] (raw2AExpr [] fenv stm)

raw2Args :: AbsRawRealPVSLang.Args -> [AbsPVSLang.Arg]
raw2Args (FArgs args) = concatMap raw2Arg args
raw2Args (FArgsNoType _) = error "Arguments have no type."

raw2Arg :: AbsRawRealPVSLang.Arg -> [AbsPVSLang.Arg]
raw2Arg (FArg xs t)         = map (raw2ArgWithType           t) xs
raw2Arg (FArgSubrange xs _) = map (raw2ArgWithType TypeInteger) xs
raw2Arg (FArgGuard xs t _)  = map (raw2ArgWithType           t) xs

raw2ArgWithType :: Type -> AbsRawRealPVSLang.Id -> AbsPVSLang.Arg
raw2ArgWithType t x = AbsPVSLang.Arg (raw2Id x) (raw2FPType t)

raw2Elsif :: VarTypeEnv -> FunTypeEnv -> AbsRawRealPVSLang.ElsIf -> (AbsPVSLang.BExpr, AbsPVSLang.AExpr)
raw2Elsif env fenv (ElsIf fbexpr stm) = (raw2BExpr env fenv fbexpr, raw2AExpr env fenv stm)

raw2BElsif :: VarTypeEnv -> FunTypeEnv -> AbsRawRealPVSLang.ElsIf -> (AbsPVSLang.BExpr, AbsPVSLang.BExprStm)
raw2BElsif env fenv (ElsIf fbexpr stm) = (raw2BExpr env fenv fbexpr, raw2BExprStm env fenv stm)

raw2LetElem :: VarTypeEnv -> FunTypeEnv -> AbsRawRealPVSLang.LetElem -> AbsPVSLang.LetElem
raw2LetElem env fenv (AbsRawRealPVSLang.LetElem x rawExpr)
  | (isIntAExpr expr) = AbsPVSLang.LetElem {letVar = raw2Id x, letType = TInt, letExpr = expr}
  | otherwise         = AbsPVSLang.LetElem {letVar = raw2Id x, letType = Real, letExpr = expr}
  where
    expr = raw2AExpr env fenv rawExpr
raw2LetElem env fenv (LetElemType x t rawExpr) = AbsPVSLang.LetElem {letVar  = raw2Id x
                                                                    ,letType = raw2FPType t
                                                                    ,letExpr = raw2AExpr env fenv rawExpr}

raw2AExpr :: VarTypeEnv -> FunTypeEnv -> AbsRawRealPVSLang.Expr -> AbsPVSLang.AExpr
raw2AExpr env fenv (AbsRawRealPVSLang.Let letElems stm)
  = RLet letList (raw2AExpr newenv fenv stm)
  where
    (newenv,letList) = foldl aux_fold (env,[]) letElems
    aux_fold  (accEnv,elems) letElem =  (env',elems ++ [newLetElem])
      where
        newLetElem = raw2LetElem accEnv fenv letElem
        env' = (letVar newLetElem, letType newLetElem):accEnv
    
raw2AExpr env fenv (AbsRawRealPVSLang.For retType startIdx endIdx initValueAcc idxVarId@(AbsRawRealPVSLang.Id idx) _ _ accVarId@(AbsRawRealPVSLang.Id acc) accType forBody)
  = if retType == accType
    then RForLoop fp
                  (raw2AExpr env fenv startIdx)
                  (raw2AExpr env fenv endIdx)
                  (raw2AExpr env fenv initValueAcc)
                  (raw2Id idxVarId)
                  (raw2Id accVarId)
                  (raw2AExpr ((idx,TInt):(acc,fp):env) fenv forBody)
    else error "Type mismatch for for loop."
    where
      fp = raw2FPType retType

raw2AExpr env fenv (AbsRawRealPVSLang.If be thenSmt elseStm)  = RIte (raw2BExpr env fenv be) (raw2AExpr env fenv thenSmt) (raw2AExpr env fenv elseStm)

raw2AExpr env fenv (AbsRawRealPVSLang.ListIf be stmThen listElsif elseStm) =
    RListIte ((raw2BExpr env fenv be,raw2AExpr env fenv stmThen) : map (raw2Elsif env fenv) listElsif) (raw2AExpr env fenv elseStm)
    
raw2AExpr _ _ AbsRawRealPVSLang.UnstWarning = RUnstWarning

raw2AExpr env fenv (AbsRawRealPVSLang.Add  ae1 ae2) = AbsPVSLang.BinaryOp Op.AddOp   (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawRealPVSLang.Sub  ae1 ae2) = AbsPVSLang.BinaryOp Op.SubOp   (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawRealPVSLang.Mul  ae1 ae2) = AbsPVSLang.BinaryOp Op.MulOp   (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawRealPVSLang.Div  ae1 ae2) = AbsPVSLang.BinaryOp Op.DivOp   (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawRealPVSLang.Pow  ae1 ae2) = AbsPVSLang.BinaryOp Op.PowOp   (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawRealPVSLang.Mod1 ae1 ae2) = AbsPVSLang.BinaryOp Op.ModOp   (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawRealPVSLang.Mod2 ae1 ae2) = AbsPVSLang.BinaryOp Op.ModOp   (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr   _    _ (AbsRawRealPVSLang.Neg (AbsRawRealPVSLang.Int i)) = AbsPVSLang.Int (-i)
raw2AExpr   _    _ (AbsRawRealPVSLang.Neg (AbsRawRealPVSLang.Rat r)) = AbsPVSLang.Rat (-(toRational r))
raw2AExpr env fenv (AbsRawRealPVSLang.Neg      ae)  = AbsPVSLang.UnaryOp  Op.NegOp   (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.Floor    ae)  = AbsPVSLang.UnaryOp  Op.FloorOp (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.Sqrt     ae)  = AbsPVSLang.UnaryOp  Op.SqrtOp  (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.Abs      ae)  = AbsPVSLang.UnaryOp  Op.AbsOp   (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.Sin      ae)  = AbsPVSLang.UnaryOp  Op.SinOp   (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.Cos      ae)  = AbsPVSLang.UnaryOp  Op.CosOp   (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.Tan      ae)  = AbsPVSLang.UnaryOp  Op.TanOp   (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.ASin     ae)  = AbsPVSLang.UnaryOp  Op.AsinOp  (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.ACos     ae)  = AbsPVSLang.UnaryOp  Op.AcosOp  (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.ATan     ae)  = AbsPVSLang.UnaryOp  Op.AtanOp  (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.Ln       ae)  = AbsPVSLang.UnaryOp  Op.LnOp    (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.Exp      ae)  = AbsPVSLang.UnaryOp  Op.ExpoOp  (raw2AExpr env fenv ae)
raw2AExpr   _    _ (AbsRawRealPVSLang.Int      i)   = AbsPVSLang.Int i
raw2AExpr   _    _ (AbsRawRealPVSLang.Rat      d)   = AbsPVSLang.Rat (toRational d)
raw2AExpr env    _ (AbsRawRealPVSLang.Var (AbsRawRealPVSLang.Id x)) = AbsPVSLang.Var fp x
  where 
    fp = fromMaybe (error $ "raw2FAExpr: variable " ++ show x ++ " not found in " ++ show env ++ ".")
                   (lookup x env)

raw2AExpr env fenv (FCallN (AbsRawRealPVSLang.Id f) actArgs) =
  case lookup f fenv of
    Just Boolean -> error "raw2AExpr: Numerical function expected."
    Just fp -> AbsPVSLang.EFun f fp (map (raw2AExpr env fenv) actArgs)
    Nothing -> case lookup f env of
                  Just (Array fp size) -> AbsPVSLang.ArrayElem fp size f idx
                  _  -> error $ "raw2FAExpr: something went wrong "++ show f ++ " is not an array or function."
  where
   idx = case actArgs of
           [i] -> raw2AExpr env fenv i
           _   -> error "raw2FAExpr: index should be unique."
raw2AExpr _ _ Pi1           = error "Constant Pi not supported, use, for instance, 3.14"
raw2AExpr _ _ Pi2           = error "Constant Pi not supported, use, for instance, 3.14"
raw2AExpr _ _ ae = error $ "Something went wrong: arithmetic expression expected but got " ++ show ae ++ "."

raw2BExprStm :: VarTypeEnv -> FunTypeEnv -> AbsRawRealPVSLang.Expr -> AbsPVSLang.BExprStm

raw2BExprStm env fenv (AbsRawRealPVSLang.Let letElems stm)
  = RBLet letList (raw2BExprStm newenv fenv stm)
  where
    (newenv,letList) = foldr aux_fold (env,[]) letElems
    aux_fold letElem (accEnv,elems) =  (env',elems ++ [newLetElem])
      where
        newLetElem = raw2LetElem accEnv fenv letElem
        env' = (letVar newLetElem, letType newLetElem):accEnv

raw2BExprStm env fenv (AbsRawRealPVSLang.If be thenSmt elseStm)
  = RBIte (raw2BExpr env fenv be) (raw2BExprStm env fenv thenSmt)
                                  (raw2BExprStm env fenv elseStm)

raw2BExprStm env fenv (AbsRawRealPVSLang.ListIf be stmThen listElsif elseStm) =
    RBListIte ((raw2BExpr env fenv be,raw2BExprStm env fenv stmThen) : map (raw2BElsif env fenv) listElsif) (raw2BExprStm env fenv elseStm)

raw2BExprStm env fenv be = RBExpr $ raw2BExpr env fenv be


raw2BExpr :: VarTypeEnv -> FunTypeEnv -> AbsRawRealPVSLang.Expr -> AbsPVSLang.BExpr 
raw2BExpr env fenv (AbsRawRealPVSLang.Or  be1 be2) = AbsPVSLang.Or  (raw2BExpr env fenv be1) (raw2BExpr env fenv be2)
raw2BExpr env fenv (AbsRawRealPVSLang.And be1 be2) = AbsPVSLang.And (raw2BExpr env fenv be1) (raw2BExpr env fenv be2)
raw2BExpr env fenv (AbsRawRealPVSLang.Not be)      = AbsPVSLang.Not (raw2BExpr env fenv be)
raw2BExpr env fenv (AbsRawRealPVSLang.Eq  ae1 ae2) = AbsPVSLang.Rel Op.Eq  (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2) 
raw2BExpr env fenv (AbsRawRealPVSLang.Neq ae1 ae2) = AbsPVSLang.Rel Op.Neq (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2) 
raw2BExpr env fenv (AbsRawRealPVSLang.Lt  ae1 ae2) = AbsPVSLang.Rel Op.Lt  (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2) 
raw2BExpr env fenv (AbsRawRealPVSLang.LtE ae1 ae2) = AbsPVSLang.Rel Op.LtE (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2) 
raw2BExpr env fenv (AbsRawRealPVSLang.Gt  ae1 ae2) = AbsPVSLang.Rel Op.Gt  (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2) 
raw2BExpr env fenv (AbsRawRealPVSLang.GtE ae1 ae2) = AbsPVSLang.Rel Op.GtE (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2) 
raw2BExpr _   _     AbsRawRealPVSLang.BTrue        = AbsPVSLang.BTrue
raw2BExpr _   _     AbsRawRealPVSLang.BFalse       = AbsPVSLang.BFalse
raw2BExpr env fenv  (FCallN (Id f) args) = 
  case lookup f fenv of
    Just Boolean -> AbsPVSLang.EPred f (map (raw2AExpr env fenv) args)
    Just _ -> error "raw2BExpr: Boolean function expected."
    Nothing -> error $ "raw2BExpr: something went wrong "++ show f ++ " is not a predicate."
raw2BExpr  _ _ be = error $ "Something went wrong: boolean expression expected but got " ++ show be ++ "."
