module MapRealPVSLangAST
where

import Parser.ParRawRealPVSLang
import Parser.LexRawRealPVSLang
import AbsRawRealPVSLang
import AbsPVSLang
import ErrM
import FPrec
--import Debug.Trace
import Data.Maybe(fromMaybe)

type VarTypeEnv = [(String, FPrec)]
type FunTypeEnv = [(String, FPrec)]


raw2VarId :: AbsRawRealPVSLang.VarId -> VarName
raw2VarId (AbsRawRealPVSLang.VarId x) = x

raw2NonVarId :: AbsRawRealPVSLang.NonVarId -> FunName
raw2NonVarId (AbsRawRealPVSLang.NonVarId x) = x

raw2FPType :: AbsRawRealPVSLang.Type -> FPrec
raw2FPType TypeInt       = TInt
raw2FPType TypeInteger   = TInt
raw2FPType TypeReal      = Real
raw2FPType TypePosNat    = TInt
raw2FPType (TypeBelow _) = TInt
raw2FPType (TypeArrayInteger    t) = Array (raw2FPType t) Nothing
raw2FPType (TypeArrayInt        t) = Array (raw2FPType t) Nothing
raw2FPType (TypeArrayBelow (AbsRawRealPVSLang.Int n) t) = Array (raw2FPType t) (Just (ArraySizeInt n))
raw2FPType (TypeArrayBelow (AbsRawRealPVSLang.Var (AbsRawRealPVSLang.VarId x)) t) = Array (raw2FPType t) (Just (ArraySizeVar x))
raw2FPType t = error $ "raw2FPType: unexpected value " ++ show t ++ "."

rawparserRealPVS :: String -> Err AbsRawRealPVSLang.Program
rawparserRealPVS = pProgram . tokens

raw2RealProg :: AbsRawRealPVSLang.Program -> AbsPVSLang.RProgram
raw2RealProg (AbsRawRealPVSLang.Prog    _ _ _ listDecl _) = raw2Decsl (map retTypeFun listDecl) listDecl
raw2RealProg (AbsRawRealPVSLang.ProgImp _   _ listDecl _) = raw2Decsl (map retTypeFun listDecl) listDecl

retTypeFun :: AbsRawRealPVSLang.Decl -> (String, FPrec)
retTypeFun (Decl0   (AbsRawRealPVSLang.NonVarId f)      t _) = (f, raw2FPType t)
retTypeFun (DeclN   (AbsRawRealPVSLang.NonVarId f) _ t _) = (f, raw2FPType t)
retTypeFun (DeclRec (AbsRawRealPVSLang.NonVarId f) _ t _) = (f, raw2FPType t)

raw2Decsl :: FunTypeEnv -> [AbsRawRealPVSLang.Decl] -> [AbsPVSLang.RDecl]
raw2Decsl fenv = map (raw2Decl fenv)

raw2Decl :: FunTypeEnv -> AbsRawRealPVSLang.Decl -> AbsPVSLang.RDecl
raw2Decl fenv (DeclN   f (FArgs rawArgs) t stm) = RDecl (raw2FPType t) (raw2NonVarId f) args (raw2RealStm env fenv stm)
  where
    args = raw2Args rawArgs
    env = map mapArg2Pair args
raw2Decl fenv (DeclRec f (FArgs rawArgs) t stm) = RDecl (raw2FPType t) (raw2NonVarId f) args (raw2RealStm env fenv stm)
  where
    args = raw2Args rawArgs
    env = map mapArg2Pair args
raw2Decl fenv (Decl0   f      t stm) = RDecl (raw2FPType t) (raw2NonVarId f) [] (raw2RealStm [] fenv stm)

raw2Args :: [AbsRawRealPVSLang.Arg] -> [AbsPVSLang.Arg]
raw2Args = concatMap raw2Arg

raw2Arg :: AbsRawRealPVSLang.Arg -> [AbsPVSLang.Arg]
raw2Arg (FArg xs t)         = map (raw2ArgWithType           t) xs
raw2Arg (FArgSubrange xs _) = map (raw2ArgWithType TypeInteger) xs
raw2Arg (FArgGuard xs t _)  = map (raw2ArgWithType           t) xs
raw2Arg (FArgNoType x)      = error ("Variable "++ show x ++ "has no type.")

raw2ArgWithType :: Type -> AbsRawRealPVSLang.VarId -> AbsPVSLang.Arg
raw2ArgWithType t x = AbsPVSLang.Arg (raw2VarId x) (raw2FPType t)

raw2RealStm :: VarTypeEnv -> FunTypeEnv -> AbsRawRealPVSLang.Stm -> AbsPVSLang.RStm
raw2RealStm env fenv (AbsRawRealPVSLang.Let var@(AbsRawRealPVSLang.VarId x) ae stm) =
--  trace ("Variable "++ show x ++ " type set to Real.\n") $
  RLet (raw2VarId var) Real (raw2AExpr env fenv ae) (raw2RealStm ((x, Real):env) fenv stm)

raw2RealStm env fenv (AbsRawRealPVSLang.LetWithType var@(AbsRawRealPVSLang.VarId x) t ae stm) =
--  trace ("Variable "++ show x ++ " type set to " ++ show fp ++".\n") $
  RLet (raw2VarId var) fp (raw2AExpr env fenv ae) (raw2RealStm ((x, fp):env) fenv stm)
  where
    fp = raw2FPType t

raw2RealStm env fenv (AbsRawRealPVSLang.For retType startIdx endIdx initValueAcc idxVarId@(AbsRawRealPVSLang.VarId idx) _ _ accVarId@(AbsRawRealPVSLang.VarId acc) accType forBody)
  = if retType == accType
    then RForLoop fp
                  (raw2AExpr env fenv startIdx)
                  (raw2AExpr env fenv endIdx)
                  (raw2AExpr env fenv initValueAcc)
                  (raw2VarId idxVarId)
                  (raw2VarId accVarId)
                  (raw2RealStm ((idx,TInt):(acc,fp):env) fenv forBody)
    else error "Type mismatch for for loop."
    where
      fp = raw2FPType retType

raw2RealStm env fenv (AbsRawRealPVSLang.If be thenSmt elseStm)  = RIte (raw2BExpr env fenv be) (raw2RealStm env fenv thenSmt) (raw2RealStm env fenv elseStm)

raw2RealStm env fenv (AbsRawRealPVSLang.ListIf be stmThen listElsif elseStm) =
    RListIte ((raw2BExpr env fenv be,raw2RealStm env fenv stmThen) : map (raw2Elsif env fenv) listElsif) (raw2RealStm env fenv elseStm)
raw2RealStm env fenv (AbsRawRealPVSLang.Expr ae) = RStmExpr (raw2AExpr env fenv ae)
raw2RealStm _ _ AbsRawRealPVSLang.UnstWarning = RUnstWarning

raw2Elsif :: VarTypeEnv -> FunTypeEnv -> AbsRawRealPVSLang.ElsIf -> (AbsPVSLang.BExpr, AbsPVSLang.RStm)
raw2Elsif env fenv (ElsIf fbexpr stm) = (raw2BExpr env fenv fbexpr, raw2RealStm env fenv stm)

raw2AExpr :: VarTypeEnv -> FunTypeEnv ->  AbsRawRealPVSLang.AExpr -> AbsPVSLang.AExpr
raw2AExpr env fenv (AbsRawRealPVSLang.Add  ae1 ae2) = AbsPVSLang.Add   (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawRealPVSLang.Sub  ae1 ae2) = AbsPVSLang.Sub   (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawRealPVSLang.Mul  ae1 ae2) = AbsPVSLang.Mul   (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawRealPVSLang.Div  ae1 ae2) = AbsPVSLang.Div   (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawRealPVSLang.Pow  ae1 ae2) = AbsPVSLang.Pow   (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawRealPVSLang.Mod1 ae1 ae2) = AbsPVSLang.Mod   (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawRealPVSLang.Mod2 ae1 ae2) = AbsPVSLang.Mod   (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawRealPVSLang.Neg      ae)  = AbsPVSLang.Neg   (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.Floor    ae)  = AbsPVSLang.Floor (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.Sqrt     ae)  = AbsPVSLang.Sqrt  (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.Abs      ae)  = AbsPVSLang.Abs   (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.Sin      ae)  = AbsPVSLang.Sin   (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.Cos      ae)  = AbsPVSLang.Cos   (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.Tan      ae)  = AbsPVSLang.Tan   (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.ASin     ae)  = AbsPVSLang.ASin  (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.ACos     ae)  = AbsPVSLang.ACos  (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.ATan     ae)  = AbsPVSLang.ATan  (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.Ln       ae)  = AbsPVSLang.Ln    (raw2AExpr env fenv ae)
raw2AExpr env fenv (AbsRawRealPVSLang.Exp      ae)  = AbsPVSLang.Expo  (raw2AExpr env fenv ae)
raw2AExpr _ _ (AbsRawRealPVSLang.Int      i)   = AbsPVSLang.Int i
raw2AExpr _ _ (AbsRawRealPVSLang.Rat      d)   = AbsPVSLang.Rat (toRational d)
raw2AExpr env _ (AbsRawRealPVSLang.Var (AbsRawRealPVSLang.VarId x)) = AbsPVSLang.Var fp x
  where 
    fp = fromMaybe
          (error $ "raw2FAExpr: variable " ++ show x ++ " not found.")
          (lookup x env)
raw2AExpr _ fenv (FCall0 (AbsRawRealPVSLang.NonVarId f)) = AbsPVSLang.EFun f fp []
  where
    fp = fromMaybe
          (error $ "raw2FAExpr: function " ++ show f ++ " not found.")
          (lookup f fenv)
raw2AExpr env fenv (FCallN (AbsRawRealPVSLang.NonVarId f) actArgs) = AbsPVSLang.EFun f fp (map (raw2AExpr env fenv) actArgs)
  where
    fp = fromMaybe
          (error $ "raw2FAExpr: function " ++ show f ++ " not found.")
          (lookup f fenv)
raw2AExpr env fenv (AbsRawRealPVSLang.ArrayElem (AbsRawRealPVSLang.VarId v) idxExpr) = AbsPVSLang.ArrayElem fp size v (raw2AExpr env fenv idxExpr)
  where 
    fp = fromMaybe
          (error $ "raw2FAExpr: variable " ++ show v ++ " not found.")
          (lookup v env)
    size = case lookup v env of
            Just (Array _ sz) -> sz
            Nothing -> error $ "raw2FAExpr: variable "++ show v ++ " not found."
            _ -> error $ "raw2FAExpr: something went wrong "++ show v ++ " is not an array."
raw2AExpr _ _ Pi1           = error "Constant Pi not supported, use, for instance, 3.14"
raw2AExpr _ _ Pi2           = error "Constant Pi not supported, use, for instance, 3.14"
raw2AExpr _ _ ae = error $ "raw2AExpr niy for " ++ show ae

raw2BExpr :: VarTypeEnv -> FunTypeEnv -> AbsRawRealPVSLang.BExpr -> AbsPVSLang.BExpr 
raw2BExpr env fenv (AbsRawRealPVSLang.Or  be1 be2) = AbsPVSLang.Or  (raw2BExpr env fenv be1) (raw2BExpr env fenv be2)
raw2BExpr env fenv (AbsRawRealPVSLang.And be1 be2) = AbsPVSLang.And (raw2BExpr env fenv be1) (raw2BExpr env fenv be2)
raw2BExpr env fenv (AbsRawRealPVSLang.Not be)      = AbsPVSLang.Not (raw2BExpr env fenv be)
raw2BExpr env fenv (AbsRawRealPVSLang.Eq  ae1 ae2) = AbsPVSLang.Eq  (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2) 
raw2BExpr env fenv (AbsRawRealPVSLang.Neq ae1 ae2) = AbsPVSLang.Neq (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2) 
raw2BExpr env fenv (AbsRawRealPVSLang.Lt  ae1 ae2) = AbsPVSLang.Lt  (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2) 
raw2BExpr env fenv (AbsRawRealPVSLang.LtE ae1 ae2) = AbsPVSLang.LtE (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2) 
raw2BExpr env fenv (AbsRawRealPVSLang.Gt  ae1 ae2) = AbsPVSLang.Gt  (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2) 
raw2BExpr env fenv (AbsRawRealPVSLang.GtE ae1 ae2) = AbsPVSLang.GtE (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2) 
raw2BExpr _   _     AbsRawRealPVSLang.BTrue        = AbsPVSLang.BTrue
raw2BExpr _   _     AbsRawRealPVSLang.BFalse       = AbsPVSLang.BFalse

