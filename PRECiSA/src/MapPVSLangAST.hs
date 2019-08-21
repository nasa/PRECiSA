module MapPVSLangAST
where

import AbsRawPVSLang
import AbsPVSLang
--import Debug.Trace
import Data.Maybe(fromMaybe)
import ErrM
import FPrec
import Parser.ParRawPVSLang
import Parser.LexRawPVSLang

type VarTypeEnv = [(String, FPrec)]
type FunTypeEnv = [(String, FPrec)]

raw2Prog :: AbsRawPVSLang.Program -> AbsPVSLang.Program
raw2Prog (AbsRawPVSLang.Prog    _ _ _ listDecl _) = raw2Decsl (map retTypeFun listDecl) listDecl
raw2Prog (AbsRawPVSLang.ProgImp _ _ listDecl _)   = raw2Decsl (map retTypeFun listDecl) listDecl

retTypeFun :: AbsRawPVSLang.Decl -> (String, FPrec)
retTypeFun (Decl0 (AbsRawPVSLang.NonVarId f)   fp _) = (f, raw2FPType fp)
retTypeFun (DeclN (AbsRawPVSLang.NonVarId f) _ fp _) = (f, raw2FPType fp)

raw2Decsl :: FunTypeEnv -> [AbsRawPVSLang.Decl] -> [AbsPVSLang.Decl]
raw2Decsl fenv = map (raw2Decl fenv)

raw2Decl :: FunTypeEnv -> AbsRawPVSLang.Decl -> AbsPVSLang.Decl
raw2Decl fenv (DeclN f rawArgs fptype stm) = Decl (raw2FPType fptype) (raw2NonVarId f) args (raw2Stm env fenv stm)
    where
        args = raw2Args rawArgs
        env  = map mapArg2Pair args
raw2Decl fenv (Decl0 f      fptype stm) = Decl (raw2FPType fptype) (raw2NonVarId f) [] (raw2Stm [] fenv stm)

raw2Elsif :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.ElsIf -> (AbsPVSLang.FBExpr, AbsPVSLang.Stm)
raw2Elsif env fenv (ElsIf fbexpr stm) = (raw2FBExpr env fenv fbexpr, raw2Stm env fenv stm)

raw2Stm :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Stm -> AbsPVSLang.Stm
raw2Stm env fenv (AbsRawPVSLang.Let rawVar@(AbsRawPVSLang.VarId x) aexpr stm) =
--    trace ("Variable "++ show x ++ " type set to FPDouble.\n") $
    AbsPVSLang.Let var FPrec.FPDouble (raw2FAExpr env fenv aexpr) (raw2Stm ((var, FPrec.FPDouble):env) fenv stm)
    where
        var = raw2VarId rawVar

raw2Stm env fenv (AbsRawPVSLang.LetWithType x t aexpr stm) =
    AbsPVSLang.Let (raw2VarId x) fp (raw2FAExpr env fenv aexpr) (raw2Stm ((var, fp):env) fenv stm)
    where
        var = raw2VarId x
        fp = raw2FPType t

raw2Stm env fenv (If bexpr stmThen stmElse) = 
    Ite (raw2FBExpr env fenv bexpr) (raw2Stm env fenv stmThen) (raw2Stm env fenv stmElse)

raw2Stm env fenv (ListIf bexpr stmThen listElsif stmElse) =
    ListIte ((raw2FBExpr env fenv bexpr,raw2Stm env fenv stmThen): map (raw2Elsif env fenv) listElsif) (raw2Stm env fenv stmElse)

raw2Stm env fenv (Expr aexpr)         = StmExpr(raw2FAExpr env fenv aexpr)
raw2Stm _ _ AbsRawPVSLang.UnstWarning = AbsPVSLang.UnstWarning
raw2Stm _ _ For{} = undefined -- change for in 

raw2Args :: AbsRawPVSLang.Args -> [AbsPVSLang.Arg]
raw2Args (FArgs args) = concatMap raw2Arg args

raw2Arg :: AbsRawPVSLang.Arg -> [AbsPVSLang.Arg]
raw2Arg (FArg xs t)         = map (raw2ArgWithType t) xs
raw2Arg (FArgSubrange xs _) = map (raw2ArgWithType TypeInteger) xs
raw2Arg (FArgGuard xs t _)  = map (raw2ArgWithType t) xs
raw2Arg (FArgNoType x)      = error ("Variable "++ show x ++ "has no type.")

raw2ArgWithType :: FPtype -> AbsRawPVSLang.VarId -> AbsPVSLang.Arg
raw2ArgWithType t x = AbsPVSLang.Arg (raw2VarId x) (raw2FPType t)

raw2VarId :: AbsRawPVSLang.VarId -> VarName
raw2VarId (AbsRawPVSLang.VarId x) = x

raw2NonVarId :: AbsRawPVSLang.NonVarId -> FunName
raw2NonVarId (AbsRawPVSLang.NonVarId x) = x

raw2FPType :: AbsRawPVSLang.FPtype -> FPrec
raw2FPType TypeInt      = TInt
raw2FPType TypeInteger  = TInt
raw2FPType UnbSingle    = FPSingle 
raw2FPType UnbDouble    = FPDouble 
raw2FPType UnbPosSingle = FPSingle 
raw2FPType UnbPosDouble = FPDouble 
raw2FPType UnbNzSingle  = FPSingle 
raw2FPType UnbNzDouble  = FPDouble 

raw2AExpr :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.AExpr -> AbsPVSLang.AExpr
raw2AExpr env fenv (AbsRawPVSLang.Add ae1 ae2) = AbsPVSLang.Add   (raw2AExpr  env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawPVSLang.Sub ae1 ae2) = AbsPVSLang.Sub   (raw2AExpr  env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawPVSLang.Mul ae1 ae2) = AbsPVSLang.Mul   (raw2AExpr  env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawPVSLang.Div ae1 ae2) = AbsPVSLang.Div   (raw2AExpr  env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawPVSLang.Pow ae1 ae2) = AbsPVSLang.Pow   (raw2AExpr  env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawPVSLang.Mod ae1 ae2) = AbsPVSLang.Mod   (raw2AExpr  env fenv ae1) (raw2AExpr env fenv ae2)
raw2AExpr env fenv (AbsRawPVSLang.Neg     ae)  = AbsPVSLang.Neg   (raw2AExpr  env fenv ae)
raw2AExpr env fenv (AbsRawPVSLang.Floor   ae)  = AbsPVSLang.Floor (raw2AExpr  env fenv ae)
raw2AExpr env fenv (AbsRawPVSLang.Sqrt    ae)  = AbsPVSLang.Sqrt  (raw2AExpr  env fenv ae)
raw2AExpr env fenv (AbsRawPVSLang.Abs     ae)  = AbsPVSLang.Abs   (raw2AExpr  env fenv ae)
raw2AExpr env fenv (AbsRawPVSLang.Sin     ae)  = AbsPVSLang.Sin   (raw2AExpr  env fenv ae)
raw2AExpr env fenv (AbsRawPVSLang.Cos     ae)  = AbsPVSLang.Cos   (raw2AExpr  env fenv ae)
raw2AExpr env fenv (AbsRawPVSLang.Tan     ae)  = AbsPVSLang.Tan   (raw2AExpr  env fenv ae)
raw2AExpr env fenv (AbsRawPVSLang.ASin    ae)  = AbsPVSLang.ASin  (raw2AExpr  env fenv ae)
raw2AExpr env fenv (AbsRawPVSLang.ACos    ae)  = AbsPVSLang.ACos  (raw2AExpr  env fenv ae)
raw2AExpr env fenv (AbsRawPVSLang.ATan    ae)  = AbsPVSLang.ATan  (raw2AExpr  env fenv ae)
raw2AExpr env fenv (AbsRawPVSLang.Ln      ae)  = AbsPVSLang.Ln    (raw2AExpr  env fenv ae)
raw2AExpr env fenv (AbsRawPVSLang.Exp     ae)  = AbsPVSLang.Expo  (raw2AExpr  env fenv ae)
raw2AExpr env fenv (AbsRawPVSLang.StoR   fae)  = AbsPVSLang.StoR  (raw2FAExpr env fenv fae)
raw2AExpr env fenv (AbsRawPVSLang.DtoR   fae)  = AbsPVSLang.DtoR  (raw2FAExpr env fenv fae)
raw2AExpr _ _ (AbsRawPVSLang.Int    i)    = AbsPVSLang.Int i
raw2AExpr _ _ (AbsRawPVSLang.Rat d)       = AbsPVSLang.Rat (toRational d)
raw2AExpr _ _ Pi1           = error "Constant Pi not supported, use, for instance, 3.14"
raw2AExpr _ _ Pi2           = error "Constant Pi not supported, use, for instance, 3.14"

raw2FAExpr :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.FAExpr -> AbsPVSLang.FAExpr 
raw2FAExpr env fenv (AbsRawPVSLang.SAdd fae1 fae2) = AbsPVSLang.FAdd FPSingle   (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.DAdd fae1 fae2) = AbsPVSLang.FAdd FPDouble   (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.IAdd fae1 fae2) = AbsPVSLang.FIAdd           (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.SSub fae1 fae2) = AbsPVSLang.FSub FPSingle   (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.DSub fae1 fae2) = AbsPVSLang.FSub FPDouble   (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.ISub fae1 fae2) = AbsPVSLang.FISub           (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.SMul fae1 fae2) = AbsPVSLang.FMul FPSingle   (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.DMul fae1 fae2) = AbsPVSLang.FMul FPDouble   (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.IMul fae1 fae2) = AbsPVSLang.FIMul           (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.SDiv fae1 fae2) = AbsPVSLang.FDiv FPSingle   (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.DDiv fae1 fae2) = AbsPVSLang.FDiv FPDouble   (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.IDiv fae1 fae2) = AbsPVSLang.FIDiv           (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.SMod fae1 fae2) = AbsPVSLang.FMod FPSingle   (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.DMod fae1 fae2) = AbsPVSLang.FMod FPDouble   (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.IMod fae1 fae2) = AbsPVSLang.FIMod           (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.FPow fae1 fae2) = AbsPVSLang.FPow FPSingle   (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FAExpr env fenv (AbsRawPVSLang.SNeg      fae)  = AbsPVSLang.FNeg FPSingle   (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DNeg      fae)  = AbsPVSLang.FNeg FPDouble   (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.INeg      fae)  = AbsPVSLang.FINeg           (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SFloor    fae)  = AbsPVSLang.FFloor FPSingle (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DFloor    fae)  = AbsPVSLang.FFloor FPDouble (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SSqrt     fae)  = AbsPVSLang.FSqrt  FPSingle (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DSqrt     fae)  = AbsPVSLang.FSqrt  FPDouble (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SAbs      fae)  = AbsPVSLang.FAbs   FPSingle (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DAbs      fae)  = AbsPVSLang.FAbs   FPDouble (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.IAbs      fae)  = AbsPVSLang.FIAbs           (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SSin      fae)  = AbsPVSLang.FSin  FPSingle  (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DSin      fae)  = AbsPVSLang.FSin  FPDouble  (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SCos      fae)  = AbsPVSLang.FCos  FPSingle  (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DCos      fae)  = AbsPVSLang.FCos  FPDouble  (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.STan      fae)  = AbsPVSLang.FTan  FPSingle  (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DTan      fae)  = AbsPVSLang.FTan  FPDouble  (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SAcos     fae)  = AbsPVSLang.FAcos FPSingle  (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DAcos     fae)  = AbsPVSLang.FAcos FPDouble  (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SAsin     fae)  = AbsPVSLang.FAsin FPSingle  (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DAsin     fae)  = AbsPVSLang.FAsin FPDouble  (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SAtan     fae)  = AbsPVSLang.FAtan FPSingle  (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DAtan     fae)  = AbsPVSLang.FAtan FPDouble  (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SLn       fae)  = AbsPVSLang.FLn   FPSingle  (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DLn       fae)  = AbsPVSLang.FLn   FPDouble  (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.SExp      fae)  = AbsPVSLang.FExpo FPSingle  (raw2FAExpr env fenv fae)
raw2FAExpr env fenv (AbsRawPVSLang.DExp      fae)  = AbsPVSLang.FExpo FPDouble  (raw2FAExpr env fenv fae)
--------
raw2FAExpr   _ _ (AbsRawPVSLang.FNegN (AbsRawPVSLang.FInt i))        = AbsPVSLang.FInt (-i)
raw2FAExpr   _ _ (AbsRawPVSLang.FNegN (AbsRawPVSLang.RtoD (AbsRawPVSLang.Int i)))  = AbsPVSLang.RtoD $ AbsPVSLang.Int (-i)
raw2FAExpr   _ _ (AbsRawPVSLang.FNegN (AbsRawPVSLang.RtoS (AbsRawPVSLang.Int i)))  = AbsPVSLang.RtoS $ AbsPVSLang.Int (-i)
raw2FAExpr   _ _ (AbsRawPVSLang.FNegN (AbsRawPVSLang.RtoD (AbsRawPVSLang.Rat d)))  = AbsPVSLang.RtoD $ AbsPVSLang.Rat (toRational (-d))
raw2FAExpr   _ _ (AbsRawPVSLang.FNegN (AbsRawPVSLang.RtoS (AbsRawPVSLang.Rat d)))  = AbsPVSLang.RtoS $ AbsPVSLang.Rat (toRational (-d))
raw2FAExpr env fenv (AbsRawPVSLang.FNegN fae)  = AbsPVSLang.FNeg  FPDouble (raw2FAExpr env fenv fae)
--------
raw2FAExpr env fenv (AbsRawPVSLang.FCallN (AbsRawPVSLang.NonVarId f) actArgs)  = AbsPVSLang.FEFun f fp (map (raw2FAExpr env fenv) actArgs)
    where
        fp = fromMaybe (error $ "raw2FAExpr: function " ++ show f ++ " not found.")
                      (lookup f fenv)
raw2FAExpr _ fenv (AbsRawPVSLang.FCall0 (AbsRawPVSLang.NonVarId f)) = AbsPVSLang.FEFun f fp []
    where
        fp = fromMaybe (error $ "raw2FAExpr: function " ++ show f ++ " not found.")
                       (lookup f fenv)
raw2FAExpr   _ _ (AbsRawPVSLang.RtoS (AbsRawPVSLang.Int i)) = AbsPVSLang.RtoS $ AbsPVSLang.Int i
raw2FAExpr   _ _ (AbsRawPVSLang.RtoD (AbsRawPVSLang.Int i)) = AbsPVSLang.RtoD $ AbsPVSLang.Int i
raw2FAExpr   _ _ (AbsRawPVSLang.RtoS (AbsRawPVSLang.Rat d)) = AbsPVSLang.RtoS $ AbsPVSLang.Rat (toRational d)
raw2FAExpr   _ _ (AbsRawPVSLang.RtoD (AbsRawPVSLang.Rat d)) = AbsPVSLang.RtoD $ AbsPVSLang.Rat (toRational d) 
raw2FAExpr   _ _ (AbsRawPVSLang.RtoS (AbsRawPVSLang.Neg(AbsRawPVSLang.Int i))) = AbsPVSLang.RtoS $ AbsPVSLang.Int (-i)
raw2FAExpr   _ _ (AbsRawPVSLang.RtoD (AbsRawPVSLang.Neg(AbsRawPVSLang.Int i))) = AbsPVSLang.RtoD $ AbsPVSLang.Int (-i)
raw2FAExpr   _ _ (AbsRawPVSLang.RtoS (AbsRawPVSLang.Neg(AbsRawPVSLang.Rat d))) = AbsPVSLang.RtoS $ AbsPVSLang.Rat (toRational (-d))
raw2FAExpr   _ _ (AbsRawPVSLang.RtoD (AbsRawPVSLang.Neg(AbsRawPVSLang.Rat d))) = AbsPVSLang.RtoD $ AbsPVSLang.Rat (toRational (-d)) 
raw2FAExpr   _ _ (AbsRawPVSLang.RtoS ae) = error $ "RtoS not defined for expression " ++ show ae
raw2FAExpr   _ _ (AbsRawPVSLang.RtoD ae) = error $ "RtoD not defined for expression " ++ show ae
raw2FAExpr   _ _ (AbsRawPVSLang.ItoS  _) = error "niy: raw2FAExpr for ItoS"
raw2FAExpr   _ _ (AbsRawPVSLang.ItoD  _) = error "niy: raw2FAExpr for ItoD"
raw2FAExpr   env _ (AbsRawPVSLang.Var (AbsRawPVSLang.VarId x))  = AbsPVSLang.FVar fp x
    where 
        fp = fromMaybe (error $ "raw2FAExpr: variable " ++ show x ++ " not found.")
                       (lookup x env)
raw2FAExpr  _ _ (AbsRawPVSLang.FInt n) = AbsPVSLang.FInt n

raw2FBExpr :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.FBExpr -> AbsPVSLang.FBExpr 
raw2FBExpr env fenv (AbsRawPVSLang.FOr  fbe1 fbe2) = AbsPVSLang.FOr  (raw2FBExpr env fenv fbe1) (raw2FBExpr env fenv fbe2)
raw2FBExpr env fenv (AbsRawPVSLang.FAnd fbe1 fbe2) = AbsPVSLang.FAnd (raw2FBExpr env fenv fbe1) (raw2FBExpr env fenv fbe2)
raw2FBExpr env fenv (AbsRawPVSLang.FNot       fbe) = AbsPVSLang.FNot (raw2FBExpr env fenv fbe)
raw2FBExpr env fenv (AbsRawPVSLang.FEq  fae1 fae2) = AbsPVSLang.FEq  (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FBExpr env fenv (AbsRawPVSLang.FNeq fae1 fae2) = AbsPVSLang.FNeq (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FBExpr env fenv (AbsRawPVSLang.FLt  fae1 fae2) = AbsPVSLang.FLt  (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FBExpr env fenv (AbsRawPVSLang.FLtE fae1 fae2) = AbsPVSLang.FLtE (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FBExpr env fenv (AbsRawPVSLang.FGt  fae1 fae2) = AbsPVSLang.FGt  (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FBExpr env fenv (AbsRawPVSLang.FGtE fae1 fae2) = AbsPVSLang.FGtE (raw2FAExpr env fenv fae1) (raw2FAExpr env fenv fae2)
raw2FBExpr   _    _  AbsRawPVSLang.FBTrue          = AbsPVSLang.FBTrue
raw2FBExpr   _    _  AbsRawPVSLang.FBFalse         = AbsPVSLang.FBFalse

raw2BExpr :: VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.BExpr -> AbsPVSLang.BExpr 
raw2BExpr env fenv (AbsRawPVSLang.Or  be1 be2) = AbsPVSLang.Or  (raw2BExpr env fenv be1) (raw2BExpr env fenv be2)
raw2BExpr env fenv (AbsRawPVSLang.And be1 be2) = AbsPVSLang.And (raw2BExpr env fenv be1) (raw2BExpr env fenv be2)
raw2BExpr env fenv (AbsRawPVSLang.Not be)      = AbsPVSLang.Not (raw2BExpr env fenv be)
raw2BExpr env fenv (AbsRawPVSLang.Eq  ae1 ae2) = AbsPVSLang.Eq  (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2) 
raw2BExpr env fenv (AbsRawPVSLang.Neq ae1 ae2) = AbsPVSLang.Neq (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2) 
raw2BExpr env fenv (AbsRawPVSLang.Lt  ae1 ae2) = AbsPVSLang.Lt  (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2) 
raw2BExpr env fenv (AbsRawPVSLang.LtE ae1 ae2) = AbsPVSLang.LtE (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2) 
raw2BExpr env fenv (AbsRawPVSLang.Gt  ae1 ae2) = AbsPVSLang.Gt  (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2) 
raw2BExpr env fenv (AbsRawPVSLang.GtE ae1 ae2) = AbsPVSLang.GtE (raw2AExpr env fenv ae1) (raw2AExpr env fenv ae2) 
raw2BExpr   _ _ AbsRawPVSLang.BTrue        = AbsPVSLang.BTrue
raw2BExpr   _ _ AbsRawPVSLang.BFalse       = AbsPVSLang.BFalse

rawparserPVS :: String -> Err AbsRawPVSLang.Program
rawparserPVS = pProgram . tokens

--mapArg2Pair :: AbsPVSLang.Arg -> (String, FPrec)
--mapArg2Pair (Arg (AbsPVSLang.VarId x) fp) = (x,fp)
