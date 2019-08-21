module Translation.Float2Real where

import AbsPVSLang
import FPrec
import Data.Function (fix)

fp2realVarName :: VarName -> VarName
fp2realVarName x = "r_"++x

fp2realFunName :: FunName -> FunName
fp2realFunName f = f++"_real"

fp2realArg :: Arg -> Arg
fp2realArg (Arg x TInt)     = Arg (fp2realVarName x) TInt
fp2realArg (Arg x FPSingle) = Arg (fp2realVarName x) Real
fp2realArg (Arg x FPDouble) = Arg (fp2realVarName x) Real
fp2realArg (Arg x fp)       = error $ "fp2realArg: unexpected type " ++ show fp ++ " for argument "++ show x

fp2realProg :: Program -> RProgram
fp2realProg decls = map fp2realDecl decls

fp2realDecl :: Decl -> RDecl
fp2realDecl (Decl fp f xs stm) = RDecl retType (fp2realFunName f) (map fp2realArg xs) (fp2realStm stm)
    where
        retType = if fp == TInt then TInt
                  else Real

fp2realStm :: Stm -> RStm
fp2realStm UnstWarning                = RUnstWarning
fp2realStm (StmExpr fae)              = RStmExpr (fae2real fae)
fp2realStm (Let x TInt fae stm)       = RLet (fp2realVarName x) TInt (fae2real fae) (fp2realStm stm)
fp2realStm (Let x _ fae stm)          = RLet (fp2realVarName x) Real (fae2real fae) (fp2realStm stm)
fp2realStm (Ite fbe stm1 stm2)        = RIte (fbe2be fbe) (fp2realStm stm1) (fp2realStm stm2)
fp2realStm (ListIte thenList stmElse) = RListIte (map fp2realItePair thenList) (fp2realStm stmElse)
    where
        fp2realItePair (fbe, stm) = (fbe2be fbe, fp2realStm stm)
fp2realStm (ForLoop retType startIdx endIdx initValueAcc idx acc forBody) =
  RForLoop newRetType (fae2real startIdx) (fae2real endIdx) (fae2real initValueAcc) (fp2realVarName idx) (fp2realVarName acc) (fp2realStm forBody)
  where
    newRetType = if retType == TInt
                   then TInt
                   else Real

fbe2be :: FBExpr -> BExpr
fbe2be (FOr b1 b2)  = Or  (fbe2be b1) (fbe2be b2)
fbe2be (FAnd b1 b2) = And (fbe2be b1) (fbe2be b2)
fbe2be (FNot b)     = Not (fbe2be b)
fbe2be (FEq a1 a2)  = Eq  (fae2real a1) (fae2real a2)
fbe2be (FNeq a1 a2) = Neq (fae2real a1) (fae2real a2)
fbe2be (FLt a1 a2)  = Lt  (fae2real a1) (fae2real a2)
fbe2be (FLtE a1 a2) = LtE (fae2real a1) (fae2real a2)
fbe2be (FGt a1 a2)  = Gt  (fae2real a1) (fae2real a2)
fbe2be (FGtE a1 a2) = GtE (fae2real a1) (fae2real a2)
fbe2be FBTrue       = BTrue
fbe2be FBFalse      = BFalse
fbe2be be = error $ "fbe2be: unexpected value " ++ show be ++ "."

fae2real :: FAExpr -> AExpr
fae2real = fix fae2real_rec

fae2real_rec :: (FAExpr -> AExpr) -> FAExpr -> AExpr
fae2real_rec f (FAdd _ a1 a2)     = Add   (f a1) (f a2)
fae2real_rec f (FSub _ a1 a2)     = Sub   (f a1) (f a2)
fae2real_rec f (FMul _ a1 a2)     = Mul   (f a1) (f a2)
fae2real_rec f (FDiv _ a1 a2)     = Div   (f a1) (f a2)
fae2real_rec f (FPow _ a1 a2)     = Pow   (f a1) (f a2)
fae2real_rec f (FMod _ a1 a2)     = Mod   (f a1) (f a2)
fae2real_rec f (FIAdd a1 a2)      = Add   (f a1) (f a2)
fae2real_rec f (FISub a1 a2)      = Sub   (f a1) (f a2)
fae2real_rec f (FIMul a1 a2)      = Mul   (f a1) (f a2)
fae2real_rec f (FIDiv a1 a2)      = IDiv  (f a1) (f a2)
fae2real_rec f (FItDiv a1 a2)     = ItDiv (f a1) (f a2)
fae2real_rec f (FIMod a1 a2)      = IMod  (f a1) (f a2)
fae2real_rec f (FItMod a1 a2)     = ItMod (f a1) (f a2)
fae2real_rec f (FIPow a1 a2)      = Pow   (f a1) (f a2)
fae2real_rec f (FFma _ a1 a2 a3)  = Fma   (f a1) (f a2) (f a3)
fae2real_rec f (FINeg a)          = Neg   (f a)
fae2real_rec f (FIAbs a)          = Abs   (f a)
fae2real_rec f (FNeg   _ a)       = Neg   (f a)
fae2real_rec f (FFloor _ a)       = Floor (f a)
fae2real_rec f (FSqrt  _ a)       = Sqrt  (f a)
fae2real_rec f (FAbs   _ a)       = Abs   (f a)
fae2real_rec f (FSin   _ a)       = Sin   (f a)
fae2real_rec f (FCos   _ a)       = Cos   (f a)
fae2real_rec f (FTan   _ a)       = Tan   (f a)
fae2real_rec f (FAcos  _ a)       = ACos  (f a)
fae2real_rec f (FAsin  _ a)       = ASin  (f a)
fae2real_rec f (FAtan  _ a)       = ATan  (f a)
fae2real_rec f (FLn    _ a)       = Ln    (f a)
fae2real_rec f (FExpo  _ a)       = Expo  (f a)
fae2real_rec f (FIExp a1 a2)      = Expt  (f a1) (f a2)
fae2real_rec _ (FInt n)           = Int n
fae2real_rec _ (FCnst FPSingle n) = StoR (FCnst FPSingle n)
fae2real_rec _ (FCnst FPDouble n) = DtoR (FCnst FPDouble n)
fae2real_rec _ (FCnst TInt _)     = error $ "fae2real: unexpected TInt value for FCnst"
fae2real_rec _ (FCnst Real _)     = error $ "fae2real: unexpected Real value for FCnst"
fae2real_rec f (FEFun f' TInt args)  = EFun f' TInt (map f args)
fae2real_rec f (FEFun f' _ args)  = EFun f' Real (map f args)
fae2real_rec _ (FVar _ x)         = RealMark x
fae2real_rec f (FMin as)          = Min (map f as)
fae2real_rec f (FMax as)          = Max (map f as)
fae2real_rec _ (RtoS a)           = a
fae2real_rec _ (RtoD a)           = a
fae2real_rec _ (DtoS a)           = fae2real a
fae2real_rec _ (StoD a)           = fae2real a
fae2real_rec _ (ItoS a)           = fae2real a
fae2real_rec _ (ItoD a)           = fae2real a
fae2real_rec f (FArrayElem fp size v idx) = ArrayElem fp size v (f idx)

fae2real_rec _ ae = error $ "fae2real_rec not defined for " ++ show ae

