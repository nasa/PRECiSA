module Translation.Real2Float where

import AbsPVSLang
import FPrec

real2fpArg :: FPrec -> Arg -> Arg
real2fpArg  _ (Arg x  TInt) = Arg x TInt
real2fpArg fp (Arg x  Real) = Arg x fp
real2fpArg _  (Arg x (Array TInt size)) = Arg x (Array TInt size)
real2fpArg fp (Arg x (Array Real size)) = Arg x (Array fp size)
real2fpArg _  (Arg _  t) = error $ "real2fpArg not defined for " ++ show t

real2fpProg :: FPrec -> RProgram -> Program
real2fpProg fp decls = map (real2fpDecl fp) decls

real2fpDecl :: FPrec -> RDecl -> Decl
real2fpDecl fp (RDecl retType f xs stm) = (Decl newRetType f (map (real2fpArg fp) xs) (real2fpStm fp stm)) 
  where
    newRetType = if retType == TInt then TInt else fp

real2fpStm :: FPrec -> RStm -> Stm 
real2fpStm fp (RLet x t ae stm)           = Let x newFp (real2fpAexpr fp ae) (real2fpStm fp stm)
    where
      newFp = if t == TInt then TInt else fp
real2fpStm fp (RIte be thenStm elseStm)   = Ite (real2fpBexpr fp be) (real2fpStm fp thenStm) (real2fpStm fp elseStm)
real2fpStm fp (RListIte thenList stmElse) = ListIte (map real2fpItePair thenList) (real2fpStm fp stmElse)
  where
    real2fpItePair (fbe, stm) = (real2fpBexpr fp fbe, real2fpStm fp stm)
real2fpStm fp (RStmExpr ae)               = StmExpr $ real2fpAexpr fp ae
real2fpStm fp (RForLoop retType startIdx endIdx initValueAcc idx acc forBody) =
    ForLoop retType (real2fpAexpr fp startIdx) (real2fpAexpr fp endIdx) (real2fpAexpr fp initValueAcc) idx acc (real2fpStm fp forBody)
real2fpStm  _  RUnstWarning               = UnstWarning

real2fpBexpr :: FPrec -> BExpr -> FBExpr
real2fpBexpr fp (Or  b1 b2) = FOr  (real2fpBexpr fp b1) (real2fpBexpr fp b2)
real2fpBexpr fp (And b1 b2) = FAnd (real2fpBexpr fp b1) (real2fpBexpr fp b2)
real2fpBexpr fp (Not b)     = FNot (real2fpBexpr fp b) 
real2fpBexpr fp (Eq  a1 a2) = FEq  (real2fpAexpr fp a1) (real2fpAexpr fp a2)
real2fpBexpr fp (Neq a1 a2) = FNeq (real2fpAexpr fp a1) (real2fpAexpr fp a2)
real2fpBexpr fp (Lt  a1 a2) = FLt  (real2fpAexpr fp a1) (real2fpAexpr fp a2)
real2fpBexpr fp (LtE a1 a2) = FLtE (real2fpAexpr fp a1) (real2fpAexpr fp a2)
real2fpBexpr fp (Gt  a1 a2) = FGt  (real2fpAexpr fp a1) (real2fpAexpr fp a2)
real2fpBexpr fp (GtE a1 a2) = FGtE (real2fpAexpr fp a1) (real2fpAexpr fp a2)
real2fpBexpr  _ BTrue  = FBTrue
real2fpBexpr  _ BFalse = FBFalse

isIntAExpr :: AExpr -> Bool
isIntAExpr (Add ae1 ae2) = (isIntAExpr ae1) && (isIntAExpr ae2)
isIntAExpr (Sub ae1 ae2) = (isIntAExpr ae1) && (isIntAExpr ae2)
isIntAExpr (Mul ae1 ae2) = (isIntAExpr ae1) && (isIntAExpr ae2)
isIntAExpr (Pow ae1 ae2) = (isIntAExpr ae1) && (isIntAExpr ae2)
isIntAExpr (Mod ae1 ae2) = (isIntAExpr ae1) && (isIntAExpr ae2)
isIntAExpr (Neg ae) = isIntAExpr ae
isIntAExpr (Int _) = True
isIntAExpr (Rat n) = toRational (floor $ (fromRational n :: Double) :: Integer) == n
isIntAExpr (Var TInt _) = True
isIntAExpr (IDiv  ae1 ae2) = (isIntAExpr ae1) && (isIntAExpr ae2)
isIntAExpr (ItDiv ae1 ae2) = (isIntAExpr ae1) && (isIntAExpr ae2)
isIntAExpr (IMod  ae1 ae2) = (isIntAExpr ae1) && (isIntAExpr ae2)
isIntAExpr (ItMod ae1 ae2) = (isIntAExpr ae1) && (isIntAExpr ae2)
isIntAExpr _ = False


real2fpAexpr :: FPrec -> AExpr -> FAExpr
--
real2fpAexpr TInt (Add   ae1 ae2) = FIAdd (real2fpAexpr TInt ae1) (real2fpAexpr TInt ae2)
real2fpAexpr fp   (Add   ae1 ae2)
    | (isIntAExpr ae1) && (isIntAExpr ae2) = FIAdd  (real2fpAexpr fp ae1) (real2fpAexpr fp ae2)
    | otherwise                            = FAdd  fp (real2fpAexpr fp ae1) (real2fpAexpr fp ae2)
real2fpAexpr TInt (Sub   ae1 ae2) = FISub (real2fpAexpr TInt ae1) (real2fpAexpr TInt ae2)
real2fpAexpr fp   (Sub   ae1 ae2)
    | (isIntAExpr ae1) && (isIntAExpr ae2) = FISub   (real2fpAexpr fp ae1) (real2fpAexpr fp ae2)
    | otherwise                            = FSub fp (real2fpAexpr fp ae1) (real2fpAexpr fp ae2)
real2fpAexpr TInt (Mul   ae1 ae2) = FIMul (real2fpAexpr TInt ae1) (real2fpAexpr TInt ae2)
real2fpAexpr fp   (Mul   ae1 ae2)
    | (isIntAExpr ae1) && (isIntAExpr ae2) = FIMul   (real2fpAexpr fp ae1) (real2fpAexpr fp ae2)
    | otherwise                            = FMul fp (real2fpAexpr fp ae1) (real2fpAexpr fp ae2)
real2fpAexpr fp (Div   ae1 ae2)   = FDiv fp (real2fpAexpr fp ae1) (real2fpAexpr fp ae2)
real2fpAexpr fp (Pow   ae1 ae2)   = FPow fp (real2fpAexpr fp ae1) (real2fpAexpr fp ae2)
real2fpAexpr fp (Mod   ae1 ae2)   = FMod fp (real2fpAexpr fp ae1) (real2fpAexpr fp ae2)
real2fpAexpr fp (IDiv  ae1 ae2)   = FIDiv   (real2fpAexpr fp ae1) (real2fpAexpr fp ae2)
real2fpAexpr fp (ItDiv ae1 ae2)   = FItDiv  (real2fpAexpr fp ae1) (real2fpAexpr fp ae2)
real2fpAexpr fp (IMod  ae1 ae2)   = FIMod   (real2fpAexpr fp ae1) (real2fpAexpr fp ae2)
real2fpAexpr fp (ItMod ae1 ae2)   = FItMod  (real2fpAexpr fp ae1) (real2fpAexpr fp ae2)
real2fpAexpr fp (Expt ae1 ae2)    = FIExp   (real2fpAexpr fp ae1) (real2fpAexpr fp ae2)
real2fpAexpr fp (Fma ae1 ae2 ae3) = FFma fp (real2fpAexpr fp ae1) (real2fpAexpr fp ae2) (real2fpAexpr fp ae3)
real2fpAexpr fp (Neg ae)          = FNeg   fp (real2fpAexpr fp ae)
real2fpAexpr fp (Floor ae)        = FFloor fp (real2fpAexpr fp ae)
real2fpAexpr fp (Sqrt  ae)        = FSqrt  fp (real2fpAexpr fp ae)
real2fpAexpr fp (Abs   ae)        = FAbs   fp (real2fpAexpr fp ae)
real2fpAexpr fp (Sin   ae)        = FSin   fp (real2fpAexpr fp ae)
real2fpAexpr fp (Cos   ae)        = FCos   fp (real2fpAexpr fp ae)
real2fpAexpr fp (Tan   ae)        = FTan   fp (real2fpAexpr fp ae)
real2fpAexpr fp (ASin ae)         = FAsin  fp (real2fpAexpr fp ae)
real2fpAexpr fp (ACos ae)         = FAcos  fp (real2fpAexpr fp ae)
real2fpAexpr fp (ATan ae)         = FAtan  fp (real2fpAexpr fp ae)
real2fpAexpr fp (Ln   ae)         = FLn    fp (real2fpAexpr fp ae)
real2fpAexpr fp (Expo ae)         = FExpo  fp (real2fpAexpr fp ae)
real2fpAexpr FPSingle (Int n) = RtoS (Int n)
real2fpAexpr FPDouble (Int n) = RtoD (Int n)
real2fpAexpr FPSingle (Rat r) = RtoS (Rat r)
real2fpAexpr FPDouble (Rat r) = RtoD (Rat r)
real2fpAexpr FPSingle (StoR ae) = RtoS (StoR ae)
real2fpAexpr FPDouble (DtoR ae) = RtoD (DtoR ae)
real2fpAexpr _ (Var TInt x) = FVar TInt x
real2fpAexpr fp (Var _ x) = FVar fp x
--real2fpAexpr FPDouble (Var _ x) = FVar FPDouble x
real2fpAexpr fp (ArrayElem TInt size v idxExpr) = FArrayElem TInt size v (real2fpAexpr fp idxExpr)
real2fpAexpr fp (ArrayElem _ size v idxExpr) = FArrayElem fp size v (real2fpAexpr fp idxExpr)
real2fpAexpr fp (EFun f TInt args) = FEFun f TInt (map (real2fpAexpr fp) args)
real2fpAexpr fp (EFun f _ args) = FEFun f fp (map (real2fpAexpr fp) args)
real2fpAexpr _ ae = error $ "real2fpAexpr not defined for " ++ show ae
