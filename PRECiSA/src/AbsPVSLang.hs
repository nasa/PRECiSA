-- Notices:
--
-- Copyright 2017 United States Government as represented by the Administrator of the National Aeronautics and Space Administration.
-- All Rights Reserved.
--
-- Disclaimers:
--
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED,
-- IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT
-- SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS,
-- HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
--
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT,
-- ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING
-- FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH
-- MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module AbsPVSLang
where

import PPExt
import Numeric
import FPrec
import Data.Ratio
import Debug.Trace
import Foreign.C.Types (CDouble, CFloat)
import Data.Bits.Floating

type FunctionName = String
type VarName = String


-- variables and process symbols --
newtype VarId    = VarId String deriving (Eq, Ord, Show, Read)
newtype NonVarId = NonVarId String deriving (Eq, Ord, Show, Read)

data VarDecl = VarDecl VarId
data Imp = Imp [NonVarId]

type EExpr = AExpr

data AExpr
 -- real arithmetic expressions
    = Add AExpr AExpr
    | Sub AExpr AExpr
    | Mul AExpr AExpr
    | Div AExpr AExpr
    | Pow AExpr AExpr
    | Mod AExpr AExpr
    | Neg AExpr
    | Floor AExpr
    | Sqrt AExpr
    | Abs  AExpr
    | Sin  AExpr
    | Cos  AExpr
    | Tan  AExpr
    | ASin AExpr
    | ACos AExpr
    | ATan AExpr
    | Ln   AExpr
    | Expo AExpr
    | Int Integer
    | Double Rational
    | EFun FunctionName [AExpr]
    | Var VarName
    | Pi
    | SUlp AExpr
    | DUlp AExpr
    | StoR FAExpr
    | DtoR FAExpr
    | EE EExpr
    | FPrec
    | FExp FAExpr
    | RealMark VarName
    | Min [AExpr]
    | Max [AExpr]
    | ErrAdd    AExpr EExpr AExpr EExpr
    | ErrSub    AExpr EExpr AExpr EExpr
    | ErrMul    AExpr EExpr AExpr EExpr
    | ErrDiv    AExpr EExpr AExpr EExpr
    | ErrFloor  AExpr EExpr
    | ErrFloor0  AExpr EExpr
    | ErrSqrt   AExpr EExpr 
    | ErrSin    AExpr EExpr 
    | ErrCos    AExpr EExpr 
    | ErrTan    AExpr EExpr 
    | ErrAsin AExpr EExpr
    | ErrAcos AExpr EExpr 
    | ErrAtan AExpr EExpr
    | ErrAtanT   AExpr EExpr 
    | ErrNeg AExpr EExpr
    | ErrAbs AExpr EExpr
    | ErrLn  AExpr EExpr
    | ErrExpo AExpr EExpr
    | ErrMulPow2R Integer EExpr
    | ErrMulPow2L Integer EExpr
    | AE AExpr
    | HalfUlp AExpr
    | ErrRat Rational
    | MaxErr [EExpr]
    | ErrorMark VarName
    | Infinity
    | ErrUndefined
    deriving (Eq, Ord, Show, Read)

data FAExpr
-- fp arithmetic expressions
    = FAdd FAExpr FAExpr
    | FSub FAExpr FAExpr
    | FMul FAExpr FAExpr
    | FDiv FAExpr FAExpr
    | FPow FAExpr FAExpr
    | FMod FAExpr FAExpr
    | FNeg FAExpr
    | FFloor FAExpr
    | FSqrt FAExpr
    | FAbs FAExpr
    | FSin FAExpr
    | FCos FAExpr
    | FTan FAExpr
    | FAcos FAExpr
    | FAsin FAExpr
    | FAtan FAExpr
    | FLn   FAExpr
    | FExpo FAExpr
    | FInt Integer
    | FDouble Rational -- Double
    | FEFun String [FAExpr]
    | FVar String
    | FPi
    | RtoS AExpr
    | RtoD AExpr
    | FMin [FAExpr]
    | FMax [FAExpr]
    deriving (Eq, Ord, Show, Read)

data BExpr
-- real valued boolean expressions 
    = Or  BExpr BExpr
    | And BExpr BExpr
    | Not BExpr
    | Eq  AExpr AExpr
    | Neq AExpr AExpr
    | Lt  AExpr AExpr
    | LtE AExpr AExpr
    | Gt  AExpr AExpr
    | GtE AExpr AExpr
    | BTrue
    | BFalse
    deriving (Eq, Ord, Show, Read) 

data FBExpr
-- fp valued boolean expressions 
    = FOr  FBExpr FBExpr
    | FAnd FBExpr FBExpr
    | FNot FBExpr
    | FEq  FAExpr FAExpr
    | FNeq FAExpr FAExpr
    | FLt  FAExpr FAExpr
    | FLtE FAExpr FAExpr
    | FGt  FAExpr FAExpr
    | FGtE FAExpr FAExpr
    | FBTrue
    | FBFalse
    deriving (Eq, Ord, Show, Read)    

-- progam
newtype Program = Prog [Decl]
    deriving (Eq, Ord, Show, Read)

-- set of declarations
data Decl = Decl FPrec NonVarId [VarId] Stm
    deriving (Eq, Ord, Show, Read)

-- program expression
data Stm = Let VarId FAExpr Stm
         | Ite FBExpr Stm Stm
         | StmExpr FAExpr
         | ForLoop Integer Integer FAExpr NonVarId
    deriving (Eq, Ord, Show, Read)

-- real valued progam
data RProgram = RProg [RDecl]
    deriving (Eq, Ord, Show, Read)

-- real valued set of declarations
data RDecl = RDecl FPrec NonVarId [VarId] RStm
    deriving (Eq, Ord, Show, Read)

-- real valued program expression
data RStm = RLet VarId AExpr RStm
          | RIte BExpr RStm RStm
          | RStmExpr AExpr
          | RForLoop Integer Integer AExpr NonVarId
    deriving (Eq, Ord, Show, Read)

isBExprEquivFalse :: BExpr -> Bool
isBExprEquivFalse BFalse = True
isBExprEquivFalse b = any (flip elem bs . Not) bs
    where
        bs = flatAnd b

isFBExprEquivFalse :: FBExpr -> Bool
isFBExprEquivFalse FBFalse = True
isFBExprEquivFalse b = any (flip elem bs . FNot) bs
    where
        bs = flatFAnd b

flatAnd :: BExpr -> [BExpr]
flatAnd (And b1 b2) = (flatAnd b1) ++ (flatAnd b2)
flatAnd b = [b]

flatFAnd :: FBExpr -> [FBExpr]
flatFAnd (FAnd b1 b2) = (flatFAnd b1) ++ (flatFAnd b2)
flatFAnd b = [b]

--------------------------------------------
-- semantic equivalence error expressions --
--------------------------------------------

rewriteEquivEExpr :: EExpr -> EExpr
rewriteEquivEExpr (ErrMulPow2L _ ee) = rewriteEquivEExpr ee
rewriteEquivEExpr (ErrMulPow2R _ ee) = rewriteEquivEExpr ee
rewriteEquivEExpr (ErrAbs      _ ee) = rewriteEquivEExpr ee
rewriteEquivEExpr (ErrNeg      _ ee) = rewriteEquivEExpr ee
rewriteEquivEExpr (MaxErr       ees)
    | and $ zipWith (==) ees' $ tail ees' = head ees'
    | otherwise = MaxErr ees'
    where
        ees' = map rewriteEquivEExpr ees
rewriteEquivEExpr (ErrAdd ae1 ee1 ae2 ee2) = ErrAdd ae1 (rewriteEquivEExpr ee1) ae2 (rewriteEquivEExpr ee2)
rewriteEquivEExpr (ErrSub ae1 ee1 ae2 ee2) = ErrSub ae1 (rewriteEquivEExpr ee1) ae2 (rewriteEquivEExpr ee2)
rewriteEquivEExpr (ErrMul ae1 ee1 ae2 ee2) = ErrMul ae1 (rewriteEquivEExpr ee1) ae2 (rewriteEquivEExpr ee2)
rewriteEquivEExpr (ErrDiv ae1 ee1 ae2 ee2) = ErrDiv ae1 (rewriteEquivEExpr ee1) ae2 (rewriteEquivEExpr ee2)
rewriteEquivEExpr (ErrFloor  ae ee) = ErrFloor  ae (rewriteEquivEExpr ee)
rewriteEquivEExpr (ErrFloor0 ae ee) = ErrFloor0 ae (rewriteEquivEExpr ee)
rewriteEquivEExpr (ErrSqrt   ae ee) = ErrSqrt   ae (rewriteEquivEExpr ee)
rewriteEquivEExpr (ErrSin    ae ee) = ErrSin    ae (rewriteEquivEExpr ee)
rewriteEquivEExpr (ErrCos    ae ee) = ErrCos    ae (rewriteEquivEExpr ee)
rewriteEquivEExpr (ErrTan    ae ee) = ErrTan    ae (rewriteEquivEExpr ee)
rewriteEquivEExpr (ErrAsin   ae ee) = ErrAsin   ae (rewriteEquivEExpr ee)
rewriteEquivEExpr (ErrAcos   ae ee) = ErrAcos   ae (rewriteEquivEExpr ee)
rewriteEquivEExpr (ErrAtan   ae ee) = ErrAtan   ae (rewriteEquivEExpr ee)
rewriteEquivEExpr (ErrAtanT  ae ee) = ErrAtanT  ae (rewriteEquivEExpr ee)
rewriteEquivEExpr (ErrLn     ae ee) = ErrLn     ae (rewriteEquivEExpr ee)
rewriteEquivEExpr (ErrExpo   ae ee) = ErrExpo   ae (rewriteEquivEExpr ee)
rewriteEquivEExpr ee = ee


equivEExpr :: EExpr -> EExpr -> Bool
equivEExpr ee ee' = (rewriteEquivEExpr ee) == (rewriteEquivEExpr ee')

---------------------------------------------
-- float->real trasformations for programs --
---------------------------------------------

fp2realVarId :: VarId -> VarId
fp2realVarId (VarId x) = VarId ("r_"++x)

fp2realProg :: Program -> RProgram
fp2realProg (Prog decls) = RProg (map fp2realDecl decls)

fp2realDecl :: Decl -> RDecl
fp2realDecl (Decl fp (NonVarId f) xs stm) = RDecl fp (NonVarId (f++"_real")) (map fp2realVarId xs) (fp2realStm stm)

fp2realStm :: Stm -> RStm
fp2realStm (Let x fae stm)     = RLet (fp2realVarId x) (fae2real fae) (fp2realStm stm)
fp2realStm (Ite fbe stm1 stm2) = RIte (fbe2be fbe) (fp2realStm stm1) (fp2realStm stm2)
fp2realStm (StmExpr fae)       = RStmExpr (fae2real fae)

---------------------------------------------
-- real/float trasformations for variables --
---------------------------------------------

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

fae2real :: FAExpr -> AExpr
-- traslation from FP to Real expressions without considering the FP errors of the operations
-- it is used to add the (FP) guards to the (Real) conditions
fae2real (FAdd a1 a2)   = Add    (fae2real a1) (fae2real a2)
fae2real (FSub a1 a2)   = Sub    (fae2real a1) (fae2real a2)
fae2real (FMul a1 a2)   = Mul    (fae2real a1) (fae2real a2)
fae2real (FDiv a1 a2)   = Div    (fae2real a1) (fae2real a2)
fae2real (FPow a1 a2)   = Pow    (fae2real a1) (fae2real a2)
fae2real (FMod a1 a2)   = Mod    (fae2real a1) (fae2real a2)
fae2real (FNeg a)       = Neg    (fae2real a)
fae2real (FFloor a)     = Floor  (fae2real a)
fae2real (FSqrt a)      = Sqrt   (fae2real a)
fae2real (FAbs a)       = Abs    (fae2real a)
fae2real (FSin a)       = Sin    (fae2real a)
fae2real (FCos a)       = Cos    (fae2real a)
fae2real (FTan a)       = Tan    (fae2real a)
fae2real (FAcos a)      = ACos (fae2real a)
fae2real (FAsin a)      = ASin (fae2real a)
fae2real (FAtan a)      = ATan (fae2real a)
fae2real (FLn   a)      = Ln   (fae2real a)
fae2real (FExpo a)      = Expo (fae2real a)
fae2real (FInt n)       = StoR (FInt n)
fae2real (FDouble n)    = StoR (FDouble n)
fae2real (FEFun f args) = EFun f (map fae2real args)
fae2real (FVar x)       = RealMark x
fae2real FPi            = Pi 
fae2real (RtoS a)       = a -- StoR (RtoS a)
fae2real (RtoD a)       = a -- DtoR (RtoD a)
fae2real (FMin as)      = Min (map fae2real as)
fae2real (FMax as)      = Max (map fae2real as)


real2fae :: FPrec -> AExpr -> FAExpr
real2fae fp (Add a1 a2) = FAdd (real2fae fp a1) (real2fae fp a2)
real2fae fp (Sub a1 a2) = FSub (real2fae fp a1) (real2fae fp a2)
real2fae fp (Mul a1 a2) = FMul (real2fae fp a1) (real2fae fp a2)
real2fae fp (Div a1 a2) = FDiv (real2fae fp a1) (real2fae fp a2)
real2fae fp (Pow a1 a2) = FPow (real2fae fp a1) (real2fae fp a2)
real2fae fp (Mod a1 a2) = FMod (real2fae fp a1) (real2fae fp a2)
real2fae fp (Neg   a) = FNeg   (real2fae fp a)
real2fae fp (Floor a) = FFloor (real2fae fp a)
real2fae fp (Sqrt  a) = FSqrt  (real2fae fp a)
real2fae fp (Abs   a) = FAbs   (real2fae fp a)
real2fae fp (Sin   a) = FSin   (real2fae fp a)
real2fae fp (Cos   a) = FCos   (real2fae fp a)
real2fae fp (Tan   a) = FTan   (real2fae fp a)
real2fae fp (ASin  a) = FAsin  (real2fae fp a)
real2fae fp (ACos  a) = FAcos  (real2fae fp a)
real2fae fp (ATan  a) = FAtan  (real2fae fp a)
real2fae fp (Ln    a) = FLn    (real2fae fp a)
real2fae fp (Expo  a) = FExpo  (real2fae fp a)
real2fae FPSingle    (Int n) = RtoS $ Int n
real2fae FPDouble    (Int n) = RtoD $ Int n
real2fae FPSingle (Double n) = RtoS $ Double n
real2fae FPDouble (Double n) = RtoD $ Double n
real2fae fp (EFun f args) = FEFun f (map (real2fae fp) args)
real2fae _ (Var x) = FVar x
real2fae _ (RealMark x) = FVar x
real2fae _ (RealMark x) = FVar x
real2fae fp (Pi)    = FPi
real2fae FPSingle (StoR fae) = fae
real2fae FPDouble (DtoR fae) = fae
real2fae FPDouble (StoR fae) = error "real2fpe: FP precision mismatch"
real2fae FPSingle (DtoR fae) = error "real2fpe: FP precision mismatch"
real2fae _ a = error $ show a

---------------------------------
-- fp bool expr simplification --
---------------------------------

simplFBExprFix :: FBExpr -> FBExpr
simplFBExprFix be =
  if be' == be
  then be
  else simplFBExprFix be'
  where
    be' = simplFBExpr be

simplFBExpr :: FBExpr -> FBExpr
simplFBExpr (FAnd FBTrue c)  = simplFBExpr c
simplFBExpr (FAnd c FBTrue)  = simplFBExpr c
simplFBExpr (FAnd FBFalse _) = FBFalse
simplFBExpr (FAnd _ FBFalse) = FBFalse
simplFBExpr (FAnd c1 c2)     = FAnd (simplFBExpr c1) (simplFBExpr c2)
simplFBExpr (FOr FBTrue _)   = FBTrue
simplFBExpr (FOr _ FBTrue)   = FBTrue
simplFBExpr (FOr FBFalse c)  = simplFBExpr c
simplFBExpr (FOr c FBFalse)  = simplFBExpr c
simplFBExpr (FOr c1 c2)      = FOr (simplFBExpr c1) (simplFBExpr c2)
simplFBExpr (FNot (FNot c))  = simplFBExpr c
simplFBExpr (FNot FBTrue)    = FBFalse
simplFBExpr (FNot FBFalse)   = FBTrue
simplFBExpr (FNot c)         = FNot $ simplFBExpr c
simplFBExpr (FEq a1 a2)      = simplFRel (FEq a1 a2)
simplFBExpr (FNeq a1 a2)     = simplFRel (FNeq a1 a2)
simplFBExpr (FLt a1 a2)      = simplFRel (FLt a1 a2)
simplFBExpr (FLtE a1 a2)     = simplFRel (FLtE a1 a2)
simplFBExpr (FGt a1 a2)      = simplFRel (FGt a1 a2)
simplFBExpr (FGtE a1 a2)     = simplFRel (FGtE a1 a2)
simplFBExpr FBTrue           = FBTrue
simplFBExpr FBFalse          = FBFalse

simplFRel :: FBExpr -> FBExpr
simplFRel (FEq a1 a2) = case a1 of
    (FInt n)    -> case a2 of
                    (FInt m) ->  if n == m then FBTrue else FBFalse
                    (FDouble m) ->  if (fromIntegral n) == m then FBTrue else FBFalse
                    _ -> FEq a1 a2 -- (simplIAExpr a2)
    (FDouble n) -> case a2 of
                    (FInt m) ->  if n == (fromIntegral m) then FBTrue else FBFalse
                    (FDouble m) ->  if n == m then FBTrue else FBFalse
                    _ -> FEq a1 a2 -- (simplIAExpr a2)
    _           -> FEq a1 a2 -- (simplFAExpr a1) (simplFAExpr a2)                
simplFRel (FNeq a1 a2) = case a1 of
    (FInt n)    -> case a2 of
                    (FInt m) ->  if n /= m then FBTrue else FBFalse
                    (FDouble m) ->  if (fromIntegral n) /= m then FBTrue else FBFalse
                    _ -> FNeq a1 (simplFAExpr a2)
    (FDouble n) -> case a2 of
                    (FInt m) ->  if n /= (fromIntegral m) then FBTrue else FBFalse
                    (FDouble m) ->  if n /= m then FBTrue else FBFalse
                    _ -> FNeq a1 (simplFAExpr a2)
    _           ->  FNeq (simplFAExpr a1) (simplFAExpr a2)                    
simplFRel (FLt a1 a2) = case a1 of
    (FInt n)    -> case a2 of
                    (FInt m) ->  if n < m then FBTrue else FBFalse
                    (FDouble m) ->  if (fromIntegral n) < m then FBTrue else FBFalse
                    _ -> FLt a1 (simplFAExpr a2)
    (FDouble n)  -> case a2 of
                    (FInt m) ->  if n < (fromIntegral m) then FBTrue else FBFalse
                    (FDouble m) ->  if n < m then FBTrue else FBFalse
                    _ -> FLt a1 (simplFAExpr a2)
    _           -> FLt (simplFAExpr a1) (simplFAExpr a2)  
simplFRel (FLtE a1 a2) = case a1 of
    (FInt n)    -> case a2 of
                    (FInt m) ->  if n <= m then FBTrue else FBFalse
                    (FDouble m) ->  if (fromIntegral n) <= m then FBTrue else FBFalse
                    _ -> FLtE (simplFAExpr a1) (simplFAExpr a2)
    (FDouble n) -> case a2 of
                    (FInt m) ->  if n <= (fromIntegral m) then FBTrue else FBFalse
                    (FDouble m) ->  if n <= m then FBTrue else FBFalse
                    _ -> FLtE a1 (simplFAExpr a2)
    _          -> FLtE (simplFAExpr a1) (simplFAExpr a2)  
simplFRel (FGt a1 a2) = case a1 of
    (FInt n)    -> case a2 of
                    (FInt m) ->  if n > m then FBTrue else FBFalse
                    (FDouble m) ->  if (fromIntegral n) > m then FBTrue else FBFalse
                    _ -> FGt a1 (simplFAExpr a2)
    (FDouble n) -> case a2 of
                    (FInt m) ->  if n > (fromIntegral m) then FBTrue else FBFalse
                    (FDouble m) ->  if n > m then FBTrue else FBFalse
                    _ -> FGt a1 (simplFAExpr a2)
    _          -> FGt (simplFAExpr a1) (simplFAExpr a2) 
simplFRel (FGtE a1 a2) = case a1 of
    (FInt n)    -> case a2 of
                    (FInt m) ->  if n >= m then FBTrue else FBFalse
                    (FDouble m) ->  if (fromIntegral n) >= m then FBTrue else FBFalse
                    _ -> FGtE a1 (simplFAExpr a2)
    (FDouble n) -> case a2 of
                    (FInt m) ->  if n >= (fromIntegral m) then FBTrue else FBFalse
                    (FDouble m) ->  if n >= m then FBTrue else FBFalse
                    _ -> FGtE a1 (simplFAExpr a2)
    _          -> FGtE (simplFAExpr a1) (simplFAExpr a2)


-----------------------------------
-- real bool expr simplification --
-----------------------------------

simplBExprFix :: BExpr -> BExpr
simplBExprFix be =
  if be' == be
  then be
  else simplBExprFix be'
  where
    be' = simplBExpr be

simplBExpr :: BExpr -> BExpr
simplBExpr (And BTrue c)  = simplBExpr c
simplBExpr (And c BTrue)  = simplBExpr c
simplBExpr (And BFalse _) = BFalse
simplBExpr (And _ BFalse) = BFalse
simplBExpr (And c1 c2)    = And (simplBExpr c1) (simplBExpr c2)
simplBExpr (Or BTrue _)   = BTrue
simplBExpr (Or _ BTrue)   = BTrue
simplBExpr (Or BFalse c)  = simplBExpr c
simplBExpr (Or c BFalse)  = simplBExpr c
simplBExpr (Or c1 c2)     = Or (simplBExpr c1) (simplBExpr c2)
simplBExpr (Not (Not c))  = simplBExpr c
simplBExpr (Not BTrue)    = BFalse
simplBExpr (Not BFalse)   = BTrue
simplBExpr (Not c)        = Not $ simplBExpr c
simplBExpr (Eq a1 a2)     = simplRel (Eq a1 a2)
simplBExpr (Neq a1 a2)    = simplRel (Neq a1 a2)
simplBExpr (Lt a1 a2)     = simplRel (Lt a1 a2)
simplBExpr (LtE a1 a2)    = simplRel (LtE a1 a2)
simplBExpr (Gt a1 a2)     = simplRel (Gt a1 a2)
simplBExpr (GtE a1 a2)    = simplRel (GtE a1 a2)
simplBExpr BTrue          = BTrue
simplBExpr BFalse         = BFalse

simplRel :: BExpr -> BExpr
simplRel (Eq a1 a2) = case a1 of
    (Int n)    -> case a2 of
                    (Int m) ->  if n == m then BTrue else BFalse
                    (Double m) ->  if (fromIntegral n) == m then BTrue else BFalse
                    _ -> Eq a1 (simplAExpr a2)
    (Double n) -> case a2 of
                    (Int m) ->  if n == (fromIntegral m) then BTrue else BFalse
                    (Double m) ->  if n == m then BTrue else BFalse
                    _ -> Eq a1 (simplAExpr a2)
    _          -> Eq (simplAExpr a1) (simplAExpr a2)                
simplRel (Neq a1 a2) = case a1 of
    (Int n)    -> case a2 of
                    (Int m) ->  if n /= m then BTrue else BFalse
                    (Double m) ->  if (fromIntegral n) /= m then BTrue else BFalse
                    _ -> Neq a1 (simplAExpr a2)
    (Double n) -> case a2 of
                    (Int m) ->  if n /= (fromIntegral m) then BTrue else BFalse
                    (Double m) ->  if n /= m then BTrue else BFalse
                    _ -> Neq a1 (simplAExpr a2)
    _          ->  Neq (simplAExpr a1) (simplAExpr a2)                    
simplRel (Lt a1 a2) = case a1 of
    (Int n)    -> case a2 of
                    (Int m) ->  if n < m then BTrue else BFalse
                    (Double m) ->  if (fromIntegral n) < m then BTrue else BFalse
                    _ -> Lt a1 (simplAExpr a2)
    (Double n)  -> case a2 of
                    (Int m) ->  if n < (fromIntegral m) then BTrue else BFalse
                    (Double m) ->  if n < m then BTrue else BFalse
                    _ -> Lt a1 (simplAExpr a2)
    _           -> Lt (simplAExpr a1) (simplAExpr a2)  
simplRel (LtE a1 a2) = case a1 of
    (Int n)    -> case a2 of
                    (Int m) ->  if n <= m then BTrue else BFalse
                    (Double m) ->  if (fromIntegral n) <= m then BTrue else BFalse
                    _ -> LtE a1 (simplAExpr a2)
    (Double n) -> case a2 of
                    (Int m) ->  if n <= (fromIntegral m) then BTrue else BFalse
                    (Double m) ->  if n <= m then BTrue else BFalse
                    _ -> LtE a1 (simplAExpr a2)
    _          -> LtE (simplAExpr a1) (simplAExpr a2)  
simplRel (Gt a1 a2) = case a1 of
    (Int n)    -> case a2 of
                    (Int m) ->  if n > m then BTrue else BFalse
                    (Double m) ->  if (fromIntegral n) > m then BTrue else BFalse
                    _ -> Gt a1 (simplAExpr a2)
    (Double n) -> case a2 of
                    (Int m) ->  if n > (fromIntegral m) then BTrue else BFalse
                    (Double m) ->  if n > m then BTrue else BFalse
                    _ -> Gt a1 (simplAExpr a2)
    _          -> Gt (simplAExpr a1) (simplAExpr a2) 
simplRel (GtE a1 a2) = case a1 of
    (Int n)    -> case a2 of
                    (Int m) ->  if n >= m then BTrue else BFalse
                    (Double m) ->  if (fromIntegral n) >= m then BTrue else BFalse
                    _ -> GtE a1 (simplAExpr a2)
    (Double n) -> case a2 of
                    (Int m) ->  if n >= (fromIntegral m) then BTrue else BFalse
                    (Double m) ->  if n >= m then BTrue else BFalse
                    _ -> GtE a1 (simplAExpr a2)
    _          -> GtE (simplAExpr a1) (simplAExpr a2)

--simplFRel (And b1 b2) = And (simplBExprRel b1) (simplBExprRel b2)
--simplFRel (Or b1 b2) = Or (simplBExprRel b1) (simplBExprRel b2)
--simplFRel (Not b) = Not (simplBExprRel b)
--simplFRel BTrue = BTrue
--simplFRel BFalse = BFalse


------------------------------------
-- real arith expr simplification --
------------------------------------

simplAExpr :: AExpr -> AExpr
simplAExpr ae =
  if ae' == ae
  then ae
  else simplAExpr ae'
  where
    ae' = simplAExprAux ae 

simplAExprAux :: AExpr -> AExpr
simplAExprAux (Add a (Int 0)) = simplAExprAux a
simplAExprAux (Add (Int 0) a) = simplAExprAux a
simplAExprAux (Add a (Double 0)) = simplAExprAux a
simplAExprAux (Add (Double 0) a) = simplAExprAux a
simplAExprAux (Add (Int n) (Int m)) = Int (n+m)
simplAExprAux (Add (Int n) (Double m)) = Double ((fromIntegral n)+m)
simplAExprAux (Add (Double n) (Int m)) = Double (n+(fromIntegral m))
simplAExprAux (Add (Double n) (Double m)) = Double (n+m)
simplAExprAux (Add a1 a2) = Add (simplAExpr a1) (simplAExpr a2)
simplAExprAux (Sub a (Int 0)) = simplAExprAux a
simplAExprAux (Sub (Int 0) a) = Neg $ simplAExprAux a
simplAExprAux (Sub a (Double 0)) = simplAExprAux a
simplAExprAux (Sub (Double 0) a) = Neg $ simplAExprAux a
simplAExprAux (Sub (Int n) (Int m)) = Int (n-m)
simplAExprAux (Sub (Int n) (Double m)) = Double ((fromIntegral n)-m)
simplAExprAux (Sub (Double n) (Int m)) = Double (n-(fromIntegral m))
simplAExprAux (Sub (Double n) (Double m)) = Double (n-m)
simplAExprAux (Sub a1 a2) = Sub (simplAExpr a1) (simplAExpr a2)
simplAExprAux (Mul a (Int 1)) = simplAExprAux a
simplAExprAux (Mul (Int 1) a) = simplAExprAux a
simplAExprAux (Mul a (Double 1)) = simplAExprAux a
simplAExprAux (Mul (Double 1) a) = simplAExprAux a
simplAExprAux (Mul (Int 0) a) = Int 0
simplAExprAux (Mul a (Int 0)) = Int 0
simplAExprAux (Mul (Double 0) a) = Double 0
simplAExprAux (Mul a (Double 0)) = Double 0    
simplAExprAux (Mul (Int n) (Int m)) = Int (n*m)
simplAExprAux (Mul (Int n) (Double m)) = Double ((fromIntegral n)*m)
simplAExprAux (Mul (Double n) (Int m)) = Double (n*(fromIntegral m))
simplAExprAux (Mul (Double n) (Double m)) = Double (n*m)
simplAExprAux (Mul a1 a2) = Mul (simplAExpr a1) (simplAExpr a2)
simplAExprAux (Div a1 a2) = Div (simplAExpr a1) (simplAExpr a2)
simplAExprAux (Neg (Int n)) = Int (-n)
--simplAExprAux (Neg (Double n)) = Double (-n)
simplAExprAux (Neg a) = Neg (simplAExpr a)
simplAExprAux (Pow a1 a2) = Pow (simplAExprAux a1) (simplAExprAux a2)
simplAExprAux (Floor a) = Floor (simplAExprAux a)
simplAExprAux (Sqrt a) = Sqrt (simplAExprAux a)
--simplAExprAux (Abs (Int n)) = if n>=0 then Int n else Int (-n)
--simplAExprAux (Abs (Double n)) = if n>=0 then Double n else Double (-n)
simplAExprAux (Abs (Abs a)) = Abs (simplAExprAux a)
simplAExprAux (Abs a)  = Abs  (simplAExprAux a)
simplAExprAux (Sin a)  = Sin  (simplAExprAux a)
simplAExprAux (Cos a)  = Cos  (simplAExprAux a)
simplAExprAux (Tan a)  = Cos  (simplAExprAux a)
simplAExprAux (ACos a) = ACos (simplAExprAux a)
simplAExprAux (ASin a) = ASin (simplAExprAux a)
simplAExprAux (ATan a) = ATan (simplAExprAux a)
simplAExprAux (Ln a)   = Ln   (simplAExprAux a)
simplAExprAux (Expo a) = Expo (simplAExprAux a)
simplAExprAux (Mod a1 a2) = Mod (simplAExprAux a1) (simplAExprAux a2)
simplAExprAux (Int n) = Int n
simplAExprAux (Double n) = Double n
simplAExprAux (Var x) = Var x
simplAExprAux (Pi) = Pi
simplAExprAux (EFun f args) = EFun f (map simplAExprAux args)
simplAExprAux (SUlp a) = SUlp $ simplAExpr a
simplAExprAux (DUlp a) = DUlp $ simplAExpr a
simplAExprAux (StoR (RtoS (Int n))) = Int n
simplAExprAux (StoR a) = StoR $ simplFAExpr a
simplAExprAux (DtoR (RtoD (Int n))) = Int n
simplAExprAux (DtoR a) = DtoR $ simplFAExpr a
simplAExprAux a = a

----------------------------------
-- fp arith expr simplification --
----------------------------------

simplFAExpr :: FAExpr -> FAExpr
simplFAExpr ae =
  if ae' == ae
  then ae
  else simplFAExpr ae'
  where
    ae' = simplFAExprAux ae 

simplFAExprAux :: FAExpr -> FAExpr
simplFAExprAux (FAdd a (FInt 0)) = simplFAExprAux a
simplFAExprAux (FAdd (FInt 0) a) = simplFAExprAux a
simplFAExprAux (FAdd a (FDouble 0)) = simplFAExprAux a
simplFAExprAux (FAdd (FDouble 0) a) = simplFAExprAux a
simplFAExprAux (FAdd (FInt n) (FInt m)) = FInt (n+m)
simplFAExprAux (FAdd (FInt n) (FDouble m)) = FDouble ((fromIntegral n)+m)
simplFAExprAux (FAdd (FDouble n) (FInt m)) = FDouble (n+(fromIntegral m))
simplFAExprAux (FAdd (FDouble n) (FDouble m)) = FDouble (n+m)
simplFAExprAux (FAdd a1 a2) = FAdd (simplFAExpr a1) (simplFAExpr a2)
simplFAExprAux (FSub a (FInt 0)) = simplFAExprAux a
simplFAExprAux (FSub (FInt 0) a) = FNeg $ simplFAExprAux a
simplFAExprAux (FSub a (FDouble 0)) = simplFAExprAux a
simplFAExprAux (FSub (FDouble 0) a) = FNeg $ simplFAExprAux a
simplFAExprAux (FSub (FInt n) (FInt m)) = FInt (n-m)
simplFAExprAux (FSub (FInt n) (FDouble m)) = FDouble ((fromIntegral n)-m)
simplFAExprAux (FSub (FDouble n) (FInt m)) = FDouble (n-(fromIntegral m))
simplFAExprAux (FSub (FDouble n) (FDouble m)) = FDouble (n-m)
simplFAExprAux (FSub a1 a2) = FSub (simplFAExpr a1) (simplFAExpr a2)
simplFAExprAux (FMul a (FInt 1)) = simplFAExprAux a
simplFAExprAux (FMul (FInt 1) a) = simplFAExprAux a
simplFAExprAux (FMul a (FDouble 1)) = simplFAExprAux a
simplFAExprAux (FMul (FDouble 1) a) = simplFAExprAux a
simplFAExprAux (FMul (FInt 0) a) = FInt 0
simplFAExprAux (FMul a (FInt 0)) = FInt 0
simplFAExprAux (FMul (FDouble 0) a) = FDouble 0
simplFAExprAux (FMul a (FDouble 0)) = FDouble 0    
simplFAExprAux (FMul (FInt n) (FInt m)) = FInt (n*m)
simplFAExprAux (FMul (FInt n) (FDouble m)) = FDouble ((fromIntegral n)*m)
simplFAExprAux (FMul (FDouble n) (FInt m)) = FDouble (n*(fromIntegral m))
simplFAExprAux (FMul (FDouble n) (FDouble m)) = FDouble (n*m)
simplFAExprAux (FMul a1 a2) = FMul (simplFAExpr a1) (simplFAExpr a2)
simplFAExprAux (FDiv a1 a2) = FDiv (simplFAExpr a1) (simplFAExpr a2)
simplFAExprAux (FNeg (FInt n)) = FInt (-n)
--simplFAExprAux (FNeg (FDouble n)) = FDouble (-n)
simplFAExprAux (FNeg a) = FNeg (simplFAExpr a)
simplFAExprAux (FPow a1 a2) = FPow (simplFAExprAux a1) (simplFAExprAux a2)
simplFAExprAux (FFloor a) = FFloor (simplFAExprAux a)
simplFAExprAux (FSqrt a) = FSqrt (simplFAExprAux a)
simplFAExprAux (FAbs (FInt n)) = if n>=0 then FInt n else FInt (-n)
simplFAExprAux (FAbs (FDouble n)) = if n>=0 then FDouble n else FDouble (-n)
simplFAExprAux (FAbs (FAbs a)) = FAbs (simplFAExprAux a)
simplFAExprAux (FAbs a) = FAbs (simplFAExprAux a)
simplFAExprAux (FSin a) = FSin (simplFAExprAux a)
simplFAExprAux (FCos a) = FCos (simplFAExprAux a)
simplFAExprAux (FTan a) = FTan (simplFAExprAux a)
simplFAExprAux (FAcos a) = FAcos (simplFAExprAux a)
simplFAExprAux (FAsin a) = FAsin (simplFAExprAux a)
simplFAExprAux (FAtan a) = FAtan (simplFAExprAux a)
simplFAExprAux (FLn a) = FLn (simplFAExprAux a)
simplFAExprAux (FExpo a) = FExpo (simplFAExprAux a)
simplFAExprAux (FMod a1 a2) = FMod (simplFAExprAux a1) (simplFAExprAux a2)
simplFAExprAux (FInt n) = FInt n
simplFAExprAux (FDouble n) = FDouble n
simplFAExprAux (FVar x) = FVar x
simplFAExprAux (FPi) = FPi
simplFAExprAux (FEFun f args) = FEFun f (map simplFAExprAux args)
simplFAExprAux (RtoS (StoR fa)) = fa
simplFAExprAux (RtoS a) = RtoS $ simplAExprAux a
simplFAExprAux (RtoD (DtoR fa)) = fa
simplFAExprAux (RtoD a) = RtoD $ simplAExprAux a
-- simplAExprAux c = c


initErrExpr :: EExpr -> EExpr
initErrExpr r@(ErrRat _)  = r
initErrExpr (ErrorMark x) = HalfUlp (RealMark x)
initErrExpr (ErrAdd    a1 e1 a2 e2) = ErrAdd (initAExpr a1) (initErrExpr e1) (initAExpr a2) (initErrExpr e2)
initErrExpr (ErrSub    a1 e1 a2 e2) = ErrSub (initAExpr a1) (initErrExpr e1) (initAExpr a2) (initErrExpr e2)
initErrExpr (ErrMul    a1 e1 a2 e2) = ErrMul (initAExpr a1) (initErrExpr e1) (initAExpr a2) (initErrExpr e2)
initErrExpr (ErrDiv    a1 e1 a2 e2) = ErrDiv (initAExpr a1) (initErrExpr e1) (initAExpr a2) (initErrExpr e2)
initErrExpr (ErrFloor  a e)         = ErrFloor   (initAExpr a) (initErrExpr e)
initErrExpr (ErrFloor0 a e)         = ErrFloor0  (initAExpr a) (initErrExpr e)
initErrExpr (ErrSqrt   a e)         = ErrSqrt    (initAExpr a) (initErrExpr e)
initErrExpr (ErrSin    a e)         = ErrSin     (initAExpr a) (initErrExpr e)
initErrExpr (ErrCos    a e)         = ErrCos     (initAExpr a) (initErrExpr e)
initErrExpr (ErrTan    a e)         = ErrTan     (initAExpr a) (initErrExpr e)
initErrExpr (ErrAsin   a e)         = ErrAsin    (initAExpr a) (initErrExpr e)
initErrExpr (ErrAcos   a e)         = ErrAcos    (initAExpr a) (initErrExpr e)
initErrExpr (ErrAtan   a e)         = ErrAtan    (initAExpr a) (initErrExpr e)
initErrExpr (ErrAtanT  a e)         = ErrAtanT   (initAExpr a) (initErrExpr e)
initErrExpr (ErrNeg    a e)         = ErrNeg     (initAExpr a) (initErrExpr e)
initErrExpr (ErrAbs    a e)         = ErrAbs     (initAExpr a) (initErrExpr e)
initErrExpr (ErrLn     a e)         = ErrLn      (initAExpr a) (initErrExpr e)
initErrExpr (ErrExpo   a e)         = ErrExpo    (initAExpr a) (initErrExpr e)
initErrExpr (ErrMulPow2L n e)       = ErrMulPow2L n (initErrExpr e)
initErrExpr (ErrMulPow2R n e)       = ErrMulPow2R n (initErrExpr e)
initErrExpr (AE a)                  = AE (initAExpr a)
initErrExpr (HalfUlp a)             = HalfUlp (initAExpr a)
initErrExpr (MaxErr es)             = MaxErr (map initErrExpr es)
initErrExpr Infinity                = Infinity
initErrExpr ErrUndefined            = ErrUndefined


initAExpr :: AExpr -> AExpr
initAExpr Pi              = Pi
initAExpr FPrec           = FPrec
initAExpr a@(Int _)       = a
initAExpr a@(Double _)    = a 
initAExpr a@(Var _)       = a
initAExpr a@(RealMark _)  = a
initAExpr a@(ErrorMark _) = a
initAExpr (Add a1 a2) = Add (initAExpr a1) (initAExpr a2)
initAExpr (Sub a1 a2) = Sub (initAExpr a1) (initAExpr a2)
initAExpr (Mul a1 a2) = Mul (initAExpr a1) (initAExpr a2)
initAExpr (Div a1 a2) = Div (initAExpr a1) (initAExpr a2)
initAExpr (Pow a1 a2) = Pow (initAExpr a1) (initAExpr a2)
initAExpr (Mod a1 a2) = Mod (initAExpr a1) (initAExpr a2)
initAExpr (Neg a)     = Neg   (initAExpr a)
initAExpr (Floor a)   = Floor (initAExpr a)
initAExpr (Sqrt  a)   = Sqrt  (initAExpr a)
initAExpr (Abs   a)   = Abs   (initAExpr a)
initAExpr (Sin   a)   = Sin   (initAExpr a)
initAExpr (Cos   a)   = Cos   (initAExpr a)
initAExpr (Tan   a)   = Tan   (initAExpr a)
initAExpr (ASin  a)   = ASin  (initAExpr a)
initAExpr (ACos  a)   = ACos  (initAExpr a)
initAExpr (ATan  a)   = ATan  (initAExpr a)
initAExpr (Ln  a)     = Ln    (initAExpr a)
initAExpr (Expo  a)   = Expo  (initAExpr a)
initAExpr (SUlp a)    = SUlp (initAExpr a)
initAExpr (DUlp a)    = DUlp (initAExpr a)
initAExpr (StoR fa)   = StoR (initFAExpr fa)
initAExpr (DtoR fa)   = DtoR (initFAExpr fa)
initAExpr (EE e)      = EE (initErrExpr e)
initAExpr (FExp fa)   = FExp (initFAExpr fa)
initAExpr (EFun f as) = EFun f (map initAExpr as)
initAExpr (Min as)    = Min (map initAExpr as)
initAExpr (Max as)    = Max (map initAExpr as)
initAExpr (MaxErr as) = MaxErr (map initAExpr as)
initAExpr (ErrAdd a1 e1 a2 e2) = ErrAdd (initAExpr a1) (initErrExpr e1) (initAExpr a2) (initErrExpr e2)
initAExpr (ErrSub a1 e1 a2 e2) = ErrSub (initAExpr a1) (initErrExpr e1) (initAExpr a2) (initErrExpr e2)
initAExpr (ErrMul a1 e1 a2 e2) = ErrMul (initAExpr a1) (initErrExpr e1) (initAExpr a2) (initErrExpr e2)
initAExpr (ErrDiv a1 e1 a2 e2) = ErrDiv (initAExpr a1) (initErrExpr e1) (initAExpr a2) (initErrExpr e2)
initAExpr (ErrFloor  a e) = ErrFloor  (initAExpr a) (initErrExpr e)
initAExpr (ErrFloor0 a e) = ErrFloor0 (initAExpr a) (initErrExpr e)
initAExpr (ErrSqrt   a e) = ErrSqrt   (initAExpr a) (initErrExpr e)
initAExpr (ErrSin    a e) = ErrSin    (initAExpr a) (initErrExpr e)
initAExpr (ErrCos    a e) = ErrCos    (initAExpr a) (initErrExpr e)
initAExpr (ErrTan    a e) = ErrTan    (initAExpr a) (initErrExpr e)
initAExpr (ErrAsin   a e) = ErrAsin   (initAExpr a) (initErrExpr e)
initAExpr (ErrAcos   a e) = ErrAcos   (initAExpr a) (initErrExpr e)
initAExpr (ErrAtan   a e) = ErrAtan   (initAExpr a) (initErrExpr e)
initAExpr (ErrAtanT  a e) = ErrAtanT  (initAExpr a) (initErrExpr e)
initAExpr (ErrNeg    a e) = ErrNeg    (initAExpr a) (initErrExpr e)
initAExpr (ErrAbs    a e) = ErrAbs    (initAExpr a) (initErrExpr e)
initAExpr (ErrLn     a e) = ErrLn     (initAExpr a) (initErrExpr e)
initAExpr (ErrExpo   a e) = ErrExpo   (initAExpr a) (initErrExpr e)
initAExpr (ErrMulPow2R n e) = ErrMulPow2R n (initErrExpr e)
initAExpr (ErrMulPow2L n e) = ErrMulPow2L n (initErrExpr e)
initAExpr (AE a) = AE (initAExpr a)
initAExpr (HalfUlp a) = HalfUlp (initAExpr a)
initAExpr r@(ErrRat _) = r
initAExpr Infinity = Infinity
initAExpr ErrUndefined = ErrUndefined
initAExpr a = error $ "niy "++ show a

initFAExpr :: FAExpr -> FAExpr
initFAExpr a@(FInt _)     = a
initFAExpr a@(FDouble _)  = a
initFAExpr a@(FVar _)     = a
initFAExpr FPi            = FPi
initFAExpr (FAdd fa1 fa2) = FAdd (initFAExpr fa1) (initFAExpr fa2)
initFAExpr (FSub fa1 fa2) = FSub (initFAExpr fa1) (initFAExpr fa2)
initFAExpr (FMul fa1 fa2) = FMul (initFAExpr fa1) (initFAExpr fa2)
initFAExpr (FDiv fa1 fa2) = FDiv (initFAExpr fa1) (initFAExpr fa2)
initFAExpr (FPow fa1 fa2) = FPow (initFAExpr fa1) (initFAExpr fa2)
initFAExpr (FMod fa1 fa2) = FMod (initFAExpr fa1) (initFAExpr fa2)
initFAExpr (FNeg fa)      = FNeg   (initFAExpr fa)
initFAExpr (FFloor fa)    = FFloor (initFAExpr fa)
initFAExpr (FSqrt  fa)    = FSqrt  (initFAExpr fa)
initFAExpr (FAbs   fa)    = FAbs   (initFAExpr fa)
initFAExpr (FSin   fa)    = FSin   (initFAExpr fa)
initFAExpr (FCos   fa)    = FCos   (initFAExpr fa)
initFAExpr (FTan   fa)    = FTan   (initFAExpr fa)
initFAExpr (FAcos fa)     = FAcos (initFAExpr fa)
initFAExpr (FAsin fa)     = FAsin (initFAExpr fa)
initFAExpr (FAtan fa)     = FAtan (initFAExpr fa)
initFAExpr (FLn fa)       = FLn (initFAExpr fa)
initFAExpr (FExpo fa)     = FExpo (initFAExpr fa)
initFAExpr (RtoS a)       = RtoS (initAExpr a)
initFAExpr (RtoD a)       = RtoD (initAExpr a)
initFAExpr (FEFun f fas)  = FEFun f (map initFAExpr fas)
initFAExpr (FMin fas)     = FMin  (map initFAExpr fas)
initFAExpr (FMax fas)     = FMax  (map initFAExpr fas)

initFBExpr :: FBExpr -> FBExpr
initFBExpr (FOr b1 b2)  = FOr  (initFBExpr b1) (initFBExpr b2)
initFBExpr (FAnd b1 b2) = FAnd (initFBExpr b1) (initFBExpr b2)
initFBExpr (FNot b)     = FNot (initFBExpr b) 
initFBExpr (FEq  a1 a2) = FEq  (initFAExpr a1) (initFAExpr a2)
initFBExpr (FNeq a1 a2) = FNeq (initFAExpr a1) (initFAExpr a2)
initFBExpr (FLt  a1 a2) = FLt  (initFAExpr a1) (initFAExpr a2)
initFBExpr (FLtE a1 a2) = FLtE (initFAExpr a1) (initFAExpr a2)
initFBExpr (FGt  a1 a2) = FGt  (initFAExpr a1) (initFAExpr a2)
initFBExpr (FGtE a1 a2) = FGtE (initFAExpr a1) (initFAExpr a2)
initFBExpr FBTrue       = FBTrue
initFBExpr FBFalse      = FBFalse

initBExpr :: BExpr -> BExpr
initBExpr (Or b1 b2)  = Or  (initBExpr b1) (initBExpr b2)
initBExpr (And b1 b2) = And (initBExpr b1) (initBExpr b2)
initBExpr (Not b)     = Not (initBExpr b) 
initBExpr (Eq  a1 a2) = Eq  (initAExpr a1) (initAExpr a2)
initBExpr (Neq a1 a2) = Neq (initAExpr a1) (initAExpr a2)
initBExpr (Lt  a1 a2) = Lt  (initAExpr a1) (initAExpr a2)
initBExpr (LtE a1 a2) = LtE (initAExpr a1) (initAExpr a2)
initBExpr (Gt  a1 a2) = Gt  (initAExpr a1) (initAExpr a2)
initBExpr (GtE a1 a2) = GtE (initAExpr a1) (initAExpr a2)
initBExpr BTrue       = BTrue
initBExpr BFalse      = BFalse



-----------------------
-- PPExt instances --
-----------------------


instance PPExt (NonVarId) where 
    prettyDoc (NonVarId x) = text x
    prettyDocWith _ v = prettyDoc v


instance PPExt (VarId) where
    prettyDoc (VarId x) = text x    
    prettyDocWith _ v = prettyDoc v  


maxFlatten :: [EExpr] -> [EExpr]
maxFlatten [] = []
maxFlatten ((MaxErr [e]):ees') = e:(maxFlatten ees')
maxFlatten ((MaxErr ees):ees') = (maxFlatten ees) ++ (maxFlatten ees')
maxFlatten (ee:ees) = ee:(maxFlatten ees)

rat2interval :: Rational -> (CDouble,CDouble)
rat2interval rat | (toRational ratDouble == rat) = (ratDouble, ratDouble)
                 | (toRational ratDouble  < rat) && (toRational next >= rat)  = (ratDouble, next)
                 | (toRational ratDouble  > rat) && (toRational prev <= rat)  = (prev, ratDouble)
                 | otherwise = error $ "rat2interval failed with this values:"
                                ++ "\n\trat: " ++ show rat
                                ++ "\n\tratDouble: " ++ show ratDouble
                                ++ "\n\tprev: " ++ show prev
                                ++ "\n\tnext: " ++ show next
    where
        ratDouble = ((fromRat rat) :: CDouble)
        next = nextUp ratDouble
        prev = nextDown ratDouble

showFullPrecision :: CDouble -> String
showFullPrecision x = (showFFloat Nothing x) ""

showFullPrecisionRat :: Rational -> String
showFullPrecisionRat x = (showFFloat Nothing $ fromRat x) ""

instance PPExt RProgram where
    prettyDoc (RProg decls) = vcat (map prettyDoc decls)
    prettyDocWith _ rProg = prettyDoc rProg

instance PPExt RDecl where
    prettyDoc (RDecl fp fun args stm)
      = prettyDoc fun <> text "(" <>
        (hsep $ punctuate comma $ map (\x -> prettyDoc x) args)
        <> text ": real"
        <> text  "): real =" <+> prettyDocWith fp stm

instance PPExt RStm where
    prettyDoc _ = error "prettyDoc: undefined for RStm, fp needed"

    prettyDocWith fp (RLet x ae stm) = text "LET" <+> prettyDoc x <> text "=" <> prettyDocWith fp ae $$ text "IN" <+> prettyDocWith fp stm
    prettyDocWith fp (RIte be stm1 stm2) = text "IF" <+> prettyDocWith fp be $$ text "THEN" <+> prettyDocWith fp stm1 $$ text "ELSE" <+> prettyDocWith fp stm2 $$ text "ENDIF"
    prettyDocWith fp (RStmExpr ae) = prettyDocWith fp ae
    prettyDocWith fp (RForLoop i j ae f) =  text "for" <> text "(" <> integer i <> comma <> integer j <> comma <> prettyDocWith fp ae <> comma <> prettyDoc f <> text ")"

instance PPExt AExpr where

    prettyDoc _ = error "prettyDoc: undefined for AExpr, fp needed"

    prettyDocWith _                Pi = text "pi"
    prettyDocWith _          Infinity = text "infinity"
    prettyDocWith _      ErrUndefined = text "undefined"
    prettyDocWith _           (Int i) = integer i
--    prettyDocWith _        (Double d) = parens $ text $ showFullPrecisionRat d
    prettyDocWith _        (Double d) = parens $ text $ showRational d
    prettyDocWith _           (Var x) = text x
    prettyDocWith _       (EFun f []) = text f
    prettyDocWith _     (Neg (Int i)) = text "-" <> integer i
--    prettyDocWith _  (Neg (Double d)) = text "-" <> (text $ showFullPrecisionRat d)
    prettyDocWith _  (Neg (Double d)) = text "-" <> parens (text $ showRational d)
    prettyDocWith fp      (Add a1 a2) = parens $ prettyDocWith fp a1 <+> text "+" <+> prettyDocWith fp a2
    prettyDocWith fp      (Sub a1 a2) = parens $ prettyDocWith fp a1 <+> text "-" <+> prettyDocWith fp a2
    prettyDocWith fp      (Mul a1 a2) = parens $ prettyDocWith fp a1 <+> text "*" <+> prettyDocWith fp a2
    prettyDocWith fp      (Div a1 a2) = parens $ prettyDocWith fp a1 <+> text "/" <+> prettyDocWith fp a2
    prettyDocWith fp      (Mod a1 a2) = text "mod" <> parens (prettyDocWith fp a1 <> comma <+> prettyDocWith fp a2)
    prettyDocWith fp      (Pow a1 a2) = prettyDocWith fp a1 <> text "^" <> lparen <> prettyDocWith fp a2 <> rparen
    prettyDocWith fp        (Neg   a) = text "-"     <> lparen <> prettyDocWith fp a <> rparen
    prettyDocWith fp        (Floor a) = text "floor" <> lparen <> prettyDocWith fp a <> rparen
    prettyDocWith fp        (Sqrt  a) = text "sqrt"  <> lparen <> prettyDocWith fp a <> rparen
    prettyDocWith fp        (Abs   a) = text "abs"   <> lparen <> prettyDocWith fp a <> rparen
    prettyDocWith fp        (Sin   a) = text "sin"   <> lparen <> prettyDocWith fp a <> rparen
    prettyDocWith fp        (Cos   a) = text "cos"   <> lparen <> prettyDocWith fp a <> rparen
    prettyDocWith fp        (Tan   a) = text "tan"   <> lparen <> prettyDocWith fp a <> rparen
    prettyDocWith fp        (ASin  a) = text "asin"  <> lparen <> prettyDocWith fp a <> rparen
    prettyDocWith fp        (ACos  a) = text "acos"  <> lparen <> prettyDocWith fp a <> rparen
    prettyDocWith fp        (ATan  a) = text "atan"  <> lparen <> prettyDocWith fp a <> rparen
    prettyDocWith fp        (Ln    a) = text "ln"  <> lparen <> prettyDocWith fp a <> rparen
    prettyDocWith fp        (Expo  a) = text "exp"  <> lparen <> prettyDocWith fp a <> rparen
    prettyDocWith fp    (EFun f args) = text f <> (parens $ hsep $ punctuate comma $ map (\x -> prettyDocWith fp x) args)
    prettyDocWith fp         (SUlp a) = text "ulp_sp" <> lparen <> prettyDocWith FPSingle a <> rparen
    prettyDocWith fp         (DUlp a) = text "ulp_dp" <> lparen <> prettyDocWith FPDouble a <> rparen
    prettyDocWith fp         (StoR a) = text "StoR"   <> lparen <> prettyDocWith FPSingle a <> rparen
    prettyDocWith fp         (DtoR a) = text "DtoR"   <> lparen <> prettyDocWith FPDouble a <> rparen
    prettyDocWith fp           (EE e) = prettyDocWith fp e
    prettyDocWith FPSingle    (FPrec) = text "ieee754sp_prec"
    prettyDocWith FPDouble    (FPrec) = text "ieee754dp_prec"
    prettyDocWith fp        (FExp fa) = text "Fexp" <> parens (prettyDocWith fp fa)
    prettyDocWith fp     (RealMark x) = text "r_" <> text x
--    prettyDocWith _ (ErrRat r) = parens $ text $ showFullPrecisionRat r
    prettyDocWith _ (ErrRat r) = parens $ text $ showRational r

    prettyDocWith _ (ErrorMark x)
        = text "e_" <> text x

    prettyDocWith fp (MaxErr []) = error "Something went wrong: MaxErr applied to empty list"

    prettyDocWith fp (MaxErr [es]) = prettyDocWith fp es

    prettyDocWith fp (MaxErr (e:es)) = 
        text "max" <> parens ((prettyDocWith fp e) <> comma <+> prettyDocWith fp (MaxErr es)) 
          -- (hsep $ punctuate comma (map (prettyDocWith fp) ees))

    prettyDocWith fp (MaxErr ees) = 
        text "max" <> parens (hsep $ punctuate comma (map (prettyDocWith fp) ees))

    prettyDocWith fp (AE a) = prettyDocWith fp a

    prettyDocWith FPSingle (ErrAdd r1 e1 r2 e2) 
        = text "aeboundsp_add" <> (text "(" <>  prettyDocWith FPSingle r1 <> comma
                                            <+> prettyDocWith FPSingle e1 <> comma
                                            <+> prettyDocWith FPSingle r2 <> comma
                                            <+> prettyDocWith FPSingle e2 <> text ")")
    prettyDocWith FPSingle (ErrSub r1 e1 r2 e2) 
        = text "aeboundsp_sub" <> (text "(" <>  prettyDocWith FPSingle r1 <> comma
                                            <+> prettyDocWith FPSingle e1 <> comma
                                            <+> prettyDocWith FPSingle r2 <> comma
                                            <+> prettyDocWith FPSingle e2 <> text ")")
    prettyDocWith FPSingle (ErrMul r1 e1 r2 e2) 
        = text "aeboundsp_mul" <> (text "(" <>  prettyDocWith FPSingle r1 <> comma
                                            <+> prettyDocWith FPSingle e1 <> comma
                                            <+> prettyDocWith FPSingle r2 <> comma
                                            <+> prettyDocWith FPSingle e2 <> text ")")
    prettyDocWith FPSingle (ErrDiv r1 e1 r2 e2) 
        = text "aeboundsp_div" <> (text "(" <>  prettyDocWith FPSingle r1 <> comma
                                            <+> prettyDocWith FPSingle e1 <> comma
                                            <+> prettyDocWith FPSingle r2 <> comma
                                            <+> prettyDocWith FPSingle e2 <> text ")")
    prettyDocWith FPSingle (ErrFloor r e)
        = text "aeboundsp_flr" <> (text "(" <>  prettyDocWith FPSingle r <> comma
                                            <+> prettyDocWith FPSingle e <> text ")")
    prettyDocWith FPSingle (ErrFloor0 r e)
        = text "aeboundsp_flr_t" <> (text "(" <>  prettyDocWith FPSingle r <> comma
                                              <+> prettyDocWith FPSingle e <> text ")")
    prettyDocWith FPSingle (ErrSqrt r e)
        = text "aeboundsp_sqt" <> (text "(" <>  prettyDocWith FPSingle r <> comma
                                            <+> prettyDocWith FPSingle e <> text ")")
    prettyDocWith FPSingle (ErrSin r e)
        = text "aeboundsp_sin" <> (text "(" <>  prettyDocWith FPSingle r <> comma
                                            <+> prettyDocWith FPSingle e <> text ")")
    prettyDocWith FPSingle (ErrCos r e)
        = text "aeboundsp_cos" <> (text "(" <>  prettyDocWith FPSingle r <> comma
                                            <+> prettyDocWith FPSingle e <> text ")")
    prettyDocWith FPSingle (ErrTan r e)
        = text "aeboundsp_tan" <> (text "(" <>  prettyDocWith FPSingle r <> comma
                                            <+> prettyDocWith FPSingle e <> text ")")
    prettyDocWith FPSingle (ErrAcos r e)
        = text "aeboundsp_acs" <> (text "(" <>  prettyDocWith FPSingle r <> comma
                                            <+> prettyDocWith FPSingle e <> text ")")
    prettyDocWith FPSingle (ErrAsin r e)
        = text "aeboundsp_asn" <> (text "(" <>  prettyDocWith FPSingle r <> comma
                                            <+> prettyDocWith FPSingle e <> text ")")
    prettyDocWith FPSingle (ErrAtan r e)
        = text "aeboundsp_atn" <> (text "(" <>  prettyDocWith FPSingle r <> comma
                                            <+> prettyDocWith FPSingle e <> text ")")  
    prettyDocWith FPSingle (ErrAtanT r e)
        = text "aeboundsp_atn_t" <> (text "(" <>  prettyDocWith FPSingle r <> comma
                                              <+> prettyDocWith FPSingle e <> text ")")
    prettyDocWith FPSingle (ErrNeg r e)
        = text "aeboundsp_neg" <> (text "(" <>  prettyDocWith FPSingle r <> comma
                                            <+> prettyDocWith FPSingle e <> text ")") 
    prettyDocWith FPSingle (ErrAbs r e)
        = text "aeboundsp_abs" <> (text "(" <>  prettyDocWith FPSingle r <> comma
                                            <+> prettyDocWith FPSingle e <> text ")") 
    prettyDocWith FPSingle (ErrExpo r e)
        = text "aeboundsp_exp" <> (text "(" <>  prettyDocWith FPSingle r <> comma
                                            <+> prettyDocWith FPSingle e <> text ")") 
    prettyDocWith FPSingle (ErrLn r e)
        = text "aeboundsp_ln" <> (text "(" <>  prettyDocWith FPSingle r <> comma
                                            <+> prettyDocWith FPSingle e <> text ")") 
    prettyDocWith FPSingle (ErrMulPow2L n e)
        = text "aeboundsp_mul_p2l" <> (text "(" <> integer n <> comma
                                               <+> prettyDocWith FPSingle e <> text ")") 
    prettyDocWith FPSingle (ErrMulPow2R n e)
        = text "aeboundsp_mul_p2r" <> (text "(" <> integer n <> comma
                                               <+> prettyDocWith FPSingle e <> text ")") 

    prettyDocWith FPSingle (HalfUlp r@(RealMark x))
        = text "ulp_sp" <> (text "(" <> prettyDocWith FPSingle r <> text ")/2")
    prettyDocWith FPSingle (HalfUlp a)
        = error ("ppEEExpr: unexpected value in  HalfUlp " ++ show a)

    prettyDocWith FPDouble (ErrAdd r1 e1 r2 e2)
        = text "aebounddp_add" <> (text "(" <>  prettyDocWith FPDouble r1 <> comma
                                            <+> prettyDocWith FPDouble e1 <> comma
                                            <+> prettyDocWith FPDouble r2 <> comma
                                            <+> prettyDocWith FPDouble e2 <> text ")")
    prettyDocWith FPDouble (ErrSub r1 e1 r2 e2) 
        = text "aebounddp_sub" <> (text "(" <>  prettyDocWith FPDouble r1 <> comma
                                            <+> prettyDocWith FPDouble e1 <> comma
                                            <+> prettyDocWith FPDouble r2 <> comma
                                            <+> prettyDocWith FPDouble e2 <> text ")")
    prettyDocWith FPDouble (ErrMul r1 e1 r2 e2)
        = text "aebounddp_mul" <> (text "(" <>  prettyDocWith FPDouble r1 <> comma
                                            <+> prettyDocWith FPDouble e1 <> comma
                                            <+> prettyDocWith FPDouble r2 <> comma
                                            <+> prettyDocWith FPDouble e2 <> text ")")
    prettyDocWith FPDouble (ErrDiv r1 e1 r2 e2) 
        = text "aebounddp_div" <> (text "(" <>  prettyDocWith FPDouble r1 <> comma
                                            <+> prettyDocWith FPDouble e1 <> comma
                                            <+> prettyDocWith FPDouble r2 <> comma
                                            <+> prettyDocWith FPDouble e2 <> text ")")
    prettyDocWith FPDouble (ErrFloor r e)
        = text "aebounddp_flr" <> (text "(" <>  prettyDocWith FPDouble r <> comma
                                            <+> prettyDocWith FPDouble e <> text ")")
    prettyDocWith FPDouble (ErrFloor0 r e)
        = text "aebounddp_flr_t" <> (text "(" <>  prettyDocWith FPDouble r <> comma
                                              <+> prettyDocWith FPDouble e <> text ")")
    prettyDocWith FPDouble (ErrSqrt r e)
        = text "aebounddp_sqt" <> (text "(" <>  prettyDocWith FPDouble r <> comma
                                            <+> prettyDocWith FPDouble e <> text ")")
    prettyDocWith FPDouble (ErrSin r e)
        = text "aebounddp_sin" <> (text "(" <>  prettyDocWith FPDouble r <> comma
                                            <+> prettyDocWith FPDouble e <> text ")")
    prettyDocWith FPDouble (ErrCos r e)
        = text "aebounddp_cos" <> (text "(" <>  prettyDocWith FPDouble r <> comma
                                            <+> prettyDocWith FPDouble e <> text ")")
    prettyDocWith FPDouble (ErrTan r e)
        = text "aebounddp_tan" <> (text "(" <>  prettyDocWith FPDouble r <> comma
                                            <+> prettyDocWith FPDouble e <> text ")")
    prettyDocWith FPDouble (ErrAcos r e)
        = text "aebounddp_acs" <> (text "(" <>  prettyDocWith FPDouble r <> comma
                                            <+> prettyDocWith FPDouble e <> text ")")
    prettyDocWith FPDouble (ErrAsin r e)
        = text "aebounddp_asn" <> (text "(" <>  prettyDocWith FPDouble r <> comma
                                            <+> prettyDocWith FPDouble e <> text ")")
    prettyDocWith FPDouble (ErrAtan r e)
        = text "aebounddp_atn" <> (text "(" <>  prettyDocWith FPDouble r <> comma
                                            <+> prettyDocWith FPDouble e <> text ")")  
    prettyDocWith FPDouble (ErrAtanT r e)
        = text "aebounddp_atn_t" <> (text "(" <>  prettyDocWith FPDouble r <> comma
                                              <+> prettyDocWith FPDouble e <> text ")")    
    prettyDocWith FPDouble (ErrNeg r e)
        = text "aebounddp_neg" <> (text "(" <>  prettyDocWith FPDouble r <> comma
                                            <+> prettyDocWith FPDouble e <> text ")") 
    prettyDocWith FPDouble (ErrAbs r e)
        = text "aebounddp_abs" <> (text "(" <>  prettyDocWith FPDouble r <> comma
                                            <+> prettyDocWith FPDouble e <> text ")") 
    prettyDocWith FPDouble (ErrExpo r e)
        = text "aebounddp_exp" <> (text "(" <>  prettyDocWith FPDouble r <> comma
                                            <+> prettyDocWith FPDouble e <> text ")") 
    prettyDocWith FPDouble (ErrLn r e)
        = text "aebounddp_ln" <> (text "(" <>  prettyDocWith FPDouble r <> comma
                                            <+> prettyDocWith FPDouble e <> text ")") 
    prettyDocWith FPDouble (ErrMulPow2L n e)
        = text "aebounddp_mul_p2l" <> (text "(" <> integer n <> comma
                                  <+> prettyDocWith FPDouble e <> text ")") 

    prettyDocWith FPDouble (ErrMulPow2R n e)
        = text "aebounddp_mul_p2r" <> (text "(" <> integer n <> comma
                                  <+> prettyDocWith FPDouble e <> text ")") 

    -- prettyDocWith FPDouble (HalfUlp (Var x))
    prettyDocWith FPDouble (HalfUlp r@(RealMark x))
        = text "ulp_dp" <> (text "(" <> prettyDocWith FPDouble r <> text ")/2")
    prettyDocWith FPDouble (HalfUlp a)
        = error ("ppEEExpr: unexpected value in  HalfUlp"++ show a)


    prettyDocWith fp                p = error $ "prettyDocWith "++show p

    prettyKodiakWith _                Pi = text "Pi"
    prettyKodiakWith _           (Int i) =
        if length (show i) > 9
            then text "val" <> (parens $ text "Interval" <> (parens $ (text (showFullPrecision lb) <> comma <> text (showFullPrecision ub))))
            else text "val" <> (parens $ integer i)
        where
            (lb,ub) = rat2interval (toRational i) 
    prettyKodiakWith _        (Double r) =
        if (length (show num) > 9) || (length (show den) > 9)
            then text "val" <> (parens $ text "Interval" <> (parens $ (text (showFullPrecision lb) <> comma <> text (showFullPrecision ub))))
            else parens $ text "val" <> (parens $ text "rat" <> (parens $ (integer num) <> comma <> (integer den)))
        where
            (lb,ub) = rat2interval r
            num = (numerator r)
            den = (denominator r)
    prettyKodiakWith _           (Var x) = text x
    prettyKodiakWith _       (EFun f []) = text f
    prettyKodiakWith fp      (Add a1 a2) = parens $ (prettyKodiakWith fp a1) <+> text "+" <+> (prettyKodiakWith fp a2)
    prettyKodiakWith fp      (Sub a1 a2) = parens $ (prettyKodiakWith fp a1) <+> text "-" <+> (prettyKodiakWith fp a2)
    prettyKodiakWith fp      (Mul a1 a2) = parens $ (prettyKodiakWith fp a1) <+> text "*" <+> (prettyKodiakWith fp a2)
    prettyKodiakWith fp      (Div a1 a2) = parens $ (prettyKodiakWith fp a1) <+> text "/" <+> (prettyKodiakWith fp a2)
    prettyKodiakWith fp      (Mod a1 a2) = error "prettyKodiakWith niy"
    prettyKodiakWith fp  (Pow a (Int n)) = (parens $ prettyKodiakWith fp a) <> text "^" <> integer n
    prettyKodiakWith fp      (Pow a1 a2) = error "prettyKodiakWith niy"
    prettyKodiakWith fp        (Neg   a) = parens $ text "-" <> prettyKodiakWith fp a
    prettyKodiakWith fp        (Floor a) = text "Floor" <> lparen <> prettyKodiakWith fp a <> rparen --TODO
    prettyKodiakWith fp        (Sqrt  a) = text "Sqrt"  <> lparen <> prettyKodiakWith fp a <> rparen
    prettyKodiakWith fp        (Abs   a) = text "Abs"   <> lparen <> prettyKodiakWith fp a <> rparen
    prettyKodiakWith fp        (Sin   a) = text "Sin"   <> lparen <> prettyKodiakWith fp a <> rparen
    prettyKodiakWith fp        (Cos   a) = text "Cos"   <> lparen <> prettyKodiakWith fp a <> rparen
    prettyKodiakWith fp        (Tan   a) = text "Tan"   <> lparen <> prettyKodiakWith fp a <> rparen
    prettyKodiakWith fp        (ASin  a) = text "Asin"  <> lparen <> prettyKodiakWith fp a <> rparen
    prettyKodiakWith fp        (ACos  a) = text "Acos"  <> lparen <> prettyKodiakWith fp a <> rparen
    prettyKodiakWith fp        (ATan  a) = text "Atan"  <> lparen <> prettyKodiakWith fp a <> rparen
    prettyKodiakWith fp        (Ln  a)   = text "Ln"  <> lparen <> prettyKodiakWith fp a <> rparen
    prettyKodiakWith fp        (Expo  a) = text "Exp"  <> lparen <> prettyKodiakWith fp a <> rparen
    prettyKodiakWith fp    (EFun f args) = text f <> (parens $ hsep $ punctuate comma $ map (\x -> prettyKodiakWith fp x) args)
    prettyKodiakWith fp         (SUlp a) = text "SUlp" <> lparen <> prettyKodiakWith FPDouble a <> rparen
    prettyKodiakWith fp         (DUlp a) = text "DUlp" <> lparen <> prettyKodiakWith FPDouble a <> rparen
    prettyKodiakWith fp         (StoR a) = prettyKodiakWith FPSingle a
    prettyKodiakWith fp         (DtoR a) = prettyKodiakWith FPDouble a
    prettyKodiakWith fp           (EE e) = prettyKodiakWith fp e
    prettyKodiakWith FPSingle    (FPrec) = text "val(24)"
    prettyKodiakWith FPDouble    (FPrec) = text "val(52)"
    prettyKodiakWith fp        (FExp fa) = error "prettyKodiakWith FExp niy"
    prettyKodiakWith fp     (RealMark x) = text x
    prettyKodiakWith fp         (Max es) = text "Max" <> parens (lbrace <> (hsep $ punctuate comma $ (map (prettyKodiakWith fp) es)) <> rbrace)
    prettyKodiakWith fp         (Min es) = text "Min" <> parens (lbrace <> (hsep $ punctuate comma $ (map (prettyKodiakWith fp) es)) <> rbrace)
    
    prettyKodiakWith fp (ErrRat r) = prettyKodiakWith fp (Double r)
        -- = parens $ text "val" <> (parens $ text "rat" <> (parens $ (integer (numerator r)) <> comma <> (integer (denominator r))))

    prettyKodiakWith _ (ErrorMark x)
        = error "prettyKodiakWith ErrorMark: error expression not initialized"

    prettyKodiakWith fp (MaxErr []) = error "Something went wrong: MaxErr applied to empty list"

    --prettyKodiakWith fp (MaxErr [es]) = prettyKodiakWith fp es

    prettyKodiakWith fp (MaxErr ees) = 
        case ees' of
            [e] -> prettyKodiakWith fp e
            otherwise -> text "Max" <> parens (lbrace <> (hsep $ punctuate comma (map (prettyKodiakWith fp) $ ees')) <> rbrace )
        where
            ees' = maxFlatten ees

    prettyKodiakWith fp (AE a) = prettyKodiakWith fp a

    prettyKodiakWith FPSingle (ErrAdd r1 e1 r2 e2) 
        = text "aeboundsp_add" <> (text "(" <>  prettyKodiakWith FPSingle r1 <> comma
                                            <+> prettyKodiakWith FPSingle e1 <> comma
                                            <+> prettyKodiakWith FPSingle r2 <> comma
                                            <+> prettyKodiakWith FPSingle e2 <> text ")")
    prettyKodiakWith FPSingle (ErrSub r1 e1 r2 e2) 
        = text "aeboundsp_sub" <> (text "(" <>  prettyKodiakWith FPSingle r1 <> comma
                                            <+> prettyKodiakWith FPSingle e1 <> comma
                                            <+> prettyKodiakWith FPSingle r2 <> comma
                                            <+> prettyKodiakWith FPSingle e2 <> text ")")
    prettyKodiakWith FPSingle (ErrMul r1 e1 r2 e2) 
        = text "aeboundsp_mul" <> (text "(" <>  prettyKodiakWith FPSingle r1 <> comma
                                            <+> prettyKodiakWith FPSingle e1 <> comma
                                            <+> prettyKodiakWith FPSingle r2 <> comma
                                            <+> prettyKodiakWith FPSingle e2 <> text ")")
    prettyKodiakWith FPSingle (ErrDiv r1 e1 r2 e2) 
        = text "aeboundsp_div" <> (text "(" <>  prettyKodiakWith FPSingle r1 <> comma
                                            <+> prettyKodiakWith FPSingle e1 <> comma
                                            <+> prettyKodiakWith FPSingle r2 <> comma
                                            <+> prettyKodiakWith FPSingle e2 <> text ")")
    prettyKodiakWith FPSingle (ErrFloor r e)
        = text "aeboundsp_flr" <> (text "(" <>  prettyKodiakWith FPSingle r <> comma
                                            <+> prettyKodiakWith FPSingle e <> text ")")
    prettyKodiakWith FPSingle (ErrFloor0 r e)
        = text "aeboundsp_flr_t" <> (text "(" <>  prettyKodiakWith FPSingle r <> comma
                                              <+> prettyKodiakWith FPSingle e <> text ")")
    prettyKodiakWith FPSingle (ErrSqrt r e)
        = text "aeboundsp_sqt" <> (text "(" <>  prettyKodiakWith FPSingle r <> comma
                                            <+> prettyKodiakWith FPSingle e <> text ")")
    prettyKodiakWith FPSingle (ErrSin r e)
        = text "aeboundsp_sin" <> (text "(" <>  prettyKodiakWith FPSingle r <> comma
                                            <+> prettyKodiakWith FPSingle e <> text ")")
    prettyKodiakWith FPSingle (ErrCos r e)
        = text "aeboundsp_cos" <> (text "(" <>  prettyKodiakWith FPSingle r <> comma
                                            <+> prettyKodiakWith FPSingle e <> text ")")
    prettyKodiakWith FPSingle (ErrTan r e)
        = text "aeboundsp_tan" <> (text "(" <>  prettyKodiakWith FPSingle r <> comma
                                            <+> prettyKodiakWith FPSingle e <> text ")")
    prettyKodiakWith FPSingle (ErrAcos r e)
        = text "aeboundsp_acs" <> (text "(" <>  prettyKodiakWith FPSingle r <> comma
                                            <+> prettyKodiakWith FPSingle e <> text ")")
    prettyKodiakWith FPSingle (ErrAsin r e)
        = text "aeboundsp_asn" <> (text "(" <>  prettyKodiakWith FPSingle r <> comma
                                            <+> prettyKodiakWith FPSingle e <> text ")")
    prettyKodiakWith FPSingle (ErrAtan r e)
        = text "aeboundsp_atn" <> (text "(" <>  prettyKodiakWith FPSingle r <> comma
                                            <+> prettyKodiakWith FPSingle e <> text ")")  
    prettyKodiakWith FPSingle (ErrAtanT r e)
        = text "aeboundsp_atn_t" <> (text "(" <>  prettyKodiakWith FPSingle r <> comma
                                              <+> prettyKodiakWith FPSingle e <> text ")")
    prettyKodiakWith FPSingle (ErrNeg r e)
        = text "aeboundsp_neg" <> (text "(" <>  prettyKodiakWith FPSingle r <> comma
                                            <+> prettyKodiakWith FPSingle e <> text ")") 
    prettyKodiakWith FPSingle (ErrAbs r e)
        = text "aeboundsp_abs" <> (text "(" <>  prettyKodiakWith FPSingle r <> comma
                                            <+> prettyKodiakWith FPSingle e <> text ")") 
    prettyKodiakWith FPSingle (ErrExpo r e)
        = text "aeboundsp_exp" <> (text "(" <>  prettyKodiakWith FPSingle r <> comma
                                            <+> prettyKodiakWith FPSingle e <> text ")") 
    prettyKodiakWith FPSingle (ErrLn r e)
        = text "aeboundsp_ln" <> (text "(" <>  prettyKodiakWith FPSingle r <> comma
                                            <+> prettyKodiakWith FPSingle e <> text ")") 
    prettyKodiakWith FPSingle (ErrMulPow2L n e)
        = text "aeboundsp_mul_p2l" <> (text "(" <> integer n <> comma
                                               <+> prettyKodiakWith FPSingle e <> text ")") 
    prettyKodiakWith FPSingle (ErrMulPow2R n e)
        = text "aeboundsp_mul_p2r" <> (text "(" <> integer n <> comma
                                               <+> prettyKodiakWith FPSingle e <> text ")") 
    --prettyKodiakWith FPSingle (HalfUlp (Var x))
    prettyKodiakWith FPSingle r@(HalfUlp (RealMark x))
        = text "SUlp" <> (text "(" <> prettyDocWith FPDouble r <> text ")/val(2)")

    prettyKodiakWith FPSingle (HalfUlp a)
        = error ("ppEEExpr: unexpected value in  HalfUlp " ++ show a)

    prettyKodiakWith FPDouble (ErrAdd r1 e1 r2 e2)
        = text "aebounddp_add" <> (text "(" <>  prettyKodiakWith FPDouble r1 <> comma
                                            <+> prettyKodiakWith FPDouble e1 <> comma
                                            <+> prettyKodiakWith FPDouble r2 <> comma
                                            <+> prettyKodiakWith FPDouble e2 <> text ")")
    prettyKodiakWith FPDouble (ErrSub r1 e1 r2 e2) 
        = text "aebounddp_sub" <> (text "(" <>  prettyKodiakWith FPDouble r1 <> comma
                                            <+> prettyKodiakWith FPDouble e1 <> comma
                                            <+> prettyKodiakWith FPDouble r2 <> comma
                                            <+> prettyKodiakWith FPDouble e2 <> text ")")
    prettyKodiakWith FPDouble (ErrMul r1 e1 r2 e2)
        = text "aebounddp_mul" <> (text "(" <>  prettyKodiakWith FPDouble r1 <> comma
                                            <+> prettyKodiakWith FPDouble e1 <> comma
                                            <+> prettyKodiakWith FPDouble r2 <> comma
                                            <+> prettyKodiakWith FPDouble e2 <> text ")")
    prettyKodiakWith FPDouble (ErrDiv r1 e1 r2 e2) 
        = text "aebounddp_div" <> (text "(" <>  prettyKodiakWith FPDouble r1 <> comma
                                            <+> prettyKodiakWith FPDouble e1 <> comma
                                            <+> prettyKodiakWith FPDouble r2 <> comma
                                            <+> prettyKodiakWith FPDouble e2 <> text ")")
    prettyKodiakWith FPDouble (ErrFloor r e)
        = text "aebounddp_flr" <> (text "(" <>  prettyKodiakWith FPDouble r <> comma
                                            <+> prettyKodiakWith FPDouble e <> text ")")
    prettyKodiakWith FPDouble (ErrFloor0 r e)
        = text "aebounddp_flr_t" <> (text "(" <>  prettyKodiakWith FPDouble r <> comma
                                              <+> prettyKodiakWith FPDouble e <> text ")")
    prettyKodiakWith FPDouble (ErrSqrt r e)
        = text "aebounddp_sqt" <> (text "(" <>  prettyKodiakWith FPDouble r <> comma
                                            <+> prettyKodiakWith FPDouble e <> text ")")
    prettyKodiakWith FPDouble (ErrSin r e)
        = text "aebounddp_sin" <> (text "(" <>  prettyKodiakWith FPDouble r <> comma
                                            <+> prettyKodiakWith FPDouble e <> text ")")
    prettyKodiakWith FPDouble (ErrCos r e)
        = text "aebounddp_cos" <> (text "(" <>  prettyKodiakWith FPDouble r <> comma
                                            <+> prettyKodiakWith FPDouble e <> text ")")
    prettyKodiakWith FPDouble (ErrTan r e)
        = text "aebounddp_tan" <> (text "(" <>  prettyKodiakWith FPDouble r <> comma
                                            <+> prettyKodiakWith FPDouble e <> text ")")
    prettyKodiakWith FPDouble (ErrAcos r e)
        = text "aebounddp_acs" <> (text "(" <>  prettyKodiakWith FPDouble r <> comma
                                            <+> prettyKodiakWith FPDouble e <> text ")")
    prettyKodiakWith FPDouble (ErrAsin r e)
        = text "aebounddp_asn" <> (text "(" <>  prettyKodiakWith FPDouble r <> comma
                                            <+> prettyKodiakWith FPDouble e <> text ")")
    prettyKodiakWith FPDouble (ErrAtan r e)
        = text "aebounddp_atn" <> (text "(" <>  prettyKodiakWith FPDouble r <> comma
                                            <+> prettyKodiakWith FPDouble e <> text ")")  
    prettyKodiakWith FPDouble (ErrAtanT r e)
        = text "aebounddp_atn_t" <> (text "(" <>  prettyKodiakWith FPDouble r <> comma
                                              <+> prettyKodiakWith FPDouble e <> text ")")    
    prettyKodiakWith FPDouble (ErrNeg r e)
        = text "aebounddp_neg" <> (text "(" <>  prettyKodiakWith FPDouble r <> comma
                                            <+> prettyKodiakWith FPDouble e <> text ")") 
    prettyKodiakWith FPDouble (ErrAbs r e)
        = text "aebounddp_abs" <> (text "(" <>  prettyKodiakWith FPDouble r <> comma
                                            <+> prettyKodiakWith FPDouble e <> text ")") 
    prettyKodiakWith FPDouble (ErrExpo r e)
        = text "aebounddp_exp" <> (text "(" <>  prettyKodiakWith FPDouble r <> comma
                                            <+> prettyKodiakWith FPDouble e <> text ")") 
    prettyKodiakWith FPDouble (ErrLn r e)
        = text "aebounddp_ln" <> (text "(" <>  prettyKodiakWith FPDouble r <> comma
                                            <+> prettyKodiakWith FPDouble e <> text ")") 
    prettyKodiakWith FPDouble (ErrMulPow2L n e)
        = text "aebounddp_mul_p2l" <> (text "(" <> integer n <> comma
                                  <+> prettyKodiakWith FPDouble e <> text ")")  

    prettyKodiakWith FPDouble (ErrMulPow2R n e)
        = text " aebounddp_mul_p2r" <> (text "(" <> integer n <> comma
                                  <+> prettyKodiakWith FPDouble e <> text ")")

    --prettyKodiakWith FPDouble (HalfUlp (Var x))
    prettyKodiakWith FPDouble r@(HalfUlp (RealMark x))
        = text "DUlp" <> (text "(" <> prettyDocWith FPDouble r <> text ")/val(2)")
    prettyKodiakWith FPDouble (HalfUlp a)
        = error ("ppEEExpr: unexpected value in  HalfUlp"++ show a)

    prettyKodiakWith fp                p = error $ "prettyKodiakWith " ++ show p


instance PPExt FAExpr where

    prettyDoc _ = error "prettyDoc: undefined for FAExpr, fp needed"

    prettyDocWith _            (FInt i) = integer i
    -- prettyDocWith _         (FDouble d) = parens $ text $ showFullPrecisionRat d
    prettyDocWith _         (FDouble d) = parens $ text $ showRational d
    prettyDocWith _     (FNeg (FInt i)) = text "-" <> integer i
    -- prettyDocWith _  (FNeg (FDouble d)) = text "-" <> (text $ showFullPrecisionRat d)
    prettyDocWith _  (FNeg (FDouble d)) = text "-" <> parens (text $ showRational d)
    prettyDocWith _            (FVar x) = text x
    prettyDocWith _        (FEFun f []) = text f
    prettyDocWith fp     (FEFun f args) = text f <> (parens $ hsep $ punctuate comma $ map (\x -> prettyDocWith fp x) args)
    prettyDocWith fp           (RtoS a) = text "RtoS" <> lparen <> prettyDocWith fp a <> rparen
    prettyDocWith fp           (RtoD a) = text "RtoD" <> lparen <> prettyDocWith fp a <> rparen 
    prettyDocWith FPSingle (FPow a1 a2) = (prettyDocWith FPSingle a1) <+> text "^" <+> lparen <> (prettyDocWith FPSingle a2) <> rparen
    prettyDocWith FPSingle (FAdd a1 a2) = text "Sadd" <> parens ((prettyDocWith FPSingle a1) <> comma <+> (prettyDocWith FPSingle a2))
    prettyDocWith FPSingle (FSub a1 a2) = text "Ssub" <> parens ((prettyDocWith FPSingle a1) <> comma <+> (prettyDocWith FPSingle a2))
    prettyDocWith FPSingle (FMul a1 a2) = text "Smul" <> parens ((prettyDocWith FPSingle a1) <> comma <+> (prettyDocWith FPSingle a2))
    prettyDocWith FPSingle (FDiv a1 a2) = text "Sdiv" <> parens ((prettyDocWith FPSingle a1) <> comma <+> (prettyDocWith FPSingle a2))
    prettyDocWith FPSingle (FMod a1 a2) = text "Smod" <> parens ((prettyDocWith FPSingle a1) <> comma <+> (prettyDocWith FPSingle a2))
    prettyDocWith FPSingle     (FNeg a) = text "Sneg"   <> lparen <> (prettyDocWith FPSingle a) <> rparen
    prettyDocWith FPSingle   (FFloor a) = text "Sfloor" <> lparen <> (prettyDocWith FPSingle a) <> rparen
    prettyDocWith FPSingle    (FSqrt a) = text "Ssqrt"  <> lparen <> (prettyDocWith FPSingle a) <> rparen
    prettyDocWith FPSingle     (FAbs a) = text "Sabs"   <> lparen <> (prettyDocWith FPSingle a) <> rparen
    prettyDocWith FPSingle     (FSin a) = text "Ssin"   <> lparen <> (prettyDocWith FPSingle a) <> rparen
    prettyDocWith FPSingle     (FCos a) = text "Scos"   <> lparen <> (prettyDocWith FPSingle a) <> rparen
    prettyDocWith FPSingle     (FTan a) = text "Stan"   <> lparen <> (prettyDocWith FPSingle a) <> rparen
    prettyDocWith FPSingle    (FAsin a) = text "Sasin"  <> lparen <> (prettyDocWith FPSingle a) <> rparen
    prettyDocWith FPSingle    (FAcos a) = text "Sacos"  <> lparen <> (prettyDocWith FPSingle a) <> rparen
    prettyDocWith FPSingle    (FAtan a) = text "Satan"  <> lparen <> (prettyDocWith FPSingle a) <> rparen
    prettyDocWith FPSingle    (FLn   a) = text "Sln"  <> lparen <> (prettyDocWith FPSingle a) <> rparen
    prettyDocWith FPSingle    (FExpo a) = text "Sexp"  <> lparen <> (prettyDocWith FPSingle a) <> rparen
    prettyDocWith FPSingle        (FPi) = text "Spi"
    prettyDocWith FPDouble (FAdd a1 a2) = text "Dadd" <> parens ((prettyDocWith FPDouble a1) <> comma <+> (prettyDocWith FPDouble a2))
    prettyDocWith FPDouble (FSub a1 a2) = text "Dsub" <> parens ((prettyDocWith FPDouble a1) <> comma <+> (prettyDocWith FPDouble a2))
    prettyDocWith FPDouble (FMul a1 a2) = text "Dmul" <> parens ((prettyDocWith FPDouble a1) <> comma <+> (prettyDocWith FPDouble a2))
    prettyDocWith FPDouble (FDiv a1 a2) = text "Ddiv" <> parens ((prettyDocWith FPDouble a1) <> comma <+> (prettyDocWith FPDouble a2))
    prettyDocWith FPDouble (FMod a1 a2) = text "Dmod" <> parens ((prettyDocWith FPDouble a1) <> comma <+> (prettyDocWith FPDouble a2))
    prettyDocWith FPDouble     (FNeg a) = text "Dneg"   <> lparen <> (prettyDocWith FPDouble a) <> rparen
    prettyDocWith FPDouble   (FFloor a) = text "Dfloor" <> lparen <> (prettyDocWith FPDouble a) <> rparen
    prettyDocWith FPDouble    (FSqrt a) = text "Dsqrt"  <> lparen <> (prettyDocWith FPDouble a) <> rparen
    prettyDocWith FPDouble     (FAbs a) = text "Dabs"   <> lparen <> (prettyDocWith FPDouble a) <> rparen
    prettyDocWith FPDouble     (FSin a) = text "Dsin"   <> lparen <> (prettyDocWith FPDouble a) <> rparen
    prettyDocWith FPDouble     (FCos a) = text "Dcos"   <> lparen <> (prettyDocWith FPDouble a) <> rparen
    prettyDocWith FPDouble     (FTan a) = text "Dtan"   <> lparen <> (prettyDocWith FPDouble a) <> rparen
    prettyDocWith FPDouble    (FAsin a) = text "Dasin"  <> lparen <> (prettyDocWith FPDouble a) <> rparen
    prettyDocWith FPDouble    (FAcos a) = text "Dacos"  <> lparen <> (prettyDocWith FPDouble a) <> rparen
    prettyDocWith FPDouble    (FAtan a) = text "Datan"  <> lparen <> (prettyDocWith FPDouble a) <> rparen
    prettyDocWith FPDouble    (FLn   a) = text "Dln"  <> lparen <> (prettyDocWith FPDouble a) <> rparen
    prettyDocWith FPDouble    (FExpo a) = text "Dexp"  <> lparen <> (prettyDocWith FPDouble a) <> rparen
    prettyDocWith FPDouble        (FPi) = text "Dpi"




instance PPExt BExpr where

    prettyDoc _ = error "prettyDoc: undefined for BExpr, fp needed"

    prettyDocWith fp (Or  e1 e2) = parens $ prettyDocWith fp e1 <+> text "OR"  <+> prettyDocWith fp e2 
    prettyDocWith fp (And e1 e2) = parens $ prettyDocWith fp e1 <+> text "AND" <+> prettyDocWith fp e2
    prettyDocWith fp     (Not e) = text "NOT" <> (parens $ prettyDocWith fp e)
    prettyDocWith fp  (Eq a1 a2) = parens $ prettyDocWith fp a1 <+> text "="  <+> prettyDocWith fp a2 
    prettyDocWith fp (Neq a1 a2) = parens $ prettyDocWith fp a1 <+> text "/=" <+> prettyDocWith fp a2 
    prettyDocWith fp  (Lt a1 a2) = parens $ prettyDocWith fp a1 <+> text "<"  <+> prettyDocWith fp a2 
    prettyDocWith fp (LtE a1 a2) = parens $ prettyDocWith fp a1 <+> text "<=" <+> prettyDocWith fp a2 
    prettyDocWith fp  (Gt a1 a2) = parens $ prettyDocWith fp a1 <+> text ">"  <+> prettyDocWith fp a2  
    prettyDocWith fp (GtE a1 a2) = parens $ prettyDocWith fp a1 <+> text ">=" <+> prettyDocWith fp a2  
    prettyDocWith fp       BTrue = text "TRUE"
    prettyDocWith fp      BFalse = text "FALSE"

    --prettyKodiakWith fp (Or  b1 b2) = parens $ (prettyKodiakWith fp b1) <> text "Or"  <> (prettyKodiakWith fp b1)
    --prettyKodiakWith fp (And b1 b2) = parens $ (prettyKodiakWith fp b1) <> text "And" <> (prettyKodiakWith fp b1)
    --prettyKodiakWith fp (Not b)     = parens $ text "Not" <> (prettyKodiakWith fp b)
    --prettyKodiakWith fp (Eq  a1 a2) = parens $ (prettyKodiakWith fp a1) <> text "==" <> (prettyKodiakWith fp a2)
    --prettyKodiakWith fp (Neq a1 a2) = parens $ (prettyKodiakWith fp a1) <> text "!=" <> (prettyKodiakWith fp a2)
    --prettyKodiakWith fp (Lt  a1 a2) = parens $ (prettyKodiakWith fp a1) <> text "<"  <> (prettyKodiakWith fp a2)
    --prettyKodiakWith fp (LtE a1 a2) = parens $ (prettyKodiakWith fp a1) <> text "<=" <> (prettyKodiakWith fp a2)
    --prettyKodiakWith fp (Gt  a1 a2) = parens $ (prettyKodiakWith fp a2) <> text "<"  <> (prettyKodiakWith fp a1)
    --prettyKodiakWith fp (GtE a1 a2) = parens $ (prettyKodiakWith fp a2) <> text "<=" <> (prettyKodiakWith fp a1)
    --prettyKodiakWith _ BTrue  = text "True"
    --prettyKodiakWith _ BFalse = text "False"


instance PPExt FBExpr where
    
    prettyDoc _ = error "prettyDoc: undefined for FBExpr, fp needed"

    prettyDocWith fp  (FOr e1 e2) = parens $ prettyDocWith fp e1 <+> text "OR"  <+> prettyDocWith fp e2
    prettyDocWith fp (FAnd e1 e2) = parens $ prettyDocWith fp e1 <+> text "AND" <+> prettyDocWith fp e2
    prettyDocWith fp     (FNot e) = text "NOT" <> (parens $ prettyDocWith fp e)
    prettyDocWith fp  (FEq a1 a2) = parens $ prettyDocWith fp a1 <+> text "="  <+> prettyDocWith fp a2
    prettyDocWith fp (FNeq a1 a2) = parens $ prettyDocWith fp a1 <+> text "/=" <+> prettyDocWith fp a2
    prettyDocWith fp  (FLt a1 a2) = parens $ prettyDocWith fp a1 <+> text "<"  <+> prettyDocWith fp a2
    prettyDocWith fp (FLtE a1 a2) = parens $ prettyDocWith fp a1 <+> text "<=" <+> prettyDocWith fp a2
    prettyDocWith fp  (FGt a1 a2) = parens $ prettyDocWith fp a1 <+> text ">"  <+> prettyDocWith fp a2
    prettyDocWith fp (FGtE a1 a2) = parens $ prettyDocWith fp a1 <+> text ">=" <+> prettyDocWith fp a2
    prettyDocWith fp       FBTrue = text "TRUE"
    prettyDocWith fp      FBFalse = text "FALSE"

    --prettyKodiakWith fp (FOr  fb1 fb2) = parens $ (prettyKodiakWith fp fb1) <> text "Or"  <> (prettyKodiakWith fp fb1)
    --prettyKodiakWith fp (FAnd fb1 fb2) = parens $ (prettyKodiakWith fp fb1) <> text "And" <> (prettyKodiakWith fp fb1)
    --prettyKodiakWith fp (FNot fb)      = parens $ text "Not" <> (prettyKodiakWith fp fb)
    --prettyKodiakWith fp (FEq  fa1 fa2) = parens $ (prettyKodiakWith fp fa1) <> text "==" <> (prettyKodiakWith fp fa2)
    --prettyKodiakWith fp (FNeq fa1 fa2) = parens $ (prettyKodiakWith fp fa1) <> text "!=" <> (prettyKodiakWith fp fa2)
    --prettyKodiakWith fp (FLt  fa1 fa2) = parens $ (prettyKodiakWith fp fa1) <> text "<"  <> (prettyKodiakWith fp fa2)
    --prettyKodiakWith fp (FLtE fa1 fa2) = parens $ (prettyKodiakWith fp fa1) <> text "<=" <> (prettyKodiakWith fp fa2)
    --prettyKodiakWith fp (FGt  fa1 fa2) = parens $ (prettyKodiakWith fp fa2) <> text "<"  <> (prettyKodiakWith fp fa1)
    --prettyKodiakWith fp (FGtE fa1 fa2) = parens $ (prettyKodiakWith fp fa2) <> text "<=" <> (prettyKodiakWith fp fa1)
    --prettyKodiakWith _ FBTrue  = text "True"
    --prettyKodiakWith _ FBFalse = text "False"





