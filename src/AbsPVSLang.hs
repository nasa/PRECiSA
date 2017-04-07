-- Copyright 2016 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
--
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS,
-- ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS
-- AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT
-- AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
-- 
-- Waiver and Indemnity: RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS
-- IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT,
-- ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.

module AbsPVSLang
where

import PPExt
import Numeric

-- floating point precision --
data FPrec = FPSingle | FPDouble
  deriving (Eq, Ord, Show, Read)

-- variables and process symbols --
newtype VarId    = VarId String deriving (Eq, Ord, Show, Read)
newtype NonVarId = NonVarId String deriving (Eq, Ord, Show, Read)

data VarDecl = VarDecl VarId
data Imp = Imp [NonVarId]

data EExpr
-- error expressions
    = ErrAdd    AExpr EExpr AExpr EExpr
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
    | ErrMulPow2 Integer EExpr
    | AE AExpr
    | HalfUlp AExpr
    | ErrRat Rational
    deriving (Eq, Ord, Show, Read)

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
    | Abs AExpr
    | Sin AExpr
    | Cos AExpr
    | Tan AExpr
    | ASin AExpr
    | ACos AExpr
    | ATan AExpr
    | Int Integer
    | Double Rational -- Double
    | EFun String [AExpr]
    | Var String
    | Pi
    | SUlp AExpr
    | DUlp AExpr
    | StoR FAExpr
    | DtoR FAExpr
    | EE EExpr
    | FPrec
    | FExp FAExpr
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
    | FInt Integer
    | FDouble Rational -- Double
    | FEFun String [FAExpr]
    | FVar String
    | FPi
    | RtoS AExpr
    | RtoD AExpr
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
data Program = Prog [Decl]
    deriving (Eq, Ord, Show, Read)

-- set of declarations
data Decl = Decl FPrec NonVarId [VarId] Stm
    deriving (Eq, Ord, Show, Read)

-- program expression
data Stm = Let VarId FAExpr Stm
         | Ite FBExpr Stm Stm
         | StmExpr FAExpr
    deriving (Eq, Ord, Show, Read)

-- real valued progam
data RProgram = RProg [RDecl]
    deriving (Eq, Ord, Show, Read)

-- real valued set of declarations
data RDecl = RDecl NonVarId [VarId] RStm
    deriving (Eq, Ord, Show, Read)

-- real valued program expression
data RStm = RLet VarId AExpr RStm
          | RIte BExpr RStm RStm
          | RStmExpr AExpr
    deriving (Eq, Ord, Show, Read)

---------------------------------------------
-- float->real trasformations for programs --
---------------------------------------------

fp2realProg :: Program -> RProgram
fp2realProg (Prog decls) = RProg (map fp2realDecl decls)

fp2realDecl :: Decl -> RDecl
fp2realDecl (Decl fp f xs stm) = RDecl f xs (fp2realStm stm)

fp2realStm :: Stm -> RStm
fp2realStm (Let x fae stm)     = RLet x (fae2real fae) (fp2realStm stm)
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
fae2real (FInt n)       = Int n
fae2real (FDouble n)    = Double n
fae2real (FEFun f args) = EFun f (map (\arg -> fae2real arg) args)
fae2real (FVar x)       = Var ("r_"++x)
fae2real FPi            = Pi 
fae2real (RtoS a)       = a
fae2real (RtoD a)       = a


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
simplAExprAux (Neg (Double n)) = Double (-n)
simplAExprAux (Neg a) = Neg (simplAExpr a)
simplAExprAux (Pow a1 a2) = Pow (simplAExprAux a1) (simplAExprAux a2)
simplAExprAux (Floor a) = Floor (simplAExprAux a)
simplAExprAux (Sqrt a) = Sqrt (simplAExprAux a)
--simplAExprAux (Abs (Int n)) = if n>=0 then Int n else Int (-n)
--simplAExprAux (Abs (Double n)) = if n>=0 then Double n else Double (-n)
simplAExprAux (Abs (Abs a)) = Abs (simplAExprAux a)
simplAExprAux (Abs a) = Abs (simplAExprAux a)
simplAExprAux (Sin a) = Sin (simplAExprAux a)
simplAExprAux (Cos a) = Cos (simplAExprAux a)
simplAExprAux (Tan a) = Cos (simplAExprAux a)
simplAExprAux (ACos a) = ACos (simplAExprAux a)
simplAExprAux (ASin a) = ASin (simplAExprAux a)
simplAExprAux (ATan a) = ATan (simplAExprAux a)
simplAExprAux (Mod a1 a2) = Mod (simplAExprAux a1) (simplAExprAux a2)
simplAExprAux (Int n) = Int n
simplAExprAux (Double n) = Double n
simplAExprAux (Var x) = Var x
simplAExprAux (Pi) = Pi
simplAExprAux (EFun f args) = EFun f (map simplAExprAux args)
simplAExprAux (SUlp a) = SUlp $ simplAExpr a
simplAExprAux (DUlp a) = DUlp $ simplAExpr a
simplAExprAux (StoR a) = StoR $ simplFAExpr a
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
simplFAExprAux (FNeg (FDouble n)) = FDouble (-n)
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
simplFAExprAux (FMod a1 a2) = FMod (simplFAExprAux a1) (simplFAExprAux a2)
simplFAExprAux (FInt n) = FInt n
simplFAExprAux (FDouble n) = FDouble n
simplFAExprAux (FVar x) = FVar x
simplFAExprAux (FPi) = FPi
simplFAExprAux (FEFun f args) = FEFun f (map simplFAExprAux args)
simplFAExprAux (RtoS a) = RtoS $ simplAExprAux a
simplFAExprAux (RtoD a) = RtoD $ simplAExprAux a
-- simplAExprAux c = c


-----------------------
-- PPExt instances --
-----------------------

tupleD :: PPExt a => [a] -> Doc
tupleD xs = parens $ hsep $ punctuate comma $ map prettyDoc xs

showFullPrecision :: Double -> String
showFullPrecision x = showFFloat Nothing x ""

showRational :: Rational -> String
showRational x = map (\c -> if c=='%' then '/'; else c) (show x)

instance PPExt (NonVarId) where
    prettyDoc (NonVarId x) = text x

instance PPExt (VarId) where
    prettyDoc (VarId x) = text x    

prEExpr :: EExpr -> FPrec -> Doc
prEExpr (AE a)               fp       = prAExpr a fp 
prEExpr (ErrAdd r1 e1 r2 e2) FPSingle = text "aeboundsp_add" <> (text "(" <> prAExpr r1 FPSingle <> comma
                                                                         <+> prEExpr e1 FPSingle <> comma
                                                                         <+> prAExpr r2 FPSingle <> comma
                                                                         <+> prEExpr e2 FPSingle <> text ")")
prEExpr (ErrSub r1 e1 r2 e2) FPSingle = text "aeboundsp_sub" <> (text "(" <> prAExpr r1 FPSingle <> comma
                                                                         <+> prEExpr e1 FPSingle <> comma
                                                                         <+> prAExpr r2 FPSingle <> comma
                                                                         <+> prEExpr e2 FPSingle <> text ")")
prEExpr (ErrMul r1 e1 r2 e2) FPSingle = text "aeboundsp_mul" <> (text "(" <> prAExpr r1 FPSingle <> comma
                                                                         <+> prEExpr e1 FPSingle <> comma
                                                                         <+> prAExpr r2 FPSingle <> comma
                                                                         <+> prEExpr e2 FPSingle <> text ")")
prEExpr (ErrDiv r1 e1 r2 e2) FPSingle = text "aeboundsp_div" <> (text "(" <> prAExpr r1 FPSingle <> comma
                                                                         <+> prEExpr e1 FPSingle <> comma
                                                                         <+> prAExpr r2 FPSingle <> comma
                                                                         <+> prEExpr e2 FPSingle <> text ")")
prEExpr (ErrFloor r e)       FPSingle = text "aeboundsp_flr" <> (text "(" <> prAExpr r FPSingle <> comma
                                                                         <+> prEExpr e FPSingle <> text ")")
prEExpr (ErrFloor0 r e)       FPSingle = text "aeboundsp_flr_t" <> (text "(" <> prAExpr r FPSingle <> comma
                                                                         <+> prEExpr e FPSingle <> text ")")
prEExpr (ErrSqrt r e)        FPSingle = text "aeboundsp_sqt" <> (text "(" <> prAExpr r FPSingle <> comma
                                                                         <+> prEExpr e FPSingle <> text ")")
prEExpr (ErrSin r e)         FPSingle = text "aeboundsp_sin" <> (text "(" <> prAExpr r FPSingle <> comma
                                                                         <+> prEExpr e FPSingle <> text ")")
prEExpr (ErrCos r e)         FPSingle = text "aeboundsp_cos" <> (text "(" <> prAExpr r FPSingle <> comma
                                                                         <+> prEExpr e FPSingle <> text ")")
prEExpr (ErrTan r e)         FPSingle = text "aeboundsp_tan" <> (text "(" <> prAExpr r FPSingle <> comma
                                                                         <+> prEExpr e FPSingle <> text ")")
prEExpr (ErrAcos r e)      FPSingle = text "aeboundsp_acs" <> (text "(" <> prAExpr r FPSingle <> comma
                                                                            <+> prEExpr e FPSingle <> text ")")
prEExpr (ErrAsin r e)      FPSingle = text "aeboundsp_asn" <> (text "(" <> prAExpr r FPSingle <> comma
                                                                            <+> prEExpr e FPSingle <> text ")")  
prEExpr (ErrAtan r e)      FPSingle = text "aeboundsp_atn" <> (text "(" <> prAExpr r FPSingle <> comma
                                                                            <+> prEExpr e FPSingle <> text ")")   
prEExpr (ErrAtanT r e)      FPSingle = text "aeboundsp_atn_t" <> (text "(" <> prAExpr r FPSingle <> comma
                                                                            <+> prEExpr e FPSingle <> text ")")  
prEExpr (ErrNeg r e)         FPSingle = text "aeboundsp_neg" <> (text "(" <> prAExpr r FPSingle<> comma
                                                                         <+> prEExpr e FPSingle <> text ")") 
prEExpr (ErrAbs r e)         FPSingle = text "aeboundsp_abs" <> (text "(" <> prAExpr r FPSingle <> comma
                                                                         <+> prEExpr e FPSingle <> text ")") 

prEExpr (ErrMulPow2 n e) FPSingle = text "aeboundsp_mul_p2" <> (text "(" <> integer n <> comma
                                                                         <+> prEExpr e FPSingle <> text ")") 

prEExpr (HalfUlp (Var x))          FPSingle = text "ulp_sp" <> (text "(" <> text x <> text ")/2") 
--prEExpr (HalfUlp (Double n)) FPSingle = text "ulp_sp" <> (text "(" <> text (showRational n) <> text ")/2")  
prEExpr (HalfUlp a)                 FPSingle        = error ("ppEEExpr: unexpected value in  HalfUlp" ++ show a)

prEExpr (ErrRat r) FPSingle = parens $ text $ showRational r
---------------------
prEExpr (ErrAdd r1 e1 r2 e2) FPDouble = text "aebounddp_add" <> (text "(" <> prAExpr r1 FPDouble <> comma
                                                                         <+> prEExpr e1 FPDouble <> comma
                                                                         <+> prAExpr r2 FPDouble <> comma
                                                                         <+> prEExpr e2 FPDouble <> text ")")
prEExpr (ErrSub r1 e1 r2 e2) FPDouble = text "aebounddp_sub" <> (text "(" <> prAExpr r1 FPDouble <> comma
                                                                         <+> prEExpr e1 FPDouble <> comma
                                                                         <+> prAExpr r2 FPDouble <> comma
                                                                         <+> prEExpr e2 FPDouble <> text ")")
prEExpr (ErrMul r1 e1 r2 e2) FPDouble = text "aebounddp_mul" <> (text "(" <> prAExpr r1 FPDouble <> comma
                                                                         <+> prEExpr e1 FPDouble <> comma
                                                                         <+> prAExpr r2 FPDouble <> comma
                                                                         <+> prEExpr e2 FPDouble <> text ")")
prEExpr (ErrDiv r1 e1 r2 e2) FPDouble = text "aebounddp_div" <> (text "(" <> prAExpr r1 FPDouble <> comma
                                                                         <+> prEExpr e1 FPDouble <> comma
                                                                         <+> prAExpr r2 FPDouble <> comma
                                                                         <+> prEExpr e2 FPDouble <> text ")")
prEExpr (ErrFloor r e)       FPDouble = text "aebounddp_flr" <> (text "(" <> prAExpr r FPDouble <> comma
                                                                         <+> prEExpr e FPDouble <> text ")")
prEExpr (ErrFloor0 r e)       FPDouble = text "aebounddp_flr_t" <> (text "(" <> prAExpr r FPDouble <> comma
                                                                         <+> prEExpr e FPDouble <> text ")")
prEExpr (ErrSqrt r e)        FPDouble = text "aebounddp_sqt" <> (text "(" <> prAExpr r FPDouble <> comma
                                                                         <+> prEExpr e FPDouble <> text ")")
prEExpr (ErrSin r e)         FPDouble = text "aebounddp_sin" <> (text "(" <> prAExpr r FPDouble <> comma
                                                                         <+> prEExpr e FPDouble <> text ")")
prEExpr (ErrCos r e)         FPDouble = text "aebounddp_cos" <> (text "(" <> prAExpr r FPDouble <> comma
                                                                         <+> prEExpr e FPDouble <> text ")")
prEExpr (ErrTan r e)         FPDouble = text "aebounddp_tan" <> (text "(" <> prAExpr r FPDouble <> comma
                                                                         <+> prEExpr e FPDouble <> text ")")
prEExpr (ErrAcos r e)      FPDouble = text "aebounddp_acs" <> (text "(" <> prAExpr r FPDouble <> comma
                                                                            <+> prEExpr e FPDouble <> text ")")
prEExpr (ErrAsin r e)      FPDouble = text "aebounddp_asn" <> (text "(" <> prAExpr r FPDouble <> comma
                                                                            <+> prEExpr e FPDouble <> text ")")  
prEExpr (ErrAtan r e)      FPDouble = text "aebounddp_atn" <> (text "(" <> prAExpr r FPDouble <> comma
                                                                            <+> prEExpr e FPDouble <> text ")")  
prEExpr (ErrAtanT r e)      FPDouble = text "aebounddp_atn_t" <> (text "(" <> prAExpr r FPDouble <> comma
                                                                            <+> prEExpr e FPDouble <> text ")")    
prEExpr (ErrNeg r e)         FPDouble = text "aebounddp_neg" <> (text "(" <> prAExpr r FPDouble <> comma
                                                                         <+> prEExpr e FPDouble <> text ")") 
prEExpr (ErrAbs r e)         FPDouble = text "aebounddp_abs" <> (text "(" <> prAExpr r FPDouble <> comma
                                                                         <+> prEExpr e FPDouble <> text ")") 

prEExpr (ErrMulPow2 n e) FPDouble = text "aebounddp_mul_p2" <> (text "(" <> integer n <> comma
                                                                         <+> prEExpr e FPDouble <> text ")") 

prEExpr (HalfUlp (Var x))          FPDouble = text "ulp_dp" <> (text "(" <> text x <> text ")/2") 
--prEExpr (HalfUlp (Double n)) FPDouble = text "ulp_dp" <> (text "(" <> text (showFullPrecision n) <> text ")/2") 
prEExpr (HalfUlp a)                 FPDouble        = error ("ppEEExpr: unexpected value in  HalfUlp"++ show a)

prEExpr (ErrRat r) FPDouble = parens $ text $ showRational r

prAExpr :: AExpr -> FPrec -> Doc
prAExpr (Pi)               _ = text "pi"
prAExpr (Int i)            _ = integer i
prAExpr (Double d)         _ = parens $ text $ showRational d
prAExpr (Var x)            _ = text x
prAExpr (EFun f [])        _ = text f
prAExpr (Neg a@(Int i))    _ = text "-" <> integer i
prAExpr (Neg a@(Double d)) _ = text "-" <> (text $ showRational d)
prAExpr (Add a1 a2)   fp = parens $ prAExpr a1 fp <+> text "+" <+> prAExpr a2 fp
prAExpr (Sub a1 a2)   fp = parens $ prAExpr a1 fp <+> text "-" <+> prAExpr a2 fp
prAExpr (Mul a1 a2)   fp = parens $ prAExpr a1 fp <+> text "*" <+> prAExpr a2 fp
prAExpr (Div a1 a2)   fp = parens $ prAExpr a1 fp <+> text "/" <+> prAExpr a2 fp
prAExpr (Mod a1 a2)   fp = text "mod" <> parens (prAExpr a1 fp <> comma <+> prAExpr a2 fp)
prAExpr (Pow a1 a2)   fp = prAExpr a1 fp <> text "^" <> lparen <> prAExpr a2 fp <> rparen
prAExpr (Neg a)       fp = text "-" <> lparen <> prAExpr a fp <> rparen
prAExpr (Floor a)     fp = text "floor" <> lparen <> prAExpr a fp <> rparen
prAExpr (Sqrt a)      fp = text "sqrt" <> lparen <> prAExpr a fp <> rparen
prAExpr (Abs a)       fp = text "abs" <> lparen <> prAExpr a fp <> rparen
prAExpr (Sin a)       fp = text "sin" <> lparen <> prAExpr a fp <> rparen
prAExpr (Cos a)       fp = text "cos" <> lparen <> prAExpr a fp <> rparen
prAExpr (Tan a)       fp = text "tan" <> lparen <> prAExpr a fp <> rparen
prAExpr (ASin a)      fp = text "asin" <> lparen <> prAExpr a fp <> rparen
prAExpr (ACos a)      fp = text "acos" <> lparen <> prAExpr a fp <> rparen
prAExpr (ATan a)      fp = text "atan" <> lparen <> prAExpr a fp <> rparen
prAExpr (EFun f args) fp = text f <> (parens $ hsep $ punctuate comma $ map (\x -> prAExpr x fp) args)
prAExpr (SUlp a)      fp = text "ulp_sp" <> lparen <> prAExpr a FPSingle <> rparen
prAExpr (DUlp a)      fp = text "ulp_dp" <> lparen <> prAExpr a FPDouble <> rparen
prAExpr (StoR a)      fp = text "StoR" <> lparen <> prFAExpr a FPSingle <> rparen
prAExpr (DtoR a)      fp = text "DtoR" <> lparen <> prFAExpr a FPDouble <> rparen
prAExpr (EE e)        fp = prEExpr e fp
prAExpr (FPrec)       FPSingle = text "ieee754sp_prec"
prAExpr (FPrec)       FPDouble = text "ieee754dp_prec"
prAExpr (FExp fa)     fp = text "Fexp" <> parens (prFAExpr fa fp)

prBExpr :: BExpr -> FPrec -> Doc
prBExpr (Or e1 e2)  fp = parens $ prBExpr e1 fp <+> text "OR"  <+> prBExpr e2 fp 
prBExpr (And e1 e2) fp = parens $ prBExpr e1 fp <+> text "AND" <+> prBExpr e2 fp
prBExpr (Not e)     fp = text "NOT" <> (parens $ prBExpr e fp)
prBExpr (Eq a1 a2)  fp = parens $ prAExpr a1 fp <+> text "="  <+> prAExpr a2 fp 
prBExpr (Neq a1 a2) fp = parens $ prAExpr a1 fp <+> text "/=" <+> prAExpr a2 fp 
prBExpr (Lt a1 a2)  fp = parens $ prAExpr a1 fp <+> text "<"  <+> prAExpr a2 fp 
prBExpr (LtE a1 a2) fp = parens $ prAExpr a1 fp <+> text "<=" <+> prAExpr a2 fp 
prBExpr (Gt a1 a2)  fp = parens $ prAExpr a1 fp <+> text ">"  <+> prAExpr a2 fp  
prBExpr (GtE a1 a2) fp = parens $ prAExpr a1 fp <+> text ">=" <+> prAExpr a2 fp  
prBExpr (BTrue)     fp = text "TRUE"
prBExpr (BFalse)    fp = text "FALSE"


pvsType :: FPrec -> Doc
pvsType FPSingle = text "unb_single"
pvsType FPDouble = text "unb_double" 


prFCond :: FBExpr -> FPrec -> Doc
prFCond (FOr e1 e2)  fp = parens $ prFCond e1 fp <+> text "OR" <+> prFCond e2 fp
prFCond (FAnd e1 e2) fp = parens $ prFCond e1 fp <+> text "AND" <+> prFCond e2 fp
prFCond (FNot e)     fp = text "NOT" <> (parens $ prFCond e fp)
prFCond (FEq a1 a2)  fp = parens $ prFAExpr a1 fp <+> text "="  <+> prFAExpr a2 fp
prFCond (FNeq a1 a2) fp = parens $ prFAExpr a1 fp <+> text "/=" <+> prFAExpr a2 fp
prFCond (FLt a1 a2)  fp = parens $ prFAExpr a1 fp <+> text "<"  <+> prFAExpr a2 fp
prFCond (FLtE a1 a2) fp = parens $ prFAExpr a1 fp <+> text "<=" <+> prFAExpr a2 fp
prFCond (FGt a1 a2)  fp = parens $ prFAExpr a1 fp <+> text ">"  <+> prFAExpr a2 fp 
prFCond (FGtE a1 a2) fp = parens $ prFAExpr a1 fp <+> text ">=" <+> prFAExpr a2 fp 
prFCond (FBTrue)     fp = text "TRUE"
prFCond (FBFalse)    fp = text "FALSE"

prFAExpr :: FAExpr -> FPrec -> Doc
prFAExpr (FInt i) _ = integer i
prFAExpr (FDouble d) _ = parens $ text $ showRational d
prFAExpr (FNeg a@(FInt i)) _ = text "-" <> integer i
prFAExpr (FNeg a@(FDouble d)) _ = text "-" <> (text $ showRational d)
prFAExpr (FVar x) _ = text x
prFAExpr (FEFun f []) _ = text f
prFAExpr (FEFun f args) fp = text f <> (parens $ hsep $ punctuate comma $ map (\x -> prFAExpr x fp) args)
prFAExpr (RtoS a) fp = text "RtoS" <> lparen <> prAExpr a fp <> rparen
prFAExpr (RtoD a) fp = text "RtoD" <> lparen <> prAExpr a fp <> rparen 
prFAExpr (FPow a1 a2) FPSingle = (prFAExpr a1 FPSingle) <+> text "^" <+> lparen <> (prFAExpr a2 FPSingle) <> rparen
prFAExpr (FAdd a1 a2) FPSingle = text "Sadd" <> parens ((prFAExpr a1 FPSingle) <> comma <+> (prFAExpr a2 FPSingle))
prFAExpr (FSub a1 a2) FPSingle = text "Ssub" <> parens ((prFAExpr a1 FPSingle) <> comma <+> (prFAExpr a2 FPSingle))
prFAExpr (FMul a1 a2) FPSingle = text "Smul" <> parens ((prFAExpr a1 FPSingle) <> comma <+> (prFAExpr a2 FPSingle))
prFAExpr (FDiv a1 a2) FPSingle = text "Sdiv" <> parens ((prFAExpr a1 FPSingle) <> comma <+> (prFAExpr a2 FPSingle))
prFAExpr (FMod a1 a2) FPSingle = text "Smod" <> parens ((prFAExpr a1 FPSingle) <> comma <+> (prFAExpr a2 FPSingle))
prFAExpr (FNeg a)     FPSingle = text "Sneg" <> lparen <> (prFAExpr a FPSingle) <> rparen
prFAExpr (FFloor a)   FPSingle = text "Sfloor" <> lparen <> (prFAExpr a FPSingle) <> rparen
prFAExpr (FSqrt a)    FPSingle = text "Ssqrt" <> lparen <> (prFAExpr a FPSingle) <> rparen
prFAExpr (FAbs a)     FPSingle = text "Sabs" <> lparen <> (prFAExpr a FPSingle) <> rparen
prFAExpr (FSin a)     FPSingle = text "Ssin" <> lparen <> (prFAExpr a FPSingle) <> rparen
prFAExpr (FCos a)     FPSingle = text "Scos" <> lparen <> (prFAExpr a FPSingle) <> rparen
prFAExpr (FTan a)     FPSingle = text "Stan" <> lparen <> (prFAExpr a FPSingle) <> rparen
prFAExpr (FAsin a)    FPSingle = text "Sasin" <> lparen <> (prFAExpr a FPSingle) <> rparen
prFAExpr (FAcos a)    FPSingle = text "Sacos" <> lparen <> (prFAExpr a FPSingle) <> rparen
prFAExpr (FAtan a)    FPSingle = text "Satan" <> lparen <> (prFAExpr a FPSingle) <> rparen
prFAExpr (FPi)        FPSingle = text "Spi"
prFAExpr (FAdd a1 a2) FPDouble = text "Dadd" <> parens ((prFAExpr a1 FPDouble) <> comma <+> (prFAExpr a2 FPDouble))
prFAExpr (FSub a1 a2) FPDouble = text "Dsub" <> parens ((prFAExpr a1 FPDouble) <> comma <+> (prFAExpr a2 FPDouble))
prFAExpr (FMul a1 a2) FPDouble = text "Dmul" <> parens ((prFAExpr a1 FPDouble) <> comma <+> (prFAExpr a2 FPDouble))
prFAExpr (FDiv a1 a2) FPDouble = text "Ddiv" <> parens ((prFAExpr a1 FPDouble) <> comma <+> (prFAExpr a2 FPDouble))
prFAExpr (FMod a1 a2) FPDouble = text "Dmod" <> parens ((prFAExpr a1 FPDouble) <> comma <+> (prFAExpr a2 FPDouble))
prFAExpr (FNeg a)     FPDouble = text "Dneg" <> lparen <> (prFAExpr a FPDouble) <> rparen
prFAExpr (FFloor a)   FPDouble = text "Dfloor" <> lparen <> (prFAExpr a FPDouble) <> rparen
prFAExpr (FSqrt a)    FPDouble = text "Dsqrt" <> lparen <> (prFAExpr a FPDouble) <> rparen
prFAExpr (FAbs a)     FPDouble = text "Dabs" <> lparen <> (prFAExpr a FPDouble) <> rparen
prFAExpr (FSin a)     FPDouble = text "Dsin" <> lparen <> (prFAExpr a FPDouble) <> rparen
prFAExpr (FCos a)     FPDouble = text "Dcos" <> lparen <> (prFAExpr a FPDouble) <> rparen
prFAExpr (FTan a)     FPDouble = text "Dtan" <> lparen <> (prFAExpr a FPDouble) <> rparen
prFAExpr (FAsin a)    FPDouble = text "Dasin" <> lparen <> (prFAExpr a FPDouble) <> rparen
prFAExpr (FAcos a)    FPDouble = text "Dacos" <> lparen <> (prFAExpr a FPDouble) <> rparen
prFAExpr (FAtan a)    FPDouble = text "Datan" <> lparen <> (prFAExpr a FPDouble) <> rparen
prFAExpr (FPi)        FPDouble = text "Dpi"

