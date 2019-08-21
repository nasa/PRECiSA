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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module AbsPVSLang where

import Data.Bits.Floating
import Data.Maybe(fromMaybe)
import FPrec
import Utils
import PPExt
import Prelude hiding ((<>))

type FunName = String

type EExpr = AExpr

data AExpr
 -- real arithmetic expressions
    = Add   AExpr AExpr
    | Sub   AExpr AExpr
    | Mul   AExpr AExpr
    | Div   AExpr AExpr
    | Pow   AExpr AExpr
    | Mod   AExpr AExpr
    | IDiv  AExpr AExpr
    | ItDiv AExpr AExpr
    | IMod  AExpr AExpr
    | ItMod AExpr AExpr
    | Fma   AExpr AExpr AExpr
    | Neg   AExpr
    | Floor AExpr
    | Sqrt  AExpr
    | Abs   AExpr
    | Sin   AExpr
    | Cos   AExpr
    | Tan   AExpr
    | ASin  AExpr
    | ACos  AExpr
    | ATan  AExpr
    | Ln    AExpr
    | Expo  AExpr
    | Expt  AExpr AExpr
    | Int Integer
    | Rat Rational
    | EFun FunName FPrec [AExpr]
    | Var FPrec VarName
    | ArrayElem FPrec (Maybe ArraySize) VarName AExpr
    | StoR FAExpr
    | DtoR FAExpr
    | Prec
    | FExp FAExpr
    | RealMark VarName
    | ErrorMark VarName FPrec
    | Min [AExpr]
    | Max [AExpr]
    | ErrAdd    FPrec AExpr EExpr AExpr EExpr
    | ErrSub    FPrec AExpr EExpr AExpr EExpr
    | ErrMul    FPrec AExpr EExpr AExpr EExpr
    | ErrDiv    FPrec AExpr EExpr AExpr EExpr
    | ErrFma    FPrec AExpr EExpr AExpr EExpr AExpr EExpr
    | ErrItDiv  FPrec AExpr EExpr AExpr EExpr
    | ErrMod    FPrec AExpr EExpr AExpr EExpr
    | ErrItMod  FPrec AExpr EExpr AExpr EExpr
    | ErrFloor  FPrec AExpr EExpr
    | ErrFloor0 FPrec AExpr EExpr
    | ErrSqrt   FPrec AExpr EExpr 
    | ErrSin    FPrec AExpr EExpr 
    | ErrCos    FPrec AExpr EExpr 
    | ErrTan    FPrec AExpr EExpr 
    | ErrAsin   FPrec AExpr EExpr
    | ErrAcos   FPrec AExpr EExpr 
    | ErrAtan   FPrec AExpr EExpr
    | ErrAtanT  FPrec AExpr EExpr 
    | ErrNeg    FPrec AExpr EExpr
    | ErrAbs    FPrec AExpr EExpr
    | ErrLn     FPrec AExpr EExpr
    | ErrExpo   FPrec AExpr EExpr
    | ErrMulPow2R FPrec Integer EExpr
    | ErrMulPow2L FPrec Integer EExpr
    | HalfUlp AExpr FPrec
    | ErrRat Rational
    | MaxErr [EExpr]
    | Infinity
    | ErrUndefined
    | ErrStoD AExpr EExpr
    | ErrDtoS AExpr EExpr
    | ErrItoS AExpr EExpr
    | ErrItoD AExpr EExpr
    deriving (Eq, Ord, Show, Read)


data FAExpr
-- fp arithmetic expressions
    = FIAdd  FAExpr FAExpr
    | FISub  FAExpr FAExpr
    | FIMul  FAExpr FAExpr
    | FIDiv  FAExpr FAExpr
    | FItDiv FAExpr FAExpr
    | FIMod  FAExpr FAExpr
    | FItMod FAExpr FAExpr
    | FIPow  FAExpr FAExpr
    | FIExp  FAExpr FAExpr
    | FINeg  FAExpr
    | FIAbs  FAExpr
    | FFma   FPrec FAExpr FAExpr FAExpr
    | FAdd   FPrec FAExpr FAExpr
    | FSub   FPrec FAExpr FAExpr
    | FMul   FPrec FAExpr FAExpr
    | FDiv   FPrec FAExpr FAExpr
    | FPow   FPrec FAExpr FAExpr
    | FMod   FPrec FAExpr FAExpr
    | FNeg   FPrec FAExpr
    | FFloor FPrec FAExpr
    | FSqrt  FPrec FAExpr
    | FAbs   FPrec FAExpr
    | FSin   FPrec FAExpr
    | FCos   FPrec FAExpr
    | FTan   FPrec FAExpr
    | FAcos  FPrec FAExpr
    | FAsin  FPrec FAExpr
    | FAtan  FPrec FAExpr
    | FLn    FPrec FAExpr
    | FExpo  FPrec FAExpr
    | FInt  Integer
    | FCnst FPrec Rational
    | FEFun String FPrec [FAExpr]
    | FVar  FPrec VarName
    | StructVar String
    | FArrayElem FPrec (Maybe ArraySize) VarName FAExpr
    | FMin [FAExpr]
    | FMax [FAExpr]
    | RtoD AExpr
    | RtoS AExpr
    | StoD FAExpr
    | DtoS FAExpr
    | ItoD FAExpr
    | ItoS FAExpr
    | Value FAExpr
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
    | IsValid FAExpr
    | FBTrue
    | FBFalse
    deriving (Eq, Ord, Show, Read)    

-- progam
type Program = [Decl]

data Arg = Arg VarName FPrec
    deriving (Eq, Ord, Show, Read)

-- set of declarations
data Decl = Decl FPrec FunName [Arg] Stm
    deriving (Eq, Ord, Show, Read)

-- program expression
data Stm = Let VarName FPrec FAExpr Stm
         | Ite FBExpr Stm Stm
         | ListIte [(FBExpr, Stm)] Stm
         | StmExpr FAExpr
         | ForLoop FPrec FAExpr FAExpr FAExpr VarName VarName Stm
         | UnstWarning
    deriving (Eq, Ord, Show, Read)

-- real valued progam
type RProgram = [RDecl]

-- real valued set of declarations
data RDecl = RDecl FPrec FunName [Arg] RStm
    deriving (Eq, Ord, Show, Read)

-- real valued program expression
data RStm = RLet VarName FPrec AExpr RStm
          | RIte BExpr RStm RStm
          | RListIte [(BExpr,RStm)] RStm
          | RStmExpr AExpr
          | RForLoop FPrec AExpr AExpr AExpr VarName VarName RStm
          | RUnstWarning
    deriving (Eq, Ord, Show, Read)

arg2var :: Arg -> FAExpr
arg2var (Arg x fp) = FVar fp x

mapArg2Pair :: Arg -> (VarName, FPrec)
mapArg2Pair (Arg x fp) = (x,fp)

argName :: Arg -> VarName
argName (Arg x _) = x

argPrec :: Arg -> FPrec
argPrec (Arg _ fp) = fp

isArgArray :: Arg -> Bool
isArgArray (Arg _ (Array _ _)) = True
isArgArray _ = False

isArgInt :: Arg -> Bool
isArgInt (Arg _ TInt) = True
isArgInt _ = False

arg2AExpr :: Arg -> AExpr
arg2AExpr (Arg x fp) = Var fp x 

argCast :: FPrec -> Arg -> Arg
argCast fp (Arg x _) = Arg x fp

findInDecls :: String -> [Decl] -> (FPrec,[Arg],Stm)
findInDecls fun [] = error ("findInDecls: function " ++ fun ++ " not found")
findInDecls fun (Decl retType g args stm :ds) | fun==g = (retType,args,stm)
                                              | otherwise = findInDecls fun ds

findInProg :: String -> [Decl] -> Decl
findInProg fun [] = error $ "findInProg: function "++ show fun ++ " not found."
findInProg fun (decl@(Decl _ g _ _):ds) | fun==g = decl
                                        | otherwise = findInProg fun ds

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
flatAnd (And b1 b2) = flatAnd b1 ++ flatAnd b2
flatAnd b = [b]

flatFAnd :: FBExpr -> [FBExpr]
flatFAnd (FAnd b1 b2) = flatFAnd b1 ++ flatFAnd b2
flatFAnd b = [b]

simplFAnd :: FBExpr -> FBExpr -> FBExpr
simplFAnd FBTrue  be      = be
simplFAnd be      FBTrue  = be
simplFAnd FBFalse _       = FBFalse
simplFAnd _       FBFalse = FBFalse
simplFAnd be1     be2     = FAnd be1 be2

listFAnd :: [FBExpr] -> FBExpr
listFAnd = foldl1 FAnd

listFOr :: [FBExpr] -> FBExpr
listFOr = foldl1 FOr

listAnd :: [BExpr] -> BExpr
listAnd = foldl1 And

listOr :: [BExpr] -> BExpr
listOr = foldl1 Or

getFPrec :: FAExpr -> FPrec
getFPrec (FIAdd  _ _)    = TInt
getFPrec (FISub  _ _)    = TInt
getFPrec (FIMul  _ _)    = TInt
getFPrec (FIDiv  _ _)    = TInt
getFPrec (FItDiv _ _)    = TInt
getFPrec (FIMod  _ _)    = TInt
getFPrec (FItMod _ _)    = TInt
getFPrec (FIPow  _ _)    = TInt
getFPrec (FINeg    _)    = TInt
getFPrec (FIAbs    _)    = TInt
getFPrec (FIExp  _ _)    = TInt
getFPrec (FInt     _)    = TInt
getFPrec (FFma fp _ _ _) = fp
getFPrec (FAdd fp _ _)   = fp
getFPrec (FSub fp _ _)   = fp
getFPrec (FMul fp _ _)   = fp
getFPrec (FDiv fp _ _)   = fp
getFPrec (FPow fp _ _)   = fp
getFPrec (FMod fp _ _)   = fp
getFPrec (FNeg   fp _)   = fp
getFPrec (FFloor fp _)   = fp
getFPrec (FSqrt  fp _)   = fp
getFPrec (FAbs   fp _)   = fp
getFPrec (FSin   fp _)   = fp
getFPrec (FCos   fp _)   = fp
getFPrec (FTan   fp _)   = fp
getFPrec (FAcos  fp _)   = fp
getFPrec (FAsin  fp _)   = fp
getFPrec (FAtan  fp _)   = fp
getFPrec (FLn    fp _)   = fp
getFPrec (FExpo  fp _)   = fp
getFPrec (FCnst  fp _)   = fp
getFPrec (FEFun _ fp _)  = fp 
getFPrec (FVar fp _)     = fp
getFPrec (RtoD _) = FPDouble
getFPrec (RtoS _) = FPSingle
getFPrec (StoD _) = FPDouble
getFPrec (DtoS _) = FPSingle
getFPrec (ItoD _) = FPDouble
getFPrec (ItoS _) = FPSingle
getFPrec ae = error $ "getFPrec niy for "++ show ae

--------------------------------------------
-- semantic equivalence error expressions --
--------------------------------------------

rewriteEquivEExpr :: EExpr -> EExpr
rewriteEquivEExpr = replaceInAExpr rewriteEquivEExpr'
  where
  rewriteEquivEExpr' (MaxErr       ees)
    | and $ zipWith (==) ees' $ tail ees' = Just $ head ees'
    | otherwise = Just $ MaxErr ees'
    where
        ees' = map rewriteEquivEExpr ees
  rewriteEquivEExpr' _ = Nothing

equivEExpr :: EExpr -> EExpr -> Bool
equivEExpr ee ee' = rewriteEquivEExpr ee == rewriteEquivEExpr ee'


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
simplFBExpr be@(IsValid _)   = be

simplFRel :: FBExpr -> FBExpr
simplFRel (FEq a1 a2) = case a1 of
    (FInt n)    -> case a2 of
                    (FInt m) ->  if n == m then FBTrue else FBFalse
                    (FCnst _ m) ->  if fromIntegral n == m then FBTrue else FBFalse
                    _ -> FEq a1 a2 -- (simplIAExpr a2)
    (FCnst _ n) -> case a2 of
                    (FInt m) ->  if n == fromIntegral m then FBTrue else FBFalse
                    (FCnst _ m) ->  if n == m then FBTrue else FBFalse
                    _ -> FEq a1 a2 -- (simplIAExpr a2)
    _           -> FEq a1 a2 -- (simplFAExpr a1) (simplFAExpr a2)                
simplFRel (FNeq a1 a2) = case a1 of
    (FInt n)    -> case a2 of
                    (FInt m) ->  if n /= m then FBTrue else FBFalse
                    (FCnst _ m) ->  if fromIntegral n /= m then FBTrue else FBFalse
                    _ -> FNeq a1 (simplFAExpr a2)
    (FCnst _ n) -> case a2 of
                    (FInt m) ->  if n /= fromIntegral m then FBTrue else FBFalse
                    (FCnst _ m) ->  if n /= m then FBTrue else FBFalse
                    _ -> FNeq a1 (simplFAExpr a2)
    _           ->  FNeq (simplFAExpr a1) (simplFAExpr a2)                    
simplFRel (FLt a1 a2) = case a1 of
    (FInt n)    -> case a2 of
                    (FInt m) ->  if n < m then FBTrue else FBFalse
                    (FCnst _ m) ->  if fromIntegral n < m then FBTrue else FBFalse
                    _ -> FLt a1 (simplFAExpr a2)
    (FCnst _ n)  -> case a2 of
                    (FInt m) ->  if n < fromIntegral m then FBTrue else FBFalse
                    (FCnst _ m) ->  if n < m then FBTrue else FBFalse
                    _ -> FLt a1 (simplFAExpr a2)
    _           -> FLt (simplFAExpr a1) (simplFAExpr a2)  
simplFRel (FLtE a1 a2) = case a1 of
    (FInt n)    -> case a2 of
                    (FInt m) ->  if n <= m then FBTrue else FBFalse
                    (FCnst _ m) ->  if fromIntegral n <= m then FBTrue else FBFalse
                    _ -> FLtE (simplFAExpr a1) (simplFAExpr a2)
    (FCnst _ n) -> case a2 of
                    (FInt m) ->  if n <= fromIntegral m then FBTrue else FBFalse
                    (FCnst _ m) ->  if n <= m then FBTrue else FBFalse
                    _ -> FLtE a1 (simplFAExpr a2)
    _          -> FLtE (simplFAExpr a1) (simplFAExpr a2)  
simplFRel (FGt a1 a2) = case a1 of
    (FInt n)    -> case a2 of
                    (FInt m) ->  if n > m then FBTrue else FBFalse
                    (FCnst _ m) ->  if fromIntegral n > m then FBTrue else FBFalse
                    _ -> FGt a1 (simplFAExpr a2)
    (FCnst _ n) -> case a2 of
                    (FInt m) ->  if n > fromIntegral m then FBTrue else FBFalse
                    (FCnst _ m) ->  if n > m then FBTrue else FBFalse
                    _ -> FGt a1 (simplFAExpr a2)
    _          -> FGt (simplFAExpr a1) (simplFAExpr a2) 
simplFRel (FGtE a1 a2) = case a1 of
    (FInt n)    -> case a2 of
                    (FInt m) ->  if n >= m then FBTrue else FBFalse
                    (FCnst _ m) ->  if fromIntegral n >= m then FBTrue else FBFalse
                    _ -> FGtE a1 (simplFAExpr a2)
    (FCnst _ n) -> case a2 of
                    (FInt m) ->  if n >= fromIntegral m then FBTrue else FBFalse
                    (FCnst _ m) ->  if n >= m then FBTrue else FBFalse
                    _ -> FGtE a1 (simplFAExpr a2)
    _          -> FGtE (simplFAExpr a1) (simplFAExpr a2)

simplFRel be = error $ "simplFRel: unexpected value: "++ show be

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
                    (Rat m) ->  if fromIntegral n == m then BTrue else BFalse
                    _ -> Eq a1 (simplAExpr a2)
    (Rat n) -> case a2 of
                    (Int m) ->  if n == fromIntegral m then BTrue else BFalse
                    (Rat m) ->  if n == m then BTrue else BFalse
                    _ -> Eq a1 (simplAExpr a2)
    _          -> Eq (simplAExpr a1) (simplAExpr a2)                
simplRel (Neq a1 a2) = case a1 of
    (Int n)    -> case a2 of
                    (Int m) ->  if n /= m then BTrue else BFalse
                    (Rat m) ->  if fromIntegral n /= m then BTrue else BFalse
                    _ -> Neq a1 (simplAExpr a2)
    (Rat n) -> case a2 of
                    (Int m) ->  if n /= fromIntegral m then BTrue else BFalse
                    (Rat m) ->  if n /= m then BTrue else BFalse
                    _ -> Neq a1 (simplAExpr a2)
    _          ->  Neq (simplAExpr a1) (simplAExpr a2)                    
simplRel (Lt a1 a2) = case a1 of
    (Int n)    -> case a2 of
                    (Int m) ->  if n < m then BTrue else BFalse
                    (Rat m) ->  if fromIntegral n < m then BTrue else BFalse
                    _ -> Lt a1 (simplAExpr a2)
    (Rat n)  -> case a2 of
                    (Int m) ->  if n < fromIntegral m then BTrue else BFalse
                    (Rat m) ->  if n < m then BTrue else BFalse
                    _ -> Lt a1 (simplAExpr a2)
    _           -> Lt (simplAExpr a1) (simplAExpr a2)  
simplRel (LtE a1 a2) = case a1 of
    (Int n)    -> case a2 of
                    (Int m) ->  if n <= m then BTrue else BFalse
                    (Rat m) ->  if fromIntegral n <= m then BTrue else BFalse
                    _ -> LtE a1 (simplAExpr a2)
    (Rat n) -> case a2 of
                    (Int m) ->  if n <= fromIntegral m then BTrue else BFalse
                    (Rat m) ->  if n <= m then BTrue else BFalse
                    _ -> LtE a1 (simplAExpr a2)
    _          -> LtE (simplAExpr a1) (simplAExpr a2)  
simplRel (Gt a1 a2) = case a1 of
    (Int n)    -> case a2 of
                    (Int m) ->  if n > m then BTrue else BFalse
                    (Rat m) ->  if fromIntegral n > m then BTrue else BFalse
                    _ -> Gt a1 (simplAExpr a2)
    (Rat n) -> case a2 of
                    (Int m) ->  if n > fromIntegral m then BTrue else BFalse
                    (Rat m) ->  if n > m then BTrue else BFalse
                    _ -> Gt a1 (simplAExpr a2)
    _          -> Gt (simplAExpr a1) (simplAExpr a2) 
simplRel (GtE a1 a2) = case a1 of
    (Int n)    -> case a2 of
                    (Int m) ->  if n >= m then BTrue else BFalse
                    (Rat m) ->  if fromIntegral n >= m then BTrue else BFalse
                    _ -> GtE a1 (simplAExpr a2)
    (Rat n) -> case a2 of
                    (Int m) ->  if n >= fromIntegral m then BTrue else BFalse
                    (Rat m) ->  if n >= m then BTrue else BFalse
                    _ -> GtE a1 (simplAExpr a2)
    _          -> GtE (simplAExpr a1) (simplAExpr a2)

simplRel be = error $ "simplRel: unexpected value: "++ show be

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
simplAExprAux = replaceInAExpr simplAExprAux'
  where
    simplAExprAux' (Add a       (Int 0)) = Just $ simplAExprAux a
    simplAExprAux' (Add (Int 0)       a) = Just $ simplAExprAux a
    simplAExprAux' (Add a       (Rat 0)) = Just $ simplAExprAux a
    simplAExprAux' (Add (Rat 0)       a) = Just $ simplAExprAux a
    simplAExprAux' (Add (Int n) (Int m)) = Just $ Int (n+m)
    simplAExprAux' (Add (Int n) (Rat m)) = Just $ Rat (fromIntegral n + m)
    simplAExprAux' (Add (Rat n) (Int m)) = Just $ Rat (n + fromIntegral m)
    simplAExprAux' (Add (Rat n) (Rat m)) = Just $ Rat (n+m)
    simplAExprAux' (Sub a       (Int 0)) = Just $ simplAExprAux a
    simplAExprAux' (Sub (Int 0)       a) = Just $ Neg $ simplAExprAux a
    simplAExprAux' (Sub a       (Rat 0)) = Just $ simplAExprAux a
    simplAExprAux' (Sub (Rat 0)       a) = Just $ Neg $ simplAExprAux a
    simplAExprAux' (Sub (Int n) (Int m)) = Just $ Int (n-m)
    simplAExprAux' (Sub (Int n) (Rat m)) = Just $ Rat (fromIntegral n - m)
    simplAExprAux' (Sub (Rat n) (Int m)) = Just $ Rat (n - fromIntegral m)
    simplAExprAux' (Sub (Rat n) (Rat m)) = Just $ Rat (n-m)
    simplAExprAux' (Mul      a  (Int 1)) = Just $ simplAExprAux a
    simplAExprAux' (Mul (Int 1)       a) = Just $ simplAExprAux a
    simplAExprAux' (Mul      a  (Rat 1)) = Just $ simplAExprAux a
    simplAExprAux' (Mul (Rat 1)       a) = Just $ simplAExprAux a
    simplAExprAux' (Mul (Int 0)      _ ) = Just $ Int 0
    simplAExprAux' (Mul      _  (Int 0)) = Just $ Int 0
    simplAExprAux' (Mul (Rat 0)      _ ) = Just $ Rat 0
    simplAExprAux' (Mul      _  (Rat 0)) = Just $ Rat 0    
    simplAExprAux' (Mul (Int n) (Int m)) = Just $ Int (n*m)
    simplAExprAux' (Mul (Int n) (Rat m)) = Just $ Rat (fromIntegral n * m)
    simplAExprAux' (Mul (Rat n) (Int m)) = Just $ Rat (n * fromIntegral m)
    simplAExprAux' (Mul (Rat n) (Rat m)) = Just $ Rat (n*m)
    simplAExprAux' (Neg (Int n))         = Just $ Int (-n)
    simplAExprAux' _ = Nothing

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
simplFAExprAux = replaceInFAExpr simplFAExprAux'
  where
    simplFAExprAux' :: FAExpr -> Maybe FAExpr
    simplFAExprAux' (FAdd  _          a  (FInt    0)) = Just $ simplFAExprAux a
    simplFAExprAux' (FAdd  _ (FInt    0)          a ) = Just $ simplFAExprAux a
    simplFAExprAux' (FAdd  _          a  (FCnst _ 0)) = Just $ simplFAExprAux a
    simplFAExprAux' (FAdd  _ (FCnst _ 0)          a ) = Just $ simplFAExprAux a
    simplFAExprAux' (FAdd  _ (FInt    n) (FInt    m)) = Just $ FInt (n+m)
    simplFAExprAux' (FAdd fp (FInt    n) (FCnst _ m)) = Just $ FCnst fp (fromIntegral n + m)
    simplFAExprAux' (FAdd fp (FCnst _ n) (FInt    m)) = Just $ FCnst fp (n + fromIntegral m)
    simplFAExprAux' (FAdd fp (FCnst _ n) (FCnst _ m)) = Just $ FCnst fp (n+m)
    simplFAExprAux' (FSub  _          a  (FInt    0)) = Just $ simplFAExprAux a
    simplFAExprAux' (FSub fp (FInt    0)          a ) = Just $ FNeg fp $ simplFAExprAux a
    simplFAExprAux' (FSub  _          a  (FCnst _ 0)) = Just $ simplFAExprAux a
    simplFAExprAux' (FSub fp (FCnst _ 0)          a ) = Just $ FNeg fp $ simplFAExprAux a
    simplFAExprAux' (FSub  _ (FInt    n) (FInt    m)) = Just $ FInt (n-m)
    simplFAExprAux' (FSub fp (FInt    n) (FCnst _ m)) = Just $ FCnst fp (fromIntegral n - m)
    simplFAExprAux' (FSub fp (FCnst _ n) (FInt    m)) = Just $ FCnst fp (n - fromIntegral m)
    simplFAExprAux' (FSub fp (FCnst _ n) (FCnst _ m)) = Just $ FCnst fp (n-m)
    simplFAExprAux' (FMul  _          a  (FInt    1)) = Just $ simplFAExprAux a
    simplFAExprAux' (FMul  _ (FInt    1)          a ) = Just $ simplFAExprAux a
    simplFAExprAux' (FMul  _          a  (FCnst _ 1)) = Just $ simplFAExprAux a
    simplFAExprAux' (FMul  _ (FCnst _ 1)          a ) = Just $ simplFAExprAux a
    simplFAExprAux' (FMul  _ (FInt    0)          _ ) = Just $ FInt 0
    simplFAExprAux' (FMul  _          _  (FInt    0)) = Just $ FInt 0
    simplFAExprAux' (FMul fp (FCnst _ 0)          _ ) = Just $ FCnst fp 0
    simplFAExprAux' (FMul fp          _  (FCnst _ 0)) = Just $ FCnst fp 0    
    simplFAExprAux' (FMul  _ (FInt    n) (FInt    m)) = Just $ FInt (n*m)
    simplFAExprAux' (FMul fp (FInt    n) (FCnst _ m)) = Just $ FCnst fp (fromIntegral n * m)
    simplFAExprAux' (FMul fp (FCnst _ n) (FInt    m)) = Just $ FCnst fp (n * fromIntegral m)
    simplFAExprAux' (FMul fp (FCnst _ n) (FCnst _ m)) = Just $ FCnst fp (n*m)
    simplFAExprAux' (FIAdd            a  (FInt    0)) = Just $ simplFAExprAux a
    simplFAExprAux' (FIAdd   (FInt    0)          a ) = Just $ simplFAExprAux a
    simplFAExprAux' (FIAdd   (FInt    n) (FInt    m)) = Just $ FInt (n+m)
    simplFAExprAux' (FISub            a  (FInt    0)) = Just $ simplFAExprAux a
    simplFAExprAux' (FISub   (FInt    0)          a ) = Just $ FINeg $ simplFAExprAux a
    simplFAExprAux' (FISub   (FInt    n) (FInt    m)) = Just $ FInt (n-m)
    simplFAExprAux' (FIMul            a  (FInt    1)) = Just $ simplFAExprAux a
    simplFAExprAux' (FIMul   (FInt    1)          a ) = Just $ simplFAExprAux a
    simplFAExprAux' (FIMul   (FInt    0)          _ ) = Just $ FInt 0
    simplFAExprAux' (FIMul            _  (FInt    0)) = Just $ FInt 0   
    simplFAExprAux' (FIMul   (FInt    n) (FInt    m)) = Just $ FInt (n*m)
    simplFAExprAux' (FNeg  _ (FInt    n))             = Just $ FInt (-n)
    simplFAExprAux' (FAbs  _ (FInt    n))             = Just $ if n>=0 then FInt n else FInt (-n)
    simplFAExprAux' (FAbs fp (FCnst _ n))             = Just $ if n>=0 then FCnst fp n else FCnst fp (-n)
    simplFAExprAux' (FINeg   (FInt    n))             = Just $ FInt (-n)
    simplFAExprAux' (FIAbs   (FInt    n))             = Just $ if n>=0 then FInt n else FInt (-n)
    simplFAExprAux' ae = Nothing

initBExpr :: BExpr -> BExpr
initBExpr = replaceInBExpr initErrorMark

initAExpr :: AExpr -> AExpr
initAExpr = replaceInAExpr initErrorMark

initFAExpr :: FAExpr -> FAExpr
initFAExpr = replaceInFAExpr initFAExpr' 

initFBExpr :: FBExpr -> FBExpr
initFBExpr = replaceInFBExpr initFAExpr' 

initErrorMark :: AExpr -> Maybe AExpr
initErrorMark (ErrorMark _ TInt) = Just (Int 0)
initErrorMark (ErrorMark x   fp) = Just (HalfUlp (RealMark x) fp)
initErrorMark (StoR fae) = Just (StoR (initFAExpr fae))
initErrorMark _ = Nothing

initFAExpr' :: FAExpr -> Maybe FAExpr
initFAExpr' (RtoD a)  = Just (RtoD $ initAExpr  a)
initFAExpr' (RtoS a)  = Just (RtoS $ initAExpr  a)
initFAExpr' _ = Nothing


substituteInRStm :: [(VarName, AExpr)] -> RStm -> RStm
substituteInRStm subs (RLet x fp ae stm) = RLet x fp ae (substituteInRStm subs stm)
substituteInRStm subs (RIte be stmThen stmElse) = RIte (substituteInBExpr subs be) (substituteInRStm subs stmThen) (substituteInRStm subs stmElse)
substituteInRStm subs (RForLoop fp idxStart idxEnd initAcc idx acc forBody) = RForLoop fp idxStart idxEnd initAcc idx acc (substituteInRStm subs forBody)
substituteInRStm subs (RListIte listThen stmElse) = RListIte (zip (map (substituteInBExpr subs . fst) listThen) (map (substituteInRStm subs . snd) listThen)) (substituteInRStm subs stmElse)
substituteInRStm subs (RStmExpr ae) = RStmExpr $ substituteInAExpr subs ae
substituteInRStm _ RUnstWarning = RUnstWarning

substituteInBExpr :: [(VarName, AExpr)] -> BExpr -> BExpr
substituteInBExpr subs = replaceInBExpr (replaceVarWithAExpr subs)

substituteInAExpr :: [(VarName, AExpr)] -> AExpr -> AExpr
substituteInAExpr subs = replaceInAExpr (replaceVarWithAExpr subs)

replaceVarWithAExpr :: [(VarName, AExpr)] -> AExpr -> Maybe AExpr
replaceVarWithAExpr ((y,ae):subs) var@(Var _ x) | x == y = Just ae
                                                | otherwise = replaceVarWithAExpr subs var
replaceVarWithAExpr _ _ = Nothing


isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

isExactlyRepresentable :: Rational -> Bool
isExactlyRepresentable n = toRational(fromRational n :: Double) == n

nextDouble :: Data.Bits.Floating.FloatingBits a w => Rational -> a
nextDouble f = if isExactlyRepresentable f then fromRational f else
    nextUp $ fromRational f

prevDouble :: Data.Bits.Floating.FloatingBits a w => Rational -> a
prevDouble f =  if isExactlyRepresentable f then fromRational f else
   nextDown $ fromRational f

nextUp' :: (RealFrac a, Data.Bits.Floating.FloatingBits a w) => a -> a
nextUp' f = if isInt f then f else nextUp f

nextDown' :: (RealFrac a, Data.Bits.Floating.FloatingBits a w) => a -> a
nextDown' f = if isInt f then f else nextDown f

localVars :: Stm -> [(VarName, FAExpr)]
localVars (Let x _ ae stm) = (x,ae) : localVars stm
localVars (Ite _ stmThen stmElse) = localVars stmThen ++ localVars stmElse
localVars (ListIte listThen stmElse) = concatMap (localVars . snd) listThen ++ localVars stmElse
localVars (StmExpr _) = []
localVars (ForLoop _ _ _ _ _ _ stm) = localVars stm
localVars UnstWarning = []

forIndexes :: Stm -> [(VarName, FAExpr, FAExpr)]
forIndexes (Let _ _ _ stm) = forIndexes stm
forIndexes (Ite _ stmThen stmElse) = forIndexes stmThen ++ forIndexes stmElse
forIndexes (ListIte listThen stmElse) = concatMap (forIndexes . snd) listThen ++ forIndexes stmElse
forIndexes (StmExpr _) = []
forIndexes (ForLoop _ idxStart idxEnd _ idx _ forBody) = (idx, idxStart, idxEnd) : forIndexes forBody
forIndexes UnstWarning = []

varList :: FAExpr -> [FAExpr]
varList fae = elimDuplicates (foldFAExpr varList' const fae [])
  where
    varList' :: [FAExpr] -> FAExpr -> [FAExpr]
    varList' acc var@(FVar _ _) = var:acc
    varList' _   (RtoD ae)      = error $ "varList: niy for " ++ show ae ++ "."
    varList' _   (RtoS ae)      = error $ "varList: niy for " ++ show ae ++ "."
    varList' acc _              = acc

noRoundOffErrorIn :: FBExpr -> Bool
noRoundOffErrorIn be = foldFBExpr noRoundOffErrorInAExpr' const be True

noRoundOffErrorInAExpr :: FAExpr -> Bool
noRoundOffErrorInAExpr ae = foldFAExpr noRoundOffErrorInAExpr' const ae True

noRoundOffErrorInAExpr' :: Bool -> FAExpr -> Bool
noRoundOffErrorInAExpr' acc (FCnst _ n)    = acc && toRational (floor (fromRational n :: Double) :: Integer) == n
noRoundOffErrorInAExpr' acc (FVar  TInt _) = acc
noRoundOffErrorInAExpr' _   (FVar _ _)     = False
noRoundOffErrorInAExpr' _   (RtoD _)       = False 
noRoundOffErrorInAExpr' _   (RtoS _)       = False
noRoundOffErrorInAExpr' _   (StoD _)       = False
noRoundOffErrorInAExpr' _   (DtoS _)       = False
noRoundOffErrorInAExpr' _   FArrayElem{}   = False
noRoundOffErrorInAExpr' acc _              = acc

funCallListStm :: Stm -> [FAExpr]
funCallListStm = elimDuplicates . funCallListStm'
  where
    funCallListStm' :: Stm -> [FAExpr]
    funCallListStm' (Let _ _ ae stm) = funCallListFAExpr ae ++ funCallListStm' stm
    funCallListStm' (Ite _ stmThen stmElse) = funCallListStm' stmThen ++ funCallListStm' stmElse
    funCallListStm' (ListIte listThen stmElse) = concatMap (funCallListStm' . snd) listThen ++ funCallListStm' stmElse
    funCallListStm' (StmExpr ae) = funCallListFAExpr ae
    funCallListStm' (ForLoop _ _ _ _ _ _ stm) = funCallListStm' stm
    funCallListStm' UnstWarning = []

funCallListFBExpr :: FBExpr -> [FAExpr]
funCallListFBExpr be = elimDuplicates $ foldFBExpr funCallListAcc const be []

funCallListFAExpr :: FAExpr -> [FAExpr]
funCallListFAExpr ae = elimDuplicates $ foldFAExpr funCallListAcc const ae []

funCallListAcc :: [FAExpr] -> FAExpr -> [FAExpr]
funCallListAcc acc fc@(FEFun _ _ _)         = fc:acc
funCallListAcc acc (Value fc@(FEFun _ _ _)) = fc:acc
funCallListAcc acc (RtoD (Int _))           = acc
funCallListAcc acc (RtoS (Int _))           = acc
funCallListAcc acc (RtoD (Rat _))           = acc
funCallListAcc acc (RtoS (Rat _))           = acc
funCallListAcc _   (RtoD _)                 = error "funCallList: case RtoD niy"
funCallListAcc _   (RtoS _)                 = error "funCallList: case RtoS niy"
funCallListAcc acc _                        = acc

equivModuloIndex :: FAExpr -> FAExpr -> Bool
equivModuloIndex (FFma   _ ae1 ae2 ae3) (FFma   _ ae1' ae2' ae3') = equivModuloIndex ae1 ae1' && equivModuloIndex ae2 ae2' && equivModuloIndex ae3 ae3'
equivModuloIndex (FIAdd   ae1 ae2) (FIAdd   ae1' ae2') = equivModuloIndex ae1 ae1' && equivModuloIndex ae2 ae2'
equivModuloIndex (FISub   ae1 ae2) (FISub   ae1' ae2') = equivModuloIndex ae1 ae1' && equivModuloIndex ae2 ae2'
equivModuloIndex (FIMul   ae1 ae2) (FIMul   ae1' ae2') = equivModuloIndex ae1 ae1' && equivModuloIndex ae2 ae2'
equivModuloIndex (FIDiv   ae1 ae2) (FIDiv   ae1' ae2') = equivModuloIndex ae1 ae1' && equivModuloIndex ae2 ae2'
equivModuloIndex (FItDiv  ae1 ae2) (FItDiv  ae1' ae2') = equivModuloIndex ae1 ae1' && equivModuloIndex ae2 ae2'
equivModuloIndex (FIMod   ae1 ae2) (FIMod   ae1' ae2') = equivModuloIndex ae1 ae1' && equivModuloIndex ae2 ae2'
equivModuloIndex (FItMod  ae1 ae2) (FItMod  ae1' ae2') = equivModuloIndex ae1 ae1' && equivModuloIndex ae2 ae2'
equivModuloIndex (FIPow   ae1 ae2) (FIPow   ae1' ae2') = equivModuloIndex ae1 ae1' && equivModuloIndex ae2 ae2'
equivModuloIndex (FIExp   ae1 ae2) (FIExp   ae1' ae2') = equivModuloIndex ae1 ae1' && equivModuloIndex ae2 ae2'
equivModuloIndex (FAdd  _ ae1 ae2) (FAdd  _ ae1' ae2') = equivModuloIndex ae1 ae1' && equivModuloIndex ae2 ae2'
equivModuloIndex (FSub  _ ae1 ae2) (FSub  _ ae1' ae2') = equivModuloIndex ae1 ae1' && equivModuloIndex ae2 ae2'
equivModuloIndex (FMul  _ ae1 ae2) (FMul  _ ae1' ae2') = equivModuloIndex ae1 ae1' && equivModuloIndex ae2 ae2'
equivModuloIndex (FDiv  _ ae1 ae2) (FDiv  _ ae1' ae2') = equivModuloIndex ae1 ae1' && equivModuloIndex ae2 ae2'
equivModuloIndex (FPow  _ ae1 ae2) (FPow  _ ae1' ae2') = equivModuloIndex ae1 ae1' && equivModuloIndex ae2 ae2'
equivModuloIndex (FMod  _ ae1 ae2) (FMod  _ ae1' ae2') = equivModuloIndex ae1 ae1' && equivModuloIndex ae2 ae2'
equivModuloIndex (FINeg    ae)     (FINeg    ae')      = equivModuloIndex ae  ae'
equivModuloIndex (FIAbs    ae)     (FIAbs    ae')      = equivModuloIndex ae  ae'
equivModuloIndex (FNeg   _ ae)     (FNeg   _ ae')      = equivModuloIndex ae  ae'
equivModuloIndex (FFloor _ ae)     (FFloor _ ae')      = equivModuloIndex ae  ae'
equivModuloIndex (FSqrt  _ ae)     (FSqrt  _ ae')      = equivModuloIndex ae  ae'
equivModuloIndex (FAbs   _ ae)     (FAbs   _ ae')      = equivModuloIndex ae  ae'
equivModuloIndex (FSin   _ ae)     (FSin   _ ae')      = equivModuloIndex ae  ae'
equivModuloIndex (FCos   _ ae)     (FCos   _ ae')      = equivModuloIndex ae  ae'
equivModuloIndex (FTan   _ ae)     (FTan   _ ae')      = equivModuloIndex ae  ae'
equivModuloIndex (FAcos  _ ae)     (FAcos  _ ae')      = equivModuloIndex ae  ae'
equivModuloIndex (FAsin  _ ae)     (FAsin  _ ae')      = equivModuloIndex ae  ae'
equivModuloIndex (FAtan  _ ae)     (FAtan  _ ae')      = equivModuloIndex ae  ae'
equivModuloIndex (FLn    _ ae)     (FLn    _ ae')      = equivModuloIndex ae  ae'
equivModuloIndex (FExpo  _ ae)     (FExpo  _ ae')      = equivModuloIndex ae  ae'
equivModuloIndex (FInt n) (FInt m) = n==m
equivModuloIndex (FCnst fp1 r1) (FCnst fp2 r2) = fp1==fp2 && r1==r2
equivModuloIndex (FVar  fp1 x1) (FVar  fp2 x2) = fp1==fp2 && x1==x2
equivModuloIndex (FEFun f1 fp1 args1) (FEFun f2 fp2 args2) = f1 == f2 && fp1 == fp2 && foldl (\b (arg1,arg2) -> b && equivModuloIndex arg1 arg2) True (zip args1 args2)
equivModuloIndex (StructVar s1) (StructVar s2) = s1==s2
equivModuloIndex (FArrayElem fp1 _ v1 _) (FArrayElem fp2 _ v2 _) = fp1==fp2 && v1==v2
equivModuloIndex (FMin aes1) (FMin aes2) = foldl (\b (ae1,ae2) -> b && equivModuloIndex ae1 ae2) True (zip aes1 aes2)
equivModuloIndex (FMax aes1) (FMax aes2) = foldl (\b (ae1,ae2) -> b && equivModuloIndex ae1 ae2) True (zip aes1 aes2)
equivModuloIndex (RtoD  (Int n))  (RtoD  (Int m))  = n==m
equivModuloIndex (RtoS  (Int n))  (RtoS  (Int m))  = n==m
equivModuloIndex (RtoD  (Rat r1)) (RtoD  (Rat r2)) = r1==r2
equivModuloIndex (RtoS  (Rat r1)) (RtoS  (Rat r2)) = r1==r2
equivModuloIndex (RtoD _) (RtoD _) = error "equivModuloIndex niy for RtoD."
equivModuloIndex (RtoS _) (RtoS _) = error "equivModuloIndex niy for RtoS."
equivModuloIndex (StoD  ae) (StoD  ae') = equivModuloIndex ae ae'
equivModuloIndex (DtoS  ae) (DtoS  ae') = equivModuloIndex ae ae'
equivModuloIndex (ItoD  ae) (ItoD  ae') = equivModuloIndex ae ae'
equivModuloIndex (ItoS  ae) (ItoS  ae') = equivModuloIndex ae ae'
equivModuloIndex (Value ae) (Value ae') = equivModuloIndex ae ae'
equivModuloIndex _ _ = False


foldFBExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> FBExpr -> a -> a
foldFBExpr faExprF aExprF (FOr  be1 be2) a = foldFBExpr faExprF aExprF be2
                                           $ foldFBExpr faExprF aExprF be1 a
foldFBExpr faExprF aExprF (FAnd be1 be2) a = foldFBExpr faExprF aExprF be2
                                           $ foldFBExpr faExprF aExprF be1 a
foldFBExpr faExprF aExprF (FNot be1)     a = foldFBExpr faExprF aExprF be1 a
foldFBExpr faExprF aExprF (FEq  ae1 ae2) a = foldFAExpr faExprF aExprF ae2
                                           $ foldFAExpr faExprF aExprF ae1 a
foldFBExpr faExprF aExprF (FNeq ae1 ae2) a = foldFAExpr faExprF aExprF ae2
                                           $ foldFAExpr faExprF aExprF ae1 a
foldFBExpr faExprF aExprF (FLt  ae1 ae2) a = foldFAExpr faExprF aExprF ae2
                                           $ foldFAExpr faExprF aExprF ae1 a
foldFBExpr faExprF aExprF (FLtE ae1 ae2) a = foldFAExpr faExprF aExprF ae2
                                           $ foldFAExpr faExprF aExprF ae1 a
foldFBExpr faExprF aExprF (FGt  ae1 ae2) a = foldFAExpr faExprF aExprF ae2
                                           $ foldFAExpr faExprF aExprF ae1 a
foldFBExpr faExprF aExprF (FGtE ae1 ae2) a = foldFAExpr faExprF aExprF ae2
                                           $ foldFAExpr faExprF aExprF ae1 a
foldFBExpr faExprF aExprF (IsValid  ae1) a = foldFAExpr faExprF aExprF ae1 a
foldFBExpr _ _ FBTrue  a = a
foldFBExpr _ _ FBFalse a = a

foldFAExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> FAExpr -> a -> a
foldFAExpr faExprF _ ae@(FCnst _ _)   a = faExprF a ae
foldFAExpr faExprF _ ae@(FVar  _ _)   a = faExprF a ae
foldFAExpr faExprF _ ae@(StructVar _) a = faExprF a ae
foldFAExpr faExprF _ ae@(FInt _)      a = faExprF a ae
foldFAExpr faExprF aExprF ae@(FINeg            ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(FNeg   _        ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(FFloor _        ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(FSqrt  _        ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(FIAbs            ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(FAbs   _        ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(FSin   _        ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(FCos   _        ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(FTan   _        ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(FAcos  _        ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(FAsin  _        ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(FAtan  _        ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(FLn    _        ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(FExpo  _        ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(StoD             ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(DtoS             ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(ItoD             ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(ItoS             ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(Value            ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(FArrayElem _ _ _ ae1) a = foldUnaryFAExpr faExprF aExprF ae ae1 a
foldFAExpr faExprF aExprF ae@(FIAdd     ae1 ae2) a = foldBinaryFAExpr faExprF aExprF ae ae1 ae2 a
foldFAExpr faExprF aExprF ae@(FISub     ae1 ae2) a = foldBinaryFAExpr faExprF aExprF ae ae1 ae2 a
foldFAExpr faExprF aExprF ae@(FIMul     ae1 ae2) a = foldBinaryFAExpr faExprF aExprF ae ae1 ae2 a
foldFAExpr faExprF aExprF ae@(FIDiv     ae1 ae2) a = foldBinaryFAExpr faExprF aExprF ae ae1 ae2 a
foldFAExpr faExprF aExprF ae@(FItDiv    ae1 ae2) a = foldBinaryFAExpr faExprF aExprF ae ae1 ae2 a
foldFAExpr faExprF aExprF ae@(FIMod     ae1 ae2) a = foldBinaryFAExpr faExprF aExprF ae ae1 ae2 a
foldFAExpr faExprF aExprF ae@(FItMod    ae1 ae2) a = foldBinaryFAExpr faExprF aExprF ae ae1 ae2 a
foldFAExpr faExprF aExprF ae@(FIPow     ae1 ae2) a = foldBinaryFAExpr faExprF aExprF ae ae1 ae2 a
foldFAExpr faExprF aExprF ae@(FIExp     ae1 ae2) a = foldBinaryFAExpr faExprF aExprF ae ae1 ae2 a
foldFAExpr faExprF aExprF ae@(FAdd   _ ae1 ae2) a = foldBinaryFAExpr faExprF aExprF ae ae1 ae2 a
foldFAExpr faExprF aExprF ae@(FSub   _ ae1 ae2) a = foldBinaryFAExpr faExprF aExprF ae ae1 ae2 a
foldFAExpr faExprF aExprF ae@(FMul   _ ae1 ae2) a = foldBinaryFAExpr faExprF aExprF ae ae1 ae2 a
foldFAExpr faExprF aExprF ae@(FDiv   _ ae1 ae2) a = foldBinaryFAExpr faExprF aExprF ae ae1 ae2 a
foldFAExpr faExprF aExprF ae@(FPow   _ ae1 ae2) a = foldBinaryFAExpr faExprF aExprF ae ae1 ae2 a
foldFAExpr faExprF aExprF ae@(FMod   _ ae1 ae2) a = foldBinaryFAExpr faExprF aExprF ae ae1 ae2 a
foldFAExpr faExprF aExprF ae@(RtoD ae1) a = foldAExpr faExprF aExprF ae1
                                          $ faExprF a ae
foldFAExpr faExprF aExprF ae@(RtoS ae1) a = foldAExpr faExprF aExprF ae1
                                          $ faExprF a ae
foldFAExpr faExprF aExprF ae@(FFma _ ae1 ae2 ae3) a = foldFAExpr faExprF aExprF ae3
                                                       $ foldFAExpr faExprF aExprF ae2
                                                       $ foldFAExpr faExprF aExprF ae1
                                                       $ faExprF a ae
foldFAExpr faExprF aExprF ae@(FMin      aes) a = foldListFAExpr faExprF aExprF ae aes a
foldFAExpr faExprF aExprF ae@(FMax      aes) a = foldListFAExpr faExprF aExprF ae aes a
foldFAExpr faExprF aExprF ae@(FEFun _ _ aes) a = foldListFAExpr faExprF aExprF ae aes a


foldUnaryFAExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> FAExpr -> FAExpr -> a -> a
foldUnaryFAExpr faExprF aExprF ae ae1 a = foldFAExpr faExprF aExprF ae1
                                        $ faExprF a ae

foldBinaryFAExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> FAExpr -> FAExpr -> FAExpr -> a -> a
foldBinaryFAExpr faExprF aExprF ae ae1 ae2 a = foldFAExpr faExprF aExprF ae2
                                             $ foldFAExpr faExprF aExprF ae1
                                             $ faExprF a ae

foldQuadFAExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> FAExpr -> FAExpr -> FAExpr -> FAExpr -> FAExpr -> a -> a
foldQuadFAExpr faExprF aExprF ae ae1 ae2 ae3 ae4 a = foldFAExpr faExprF aExprF ae4
                                                   $ foldFAExpr faExprF aExprF ae3
                                                   $ foldFAExpr faExprF aExprF ae2
                                                   $ foldFAExpr faExprF aExprF ae1
                                                   $ faExprF a ae

foldListFAExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> FAExpr -> [FAExpr] -> a -> a
foldListFAExpr faExprF aExprF ae aeList a = foldr (foldFAExpr faExprF aExprF) (faExprF a ae) aeList

foldBExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> BExpr -> a -> a
foldBExpr faExprF aExprF (Or  be1 be2)  a = foldBExpr faExprF aExprF be2
                                          $ foldBExpr faExprF aExprF be1 a
foldBExpr faExprF aExprF (And be1 be2)  a = foldBExpr faExprF aExprF be2
                                          $ foldBExpr faExprF aExprF be1 a
foldBExpr faExprF aExprF (Not be1)      a = foldBExpr faExprF aExprF be1 a
foldBExpr faExprF aExprF (Eq  ae1 ae2)  a = foldAExpr faExprF aExprF ae2 
                                          $ foldAExpr faExprF aExprF ae1 a
foldBExpr faExprF aExprF (Neq ae1 ae2)  a = foldAExpr faExprF aExprF ae2
                                          $ foldAExpr faExprF aExprF ae1 a
foldBExpr faExprF aExprF (Lt  ae1 ae2)  a = foldAExpr faExprF aExprF ae2
                                          $ foldAExpr faExprF aExprF ae1 a
foldBExpr faExprF aExprF (LtE ae1 ae2)  a = foldAExpr faExprF aExprF ae2
                                          $ foldAExpr faExprF aExprF ae1 a
foldBExpr faExprF aExprF (Gt  ae1 ae2)  a = foldAExpr faExprF aExprF ae2
                                          $ foldAExpr faExprF aExprF ae1 a
foldBExpr faExprF aExprF (GtE ae1 ae2)  a = foldAExpr faExprF aExprF ae2
                                          $ foldAExpr faExprF aExprF ae1 a
foldBExpr _ _ BTrue          a = a
foldBExpr _ _ BFalse         a = a 


foldAExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> AExpr -> a  -> a
foldAExpr _       aExprF ae@(Int _)         a = aExprF a ae
foldAExpr _       aExprF ae@(Rat _)         a = aExprF a ae
foldAExpr _       aExprF ae@Prec            a = aExprF a ae
foldAExpr _       aExprF ae@(Var _ _)       a = aExprF a ae
foldAExpr _       aExprF ae@Infinity        a = aExprF a ae
foldAExpr _       aExprF ae@(RealMark _)    a = aExprF a ae
foldAExpr _       aExprF ae@ErrUndefined    a = aExprF a ae
foldAExpr _       aExprF ae@(ErrRat _)      a = aExprF a ae
foldAExpr _       aExprF ae@(ErrorMark _ _) a = aExprF a ae
foldAExpr faExprF aExprF ae@(Neg   ae1)           a = foldUnaryAExpr  faExprF aExprF ae ae1     a
foldAExpr faExprF aExprF ae@(Floor ae1)           a = foldUnaryAExpr  faExprF aExprF ae ae1     a
foldAExpr faExprF aExprF ae@(Sqrt  ae1)           a = foldUnaryAExpr  faExprF aExprF ae ae1     a
foldAExpr faExprF aExprF ae@(Abs   ae1)           a = foldUnaryAExpr  faExprF aExprF ae ae1     a
foldAExpr faExprF aExprF ae@(Sin   ae1)           a = foldUnaryAExpr  faExprF aExprF ae ae1     a
foldAExpr faExprF aExprF ae@(Cos   ae1)           a = foldUnaryAExpr  faExprF aExprF ae ae1     a
foldAExpr faExprF aExprF ae@(Tan   ae1)           a = foldUnaryAExpr  faExprF aExprF ae ae1     a
foldAExpr faExprF aExprF ae@(ASin  ae1)           a = foldUnaryAExpr  faExprF aExprF ae ae1     a
foldAExpr faExprF aExprF ae@(ACos  ae1)           a = foldUnaryAExpr  faExprF aExprF ae ae1     a
foldAExpr faExprF aExprF ae@(ATan  ae1)           a = foldUnaryAExpr  faExprF aExprF ae ae1     a
foldAExpr faExprF aExprF ae@(Ln    ae1)           a = foldUnaryAExpr  faExprF aExprF ae ae1     a
foldAExpr faExprF aExprF ae@(Expo  ae1)           a = foldUnaryAExpr  faExprF aExprF ae ae1     a
foldAExpr faExprF aExprF ae@(ErrMulPow2R _ _ ee)  a = foldUnaryAExpr  faExprF aExprF ae ee      a
foldAExpr faExprF aExprF ae@(ErrMulPow2L _ _ ee)  a = foldUnaryAExpr  faExprF aExprF ae ee      a
foldAExpr faExprF aExprF ae@(ArrayElem _ _ _ ae1) a = foldUnaryAExpr  faExprF aExprF ae ae1     a
foldAExpr faExprF aExprF ae@(HalfUlp ae1 _)       a = foldUnaryAExpr  faExprF aExprF ae ae1     a
foldAExpr faExprF aExprF ae@(Add         ae1 ae2) a = foldBinaryAExpr faExprF aExprF ae ae1 ae2 a
foldAExpr faExprF aExprF ae@(Sub         ae1 ae2) a = foldBinaryAExpr faExprF aExprF ae ae1 ae2 a
foldAExpr faExprF aExprF ae@(Mul         ae1 ae2) a = foldBinaryAExpr faExprF aExprF ae ae1 ae2 a
foldAExpr faExprF aExprF ae@(Div         ae1 ae2) a = foldBinaryAExpr faExprF aExprF ae ae1 ae2 a
foldAExpr faExprF aExprF ae@(Pow         ae1 ae2) a = foldBinaryAExpr faExprF aExprF ae ae1 ae2 a
foldAExpr faExprF aExprF ae@(Mod         ae1 ae2) a = foldBinaryAExpr faExprF aExprF ae ae1 ae2 a
foldAExpr faExprF aExprF ae@(IDiv        ae1 ae2) a = foldBinaryAExpr faExprF aExprF ae ae1 ae2 a
foldAExpr faExprF aExprF ae@(ItDiv       ae1 ae2) a = foldBinaryAExpr faExprF aExprF ae ae1 ae2 a
foldAExpr faExprF aExprF ae@(IMod        ae1 ae2) a = foldBinaryAExpr faExprF aExprF ae ae1 ae2 a
foldAExpr faExprF aExprF ae@(ItMod       ae1 ae2) a = foldBinaryAExpr faExprF aExprF ae ae1 ae2 a
foldAExpr faExprF aExprF ae@(Expt        ae1 ae2) a = foldBinaryAExpr faExprF aExprF ae ae1 ae2 a
foldAExpr faExprF aExprF ae@(ErrFloor  _ ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a
foldAExpr faExprF aExprF ae@(ErrFloor0 _ ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a
foldAExpr faExprF aExprF ae@(ErrSqrt   _ ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a 
foldAExpr faExprF aExprF ae@(ErrSin    _ ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a 
foldAExpr faExprF aExprF ae@(ErrCos    _ ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a 
foldAExpr faExprF aExprF ae@(ErrTan    _ ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a 
foldAExpr faExprF aExprF ae@(ErrAsin   _ ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a
foldAExpr faExprF aExprF ae@(ErrAcos   _ ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a 
foldAExpr faExprF aExprF ae@(ErrAtan   _ ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a
foldAExpr faExprF aExprF ae@(ErrAtanT  _ ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a 
foldAExpr faExprF aExprF ae@(ErrNeg    _ ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a
foldAExpr faExprF aExprF ae@(ErrAbs    _ ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a
foldAExpr faExprF aExprF ae@(ErrLn     _ ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a
foldAExpr faExprF aExprF ae@(ErrExpo   _ ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a
foldAExpr faExprF aExprF ae@(ErrStoD     ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a
foldAExpr faExprF aExprF ae@(ErrDtoS     ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a
foldAExpr faExprF aExprF ae@(ErrItoS     ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a
foldAExpr faExprF aExprF ae@(ErrItoD     ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a
foldAExpr faExprF aExprF ae@(ErrAdd    _ ae1 ee1 ae2 ee2) a = foldQuadAExpr faExprF aExprF ae ae1 ee1 ae2 ee2 a
foldAExpr faExprF aExprF ae@(ErrSub    _ ae1 ee1 ae2 ee2) a = foldQuadAExpr faExprF aExprF ae ae1 ee1 ae2 ee2 a
foldAExpr faExprF aExprF ae@(ErrMul    _ ae1 ee1 ae2 ee2) a = foldQuadAExpr faExprF aExprF ae ae1 ee1 ae2 ee2 a
foldAExpr faExprF aExprF ae@(ErrDiv    _ ae1 ee1 ae2 ee2) a = foldQuadAExpr faExprF aExprF ae ae1 ee1 ae2 ee2 a
foldAExpr faExprF aExprF ae@(ErrItDiv  _ ae1 ee1 ae2 ee2) a = foldQuadAExpr faExprF aExprF ae ae1 ee1 ae2 ee2 a
foldAExpr faExprF aExprF ae@(ErrMod    _ ae1 ee1 ae2 ee2) a = foldQuadAExpr faExprF aExprF ae ae1 ee1 ae2 ee2 a
foldAExpr faExprF aExprF ae@(ErrItMod  _ ae1 ee1 ae2 ee2) a = foldQuadAExpr faExprF aExprF ae ae1 ee1 ae2 ee2 a
foldAExpr faExprF aExprF ae@(EFun _ _ aes) a = foldListAExpr faExprF aExprF ae aes a
foldAExpr faExprF aExprF ae@(Min      aes) a = foldListAExpr faExprF aExprF ae aes a
foldAExpr faExprF aExprF ae@(Max      aes) a = foldListAExpr faExprF aExprF ae aes a
foldAExpr faExprF aExprF ae@(MaxErr   aes) a = foldListAExpr faExprF aExprF ae aes a
foldAExpr faExprF aExprF ae@(StoR fae)        a = foldFAExpr faExprF aExprF fae
                                                $ aExprF a ae
foldAExpr faExprF aExprF ae@(DtoR fae)        a = foldFAExpr faExprF aExprF fae
                                                $ aExprF a ae
foldAExpr faExprF aExprF ae@(FExp fae)        a = foldFAExpr faExprF aExprF fae
                                                $ aExprF a ae
foldAExpr faExprF aExprF ae@(Fma ae1 ae2 ae3) a = foldAExpr faExprF aExprF ae3
                                                $ foldAExpr faExprF aExprF ae2
                                                $ foldAExpr faExprF aExprF ae1
                                                $ aExprF a ae
foldAExpr faExprF aExprF ae@(ErrFma _ ae1 ee1 ae2 ee2 ae3 ee3) a = foldAExpr faExprF aExprF ee3
                                                                 $ foldAExpr faExprF aExprF ae3
                                                                 $ foldAExpr faExprF aExprF ee2
                                                                 $ foldAExpr faExprF aExprF ae2
                                                                 $ foldAExpr faExprF aExprF ee1
                                                                 $ foldAExpr faExprF aExprF ae1
                                                                 $ aExprF a ae

foldUnaryAExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> AExpr -> AExpr -> a -> a
foldUnaryAExpr faExprF aExprF ae ae1 a = foldAExpr faExprF aExprF ae1
                                       $ aExprF a ae
foldBinaryAExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> AExpr -> AExpr -> AExpr -> a -> a
foldBinaryAExpr faExprF aExprF ae ae1 ae2 a = foldAExpr faExprF aExprF ae2
                                            $ foldAExpr faExprF aExprF ae1
                                            $ aExprF a ae

foldQuadAExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> AExpr -> AExpr -> AExpr -> AExpr -> AExpr -> a -> a
foldQuadAExpr faExprF aExprF ae ae1 ae2 ae3 ae4 a = foldAExpr faExprF aExprF ae4
                                                    $ foldAExpr faExprF aExprF ae3
                                                    $ foldAExpr faExprF aExprF ae2
                                                    $ foldAExpr faExprF aExprF ae1
                                                    $ aExprF a ae

foldListAExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> AExpr -> [AExpr] -> a -> a
foldListAExpr faExprF aExprF ae aeList a = foldl aExprF (foldAExpr faExprF aExprF ae a) aeList


replaceInFAExpr :: (FAExpr -> Maybe FAExpr) -> FAExpr -> FAExpr
replaceInFAExpr f fexpr = fromMaybe (replaceInFAExpr' fexpr) (f fexpr)
  where
    replaceInFAExpr' :: FAExpr -> FAExpr
    replaceInFAExpr' ae@(FInt _)      = ae
    replaceInFAExpr' ae@(FCnst _ _)   = ae
    replaceInFAExpr' ae@(FVar  _ _)   = ae
    replaceInFAExpr' ae@(StructVar _) = ae
    replaceInFAExpr' ae@FArrayElem{}  = ae
    replaceInFAExpr' (FEFun g fp args)   = FEFun g fp (map (replaceInFAExpr f) args)
    replaceInFAExpr' (FIAdd  ae1 ae2)    = FIAdd     (replaceInFAExpr f ae1) (replaceInFAExpr f ae2)
    replaceInFAExpr' (FISub  ae1 ae2)    = FISub     (replaceInFAExpr f ae1) (replaceInFAExpr f ae2)
    replaceInFAExpr' (FIMul  ae1 ae2)    = FIMul     (replaceInFAExpr f ae1) (replaceInFAExpr f ae2)
    replaceInFAExpr' (FIDiv  ae1 ae2)    = FIDiv     (replaceInFAExpr f ae1) (replaceInFAExpr f ae2)
    replaceInFAExpr' (FItDiv ae1 ae2)    = FItDiv    (replaceInFAExpr f ae1) (replaceInFAExpr f ae2)
    replaceInFAExpr' (FIMod  ae1 ae2)    = FIMod     (replaceInFAExpr f ae1) (replaceInFAExpr f ae2)
    replaceInFAExpr' (FItMod ae1 ae2)    = FItMod    (replaceInFAExpr f ae1) (replaceInFAExpr f ae2)
    replaceInFAExpr' (FIPow  ae1 ae2)    = FIPow     (replaceInFAExpr f ae1) (replaceInFAExpr f ae2)
    replaceInFAExpr' (FIExp  ae1 ae2)    = FIExp     (replaceInFAExpr f ae1) (replaceInFAExpr f ae2)
    replaceInFAExpr' (FAdd   fp ae1 ae2) = FAdd   fp (replaceInFAExpr f ae1) (replaceInFAExpr f ae2)
    replaceInFAExpr' (FSub   fp ae1 ae2) = FSub   fp (replaceInFAExpr f ae1) (replaceInFAExpr f ae2)
    replaceInFAExpr' (FMul   fp ae1 ae2) = FMul   fp (replaceInFAExpr f ae1) (replaceInFAExpr f ae2)
    replaceInFAExpr' (FDiv   fp ae1 ae2) = FDiv   fp (replaceInFAExpr f ae1) (replaceInFAExpr f ae2)
    replaceInFAExpr' (FPow   fp ae1 ae2) = FPow   fp (replaceInFAExpr f ae1) (replaceInFAExpr f ae2)
    replaceInFAExpr' (FMod   fp ae1 ae2) = FMod   fp (replaceInFAExpr f ae1) (replaceInFAExpr f ae2)
    replaceInFAExpr' (FINeg  ae)         = FINeg     (replaceInFAExpr f ae)
    replaceInFAExpr' (FIAbs  ae)         = FIAbs     (replaceInFAExpr f ae)
    replaceInFAExpr' (FNeg   fp ae)      = FNeg   fp (replaceInFAExpr f ae)
    replaceInFAExpr' (FFloor fp ae)      = FFloor fp (replaceInFAExpr f ae)
    replaceInFAExpr' (FSqrt  fp ae)      = FSqrt  fp (replaceInFAExpr f ae)
    replaceInFAExpr' (FAbs   fp ae)      = FAbs   fp (replaceInFAExpr f ae)
    replaceInFAExpr' (FSin   fp ae)      = FSin   fp (replaceInFAExpr f ae)
    replaceInFAExpr' (FCos   fp ae)      = FCos   fp (replaceInFAExpr f ae)
    replaceInFAExpr' (FTan   fp ae)      = FTan   fp (replaceInFAExpr f ae)
    replaceInFAExpr' (FAcos  fp ae)      = FAcos  fp (replaceInFAExpr f ae)
    replaceInFAExpr' (FAsin  fp ae)      = FAsin  fp (replaceInFAExpr f ae)
    replaceInFAExpr' (FAtan  fp ae)      = FAtan  fp (replaceInFAExpr f ae)
    replaceInFAExpr' (FLn    fp ae)      = FLn    fp (replaceInFAExpr f ae)
    replaceInFAExpr' (FExpo  fp ae) = FExpo  fp (replaceInFAExpr f ae)
    replaceInFAExpr' (FFma   fp ae1 ae2 ae3) = FFma fp (replaceInFAExpr f ae1) (replaceInFAExpr f ae2) (replaceInFAExpr f ae3)
    replaceInFAExpr' (FMin aes) = FMin (map (replaceInFAExpr f) aes)
    replaceInFAExpr' (FMax aes) = FMax (map (replaceInFAExpr f) aes)
    replaceInFAExpr' ae@(RtoD (Int _))  = ae
    replaceInFAExpr' ae@(RtoS (Int _))  = ae
    replaceInFAExpr' ae@(RtoD (Rat _))  = ae
    replaceInFAExpr' ae@(RtoS (Rat _))  = ae
    replaceInFAExpr' (StoD ae)  = StoD  (replaceInFAExpr f ae) 
    replaceInFAExpr' (DtoS ae)  = DtoS  (replaceInFAExpr f ae)
    replaceInFAExpr' (ItoD ae)  = ItoD  (replaceInFAExpr f ae)
    replaceInFAExpr' (ItoS ae)  = ItoS  (replaceInFAExpr f ae)
    replaceInFAExpr' (Value ae) = Value (replaceInFAExpr f ae)
    replaceInFAExpr' ae         = error $ "replaceInFAExpr' niy for " ++ show ae

replaceInFBExpr :: (FAExpr -> Maybe FAExpr) -> FBExpr -> FBExpr
replaceInFBExpr g (FOr  be1 be2) = FOr  (replaceInFBExpr g be1) (replaceInFBExpr g be2)
replaceInFBExpr g (FAnd be1 be2) = FAnd (replaceInFBExpr g be1) (replaceInFBExpr g be2)
replaceInFBExpr g (FNot be)      = FNot (replaceInFBExpr g be)
replaceInFBExpr g (FEq  ae1 ae2) = FEq  (replaceInFAExpr g ae1) (replaceInFAExpr g ae2)
replaceInFBExpr g (FNeq ae1 ae2) = FNeq (replaceInFAExpr g ae1) (replaceInFAExpr g ae2)
replaceInFBExpr g (FLt  ae1 ae2) = FLt  (replaceInFAExpr g ae1) (replaceInFAExpr g ae2)
replaceInFBExpr g (FLtE ae1 ae2) = FLtE (replaceInFAExpr g ae1) (replaceInFAExpr g ae2)
replaceInFBExpr g (FGt  ae1 ae2) = FGt  (replaceInFAExpr g ae1) (replaceInFAExpr g ae2)
replaceInFBExpr g (FGtE ae1 ae2) = FGtE (replaceInFAExpr g ae1) (replaceInFAExpr g ae2)
replaceInFBExpr g (IsValid ae)   = IsValid (replaceInFAExpr g ae)
replaceInFBExpr _ FBTrue  = FBTrue
replaceInFBExpr _ FBFalse = FBFalse

replaceInAExpr :: (AExpr -> Maybe AExpr) -> AExpr -> AExpr
replaceInAExpr f expr = fromMaybe (replaceInAExpr' f expr) (f expr)

replaceInAExpr' :: (AExpr -> Maybe AExpr) -> AExpr -> AExpr
replaceInAExpr' _ ae@(Int _)         = ae
replaceInAExpr' _ ae@(Rat _)         = ae
replaceInAExpr' _ ae@(Var _ _)       = ae
replaceInAExpr' _ ae@ArrayElem{}     = ae 
replaceInAExpr' _ ae@(RealMark _)    = ae
replaceInAExpr' _ ae@(ErrorMark _ _) = ae
replaceInAExpr' _ ae@(ErrRat _)      = ae
replaceInAExpr' _ Prec = Prec
replaceInAExpr' _ Infinity     = Infinity
replaceInAExpr' _ ErrUndefined = ErrUndefined
replaceInAExpr' f (Neg   ae)      = Neg   (replaceInAExpr f ae)
replaceInAExpr' f (Floor ae)      = Floor (replaceInAExpr f ae)
replaceInAExpr' f (Sqrt  ae)      = Sqrt  (replaceInAExpr f ae)
replaceInAExpr' f (Abs   ae)      = Abs   (replaceInAExpr f ae)
replaceInAExpr' f (Sin   ae)      = Sin   (replaceInAExpr f ae)
replaceInAExpr' f (Cos   ae)      = Cos   (replaceInAExpr f ae)
replaceInAExpr' f (Tan   ae)      = Tan   (replaceInAExpr f ae)
replaceInAExpr' f (ASin  ae)      = ASin  (replaceInAExpr f ae)
replaceInAExpr' f (ACos  ae)      = ACos  (replaceInAExpr f ae)
replaceInAExpr' f (ATan  ae)      = ATan  (replaceInAExpr f ae)
replaceInAExpr' f (Ln    ae)      = Ln    (replaceInAExpr f ae)
replaceInAExpr' f (Expo  ae)      = Expo  (replaceInAExpr f ae)
replaceInAExpr' f (Add   ae1 ae2) = Add   (replaceInAExpr f ae1) (replaceInAExpr f ae2)
replaceInAExpr' f (Sub   ae1 ae2) = Sub   (replaceInAExpr f ae1) (replaceInAExpr f ae2)
replaceInAExpr' f (Mul   ae1 ae2) = Mul   (replaceInAExpr f ae1) (replaceInAExpr f ae2)
replaceInAExpr' f (Div   ae1 ae2) = Div   (replaceInAExpr f ae1) (replaceInAExpr f ae2)
replaceInAExpr' f (Pow   ae1 ae2) = Pow   (replaceInAExpr f ae1) (replaceInAExpr f ae2)
replaceInAExpr' f (Mod   ae1 ae2) = Mod   (replaceInAExpr f ae1) (replaceInAExpr f ae2)
replaceInAExpr' f (IDiv  ae1 ae2) = IDiv  (replaceInAExpr f ae1) (replaceInAExpr f ae2)
replaceInAExpr' f (ItDiv ae1 ae2) = ItDiv (replaceInAExpr f ae1) (replaceInAExpr f ae2)
replaceInAExpr' f (IMod  ae1 ae2) = IMod  (replaceInAExpr f ae1) (replaceInAExpr f ae2)
replaceInAExpr' f (ItMod ae1 ae2) = ItMod (replaceInAExpr f ae1) (replaceInAExpr f ae2)
replaceInAExpr' f (Expt  ae1 ae2) = Expt  (replaceInAExpr f ae1) (replaceInAExpr f ae2)
replaceInAExpr' f (EFun g fp args) = EFun g fp (map (replaceInAExpr f) args)
replaceInAExpr' f (Min    aes) = Min    (map (replaceInAExpr f) aes)
replaceInAExpr' f (Max    aes) = Max    (map (replaceInAExpr f) aes)
replaceInAExpr' f (MaxErr aes) = MaxErr (map (replaceInAExpr f) aes)
replaceInAExpr' f (Fma   ae1 ae2 ae3) = Fma (replaceInAExpr f ae1) (replaceInAExpr f ae2) (replaceInAExpr f ae3)
replaceInAExpr' f (ErrAdd   fp ae1 ee1 ae2 ee2) = ErrAdd fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) (replaceInAExpr f ae2) (replaceInAExpr f ee2)
replaceInAExpr' f (ErrSub   fp ae1 ee1 ae2 ee2) = ErrSub fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) (replaceInAExpr f ae2) (replaceInAExpr f ee2)
replaceInAExpr' f (ErrMul   fp ae1 ee1 ae2 ee2) = ErrMul fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) (replaceInAExpr f ae2) (replaceInAExpr f ee2)
replaceInAExpr' f (ErrDiv   fp ae1 ee1 ae2 ee2) = ErrDiv fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) (replaceInAExpr f ae2) (replaceInAExpr f ee2)
replaceInAExpr' f (ErrItDiv fp ae1 ee1 ae2 ee2) = ErrItDiv fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) (replaceInAExpr f ae2) (replaceInAExpr f ee2)
replaceInAExpr' f (ErrMod   fp ae1 ee1 ae2 ee2) = ErrMod fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) (replaceInAExpr f ae2) (replaceInAExpr f ee2)
replaceInAExpr' f (ErrItMod fp ae1 ee1 ae2 ee2) = ErrItMod fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) (replaceInAExpr f ae2) (replaceInAExpr f ee2)
replaceInAExpr' f (ErrFma fp ae1 ee1 ae2 ee2 ae3 ee3) = ErrFma fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) (replaceInAExpr f ae2) (replaceInAExpr f ee2) (replaceInAExpr f ae3) (replaceInAExpr f ee3)
replaceInAExpr' f (ErrFloor  fp ae1 ee1) = ErrFloor  fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) 
replaceInAExpr' f (ErrFloor0 fp ae1 ee1) = ErrFloor0 fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) 
replaceInAExpr' f (ErrSqrt   fp ae1 ee1) = ErrSqrt   fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) 
replaceInAExpr' f (ErrSin    fp ae1 ee1) = ErrSin    fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) 
replaceInAExpr' f (ErrCos    fp ae1 ee1) = ErrCos    fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) 
replaceInAExpr' f (ErrTan    fp ae1 ee1) = ErrTan    fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) 
replaceInAExpr' f (ErrAsin   fp ae1 ee1) = ErrAsin   fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) 
replaceInAExpr' f (ErrAcos   fp ae1 ee1) = ErrAcos   fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) 
replaceInAExpr' f (ErrAtan   fp ae1 ee1) = ErrAtan   fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) 
replaceInAExpr' f (ErrAtanT  fp ae1 ee1) = ErrAtanT  fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) 
replaceInAExpr' f (ErrNeg    fp ae1 ee1) = ErrNeg    fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) 
replaceInAExpr' f (ErrAbs    fp ae1 ee1) = ErrAbs    fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) 
replaceInAExpr' f (ErrLn     fp ae1 ee1) = ErrLn     fp (replaceInAExpr f ae1) (replaceInAExpr f ee1) 
replaceInAExpr' f (ErrExpo   fp ae1 ee1) = ErrExpo   fp (replaceInAExpr f ae1) (replaceInAExpr f ee1)  
replaceInAExpr' f (ErrMulPow2R fp i ee) = ErrMulPow2R fp i (replaceInAExpr f ee)
replaceInAExpr' f (ErrMulPow2L fp i ee) = ErrMulPow2R fp i (replaceInAExpr f ee)
replaceInAExpr' f (HalfUlp ae fp) = HalfUlp (replaceInAExpr f ae) fp
replaceInAExpr' f (ErrStoD ae ee) = ErrStoD (replaceInAExpr f ae) (replaceInAExpr f ee)
replaceInAExpr' f (ErrDtoS ae ee) = ErrDtoS (replaceInAExpr f ae) (replaceInAExpr f ee)
replaceInAExpr' f (ErrItoS ae ee) = ErrItoS (replaceInAExpr f ae) (replaceInAExpr f ee)
replaceInAExpr' f (ErrItoD ae ee) = ErrItoD (replaceInAExpr f ae) (replaceInAExpr f ee)
replaceInAExpr' _ ae = error $ "replaceInAExpr': niy for " ++ show ae

replaceInBExpr :: (AExpr -> Maybe AExpr) -> BExpr -> BExpr
replaceInBExpr g (Or  be1 be2) = Or  (replaceInBExpr g be1) (replaceInBExpr g be2)
replaceInBExpr g (And be1 be2) = And (replaceInBExpr g be1) (replaceInBExpr g be2)
replaceInBExpr g (Not be)      = Not (replaceInBExpr g be)
replaceInBExpr g (Eq  ae1 ae2) = Eq  (replaceInAExpr g ae1) (replaceInAExpr g ae2)
replaceInBExpr g (Neq ae1 ae2) = Neq (replaceInAExpr g ae1) (replaceInAExpr g ae2)
replaceInBExpr g (Lt  ae1 ae2) = Lt  (replaceInAExpr g ae1) (replaceInAExpr g ae2)
replaceInBExpr g (LtE ae1 ae2) = LtE (replaceInAExpr g ae1) (replaceInAExpr g ae2)
replaceInBExpr g (Gt  ae1 ae2) = Gt  (replaceInAExpr g ae1) (replaceInAExpr g ae2)
replaceInBExpr g (GtE ae1 ae2) = GtE (replaceInAExpr g ae1) (replaceInAExpr g ae2)
replaceInBExpr _ BTrue  = BTrue
replaceInBExpr _ BFalse = BFalse

isZero :: Rational -> Bool
isZero r = r == toRational (0 :: Integer)

isZeroExpr :: FAExpr -> Bool
isZeroExpr (FInt 0)       = True
isZeroExpr (RtoD (Int 0)) = True
isZeroExpr (RtoS (Int 0)) = True
isZeroExpr (FCnst _ r) = isZero r
isZeroExpr _ = False

errVar :: FAExpr -> AExpr
errVar  (FVar fp x) = ErrorMark x fp
errVar ae = error $ "errVar not defined for " ++ show ae ++ "."

realVar :: FAExpr -> AExpr
realVar (FVar _ x) = RealMark  x
realVar ae = error $ "realVar not defined for " ++ show ae ++ "."

nameFVar :: FAExpr -> VarName
nameFVar (FVar _ x) = x
nameFVar ae = error $ "nameFVar: unexpected value " ++ show ae ++ "."

precFVar :: FAExpr -> FPrec
precFVar (FVar fp _) = fp
precFVar ae = error $ "precFVar: unexpected value " ++ show ae ++ "."

-----------------------
-- PPExt instances --
-----------------------

prettyVarWithType :: FAExpr -> Doc
prettyVarWithType (FVar fp x) = text x <> text ":" <+> prettyDoc fp
prettyVarWithType ae = error $ "prettyVarWithType: case " ++ show ae ++ " niy."


instance PPExt Arg where
    prettyDoc (Arg x fp) = text x <> text ":" <+> prettyDoc fp

instance PPExt RProgram where
    prettyDoc decls = vcat (map prettyDoc decls)

instance PPExt RDecl where
    prettyDoc (RDecl fp fun args stm)
      = text fun <> text "(" <>
        hsep (punctuate comma $ map prettyDoc args)
        <> text  "):" <+> prettyDoc fp <+> text " =" <+> prettyDoc stm

instance PPExt RStm where
    prettyDoc RUnstWarning = error "Warning should not occur in a real-valued program."-- text "warning"
    prettyDoc (RLet x t ae stm)
        = text "LET" <+> text x <> text ":" <> prettyDoc t <> text "=" <> prettyDoc ae
            $$ text "IN" <+> prettyDoc stm
    prettyDoc (RIte be stm1 stm2)
        = text "IF" <+> prettyDoc be
            $$ text "THEN" <+> prettyDoc stm1
            $$ text "ELSE" <+> prettyDoc stm2
            $$ text "ENDIF"
    prettyDoc (RListIte [] _) = error "prettyDoc RListIte: empty stmThen list"
    prettyDoc (RListIte ((beThen,stmThen):thenList) stmElse)
        = text "IF" <+> prettyDoc beThen $$ text "THEN" <+> prettyDoc stmThen
            $$ vcat (map (\(be,stm) -> text "ELSIF" <+> prettyDoc be
            $$ text "THEN" <+> prettyDoc stm) thenList) 
            $$ text "ELSE" <+> prettyDoc stmElse $$ text "ENDIF"
    prettyDoc (RStmExpr ae) = prettyDoc ae
    prettyDoc RForLoop{} = error "prettyDoc RStm: unexpected for loop."
            -- (RForLoop retType startIdx endIdx initValueAcc idx acc forBody)
            -- text "for" <> text "(" <> integer i <> comma <> integer j <> comma
            --         <> prettyDoc ae <> comma <> prettyDoc f <> text ")"

instance PPExt Program where
    prettyDoc decls = vcat (map prettyDoc decls)

instance PPExt Decl where
    prettyDoc (Decl fp fun args stm)
      = text fun <> text "(" <>
        hsep (punctuate comma $ map prettyDoc args)
        <> text  "):" <+> prettyDoc fp <+> text " =" <+> prettyDoc stm

instance PPExt Stm where
    prettyDoc UnstWarning = error "Warning should not occur in a non-transformed program."-- text "warning"
    prettyDoc (Let x t ae stm)
        = text "LET" <+> text x <> text ":" <> prettyDoc t <> text "=" <> prettyDoc ae
            $$ text "IN" <+> prettyDoc stm
    prettyDoc (Ite be stm1 stm2)
        = text "IF" <+> prettyDoc be
            $$ text "THEN" <+> prettyDoc stm1
            $$ text "ELSE" <+> prettyDoc stm2
            $$ text "ENDIF"
    prettyDoc (ListIte [] _) = error "prettyDoc RListIte: empty stmThen list"
    prettyDoc (ListIte ((beThen,stmThen):thenList) stmElse)
        = text "IF" <+> prettyDoc beThen $$ text "THEN" <+> prettyDoc stmThen
            $$ vcat (map (\(be,stm) -> text "ELSIF" <+> prettyDoc be
            $$ text "THEN" <+> prettyDoc stm) thenList) 
            $$ text "ELSE" <+> prettyDoc stmElse $$ text "ENDIF"
    prettyDoc (StmExpr ae) = prettyDoc ae
    prettyDoc ForLoop{} = undefined

printBinOpError :: (PPExt a, PPExt a1, PPExt a2, PPExt a3) => String -> a -> a1 -> a2 -> a3 -> Doc
printBinOpError nameErrFun r1 e1 r2 e2 =
    text nameErrFun <> (text "(" <>  prettyDoc r1 <> comma <+> prettyDoc e1 <> comma
                                 <+> prettyDoc r2 <> comma <+> prettyDoc e2 <> text ")")

printUnaryOpError :: (PPExt a, PPExt a1) => String -> a -> a1 -> Doc
printUnaryOpError nameErrFun r e = text nameErrFun <> (text "(" <>  prettyDoc r <> comma
                                            <+> prettyDoc e <> text ")")

instance PPExt AExpr where
    prettyDoc         Infinity = text "infinity"
    prettyDoc     ErrUndefined = text "undefined"
    prettyDoc          (Int i) = integer i
    prettyDoc          (Rat d) = parens $ text $ showRational d
    prettyDoc        (Var _ x) = text x
    prettyDoc      (EFun f _ []) = text f
    prettyDoc    (Neg (Int i)) = text "-" <> integer i
    prettyDoc    (Neg (Rat d)) = text "-" <> parens (text $ showRational d)
    prettyDoc      (Add a1 a2) = parens $ prettyDoc a1 <+> text "+" <+> prettyDoc a2
    prettyDoc      (Sub a1 a2) = parens $ prettyDoc a1 <+> text "-" <+> prettyDoc a2
    prettyDoc      (Mul a1 a2) = parens $ prettyDoc a1 <+> text "*" <+> prettyDoc a2
    prettyDoc   (Fma a1 a2 a3) = text "Fma" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2 <> comma <+> prettyDoc a3)
    prettyDoc      (Div a1 a2) = parens $ prettyDoc a1 <+> text "/" <+> prettyDoc a2
    prettyDoc     (IDiv a1 a2) = text "Idiv" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc    (ItDiv a1 a2) = text "Itdiv" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc      (Mod a1 a2) = text "mod" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc     (IMod a1 a2) = text "Imod" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc    (ItMod a1 a2) = text "Itmod" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc     (Expt a1 a2) = text "expt" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc      (Pow a1 a2) = prettyDoc a1 <> text "^" <> lparen <> prettyDoc a2 <> rparen
    prettyDoc        (Neg   a) = text "-"     <> lparen <> prettyDoc a <> rparen
    prettyDoc        (Floor a) = text "floor" <> lparen <> prettyDoc a <> rparen
    prettyDoc        (Sqrt  a) = text "sqrt"  <> lparen <> prettyDoc a <> rparen
    prettyDoc        (Abs   a) = text "abs"   <> lparen <> prettyDoc a <> rparen
    prettyDoc        (Sin   a) = text "sin"   <> lparen <> prettyDoc a <> rparen
    prettyDoc        (Cos   a) = text "cos"   <> lparen <> prettyDoc a <> rparen
    prettyDoc        (Tan   a) = text "tan"   <> lparen <> prettyDoc a <> rparen
    prettyDoc        (ASin  a) = text "asin"  <> lparen <> prettyDoc a <> rparen
    prettyDoc        (ACos  a) = text "acos"  <> lparen <> prettyDoc a <> rparen
    prettyDoc        (ATan  a) = text "atan"  <> lparen <> prettyDoc a <> rparen
    prettyDoc        (Ln    a) = text "ln"  <> lparen <> prettyDoc a <> rparen
    prettyDoc        (Expo  a) = text "exp"  <> lparen <> prettyDoc a <> rparen
    prettyDoc    (EFun f _ args) = text f <> parens (hsep $ punctuate comma $ map prettyDoc args)
    prettyDoc         (StoR a) = text "StoR"   <> lparen <> prettyDoc a <> rparen
    prettyDoc         (DtoR a) = text "DtoR"   <> lparen <> prettyDoc a <> rparen
    prettyDoc        (FExp fa) = text "Fexp" <> parens (prettyDoc fa)
    prettyDoc     (RealMark x) = text "r_" <> text x
    prettyDoc (ErrorMark x _)  = text "e_" <> text x
    prettyDoc (ErrRat r) = parens $ text $ showRational r

    prettyDoc (MaxErr []) = error "Something went wrong: MaxErr applied to empty list"
    prettyDoc (MaxErr [es]) = prettyDoc es
    prettyDoc (MaxErr ees@(_:(_:_))) = 
        text "max" <> parens (hsep $ punctuate comma (map prettyDoc ees))

    prettyDoc (ErrAdd FPSingle r1 e1 r2 e2) = printBinOpError "aeboundsp_add" r1 e1 r2 e2
    prettyDoc (ErrAdd FPDouble r1 e1 r2 e2) = printBinOpError "aebounddp_add" r1 e1 r2 e2
    prettyDoc (ErrAdd TInt     r1 e1 r2 e2) = printBinOpError  "aeboundi_add" r1 e1 r2 e2

    prettyDoc (ErrSub FPSingle r1 e1 r2 e2) = printBinOpError "aeboundsp_sub" r1 e1 r2 e2
    prettyDoc (ErrSub FPDouble r1 e1 r2 e2) = printBinOpError "aebounddp_sub" r1 e1 r2 e2
    prettyDoc (ErrSub TInt     r1 e1 r2 e2) = printBinOpError  "aeboundi_sub" r1 e1 r2 e2

    prettyDoc (ErrMul FPSingle r1 e1 r2 e2) = printBinOpError "aeboundsp_mul" r1 e1 r2 e2
    prettyDoc (ErrMul FPDouble r1 e1 r2 e2) = printBinOpError "aebounddp_mul" r1 e1 r2 e2
    prettyDoc (ErrMul TInt     r1 e1 r2 e2) = printBinOpError  "aeboundi_mul" r1 e1 r2 e2 

    prettyDoc (ErrDiv FPSingle r1 e1 r2 e2) = printBinOpError "aeboundsp_div" r1 e1 r2 e2
    prettyDoc (ErrDiv FPDouble r1 e1 r2 e2) = printBinOpError "aebounddp_div" r1 e1 r2 e2
    prettyDoc (ErrDiv TInt     r1 e1 r2 e2) = printBinOpError  "aeboundi_div" r1 e1 r2 e2

    prettyDoc (ErrMod TInt     r1 e1 r2 e2) = printBinOpError "aeboundi_mod" r1 e1 r2 e2

    prettyDoc (ErrMulPow2L fp n e) = text nameErrFun <> (text "(" <> integer n <> comma
                                                     <+> prettyDoc e <> text ")") 
        where
            nameErrFun = case fp of
                            FPSingle -> "aebounddp_mul_p2l"
                            FPDouble -> "aeboundsp_mul_p2l"
                            _ -> error $ "prettyDoc ErrMulPow2L: unexpected type " ++ show fp ++ " value."

    prettyDoc (ErrMulPow2R fp n e) = text nameErrFun <> (text "(" <> integer n <> comma
                                                     <+> prettyDoc e <> text ")") 
        where
            nameErrFun = case fp of
                            FPSingle -> "aebounddp_mul_p2r"
                            FPDouble -> "aeboundsp_mul_p2r"
                            _ -> error $ "prettyDoc ErrMulPow2R: unexpected type " ++ show fp ++ " value."

    prettyDoc (ErrFloor  FPSingle r e) = printUnaryOpError "aeboundsp_flr"   r e
    prettyDoc (ErrFloor  FPDouble r e) = printUnaryOpError "aebounddp_flr"   r e

    prettyDoc (ErrFloor0 FPSingle r e) = printUnaryOpError "aeboundsp_flr_t" r e
    prettyDoc (ErrFloor0 FPDouble r e) = printUnaryOpError "aeboundsp_flr_t" r e

    prettyDoc (ErrSqrt   FPSingle r e) = printUnaryOpError "aeboundsp_sqt" r e
    prettyDoc (ErrSqrt   FPDouble r e) = printUnaryOpError "aebounddp_sqt" r e

    prettyDoc (ErrSin    FPSingle r e) = printUnaryOpError "aeboundsp_sin" r e
    prettyDoc (ErrSin    FPDouble r e) = printUnaryOpError "aebounddp_sin" r e

    prettyDoc (ErrCos    FPSingle r e) = printUnaryOpError "aeboundsp_cos" r e
    prettyDoc (ErrCos    FPDouble r e) = printUnaryOpError "aebounddp_cos" r e

    prettyDoc (ErrTan    FPSingle r e) = printUnaryOpError "aeboundsp_tan" r e
    prettyDoc (ErrTan    FPDouble r e) = printUnaryOpError "aebounddp_tan" r e 

    prettyDoc (ErrAcos   FPSingle r e) = printUnaryOpError "aeboundsp_acs" r e 
    prettyDoc (ErrAcos   FPDouble r e) = printUnaryOpError "aebounddp_acs" r e

    prettyDoc (ErrAsin   FPSingle r e) = printUnaryOpError "aeboundsp_asn" r e
    prettyDoc (ErrAsin   FPDouble r e) = printUnaryOpError "aebounddp_asn" r e 

    prettyDoc (ErrAtan   FPSingle r e) = printUnaryOpError "aeboundsp_atn" r e
    prettyDoc (ErrAtan   FPDouble r e) = printUnaryOpError "aebounddp_atn" r e

    prettyDoc (ErrAtanT  FPSingle r e) = printUnaryOpError "aeboundsp_atn_t" r e
    prettyDoc (ErrAtanT  FPDouble r e) = printUnaryOpError "aebounddp_atn_t" r e

    prettyDoc (ErrNeg    FPSingle r e) = printUnaryOpError "aeboundsp_neg" r e
    prettyDoc (ErrNeg    FPDouble r e) = printUnaryOpError "aebounddp_neg" r e
    prettyDoc (ErrNeg    TInt     r e) = printUnaryOpError  "aeboundi_neg" r e 

    prettyDoc (ErrAbs    FPSingle r e) = printUnaryOpError  "aeboundsp_abs" r e 
    prettyDoc (ErrAbs    FPDouble r e) = printUnaryOpError  "aebounddp_abs" r e
    prettyDoc (ErrAbs    TInt     r e) = printUnaryOpError   "aeboundi_abs" r e

    prettyDoc (ErrExpo   FPSingle r e) = printUnaryOpError  "aeboundsp_exp" r e
    prettyDoc (ErrExpo   FPDouble r e) = printUnaryOpError  "aebounddp_exp" r e

    prettyDoc (ErrLn     FPSingle r e) = printUnaryOpError  "aeboundsp_ln"  r e
    prettyDoc (ErrLn     FPDouble r e) = printUnaryOpError  "aebounddp_ln"  r e

    prettyDoc (HalfUlp r@(RealMark _) FPSingle)
        = text "ulp_sp" <> (text "(" <> prettyDoc r <> text ")/2")

    prettyDoc (HalfUlp r@(RealMark _) FPDouble)
        = text "ulp_dp" <> (text "(" <> prettyDoc r <> text ")/2")

    prettyDoc (HalfUlp a _)
        = error ("ppEEExpr: unexpected value in  HalfUlp " ++ show a)

    prettyDoc (ErrStoD r e) = printUnaryOpError  "aebound_StoD"  r e
    prettyDoc (ErrDtoS r e) = printUnaryOpError  "aebound_DtoS"  r e
    prettyDoc (ErrItoS r e) = printUnaryOpError  "aebound_ItoS"  r e
    prettyDoc (ErrItoD r e) = printUnaryOpError  "aebound_ItoD"  r e
    prettyDoc ee = error $ "prettyDoc for " ++ show ee ++ "not implemented yet."


instance PPExt FAExpr where

    prettyDoc (RtoS a) = text "RtoS" <> parens (prettyDoc a)
    prettyDoc (RtoD a) = text "RtoD" <> parens (prettyDoc a)
    prettyDoc (StoD a) = text "StoD" <> parens (prettyDoc a)
    prettyDoc (DtoS a) = text "StoD" <> parens (prettyDoc a)
    prettyDoc (ItoS a) = text "ItoS" <> parens (prettyDoc a)
    prettyDoc (ItoD a) = text "ItoD" <> parens (prettyDoc a)
    prettyDoc (FInt i) = integer i
    prettyDoc (FCnst _ d) = parens $ text $ showRational d
    prettyDoc (FNeg _ (FInt i)) = text "-" <> integer i
    prettyDoc (FNeg _ (FCnst _ d)) = text "-" <> parens (text $ showRational d)
    prettyDoc (FVar _ x) = text x
    prettyDoc (FEFun f _ []) = text f
    prettyDoc (FEFun f _ args) = text f <> parens (hsep $ punctuate comma $ map prettyDoc args)
    prettyDoc (FIAdd  a1 a2) = text "Iadd"  <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc (FISub  a1 a2) = text "Isub"  <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc (FIMul  a1 a2) = text "Imul"  <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc (FIDiv  a1 a2) = text "Idiv"  <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc (FItDiv a1 a2) = text "Itdiv" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc (FIMod  a1 a2) = text "Imod"  <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc (FItMod a1 a2) = text "Itmod" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc (FIPow  a1 a2) = text "Ipow"  <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc (FINeg  a)     = text "Ineg"  <> lparen <> prettyDoc a <> rparen
    prettyDoc (FIAbs  a)     = text "Iabs"  <> lparen <> prettyDoc a <> rparen
    --
    prettyDoc (FFma   FPSingle a1 a2 a3) = text "Sfma" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2 <> comma <+> prettyDoc a3)
    prettyDoc (FAdd   FPSingle a1 a2) = text "Sadd" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc (FSub   FPSingle a1 a2) = text "Ssub" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc (FMul   FPSingle a1 a2) = text "Smul" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc (FDiv   FPSingle a1 a2) = text "Sdiv" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc (FMod   FPSingle a1 a2) = text "Smod" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc (FPow   FPSingle a1 a2) = prettyDoc a1 <+> text "^" <+> lparen <> prettyDoc a2 <> rparen
    prettyDoc (FNeg   FPSingle a)     = text "Sneg"   <> lparen <> prettyDoc a <> rparen
    prettyDoc (FFloor FPSingle a)     = text "Sfloor" <> lparen <> prettyDoc a <> rparen
    prettyDoc (FSqrt  FPSingle a)     = text "Ssqrt"  <> lparen <> prettyDoc a <> rparen
    prettyDoc (FAbs   FPSingle a)     = text "Sabs"   <> lparen <> prettyDoc a <> rparen
    prettyDoc (FSin   FPSingle a)     = text "Ssin"   <> lparen <> prettyDoc a <> rparen
    prettyDoc (FCos   FPSingle a)     = text "Scos"   <> lparen <> prettyDoc a <> rparen
    prettyDoc (FTan   FPSingle a)     = text "Stan"   <> lparen <> prettyDoc a <> rparen
    prettyDoc (FAsin  FPSingle a)     = text "Sasin"  <> lparen <> prettyDoc a <> rparen
    prettyDoc (FAcos  FPSingle a)     = text "Sacos"  <> lparen <> prettyDoc a <> rparen
    prettyDoc (FAtan  FPSingle a)     = text "Satan"  <> lparen <> prettyDoc a <> rparen
    prettyDoc (FLn    FPSingle a)     = text "Sln"    <> lparen <> prettyDoc a <> rparen
    prettyDoc (FExpo  FPSingle a)     = text "Sexp"   <> lparen <> prettyDoc a <> rparen
    --
    prettyDoc (FAdd   FPDouble a1 a2) = text "Dadd" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc (FSub   FPDouble a1 a2) = text "Dsub" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc (FMul   FPDouble a1 a2) = text "Dmul" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc (FDiv   FPDouble a1 a2) = text "Ddiv" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc (FMod   FPDouble a1 a2) = text "Dmod" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
    prettyDoc (FFma   FPDouble a1 a2 a3) = text "Dfma" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2 <> comma <+> prettyDoc a3)
    prettyDoc (FPow   FPDouble a1 a2) = prettyDoc a1 <+> text "^" <+> lparen <> prettyDoc a2 <> rparen
    prettyDoc (FNeg   FPDouble a)     = text "Dneg"   <> lparen <> prettyDoc a <> rparen
    prettyDoc (FFloor FPDouble a)     = text "Dfloor" <> lparen <> prettyDoc a <> rparen
    prettyDoc (FSqrt  FPDouble a)     = text "Dsqrt"  <> lparen <> prettyDoc a <> rparen
    prettyDoc (FAbs   FPDouble a)     = text "Dabs"   <> lparen <> prettyDoc a <> rparen
    prettyDoc (FSin   FPDouble a)     = text "Dsin"   <> lparen <> prettyDoc a <> rparen
    prettyDoc (FCos   FPDouble a)     = text "Dcos"   <> lparen <> prettyDoc a <> rparen
    prettyDoc (FTan   FPDouble a)     = text "Dtan"   <> lparen <> prettyDoc a <> rparen
    prettyDoc (FAsin  FPDouble a)     = text "Dasin"  <> lparen <> prettyDoc a <> rparen
    prettyDoc (FAcos  FPDouble a)     = text "Dacos"  <> lparen <> prettyDoc a <> rparen
    prettyDoc (FAtan  FPDouble a)     = text "Datan"  <> lparen <> prettyDoc a <> rparen
    prettyDoc (FLn    FPDouble a)     = text "Dln"    <> lparen <> prettyDoc a <> rparen
    prettyDoc (FExpo  FPDouble a)     = text "Dexp"   <> lparen <> prettyDoc a <> rparen
    --
    prettyDoc (FMin as) = text "min" <> parens (hsep $ punctuate comma $ map prettyDoc as)
    prettyDoc (FMax as) = text "min" <> parens (hsep $ punctuate comma $ map prettyDoc as)
    --
    prettyDoc ee = error $ "prettyDoc for " ++ show ee ++ "not implemented yet."

instance PPExt BExpr where

    prettyDoc (Or  e1 e2) = parens $ prettyDoc e1 <+> text "OR"  <+> prettyDoc e2 
    prettyDoc (And e1 e2) = parens $ prettyDoc e1 <+> text "AND" <+> prettyDoc e2
    prettyDoc     (Not e) = text "NOT" <> parens (prettyDoc e)
    prettyDoc  (Eq a1 a2) = parens $ prettyDoc a1 <+> text "="  <+> prettyDoc a2 
    prettyDoc (Neq a1 a2) = parens $ prettyDoc a1 <+> text "/=" <+> prettyDoc a2 
    prettyDoc  (Lt a1 a2) = parens $ prettyDoc a1 <+> text "<"  <+> prettyDoc a2 
    prettyDoc (LtE a1 a2) = parens $ prettyDoc a1 <+> text "<=" <+> prettyDoc a2 
    prettyDoc  (Gt a1 a2) = parens $ prettyDoc a1 <+> text ">"  <+> prettyDoc a2  
    prettyDoc (GtE a1 a2) = parens $ prettyDoc a1 <+> text ">=" <+> prettyDoc a2  
    prettyDoc       BTrue = text "TRUE"
    prettyDoc      BFalse = text "FALSE"


instance PPExt FBExpr where

    prettyDoc  (FOr e1 e2) = parens $ prettyDoc e1 <+> text "OR"  <+> prettyDoc e2
    prettyDoc (FAnd e1 e2) = parens $ prettyDoc e1 <+> text "AND" <+> prettyDoc e2
    prettyDoc     (FNot e) = text "NOT" <> parens (prettyDoc e)
    prettyDoc  (FEq a1 a2) = parens $ prettyDoc a1 <+> text "="  <+> prettyDoc a2
    prettyDoc (FNeq a1 a2) = parens $ prettyDoc a1 <+> text "/=" <+> prettyDoc a2
    prettyDoc  (FLt a1 a2) = parens $ prettyDoc a1 <+> text "<"  <+> prettyDoc a2
    prettyDoc (FLtE a1 a2) = parens $ prettyDoc a1 <+> text "<=" <+> prettyDoc a2
    prettyDoc  (FGt a1 a2) = parens $ prettyDoc a1 <+> text ">"  <+> prettyDoc a2
    prettyDoc (FGtE a1 a2) = parens $ prettyDoc a1 <+> text ">=" <+> prettyDoc a2
    prettyDoc       FBTrue = text "TRUE"
    prettyDoc      FBFalse = text "FALSE"
    prettyDoc (IsValid ae) = text "isValid" <> parens (prettyDoc ae)



