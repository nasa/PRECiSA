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


module AbstractSemantics where

import AbstractDomain
import AbsPVSLang
import qualified Data.List as List
import qualified Data.Set as Set
import Debug.Trace
import Text.PrettyPrint
import Numeric

traceMany :: [(String, String)] -> a -> a
traceMany xs a = trace (concat $ map showPair xs) a
  where showPair (label, expr) = label ++ ": " ++ show expr ++ "\n"  

------------------
-- Environments --
------------------

type Env = [(String, CebS)] -- variables environment

emptyEnv :: [(String, CebS)]
emptyEnv = [] 


-----------------------------
-- Semantics aux functions --
-----------------------------

addCond :: BExpr -> FBExpr -> FPrec -> Ceb -> Ceb
-- adds a real and a fp condition in a cbe
addCond be fbe fp (c, g, f, i, ie, rule) = (simplBExprFix $ And c be, simplFBExprFix $  FAnd g fbe, f, i, ie, rule)

addCondS :: BExpr -> FBExpr -> FPrec -> CebS -> CebS
-- extension of addCond to sets of ceb
addCondS be fbe fp cs = map (addCond be fbe fp) cs

insertVarEnv :: String -> FAExpr -> Interp -> Env -> FPrec -> Env
-- inserts a new variable in the environment
insertVarEnv var aexpr i env fp =
  case lookup var env of 
    Just _ -> error "insertVarEnv: Variable already present in the environment"
    Nothing -> (var, (aexprSem aexpr i env fp)):env

lub :: (CebS -> FPrec -> Ceb) -> [CebS] -> FPrec -> CebS 
lub fun cs fp = filterCondFalse $ map (\c -> fun c fp) (combos cs)

filterCondFalse :: CebS -> CebS
-- eliminates the cebs with conditions equal to false
filterCondFalse cs = filter condDiffFalse cs
  where
    condDiffFalse (BFalse,_,_,_,_,_) = False
    condDiffFalse _ = True

isPow2 :: (Floating r, RealFrac r) => r -> Bool
isPow2 n = ((logBase 2 n) == fromInteger (round $ logBase 2 n)) && ((logBase 2 n)>= 0)
----------------------------
-- Semantics declarations --
----------------------------

declsSemK :: [Decl] -> Interp -> Int -> Bool -> Interp
-- semantics F of a set of declarations 
-- k is the number of iterations 
declsSemK decls interp k sta = declsSemK' decls interp k 0 sta
  where
    declsSemK' decls interp k n sta | (n==k) = interp
                                    | otherwise = declsSemK' decls (declsSem decls interp sta) k (n+1) sta

declsSem :: [Decl] -> Interp -> Bool -> Interp
-- immediate consequence operator on sets of declarations
declsSem decls interp sta = foldl (\i -> \decl -> declSem decl i sta) interp decls

declSem :: Decl -> Interp -> Bool -> Interp
declSem (Decl fp (NonVarId fun) args stm) interp sta = addDeclInterp fun fp args (stmSem stm interp emptyEnv fp sta) interp

addDeclInterp :: String -> FPrec -> [VarId] -> CebS -> Interp -> Interp                       
addDeclInterp fun fp args sem interp =
    let (l1,l2) = List.partition (keyEqTo fun) interp in
        case l1 of 
          [] -> l2 ++ [(fun, (fp, args, (replaceFun fun args sem)))] -- (replaceFun fun args sem)
          -------------- attention!!! fix this for programs with recursion! -------------------------
          [(fun1, (fp1, args1, sem1))] -> error "niy: recursion" -- l2 ++ [(fun1, (fp1, args, sem1))]--(fun, (lubA ae ae1)):l2
          ------------------------------------------------------------------------------------------- 
          _ -> error ("addDeclInterp: More than one occurrence of function " ++ fun ++ " in the interpretation.")    
    where
        keyEqTo f1 (f2, _) = f1 == f2
        replaceFun fun args sem = map (replaceFun' fun args) sem
        replaceFun' fun args (cond, guard, _, vals, errs, pvsTree) =
          -- traceMany [("Rcond: ", render. prettyDoc $ cond), ("Fcond: ", render. prettyDoc $ guard)] $
          (cond, guard, FEFun fun (map id2fvar args), vals, errs, pvsTree)
        id2fvar (VarId x) = FVar x


-------------------------------
-- PVS Expressions Semantics --
-------------------------------

stmSem :: Stm -> Interp -> Env -> FPrec -> Bool -> CebS
stmSem (StmExpr aexpr) interp env fp _ = aexprSem aexpr interp env fp -- elimDisj $ 
stmSem (Let (VarId var) aexpr stm) interp env fp sta =
  stmSem newStm interp newEnv fp sta -- elimDisj $ 
  where
    newEnv = insertVarEnv var aexpr interp env fp
    newStm = argsBindStm [VarId var] [aexpr] stm       
stmSem (Ite fbe stm1 stm2) interp env fp sta =
  if sta
    then ((addCondS be fbe fp sem1) ++ (addCondS (Not be) (FNot fbe) fp sem2))
    else ((addCondS be fbe fp sem1) --  elimDisj $
          ++ (addCondS (Not be) (FNot fbe) fp sem2)
          ++ (addCondS be (FNot fbe) fp (unTestSem fp (mergeEqR sem1) (mergeEqFP sem2)))
          ++ (addCondS (Not be) fbe  fp (unTestSem fp (mergeEqR sem2) (mergeEqFP sem1))))
  where
    be = fbe2be fbe
    sem1 = stmSem stm1 interp env fp sta
    sem2 = stmSem stm2 interp env fp sta

mergeEqFP :: CebS -> CebS
mergeEqFP sem = map head (groupWith eqFP sem)
  where
    eqFP (c1, fc1, f1, r1, e1, t1) (c2, fc2, f2, r2, e2, t2) = (fc1 == fc2) 

mergeEqR :: CebS -> CebS
mergeEqR sem = map head (groupWith eqR sem)
  where
    eqR (c1, fc1, f1, r1, e1, t1) (c2, fc2, f2, r2, e2, t2) = (c1 == c2) 

groupWith :: (a -> a -> Bool) -> [a] -> [[a]]
groupWith f [] = []
groupWith f xs@(x:_) = elems:(groupWith f rest)
  where
    (elems, rest) = List.partition (f x) xs

-- real semantics, fp semantics
unTestSem :: FPrec -> CebS -> CebS -> CebS
unTestSem fp semR semFP = map (makeUnTestElem fp) (combos [semR, semFP])

makeUnTestElem :: FPrec -> CebS -> Ceb
makeUnTestElem fp [(c1, fc1, f1, r1, e1, _),(c2, fc2, f2, r2, e2, t2)] =
   (simplBExprFix $ And c1 c2, fc2, f2, r1, AE $ Add (EE e2) (Abs (Sub r1 r2)), rule e2 r1 r2 (f2r f2) t2)
  where
    (f2r, rule) = case fp of
      FPSingle -> (StoR,SUnTest)
      FPDouble -> (DtoR,DUnTest)
makeUnTestElem _ _ = error "makeUnTestElem: something went wrong"

--------------------------------------
-- Arithmetic Expressions Semantics --
--------------------------------------

aexprSem :: FAExpr -> Interp -> Env -> FPrec -> CebS

aexprSem (RtoS (Int n)) _ _ FPSingle = [(BTrue, FBTrue, RtoS (Int n), Int n, AE $ Int 0, SIntR)]
aexprSem (RtoS (Int n)) _ _ FPDouble = error "aexprSem: FP precision mismatch"

aexprSem (RtoS (Double n)) _ _ FPSingle = [(BTrue, FBTrue, RtoS (Double n), Double n, ErrRat $ abs $ toRational ((fromRat n) :: Float) - (n :: Rational), SDoubleR)]
-- [(BTrue, FBTrue, RtoS (Double n), Double n, HalfUlp (Double n), SDoubleR)]
aexprSem (RtoS (Double n)) _ _ FPDouble = error "aexprSem: FP precision mismatch"

aexprSem (RtoD (Int n)) _ _ FPDouble = [(BTrue, FBTrue, RtoD (Int n), Int n, AE $ Int 0, DIntR)] 
aexprSem (RtoD (Int n)) _ _ FPSingle = error "aexprSem: FP precision mismatch"

aexprSem (RtoD (Double n)) _ _ FPDouble = [(BTrue, FBTrue, RtoD (Double n), Double n, ErrRat $ abs $ toRational ((fromRat n) :: Double) - (n :: Rational), DDoubleR)]
aexprSem (RtoD (Double n)) _ _ FPSingle = error "aexprSem: FP precision mismatch"

aexprSem (RtoS (Neg (Int n))) _ _ FPSingle = [(BTrue, FBTrue, RtoS (Neg (Int n)), Neg (Int n), AE $ Int 0, SIntR)]
aexprSem (RtoS (Neg (Int n))) _ _ FPDouble = error "aexprSem: FP precision mismatch"

aexprSem (RtoS (Neg (Double n))) _ _ FPSingle = [(BTrue, FBTrue, RtoS (Neg (Double n)), Neg (Double n), ErrRat $ abs $ toRational ((fromRat (-n)) :: Double) - ((-n) :: Rational), SDoubleR)]
aexprSem (RtoS (Neg (Double n))) _ _ FPDouble = error "aexprSem: FP precision mismatch"

aexprSem (RtoD (Neg (Int n))) _ _ FPDouble = [(BTrue, FBTrue, RtoD (Neg (Int n)), Neg (Int n), AE $ Int 0, DIntR)] 
aexprSem (RtoD (Neg (Int n))) _ _ FPSingle = error "aexprSem: FP precision mismatch"

aexprSem (RtoD (Neg (Double n))) _ _ FPDouble = [(BTrue, FBTrue, RtoD (Neg (Double n)), Neg (Double n), ErrRat $ abs $ toRational ((fromRat (-n)) :: Double) - ((-n) :: Rational), DDoubleR)]
aexprSem (RtoD (Neg (Double n))) _ _ FPSingle = error "aexprSem: FP precision mismatch"

aexprSem (FInt n) _ _ fp = [(BTrue, FBTrue, expr, Int n, AE $ Int 0, rule)] -- Int 0,

  where
    (expr,rule) = case fp of
                    FPSingle -> (RtoS $ Int n, SIntR)
                    FPDouble -> (RtoD $ Int n, DIntR)

aexprSem (FDouble n) _ _ fp = [(BTrue, FBTrue, expr, Double n, AE $ Int 0, rule)]
  where
    (expr,rule) = case fp of
                    FPSingle -> (RtoS $ Double n, SDoubleR)
                    FPDouble -> (RtoD $ Double n, DDoubleR)

aexprSem (FPi) _ _ fp = error "aexprSem Pi: niy"
--[(BTrue, FBTrue, expr, Double pi, AE $ Int 0, rule)] --- TODO: change (0,0) with the symbolic error -- Div (ulp Pi) (Int 2),
--  where
--    (expr, ulp, rule) = case fp of
--                        FPSingle -> (RtoS $ Pi, SUlp, SPiR)
--                        FPDouble -> (RtoD $ Pi, DUlp, DPiR)

aexprSem (FVar x) _ env fp =
  case (lookup x env) of
    Just a -> a -- variable coming from a Let statment
    Nothing -> [(BTrue, FBTrue, FVar x, Var ("r_" ++ x), AE $ Var ("e_" ++ x), rule x)]  -- parameters
  where
    rule = case fp of
                    FPSingle -> SVarR
                    FPDouble -> DVarR          

aexprSem (FEFun f actArgs) i env fp =
  case (lookup f i) of
    Just (_, formArgs, funSem) -> semEFun f formArgs actArgs (combos argSem) funSem (length funSem) fp 
    -- 
    Nothing -> error ("Function " ++ f ++ " not found")
  where
    argSem = map (\arg -> aexprSem arg i env fp) actArgs

aexprSem (FAdd a1 a2) i env fp = lub semAdd [(aexprSem a1 i env fp),(aexprSem a2 i env fp)] fp

aexprSem (FSub a1 a2) i env fp = lub semSub [(aexprSem a1 i env fp),(aexprSem a2 i env fp)] fp
--                              ++ binLub semPrecSub a1 a2 (aexprSem a1 i env spec fun fp) (aexprSem a2 i env spec fun fp) fp

aexprSem (FMul an@(FInt n) a)         i env fp | (isPow2 (fromInteger n))  = (lub semMulPow2 [(aexprSem an i env fp),(aexprSem a i env fp)] fp)++(lub semMulPow2over [(aexprSem an i env fp),(aexprSem a i env fp)] fp)
aexprSem (FMul an@(FDouble n) a)      i env fp | (isPow2 (fromRational n)) = (lub semMulPow2 [(aexprSem an i env fp),(aexprSem a i env fp)] fp)++(lub semMulPow2over [(aexprSem an i env fp),(aexprSem a i env fp)] fp)
aexprSem (FMul a an@(FInt n))         i env fp | (isPow2 (fromInteger n))  = (lub semMulPow2 [(aexprSem an i env fp),(aexprSem a i env fp)] fp)++(lub semMulPow2over [(aexprSem an i env fp),(aexprSem a i env fp)] fp)
aexprSem (FMul a an@(FDouble n))      i env fp | (isPow2 (fromRational n)) = (lub semMulPow2 [(aexprSem an i env fp),(aexprSem a i env fp)] fp)++(lub semMulPow2over [(aexprSem an i env fp),(aexprSem a i env fp)] fp)
aexprSem (FMul an@(RtoD(Int n)) a)    i env fp | (isPow2 (fromInteger n))  = (lub semMulPow2 [(aexprSem an i env fp),(aexprSem a i env fp)] fp)++(lub semMulPow2over [(aexprSem an i env fp),(aexprSem a i env fp)] fp)
aexprSem (FMul an@(RtoD(Double n)) a) i env fp | (isPow2 (fromRational n)) = (lub semMulPow2 [(aexprSem an i env fp),(aexprSem a i env fp)] fp)++(lub semMulPow2over [(aexprSem an i env fp),(aexprSem a i env fp)] fp)
aexprSem (FMul a an@(RtoD(Int n)))    i env fp | (isPow2 (fromInteger n))  = (lub semMulPow2 [(aexprSem an i env fp),(aexprSem a i env fp)] fp)++(lub semMulPow2over [(aexprSem an i env fp),(aexprSem a i env fp)] fp)
aexprSem (FMul a an@(RtoD(Double n))) i env fp | (isPow2 (fromRational n)) = (lub semMulPow2 [(aexprSem an i env fp),(aexprSem a i env fp)] fp)++(lub semMulPow2over [(aexprSem an i env fp),(aexprSem a i env fp)] fp)
aexprSem (FMul an@(RtoS(Int n)) a)    i env fp | (isPow2 (fromInteger n))  = (lub semMulPow2 [(aexprSem an i env fp),(aexprSem a i env fp)] fp)++(lub semMulPow2over [(aexprSem an i env fp),(aexprSem a i env fp)] fp)
aexprSem (FMul an@(RtoS(Double n)) a) i env fp | (isPow2 (fromRational n)) = (lub semMulPow2 [(aexprSem an i env fp),(aexprSem a i env fp)] fp)++(lub semMulPow2over [(aexprSem an i env fp),(aexprSem a i env fp)] fp)
aexprSem (FMul a an@(RtoS(Int n)))    i env fp | (isPow2 (fromInteger n))  = (lub semMulPow2 [(aexprSem an i env fp),(aexprSem a i env fp)] fp)++(lub semMulPow2over [(aexprSem an i env fp),(aexprSem a i env fp)] fp)
aexprSem (FMul a an@(RtoS(Double n))) i env fp | (isPow2 (fromRational n)) = (lub semMulPow2 [(aexprSem an i env fp),(aexprSem a i env fp)] fp)++(lub semMulPow2over [(aexprSem an i env fp),(aexprSem a i env fp)] fp)

aexprSem (FMul a1 a2) i env fp = lub semMul [(aexprSem a1 i env fp),(aexprSem a2 i env fp)] fp

aexprSem (FDiv a1 a2) i env fp = lub semDiv [(aexprSem a1 i env fp),(aexprSem a2 i env fp)] fp

aexprSem (FMod a1 a2) i env fp = aexprSem (FSub a1 (FMul a2 (FFloor (FDiv a1 a2)))) i env fp

aexprSem (FPow a (FInt (-1))) i env fp     = lub semDiv [(aexprSem (FInt 1) i env fp),(aexprSem a i env fp)]  fp
aexprSem (FPow a (FNeg (FInt 1))) i env fp = lub semDiv [(aexprSem (FInt 1) i env fp),(aexprSem a i env fp)]  fp
aexprSem (FPow a (FInt 2)) i env fp        = lub semMul [(aexprSem a i env fp),(aexprSem a i env fp)] fp
aexprSem (FPow _ _) _ _ _                  = error "aexprSem: case Pow not implemented yet"

aexprSem (FNeg a)    i env fp = lub semNeg    [aexprSem a i env fp] fp
aexprSem (FFloor a)  i env fp = (lub semFloor  [aexprSem a i env fp] fp) ++ (lub semFloor0  [aexprSem a i env fp] fp)
aexprSem (FSqrt a)   i env fp = lub semSqrt   [aexprSem a i env fp] fp
aexprSem (FAbs a)    i env fp = lub semAbs    [aexprSem a i env fp] fp
aexprSem (FSin a)    i env fp = lub semSin    [aexprSem a i env fp] fp
aexprSem (FCos a)    i env fp = lub semCos    [aexprSem a i env fp] fp
aexprSem (FTan a)    i env fp = lub semTan    [aexprSem a i env fp] fp
aexprSem (FAsin a)   i env fp = lub semAsin   [aexprSem a i env fp] fp
aexprSem (FAcos a)   i env fp = lub semAcos   [aexprSem a i env fp] fp
aexprSem (FAtan a)   i env fp = (lub semAtan  [aexprSem a i env fp] fp) ++ (lub semAtanT  [aexprSem a i env fp] fp)

aexprSem f i env fp = error ("aexprSem "++ (render $ prFAExpr f fp)++": not implemented yet")

semEFun :: String -> [VarId] -> [FAExpr] -> [CebS] -> CebS -> Int -> FPrec -> CebS
semEFun _ _ _ _ [] _ _ = []
semEFun fun fa aa aSem funSem@(c:cs) n fp = (map (aux n c fa aa) aSem)++(semEFun fun fa aa aSem cs n fp)
  where
    aux n (cond, guard, fexpr, vals, err, _) fa aa arg =
      (simplBExprFix  $ And  (argsBindBExpr fa arg cond)  (condArgs arg),
       simplFBExprFix $ FAnd (argsBindFBExpr fa aa guard) (guardArgs arg),
       argsBindFAExpr fa aa fexpr,
       argsSemBindAExpr fa arg vals,
       argsSemBindEExpr fa arg err,
       rule fun n (errArgs arg) (valArgs arg) (exprArgs arg) (pvsArgs arg)) 
    condArgs arg  = foldl makeAnd BTrue (map condR arg)
    guardArgs arg = foldl makeFAnd FBTrue (map condFP arg)
    valArgs arg  = (map exprR arg)
    errArgs arg  = (map exprErr arg)
    exprArgs arg = (map exprFP arg)
    pvsArgs arg  = (map pvsTree arg)
    rule = case fp of
             FPSingle -> SFunR
             FPDouble -> DFunR
    makeAnd a b = And a b
    makeFAnd a b = FAnd a b

semAdd :: CebS -> FPrec -> Ceb
semAdd [(c1,g1,f1,r1,e1,t1),(c2,g2,f2,r2,e2,t2)] fp =
   (simplBExprFix $ And c1 c2,
    simplFBExprFix $ FAnd g1 g2,
    FAdd f1 f2,
    Add r1 r2,
    ErrAdd r1 e1 r2 e2,
    rule e1 e2 r1 r2 f1 f2 t1 t2)
  where
    (ulp, rule) = case fp of
                    FPSingle -> (SUlp, SAddR)
                    FPDouble -> (DUlp, DAddR)  

semSub :: CebS -> FPrec -> Ceb
semSub [(c1,g1,f1,r1,e1,t1),(c2,g2,f2,r2,e2,t2)] fp =
   (simplBExprFix $ And c1 c2,
    simplFBExprFix $ FAnd g1 g2,
    FSub f1 f2,
    Sub r1 r2,
    ErrSub r1 e1 r2 e2,
    rule e1 e2 r1 r2 f1 f2 t1 t2)
  where
    (ulp, rule) = case fp of
                    FPSingle -> (SUlp, SSubR)
                    FPDouble -> (DUlp, DSubR) 

semMul :: CebS -> FPrec -> Ceb 
semMul [(c1,g1,f1,r1,e1,t1),(c2,g2,f2,r2,e2,t2)] fp =
   (simplBExprFix $ And c1 c2,
    simplFBExprFix $ FAnd g1 g2,
    FMul f1 f2,
    Mul r1 r2,
    ErrMul r1 e1 r2 e2,
    rule e1 e2 r1 r2 f1 f2 t1 t2)
  where
    rule = case fp of
            FPSingle -> SMulR
            FPDouble -> DMulR

semMulPow2 :: CebS -> FPrec -> Ceb
semMulPow2 [(BTrue,FBTrue,f1,Int m,_,_),(c2,g2,f2,r2,e2,t2)] fp =
  (simplBExprFix $ And c2 (Lt (Int n) (Sub FPrec (FExp f2))),
   g2,
   FMul f1 f2,
   Mul (Int m) r2,
   ErrMulPow2 n e2,
   rule e2 r2 f2 t2 n)
  where
    n = round $ logBase 2 (fromIntegral m)
    rule = case fp of
             FPSingle -> SMulPow2R
             FPDouble -> DMulPow2R 
semMulPow2 [(BTrue,FBTrue,f1,Double m,_,_),(c2,g2,f2,r2,e2,t2)] fp =
  (simplBExprFix $ And c2 (Lt (Int n) (Sub FPrec (FExp f2))),
   g2,
   FMul f1 f2,
   Mul (Double m) r2,
   ErrMulPow2 n e2,
   rule e2 r2 f2 t2 n)
  where
    n = round $ logBase 2 (realToFrac m)
    rule = case fp of
             FPSingle -> SMulPow2R
             FPDouble -> DMulPow2R 
semMulPow2 _ _ = error "semMulPow2: something went wrong!"

semMulPow2over :: CebS -> FPrec -> Ceb
semMulPow2over [(BTrue,FBTrue,f1,Int m,e1,t1),(c2,g2,f2,r2,e2,t2)] fp =
  (simplBExprFix $ And c2 (GtE (Int n) (Sub FPrec (FExp f2))),
    g2,
    FMul f1 f2,
    Mul (Int m) r2,
    ErrMul (Int m) e1 r2 e2,
    rule e1 e2 (Int m) r2 f1 f2 t1 t2)
  where
    n = round $ logBase 2 (fromIntegral m)
    rule = case fp of
             FPSingle -> SMulR
             FPDouble -> DMulR
semMulPow2over [(BTrue,FBTrue,f1,Double m,e1,t1),(c2,g2,f2,r2,e2,t2)] fp =
  (simplBExprFix $ And c2 (GtE (Int n) (Sub FPrec (FExp f2))),
    g2,
    FMul f1 f2,
    Mul (Double m) r2,
    ErrMul (Double m) e1 r2 e2,
    rule e1 e2 (Double m) r2 f1 f2 t1 t2)
  where
    n = round $ logBase 2 (realToFrac m)
    rule = case fp of
             FPSingle -> SMulR
             FPDouble -> DMulR

semDiv :: CebS -> FPrec -> Ceb 
semDiv [(c1,g1,f1,r1,e1,t1),(c2,g2,f2,r2,e2,t2)] fp =
   (simplBExprFix $ And (And (And (And c1 c2) (Neq r2 (Int 0))) (Neq (f2r f2) (Int 0))) (Or (Lt (Add r2 (EE e2)) (Int 0)) (Gt (Sub r2 (EE e2)) (Int 0))) ,
    simplFBExprFix $ FAnd g1 g2 ,
    FDiv f1 f2,
    Div r1 r2,
    ErrDiv r1 e1 r2 e2,          
    rule e1 e2 r1 r2 f1 f2 t1 t2)
  where
    (rule, f2r) = case fp of
            FPSingle -> (SDivR,StoR)
            FPDouble -> (DDivR,DtoR)

semNeg :: CebS -> FPrec -> Ceb
semNeg [(c,g,f,r,e,t)] fp = (c, g, FNeg f, Neg r, ErrNeg r e, rule e r f t)
  where
    rule = case fp of
             FPSingle -> SNegR
             FPDouble -> DNegR

semAbs :: CebS -> FPrec -> Ceb
semAbs [(c,g,f,r,e,t)] fp = (c, g, FAbs f, Abs r, ErrAbs r e, rule e r f t)
  where
    rule = case fp of
             FPSingle -> SAbsR
             FPDouble -> DAbsR             

semFloor :: CebS -> FPrec -> Ceb
semFloor [(c,g,f,r,e,t)] fp =
 (And c cfloor,
  g,
  FFloor f,
  Floor r,
  ErrFloor r e,
  rule e r f t)
  where
    cfloor = Or (Neq (Floor r) (Floor (Sub r (EE e)))) (Neq (Floor r) (Floor (Add r (EE e))))
    (ulp, rule) = case fp of
                    FPSingle -> (SUlp, SFloorR)
                    FPDouble -> (DUlp, DFloorR)

semFloor0 :: CebS -> FPrec -> Ceb
semFloor0 [(c,g,f,r,e,t)] fp =
  (And c cfloor,
    g,
    FFloor f,
    Floor r,
    ErrFloor0 r e,
    rule e r f t)
  where
    cfloor = And (Eq  (Floor r) (Floor (Sub r (EE e)))) (Eq  (Floor r) (Floor (Add r (EE e))))
    (ulp, rule) = case fp of
                    FPSingle -> (SUlp, SFloor0R)
                    FPDouble -> (DUlp, DFloor0R)

semSqrt :: CebS -> FPrec -> Ceb
semSqrt [(c,g,f,r,e,t)] fp =
   (simplBExprFix $ And c (GtE (Sub r (EE e)) (Int 0)) ,
    g,
    FSqrt f,
    Sqrt r,
    ErrSqrt r e,
    rule e r f t) 
  where
    (ulp, rule) = case fp of
                    FPSingle -> (SUlp, SSqrtR)
                    FPDouble -> (DUlp, DSqrtR)

semSin :: CebS -> FPrec -> Ceb 
semSin [(c,g,f,r,e,t)] fp = 
   (c,
    g,
    FSin f,
    Sin r,
    ErrSin r e,
    rule e r f t) 
  where
    (ulp, rule) = case fp of
                    FPSingle -> (SUlp, SSinR)
                    FPDouble -> (DUlp, DSinR)

semCos :: CebS -> FPrec -> Ceb 
semCos [(c,g,f,r,e,t)] fp = 
   (c,
    g,
    FCos f,
    Cos r,
    ErrCos r e,
    rule e r f t) 
  where
    (ulp, rule) = case fp of
                    FPSingle -> (SUlp, SCosR)
                    FPDouble -> (DUlp, DCosR)

semAtan :: CebS -> FPrec -> Ceb 
semAtan [(c,g,f,r,e,t)] fp = 
   (And c catan,
    g,
    FAtan f,
    ATan r,
    ErrAtan r e,
    rule e r f t) 
  where
    catan = GtE (EE e) (Abs r)
    (ulp, rule) = case fp of
                    FPSingle -> (SUlp, SAtanR)
                    FPDouble -> (DUlp, DAtanR)

semAtanT :: CebS -> FPrec -> Ceb 
semAtanT [(c,g,f,r,e,t)] fp = 
   (And c catan,
    g,
    FAtan f,
    ATan r,
    ErrAtanT r e,
    rule e r f t) 
  where
    catan = Lt (EE e) (Abs r)
    (ulp, rule) = case fp of
                    FPSingle -> (SUlp, SAtanTR)
                    FPDouble -> (DUlp, DAtanTR)                    

semTan :: CebS -> FPrec -> Ceb 
semTan [(c,g,f,r,e,t)] fp = error "semTan: niy"

semAsin :: CebS -> FPrec -> Ceb 
semAsin [(c,g,f,r,e,t)] fp = error "semAsin: niy"

semAcos :: CebS -> FPrec -> Ceb 
semAcos [(c,g,f,r,e,t)] fp =error "semAcos: niy"     



-----------------------
-- Arguments binding --
-----------------------

argsBindBExpr :: [VarId] -> CebS -> BExpr -> BExpr
argsBindBExpr fa aa (And e1 e2) = And (argsBindBExpr fa aa e1) (argsBindBExpr fa aa e2)
argsBindBExpr fa aa (Or e1 e2)  = Or  (argsBindBExpr fa aa e1) (argsBindBExpr fa aa e2)
argsBindBExpr fa aa (Not e)     = Not (argsBindBExpr fa aa e)
argsBindBExpr fa aa (Eq a1 a2)  = Eq  (argsSemBindAExpr fa aa a1) (argsSemBindAExpr fa aa a2)
argsBindBExpr fa aa (Neq a1 a2) = Neq (argsSemBindAExpr fa aa a1) (argsSemBindAExpr fa aa a2)
argsBindBExpr fa aa (Lt a1 a2)  = Lt  (argsSemBindAExpr fa aa a1) (argsSemBindAExpr fa aa a2)
argsBindBExpr fa aa (LtE a1 a2) = LtE (argsSemBindAExpr fa aa a1) (argsSemBindAExpr fa aa a2)
argsBindBExpr fa aa (Gt a1 a2)  = Gt  (argsSemBindAExpr fa aa a1) (argsSemBindAExpr fa aa a2)
argsBindBExpr fa aa (GtE a1 a2) = GtE (argsSemBindAExpr fa aa a1) (argsSemBindAExpr fa aa a2)
argsBindBExpr fa aa BTrue  = BTrue
argsBindBExpr fa aa BFalse = BFalse

argsBindFBExpr :: [VarId] -> [FAExpr] -> FBExpr -> FBExpr
argsBindFBExpr fa aa (FAnd e1 e2) = FAnd (argsBindFBExpr fa aa e1) (argsBindFBExpr fa aa e2)
argsBindFBExpr fa aa (FOr e1 e2)  = FOr  (argsBindFBExpr fa aa e1) (argsBindFBExpr fa aa e2)
argsBindFBExpr fa aa (FNot e)     = FNot (argsBindFBExpr fa aa e)
argsBindFBExpr fa aa (FEq a1 a2)  = FEq  (argsBindFAExpr fa aa a1) (argsBindFAExpr fa aa a2)
argsBindFBExpr fa aa (FNeq a1 a2) = FNeq (argsBindFAExpr fa aa a1) (argsBindFAExpr fa aa a2)
argsBindFBExpr fa aa (FLt a1 a2)  = FLt  (argsBindFAExpr fa aa a1) (argsBindFAExpr fa aa a2)
argsBindFBExpr fa aa (FLtE a1 a2) = FLtE (argsBindFAExpr fa aa a1) (argsBindFAExpr fa aa a2)
argsBindFBExpr fa aa (FGt a1 a2)  = FGt  (argsBindFAExpr fa aa a1) (argsBindFAExpr fa aa a2)
argsBindFBExpr fa aa (FGtE a1 a2) = FGtE (argsBindFAExpr fa aa a1) (argsBindFAExpr fa aa a2)
argsBindFBExpr fa aa FBTrue = FBTrue
argsBindFBExpr fa aa FBFalse = FBFalse

argsBindAExpr :: [VarId] -> [FAExpr] -> FPrec -> AExpr  -> AExpr
argsBindAExpr fa aa fp (Var x)       = argsBindVar fa aa fp x
argsBindAExpr fa aa fp (Add a1 a2)   = Add    (argsBindAExpr fa aa fp a1) (argsBindAExpr fa aa fp a2)
argsBindAExpr fa aa fp (Sub a1 a2)   = Sub    (argsBindAExpr fa aa fp a1) (argsBindAExpr fa aa fp a2)
argsBindAExpr fa aa fp (Mul a1 a2)   = Mul    (argsBindAExpr fa aa fp a1) (argsBindAExpr fa aa fp a2)
argsBindAExpr fa aa fp (Div a1 a2)   = Div    (argsBindAExpr fa aa fp a1) (argsBindAExpr fa aa fp a2)
argsBindAExpr fa aa fp (Pow a1 a2)   = Pow    (argsBindAExpr fa aa fp a1) (argsBindAExpr fa aa fp a2)
argsBindAExpr fa aa fp (Mod a1 a2)   = Mod    (argsBindAExpr fa aa fp a1) (argsBindAExpr fa aa fp a2)
argsBindAExpr fa aa fp (Neg a)       = Neg    (argsBindAExpr fa aa fp a)
argsBindAExpr fa aa fp (Floor a)     = Floor  (argsBindAExpr fa aa fp a)
argsBindAExpr fa aa fp (Sqrt a)      = Sqrt   (argsBindAExpr fa aa fp a)
argsBindAExpr fa aa fp (Abs a)       = Abs    (argsBindAExpr fa aa fp a)
argsBindAExpr fa aa fp (Sin a)       = Sin    (argsBindAExpr fa aa fp a)
argsBindAExpr fa aa fp (Cos a)       = Cos    (argsBindAExpr fa aa fp a)
argsBindAExpr fa aa fp (Tan a)       = Tan    (argsBindAExpr fa aa fp a)
argsBindAExpr fa aa fp (ACos a)      = ACos   (argsBindAExpr fa aa fp a)
argsBindAExpr fa aa fp (ASin a)      = ASin   (argsBindAExpr fa aa fp a)
argsBindAExpr fa aa fp (ATan a)      = ATan   (argsBindAExpr fa aa fp a)
argsBindAExpr fa aa fp (SUlp a)      = SUlp   (argsBindAExpr fa aa fp a)
argsBindAExpr fa aa fp (DUlp a)      = DUlp   (argsBindAExpr fa aa fp a)
argsBindAExpr fa aa _ (StoR a)       = StoR   (argsBindFAExpr fa aa a)
argsBindAExpr fa aa _ (DtoR a)       = DtoR   (argsBindFAExpr fa aa a)
argsBindAExpr _ _ _ i@(Int _)        = i
argsBindAExpr _ _ _ d@(Double _)     = d
argsBindAExpr _ _ _ Pi               = Pi
argsBindAExpr fa aa fp (EFun f args) = EFun f (map (argsBindAExpr fa aa fp) args)
argsBindAExpr fa aa fp (EE e)        = EE $ argsBindEExpr fa aa fp e
argsBindAExpr fa aa fp FPrec         = FPrec
argsBindAExpr fa aa fp (FExp fae)    = FExp (argsBindFAExpr fa aa fae)

argsSemBindAExpr :: [VarId] -> CebS -> AExpr  -> AExpr
argsSemBindAExpr fa aa (Var x)       = argsBindSemVar fa aa x
argsSemBindAExpr fa aa (Add a1 a2)   = Add    (argsSemBindAExpr fa aa a1) (argsSemBindAExpr fa aa a2)
argsSemBindAExpr fa aa (Sub a1 a2)   = Sub    (argsSemBindAExpr fa aa a1) (argsSemBindAExpr fa aa a2)
argsSemBindAExpr fa aa (Mul a1 a2)   = Mul    (argsSemBindAExpr fa aa a1) (argsSemBindAExpr fa aa a2)
argsSemBindAExpr fa aa (Div a1 a2)   = Div    (argsSemBindAExpr fa aa a1) (argsSemBindAExpr fa aa a2)
argsSemBindAExpr fa aa (Pow a1 a2)   = Pow    (argsSemBindAExpr fa aa a1) (argsSemBindAExpr fa aa a2)
argsSemBindAExpr fa aa (Mod a1 a2)   = Mod    (argsSemBindAExpr fa aa a1) (argsSemBindAExpr fa aa a2)
argsSemBindAExpr fa aa (Neg a)       = Neg    (argsSemBindAExpr fa aa a)
argsSemBindAExpr fa aa (Floor a)     = Floor  (argsSemBindAExpr fa aa a)
argsSemBindAExpr fa aa (Sqrt a)      = Sqrt   (argsSemBindAExpr fa aa a)
argsSemBindAExpr fa aa (Abs a)       = Abs    (argsSemBindAExpr fa aa a)
argsSemBindAExpr fa aa (Sin a)       = Sin    (argsSemBindAExpr fa aa a)
argsSemBindAExpr fa aa (Cos a)       = Cos    (argsSemBindAExpr fa aa a)
argsSemBindAExpr fa aa (Tan a)       = Tan    (argsSemBindAExpr fa aa a)
argsSemBindAExpr fa aa (ACos a)      = ACos   (argsSemBindAExpr fa aa a)
argsSemBindAExpr fa aa (ASin a)      = ASin   (argsSemBindAExpr fa aa a)
argsSemBindAExpr fa aa (ATan a)      = ATan   (argsSemBindAExpr fa aa a)
argsSemBindAExpr fa aa (SUlp a)      = SUlp   (argsSemBindAExpr fa aa a)
argsSemBindAExpr fa aa (DUlp a)      = DUlp   (argsSemBindAExpr fa aa a)
argsSemBindAExpr fa aa (StoR a)      = StoR   (argsSemBindFAExpr fa aa a)
argsSemBindAExpr fa aa (DtoR a)      = DtoR   (argsSemBindFAExpr fa aa a)
argsSemBindAExpr _ _ i@(Int _)       = i
argsSemBindAExpr _ _ d@(Double _)    = d
argsSemBindAExpr _ _ Pi              = Pi
argsSemBindAExpr fa aa (EFun f args) = EFun f (map (argsSemBindAExpr fa aa) args)
argsSemBindAExpr fa aa (EE ee)       = EE (argsSemBindEExpr fa aa ee)
argsSemBindAExpr fa aa FPrec         = FPrec
argsSemBindAExpr fa aa (FExp fae)    = FExp (argsSemBindFAExpr fa aa fae)

argsSemBindEExpr :: [VarId] -> CebS -> EExpr  -> EExpr
argsSemBindEExpr fa aa (AE ae) = AE $ argsSemBindAExpr fa aa ae
argsSemBindEExpr fa aa (ErrAdd r1 e1 r2 e2) = ErrAdd    (argsSemBindAExpr fa aa r1) (argsSemBindEExpr fa aa e1)
                                                        (argsSemBindAExpr fa aa r2) (argsSemBindEExpr fa aa e2)
argsSemBindEExpr fa aa (ErrSub r1 e1 r2 e2) = ErrSub    (argsSemBindAExpr fa aa r1) (argsSemBindEExpr fa aa e1)
                                                        (argsSemBindAExpr fa aa r2) (argsSemBindEExpr fa aa e2)
argsSemBindEExpr fa aa (ErrMul r1 e1 r2 e2) = ErrMul    (argsSemBindAExpr fa aa r1) (argsSemBindEExpr fa aa e1)
                                                        (argsSemBindAExpr fa aa r2) (argsSemBindEExpr fa aa e2)
argsSemBindEExpr fa aa (ErrDiv r1 e1 r2 e2) = ErrDiv    (argsSemBindAExpr fa aa r1) (argsSemBindEExpr fa aa e1)
                                                        (argsSemBindAExpr fa aa r2) (argsSemBindEExpr fa aa e2)
argsSemBindEExpr fa aa (ErrFloor  r e)      = ErrFloor  (argsSemBindAExpr fa aa r)  (argsSemBindEExpr fa aa e)
argsSemBindEExpr fa aa (ErrFloor0  r e)     = ErrFloor0  (argsSemBindAExpr fa aa r)  (argsSemBindEExpr fa aa e)
argsSemBindEExpr fa aa (ErrSqrt   r e)      = ErrSqrt   (argsSemBindAExpr fa aa r)  (argsSemBindEExpr fa aa e)
argsSemBindEExpr fa aa (ErrNeg  r e)        = ErrNeg    (argsSemBindAExpr fa aa r)  (argsSemBindEExpr fa aa e)
argsSemBindEExpr fa aa (ErrAbs   r e)       = ErrAbs    (argsSemBindAExpr fa aa r)  (argsSemBindEExpr fa aa e)
argsSemBindEExpr fa aa (ErrSin    r e)      = ErrSin    (argsSemBindAExpr fa aa r)  (argsSemBindEExpr fa aa e)
argsSemBindEExpr fa aa (ErrCos    r e)      = ErrCos    (argsSemBindAExpr fa aa r)  (argsSemBindEExpr fa aa e)
argsSemBindEExpr fa aa (ErrTan    r e)      = ErrTan    (argsSemBindAExpr fa aa r)  (argsSemBindEExpr fa aa e)
argsSemBindEExpr fa aa (ErrAsin r e)        = ErrAsin (argsSemBindAExpr fa aa r)  (argsSemBindEExpr fa aa e)
argsSemBindEExpr fa aa (ErrAcos r e)        = ErrAcos (argsSemBindAExpr fa aa r)  (argsSemBindEExpr fa aa e)
argsSemBindEExpr fa aa (ErrAtan r e)        = ErrAtan (argsSemBindAExpr fa aa r)  (argsSemBindEExpr fa aa e)


argsBindEExpr :: [VarId] -> [FAExpr] -> FPrec -> EExpr -> EExpr
argsBindEExpr fa aa fp (AE ae) = AE $ argsBindAExpr fa aa fp ae
argsBindEExpr fa aa fp (ErrAdd r1 e1 r2 e2) = ErrAdd    (argsBindAExpr fa aa fp r1) (argsBindEExpr fa aa fp e1)
                                                        (argsBindAExpr fa aa fp r2) (argsBindEExpr fa aa fp e2)
argsBindEExpr fa aa fp (ErrSub r1 e1 r2 e2) = ErrSub    (argsBindAExpr fa aa fp r1) (argsBindEExpr fa aa fp e1)
                                                        (argsBindAExpr fa aa fp r2) (argsBindEExpr fa aa fp e2)
argsBindEExpr fa aa fp (ErrMul r1 e1 r2 e2) = ErrMul    (argsBindAExpr fa aa fp r1) (argsBindEExpr fa aa fp e1)
                                                        (argsBindAExpr fa aa fp r2) (argsBindEExpr fa aa fp e2)
argsBindEExpr fa aa fp (ErrDiv r1 e1 r2 e2) = ErrDiv    (argsBindAExpr fa aa fp r1) (argsBindEExpr fa aa fp e1)
                                                        (argsBindAExpr fa aa fp r2) (argsBindEExpr fa aa fp e2)
argsBindEExpr fa aa fp (ErrFloor  r e)      = ErrFloor  (argsBindAExpr fa aa fp r)  (argsBindEExpr fa aa fp e)
argsBindEExpr fa aa fp (ErrFloor0  r e)     = ErrFloor0  (argsBindAExpr fa aa fp r)  (argsBindEExpr fa aa fp e)
argsBindEExpr fa aa fp (ErrSqrt   r e)      = ErrSqrt   (argsBindAExpr fa aa fp r)  (argsBindEExpr fa aa fp e)
argsBindEExpr fa aa fp (ErrAbs  r e)        = ErrAbs    (argsBindAExpr fa aa fp r)  (argsBindEExpr fa aa fp e)
argsBindEExpr fa aa fp (ErrNeg   r e)       = ErrNeg    (argsBindAExpr fa aa fp r)  (argsBindEExpr fa aa fp e)
argsBindEExpr fa aa fp (ErrSin    r e)      = ErrSin    (argsBindAExpr fa aa fp r)  (argsBindEExpr fa aa fp e)
argsBindEExpr fa aa fp (ErrCos    r e)      = ErrCos    (argsBindAExpr fa aa fp r)  (argsBindEExpr fa aa fp e)
argsBindEExpr fa aa fp (ErrTan    r e)      = ErrTan    (argsBindAExpr fa aa fp r)  (argsBindEExpr fa aa fp e)
argsBindEExpr fa aa fp (ErrAsin r e)      = ErrAsin (argsBindAExpr fa aa fp r)  (argsBindEExpr fa aa fp e)
argsBindEExpr fa aa fp (ErrAcos r e)      = ErrAcos (argsBindAExpr fa aa fp r)  (argsBindEExpr fa aa fp e)


argsBindFAExpr :: [VarId] -> [FAExpr] -> FAExpr -> FAExpr
argsBindFAExpr fa aa (FVar x)       = argsBindFVar fa aa x    
argsBindFAExpr fa aa (FAdd a1 a2)   = FAdd    (argsBindFAExpr fa aa a1) (argsBindFAExpr fa aa a2)
argsBindFAExpr fa aa (FSub a1 a2)   = FSub    (argsBindFAExpr fa aa a1) (argsBindFAExpr fa aa a2)
argsBindFAExpr fa aa (FMul a1 a2)   = FMul    (argsBindFAExpr fa aa a1) (argsBindFAExpr fa aa a2)
argsBindFAExpr fa aa (FDiv a1 a2)   = FDiv    (argsBindFAExpr fa aa a1) (argsBindFAExpr fa aa a2)
argsBindFAExpr fa aa (FPow a1 a2)   = FPow    (argsBindFAExpr fa aa a1) (argsBindFAExpr fa aa a2)
argsBindFAExpr fa aa (FMod a1 a2)   = FMod    (argsBindFAExpr fa aa a1) (argsBindFAExpr fa aa a2)
argsBindFAExpr fa aa (FNeg a)       = FNeg    (argsBindFAExpr fa aa a)
argsBindFAExpr fa aa (FFloor a)     = FFloor  (argsBindFAExpr fa aa a) 
argsBindFAExpr fa aa (FSqrt a)      = FSqrt   (argsBindFAExpr fa aa a)
argsBindFAExpr fa aa (FAbs a)       = FAbs    (argsBindFAExpr fa aa a)
argsBindFAExpr fa aa (FSin a)       = FSin    (argsBindFAExpr fa aa a)
argsBindFAExpr fa aa (FCos a)       = FCos    (argsBindFAExpr fa aa a)
argsBindFAExpr fa aa (FTan a)       = FTan    (argsBindFAExpr fa aa a)
argsBindFAExpr fa aa (FAsin a)      = FAsin (argsBindFAExpr fa aa a)
argsBindFAExpr fa aa (FAcos a)      = FAcos (argsBindFAExpr fa aa a)
argsBindFAExpr fa aa (FAtan a)      = FAtan (argsBindFAExpr fa aa a)
argsBindFAExpr _  _  i@(FInt _)     = i
argsBindFAExpr _  _  d@(FDouble _)  = d
argsBindFAExpr fa aa (FEFun f args) = FEFun f (map (argsBindFAExpr fa aa) args)
argsBindFAExpr _ _    FPi           = FPi
argsBindFAExpr fa aa (RtoS a)       = RtoS (argsBindAExpr fa aa FPSingle a)
argsBindFAExpr fa aa (RtoD a)       = RtoD (argsBindAExpr fa aa FPDouble a)

argsSemBindFAExpr :: [VarId] -> CebS -> FAExpr -> FAExpr
argsSemBindFAExpr fa aa (FVar x)       = argsBindSemFVar fa aa x    
argsSemBindFAExpr fa aa (FAdd a1 a2)   = FAdd    (argsSemBindFAExpr fa aa a1) (argsSemBindFAExpr fa aa a2)
argsSemBindFAExpr fa aa (FSub a1 a2)   = FSub    (argsSemBindFAExpr fa aa a1) (argsSemBindFAExpr fa aa a2)
argsSemBindFAExpr fa aa (FMul a1 a2)   = FMul    (argsSemBindFAExpr fa aa a1) (argsSemBindFAExpr fa aa a2)
argsSemBindFAExpr fa aa (FDiv a1 a2)   = FDiv    (argsSemBindFAExpr fa aa a1) (argsSemBindFAExpr fa aa a2)
argsSemBindFAExpr fa aa (FPow a1 a2)   = FPow    (argsSemBindFAExpr fa aa a1) (argsSemBindFAExpr fa aa a2)
argsSemBindFAExpr fa aa (FMod a1 a2)   = FMod    (argsSemBindFAExpr fa aa a1) (argsSemBindFAExpr fa aa a2)
argsSemBindFAExpr fa aa (FNeg a)       = FNeg    (argsSemBindFAExpr fa aa a)
argsSemBindFAExpr fa aa (FFloor a)     = FFloor  (argsSemBindFAExpr fa aa a) 
argsSemBindFAExpr fa aa (FSqrt a)      = FSqrt   (argsSemBindFAExpr fa aa a)
argsSemBindFAExpr fa aa (FAbs a)       = FAbs    (argsSemBindFAExpr fa aa a)
argsSemBindFAExpr fa aa (FSin a)       = FSin    (argsSemBindFAExpr fa aa a)
argsSemBindFAExpr fa aa (FCos a)       = FCos    (argsSemBindFAExpr fa aa a)
argsSemBindFAExpr fa aa (FTan a)       = FTan    (argsSemBindFAExpr fa aa a)
argsSemBindFAExpr fa aa (FAsin a)      = FAsin (argsSemBindFAExpr fa aa a)
argsSemBindFAExpr fa aa (FAcos a)      = FAcos (argsSemBindFAExpr fa aa a)
argsSemBindFAExpr fa aa (FAtan a)      = FAtan (argsSemBindFAExpr fa aa a)
argsSemBindFAExpr _  _  i@(FInt _)     = i
argsSemBindFAExpr _  _  d@(FDouble _)  = d
argsSemBindFAExpr fa aa (FEFun f args) = FEFun f (map (argsSemBindFAExpr fa aa) args)
argsSemBindFAExpr _ _    FPi           = FPi
argsSemBindFAExpr fa aa (RtoS a)       = RtoS (argsSemBindAExpr fa aa a)
argsSemBindFAExpr fa aa (RtoD a)       = RtoD (argsSemBindAExpr fa aa a)

argsBindFVar :: [VarId] -> [FAExpr] -> String -> FAExpr
argsBindFVar [] [] x = FVar x
argsBindFVar ((VarId y):ys) (a:as) x | x == y = a
                                     | otherwise = argsBindFVar ys as x
 
argsBindVar :: [VarId] -> [FAExpr] -> FPrec -> String -> AExpr
argsBindVar [] [] _ x = Var x
argsBindVar ((VarId y):ys) (a:as) fp x | x == ("r_"++y) = fae2real a
                                       | otherwise = argsBindVar ys as fp x                                   

argsBindSemFVar :: [VarId] -> CebS -> String -> FAExpr
argsBindSemFVar [] [] x = FVar x
argsBindSemFVar ((VarId y):ys) ((_,_,f,_,_,_):as) x | x == y = f
                                                    | otherwise = argsBindSemFVar ys as x                                         
                                                  
argsBindSemVar :: [VarId] -> CebS -> String -> AExpr
argsBindSemVar [] [] x = Var x
argsBindSemVar ((VarId y):ys) ((_,_,_,a,e,_):as) x | x == ("r_"++y) = a
                                                   | x == ("e_"++y) = EE e
                                                   | otherwise = argsBindSemVar ys as x 


argsBindStm :: [VarId] -> [FAExpr] -> Stm -> Stm
argsBindStm vs aes (Let x ae stm)     = Let x (argsBindFAExpr vs aes ae) (argsBindStm vs aes stm)
argsBindStm vs aes (Ite be stmT stmE) = Ite (argsBindFBExpr vs aes be) (argsBindStm vs aes stmT) (argsBindStm vs aes stmE)
argsBindStm vs aes (StmExpr ae)       = StmExpr (argsBindFAExpr vs aes ae) 


----------------------------------
-- instantiation of expressions --
----------------------------------

zeroErrBE  ::[Char] -> BExpr -> BExpr
zeroErrBE p (Or  b1 b2) = Or  (zeroErrBE p b1) (zeroErrBE p b2)
zeroErrBE p (And b1 b2) = And (zeroErrBE p b1) (zeroErrBE p b2)
zeroErrBE p (Not b)     = Not (zeroErrBE p b)
zeroErrBE p (Eq  a1 a2) = Eq  (zeroErrAE p a1) (zeroErrAE p a2)
zeroErrBE p (Neq a1 a2) = Neq (zeroErrAE p a1) (zeroErrAE p a2)
zeroErrBE p (Lt  a1 a2) = Lt  (zeroErrAE p a1) (zeroErrAE p a2)
zeroErrBE p (LtE a1 a2) = LtE (zeroErrAE p a1) (zeroErrAE p a2)
zeroErrBE p (Gt  a1 a2) = Gt  (zeroErrAE p a1) (zeroErrAE p a2)
zeroErrBE p (GtE a1 a2) = GtE (zeroErrAE p a1) (zeroErrAE p a2)
zeroErrBE _ BTrue  = BTrue
zeroErrBE _ BFalse = BFalse

zeroErrFBE  ::[Char] -> FBExpr -> FBExpr
zeroErrFBE p (FOr  b1 b2) = FOr  (zeroErrFBE p b1) (zeroErrFBE p b2)
zeroErrFBE p (FAnd b1 b2) = FAnd (zeroErrFBE p b1) (zeroErrFBE p b2)
zeroErrFBE p (FNot b)     = FNot (zeroErrFBE p b)
zeroErrFBE p (FEq  a1 a2) = FEq  (zeroErrFAE p a1) (zeroErrFAE p a2)
zeroErrFBE p (FNeq a1 a2) = FNeq (zeroErrFAE p a1) (zeroErrFAE p a2)
zeroErrFBE p (FLt  a1 a2) = FLt  (zeroErrFAE p a1) (zeroErrFAE p a2)
zeroErrFBE p (FLtE a1 a2) = FLtE (zeroErrFAE p a1) (zeroErrFAE p a2)
zeroErrFBE p (FGt  a1 a2) = FGt  (zeroErrFAE p a1) (zeroErrFAE p a2)
zeroErrFBE p (FGtE a1 a2) = FGtE (zeroErrFAE p a1) (zeroErrFAE p a2)
zeroErrFBE _ FBTrue  = FBTrue
zeroErrFBE _ FBFalse = FBFalse

zeroErrEE ::[Char] -> EExpr -> EExpr
zeroErrEE p (ErrAdd ae1 ee1 ae2 ee2) = ErrAdd    (zeroErrAE p ae1) (zeroErrEE p ee1) (zeroErrAE p ae2) (zeroErrEE p ee2)
zeroErrEE p (ErrSub ae1 ee1 ae2 ee2) = ErrSub    (zeroErrAE p ae1) (zeroErrEE p ee1) (zeroErrAE p ae2) (zeroErrEE p ee2)
zeroErrEE p (ErrMul ae1 ee1 ae2 ee2) = ErrMul    (zeroErrAE p ae1) (zeroErrEE p ee1) (zeroErrAE p ae2) (zeroErrEE p ee2)
zeroErrEE p (ErrDiv ae1 ee1 ae2 ee2) = ErrDiv    (zeroErrAE p ae1) (zeroErrEE p ee1) (zeroErrAE p ae2) (zeroErrEE p ee2)
zeroErrEE p (ErrFloor  ae ee)        = ErrFloor  (zeroErrAE p ae)  (zeroErrEE p ee)
zeroErrEE p (ErrFloor0 ae ee)        = ErrFloor0 (zeroErrAE p ae)  (zeroErrEE p ee)
zeroErrEE p (ErrSqrt   ae ee)        = ErrSqrt   (zeroErrAE p ae)  (zeroErrEE p ee)
zeroErrEE p (ErrSin    ae ee)        = ErrSin    (zeroErrAE p ae)  (zeroErrEE p ee)
zeroErrEE p (ErrCos    ae ee)        = ErrCos    (zeroErrAE p ae)  (zeroErrEE p ee)
zeroErrEE p (ErrTan    ae ee)        = ErrTan    (zeroErrAE p ae)  (zeroErrEE p ee)
zeroErrEE p (ErrAsin   ae ee)        = ErrAsin   (zeroErrAE p ae)  (zeroErrEE p ee)
zeroErrEE p (ErrAcos   ae ee)        = ErrAcos   (zeroErrAE p ae)  (zeroErrEE p ee)
zeroErrEE p (ErrAtan   ae ee)        = ErrAtan   (zeroErrAE p ae)  (zeroErrEE p ee)
zeroErrEE p (ErrAtanT   ae ee)       = ErrAtanT   (zeroErrAE p ae)  (zeroErrEE p ee)
zeroErrEE p (ErrNeg    ae ee)        = ErrNeg    (zeroErrAE p ae)  (zeroErrEE p ee)
zeroErrEE p (ErrAbs    ae ee)        = ErrAbs    (zeroErrAE p ae)  (zeroErrEE p ee)
zeroErrEE p (AE ae)                  = AE        (zeroErrAE p ae)
zeroErrEE p (HalfUlp ae)             = HalfUlp   (zeroErrAE p ae)
zeroErrEE p (ErrRat r)               = ErrRat r
zeroErrEE p (ErrMulPow2 n ee)        = ErrMulPow2 n (zeroErrEE p ee)

zeroErrAE ::[Char] -> AExpr -> AExpr
zeroErrAE _ (Int n)        = Int n
zeroErrAE _ (Double n)     = Double n
zeroErrAE p v@(Var x) | (take 2 x) == ("e_") = EE $ HalfUlp (Var $ "r_"++(drop 2 x)) -- Int 0
                      | otherwise = Var x 
zeroErrAE _ (Pi)           = Pi
zeroErrAE p (Add ae1 ae2)  = Add    (zeroErrAE p ae1) (zeroErrAE p ae2)
zeroErrAE p (Sub ae1 ae2)  = Sub    (zeroErrAE p ae1) (zeroErrAE p ae2)
zeroErrAE p (Mul ae1 ae2)  = Mul    (zeroErrAE p ae1) (zeroErrAE p ae2)
zeroErrAE p (Div ae1 ae2)  = Div    (zeroErrAE p ae1) (zeroErrAE p ae2)
zeroErrAE p (Pow ae1 ae2)  = Pow    (zeroErrAE p ae1) (zeroErrAE p ae2)
zeroErrAE p (Mod ae1 ae2)  = Mod    (zeroErrAE p ae1) (zeroErrAE p ae2)
zeroErrAE p (Neg   ae)     = Neg    (zeroErrAE p ae)
zeroErrAE p (Floor ae)     = Floor  (zeroErrAE p ae)
zeroErrAE p (Sqrt  ae)     = Sqrt   (zeroErrAE p ae)
zeroErrAE p (Abs   ae)     = Abs    (zeroErrAE p ae)
zeroErrAE p (Sin   ae)     = Sin    (zeroErrAE p ae)
zeroErrAE p (Cos   ae)     = Cos    (zeroErrAE p ae)
zeroErrAE p (Tan   ae)     = Tan    (zeroErrAE p ae)
zeroErrAE p (ASin ae)      = ASin   (zeroErrAE p ae)
zeroErrAE p (ACos ae)      = ACos   (zeroErrAE p ae)
zeroErrAE p (ATan ae)      = ATan   (zeroErrAE p ae)
zeroErrAE p (SUlp ae)      = SUlp   (zeroErrAE p ae)
zeroErrAE p (DUlp ae)      = DUlp   (zeroErrAE p ae)
zeroErrAE p (EE ee)        = EE     (zeroErrEE p ee)
zeroErrAE p (EFun fun aes) = EFun fun (map (zeroErrAE p) aes)
zeroErrAE p (StoR fae)     = StoR (zeroErrFAE p fae)
zeroErrAE p (DtoR fae)     = DtoR (zeroErrFAE p fae)
zeroErrAE p FPrec          = FPrec
zeroErrAE p (FExp fae)     = FExp (zeroErrFAE p fae)

zeroErrFAE ::[Char] -> FAExpr -> FAExpr
zeroErrFAE p (FInt n)     = FInt n
zeroErrFAE p (FDouble n)  = FDouble n
zeroErrFAE p (FVar x)     = FVar x
zeroErrFAE p FPi          = FPi
zeroErrFAE p (FAdd a1 a2) = FAdd   (zeroErrFAE p a1) (zeroErrFAE p a2) 
zeroErrFAE p (FSub a1 a2) = FSub   (zeroErrFAE p a1) (zeroErrFAE p a2) 
zeroErrFAE p (FMul a1 a2) = FMul   (zeroErrFAE p a1) (zeroErrFAE p a2) 
zeroErrFAE p (FDiv a1 a2) = FDiv   (zeroErrFAE p a1) (zeroErrFAE p a2) 
zeroErrFAE p (FPow a1 a2) = FPow   (zeroErrFAE p a1) (zeroErrFAE p a2) 
zeroErrFAE p (FMod a1 a2) = FMod   (zeroErrFAE p a1) (zeroErrFAE p a2) 
zeroErrFAE p (FNeg    a)  = FNeg   (zeroErrFAE p a)
zeroErrFAE p (FFloor  a)  = FFloor (zeroErrFAE p a)
zeroErrFAE p (FSqrt   a)  = FSqrt  (zeroErrFAE p a)
zeroErrFAE p (FAbs    a)  = FAbs   (zeroErrFAE p a)
zeroErrFAE p (FSin    a)  = FSin   (zeroErrFAE p a)
zeroErrFAE p (FCos    a)  = FCos   (zeroErrFAE p a)
zeroErrFAE p (FTan    a)  = FTan   (zeroErrFAE p a)
zeroErrFAE p (FAsin a)    = FAsin  (zeroErrFAE p a)
zeroErrFAE p (FAcos a)    = FAcos  (zeroErrFAE p a)
zeroErrFAE p (FAtan a)    = FAtan  (zeroErrFAE p a)
zeroErrFAE p (FEFun fun aes) = FEFun fun (map (zeroErrFAE p) aes)
zeroErrFAE p (RtoS a)     = RtoS (zeroErrAE p a)
zeroErrFAE p (RtoD a)     = RtoD (zeroErrAE p a)


