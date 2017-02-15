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

aexprSem (RtoS (Double n)) _ _ FPSingle = [(BTrue, FBTrue, RtoS (Double n), Double n, AE $ Int 0, SDoubleR)]
aexprSem (RtoS (Double n)) _ _ FPDouble = error "aexprSem: FP precision mismatch"

aexprSem (RtoD (Int n)) _ _ FPDouble = [(BTrue, FBTrue, RtoD (Int n), Int n, AE $ Int 0, DIntR)] 
aexprSem (RtoD (Int n)) _ _ FPSingle = error "aexprSem: FP precision mismatch"

aexprSem (RtoD (Double n)) _ _ FPDouble = [(BTrue, FBTrue, RtoD (Double n), Double n, AE $ Int 0, DDoubleR)]
aexprSem (RtoD (Double n)) _ _ FPSingle = error "aexprSem: FP precision mismatch"

aexprSem (RtoS (Neg (Int n))) _ _ FPSingle = [(BTrue, FBTrue, RtoS (Neg (Int n)), Neg (Int n), AE $ Int 0, SIntR)]
aexprSem (RtoS (Neg (Int n))) _ _ FPDouble = error "aexprSem: FP precision mismatch"

aexprSem (RtoS (Neg (Double n))) _ _ FPSingle = [(BTrue, FBTrue, RtoS (Neg (Double n)), Neg (Double n), AE $ Int 0, SDoubleR)]
aexprSem (RtoS (Neg (Double n))) _ _ FPDouble = error "aexprSem: FP precision mismatch"

aexprSem (RtoD (Neg (Int n))) _ _ FPDouble = [(BTrue, FBTrue, RtoD (Neg (Int n)), Neg (Int n), AE $ Int 0, DIntR)] 
aexprSem (RtoD (Neg (Int n))) _ _ FPSingle = error "aexprSem: FP precision mismatch"

aexprSem (RtoD (Neg (Double n))) _ _ FPDouble = [(BTrue, FBTrue, RtoD (Neg (Double n)), Neg (Double n), AE $ Int 0, DDoubleR)]
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

aexprSem (FPi) _ _ fp = [(BTrue, FBTrue, expr, Double pi, AE $ Int 0, rule)] --- TODO: change (0,0) with the symbolic error -- Div (ulp Pi) (Int 2),
  where
    (expr, ulp, rule) = case fp of
                        FPSingle -> (RtoS $ Pi, SUlp, SPiR)
                        FPDouble -> (RtoD $ Pi, DUlp, DPiR)

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

aexprSem (FArcsin a) i env fp = lub semArcsin [aexprSem a i env fp] fp

aexprSem (FArccos a) i env fp = lub semArccos [aexprSem a i env fp] fp

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

semDiv :: CebS -> FPrec -> Ceb 
semDiv [(c1,g1,f1,r1,e1,t1),(c2,g2,f2,r2,e2,t2)] fp =
   (simplBExprFix $ And (And c1 c2) (Or (Lt (Add r2 (EE e2)) (Int 0)) (Gt (Sub r2 (EE e2)) (Int 0))),
    simplFBExprFix $ FAnd g1 g2,
    FDiv f1 f2,
    Div r1 r2,
    ErrDiv r1 e1 r2 e2,          
    rule e1 e2 r1 r2 f1 f2 t1 t2)
  where
    rule = case fp of
            FPSingle -> SDivR
            FPDouble -> DDivR
                  

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
    cfloor = Or  (Neq (Floor r) (Floor (Sub r (EE e)))) (Neq (Floor r) (Floor (Add r (EE e))))
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
semSin [(c,g,f,r,e,t)] fp = error "semSin: niy"

semCos :: CebS -> FPrec -> Ceb 
semCos [(c,g,f,r,e,t)] fp = error "semCos: niy"

semArcsin :: CebS -> FPrec -> Ceb 
semArcsin [(c,g,f,r,e,t)] fp = error "semArcsin: niy"

semArccos :: CebS -> FPrec -> Ceb 
semArccos [(c,g,f,r,e,t)] fp =error "semArccos: niy"     


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
argsBindAExpr fa aa fp (Arccos a)    = Arccos (argsBindAExpr fa aa fp a)
argsBindAExpr fa aa fp (Arcsin a)    = Arcsin (argsBindAExpr fa aa fp a)
argsBindAExpr fa aa fp (SUlp a)      = SUlp   (argsBindAExpr fa aa fp a)
argsBindAExpr fa aa fp (DUlp a)      = DUlp   (argsBindAExpr fa aa fp a)
argsBindAExpr fa aa _ (StoR a)       = StoR   (argsBindFAExpr fa aa a)
argsBindAExpr fa aa _ (DtoR a)       = DtoR   (argsBindFAExpr fa aa a)
argsBindAExpr _ _ _ i@(Int _)        = i
argsBindAExpr _ _ _ d@(Double _)     = d
argsBindAExpr _ _ _ Pi               = Pi
argsBindAExpr fa aa fp (EFun f args) = EFun f (map (argsBindAExpr fa aa fp) args)
argsBindAExpr fa aa fp (EE e)        = EE $ argsBindEExpr fa aa fp e

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
argsSemBindAExpr fa aa (Arccos a)    = Arccos (argsSemBindAExpr fa aa a)
argsSemBindAExpr fa aa (Arcsin a)    = Arcsin (argsSemBindAExpr fa aa a)
argsSemBindAExpr fa aa (SUlp a)      = SUlp   (argsSemBindAExpr fa aa a)
argsSemBindAExpr fa aa (DUlp a)      = DUlp   (argsSemBindAExpr fa aa a)
argsSemBindAExpr fa aa (StoR a)      = StoR   (argsSemBindFAExpr fa aa a)
argsSemBindAExpr fa aa (DtoR a)      = DtoR   (argsSemBindFAExpr fa aa a)
argsSemBindAExpr _ _ i@(Int _)       = i
argsSemBindAExpr _ _ d@(Double _)    = d
argsSemBindAExpr _ _ Pi              = Pi
argsSemBindAExpr fa aa (EFun f args) = EFun f (map (argsSemBindAExpr fa aa) args)
argsSemBindAExpr fa aa (EE ee)       = EE (argsSemBindEExpr fa aa ee)

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
argsSemBindEExpr fa aa (ErrArcsin r e)      = ErrArcsin (argsSemBindAExpr fa aa r)  (argsSemBindEExpr fa aa e)
argsSemBindEExpr fa aa (ErrArccos r e)      = ErrArccos (argsSemBindAExpr fa aa r)  (argsSemBindEExpr fa aa e)


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
argsBindEExpr fa aa fp (ErrArcsin r e)      = ErrArcsin (argsBindAExpr fa aa fp r)  (argsBindEExpr fa aa fp e)
argsBindEExpr fa aa fp (ErrArccos r e)      = ErrArccos (argsBindAExpr fa aa fp r)  (argsBindEExpr fa aa fp e)


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
argsBindFAExpr fa aa (FArccos a)    = FArccos (argsBindFAExpr fa aa a)
argsBindFAExpr fa aa (FArcsin a)    = FArcsin (argsBindFAExpr fa aa a)
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
argsSemBindFAExpr fa aa (FArccos a)    = FArccos (argsSemBindFAExpr fa aa a)
argsSemBindFAExpr fa aa (FArcsin a)    = FArcsin (argsSemBindFAExpr fa aa a)
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

zeroErrBE  :: BExpr -> BExpr
zeroErrBE (Or  b1 b2) = Or  (zeroErrBE b1) (zeroErrBE b2)
zeroErrBE (And b1 b2) = And (zeroErrBE b1) (zeroErrBE b2)
zeroErrBE (Not b)     = Not (zeroErrBE b)
zeroErrBE (Eq  a1 a2) = Eq  (zeroErrAE a1) (zeroErrAE a2)
zeroErrBE (Neq a1 a2) = Neq (zeroErrAE a1) (zeroErrAE a2)
zeroErrBE (Lt  a1 a2) = Lt  (zeroErrAE a1) (zeroErrAE a2)
zeroErrBE (LtE a1 a2) = LtE (zeroErrAE a1) (zeroErrAE a2)
zeroErrBE (Gt  a1 a2) = Gt  (zeroErrAE a1) (zeroErrAE a2)
zeroErrBE (GtE a1 a2) = GtE (zeroErrAE a1) (zeroErrAE a2)
zeroErrBE BTrue  = BTrue
zeroErrBE BFalse = BFalse

zeroErrFBE  :: FBExpr -> FBExpr
zeroErrFBE (FOr  b1 b2) = FOr  (zeroErrFBE b1) (zeroErrFBE b2)
zeroErrFBE (FAnd b1 b2) = FAnd (zeroErrFBE b1) (zeroErrFBE b2)
zeroErrFBE (FNot b)     = FNot (zeroErrFBE b)
zeroErrFBE (FEq  a1 a2) = FEq  (zeroErrFAE a1) (zeroErrFAE a2)
zeroErrFBE (FNeq a1 a2) = FNeq (zeroErrFAE a1) (zeroErrFAE a2)
zeroErrFBE (FLt  a1 a2) = FLt  (zeroErrFAE a1) (zeroErrFAE a2)
zeroErrFBE (FLtE a1 a2) = FLtE (zeroErrFAE a1) (zeroErrFAE a2)
zeroErrFBE (FGt  a1 a2) = FGt  (zeroErrFAE a1) (zeroErrFAE a2)
zeroErrFBE (FGtE a1 a2) = FGtE (zeroErrFAE a1) (zeroErrFAE a2)
zeroErrFBE FBTrue  = FBTrue
zeroErrFBE FBFalse = FBFalse

zeroErrEE :: EExpr -> EExpr
zeroErrEE (ErrAdd ae1 ee1 ae2 ee2) = ErrAdd    (zeroErrAE ae1) (zeroErrEE ee1) (zeroErrAE ae2) (zeroErrEE ee2)
zeroErrEE (ErrSub ae1 ee1 ae2 ee2) = ErrSub    (zeroErrAE ae1) (zeroErrEE ee1) (zeroErrAE ae2) (zeroErrEE ee2)
zeroErrEE (ErrMul ae1 ee1 ae2 ee2) = ErrMul    (zeroErrAE ae1) (zeroErrEE ee1) (zeroErrAE ae2) (zeroErrEE ee2)
zeroErrEE (ErrDiv ae1 ee1 ae2 ee2) = ErrDiv    (zeroErrAE ae1) (zeroErrEE ee1) (zeroErrAE ae2) (zeroErrEE ee2)
zeroErrEE (ErrFloor  ae ee)        = ErrFloor  (zeroErrAE ae)  (zeroErrEE ee)
zeroErrEE (ErrFloor0  ae ee)       = ErrFloor0  (zeroErrAE ae)  (zeroErrEE ee)
zeroErrEE (ErrSqrt   ae ee)        = ErrSqrt   (zeroErrAE ae)  (zeroErrEE ee)
zeroErrEE (ErrSin    ae ee)        = ErrSin    (zeroErrAE ae)  (zeroErrEE ee)
zeroErrEE (ErrCos    ae ee)        = ErrCos    (zeroErrAE ae)  (zeroErrEE ee)
zeroErrEE (ErrArcsin ae ee)        = ErrArcsin (zeroErrAE ae)  (zeroErrEE ee)
zeroErrEE (ErrArccos ae ee)        = ErrArccos (zeroErrAE ae)  (zeroErrEE ee)
zeroErrEE (ErrNeg    ae ee)        = ErrNeg    (zeroErrAE ae)  (zeroErrEE ee)
zeroErrEE (ErrAbs    ae ee)        = ErrAbs    (zeroErrAE ae)  (zeroErrEE ee)
zeroErrEE (AE ae)                  = AE        (zeroErrAE ae)

zeroErrAE :: AExpr -> AExpr
zeroErrAE (Int n)        = Int n
zeroErrAE (Double n)     = Double n
zeroErrAE (Var x) | take 2 x == ("e_") = Int 0
                  | otherwise          = Var x 
zeroErrAE (Pi)                         = Pi
zeroErrAE (Add ae1 ae2)  = Add    (zeroErrAE ae1) (zeroErrAE ae2)
zeroErrAE (Sub ae1 ae2)  = Sub    (zeroErrAE ae1) (zeroErrAE ae2)
zeroErrAE (Mul ae1 ae2)  = Mul    (zeroErrAE ae1) (zeroErrAE ae2)
zeroErrAE (Div ae1 ae2)  = Div    (zeroErrAE ae1) (zeroErrAE ae2)
zeroErrAE (Pow ae1 ae2)  = Pow    (zeroErrAE ae1) (zeroErrAE ae2)
zeroErrAE (Mod ae1 ae2)  = Mod    (zeroErrAE ae1) (zeroErrAE ae2)
zeroErrAE (Neg   ae)     = Neg    (zeroErrAE ae)
zeroErrAE (Floor ae)     = Floor  (zeroErrAE ae)
zeroErrAE (Sqrt  ae)     = Sqrt   (zeroErrAE ae)
zeroErrAE (Abs   ae)     = Abs    (zeroErrAE ae)
zeroErrAE (Sin   ae)     = Sin    (zeroErrAE ae)
zeroErrAE (Cos   ae)     = Cos    (zeroErrAE ae)
zeroErrAE (Arccos ae)    = Arccos (zeroErrAE ae)
zeroErrAE (Arcsin ae)    = Arcsin (zeroErrAE ae)
zeroErrAE (SUlp ae)      = SUlp   (zeroErrAE ae)
zeroErrAE (DUlp ae)      = DUlp   (zeroErrAE ae)
zeroErrAE (EE ee)        = EE     (zeroErrEE ee)
zeroErrAE (EFun fun aes) = EFun fun (map zeroErrAE aes)
zeroErrAE (StoR fae)     = StoR (zeroErrFAE fae)
zeroErrAE (DtoR fae)     = DtoR (zeroErrFAE fae)

zeroErrFAE :: FAExpr -> FAExpr
zeroErrFAE (FInt n)     = FInt n
zeroErrFAE (FDouble n)  = FDouble n
zeroErrFAE (FVar x)     = FVar x
zeroErrFAE FPi          = FPi
zeroErrFAE (FAdd a1 a2) = FAdd (zeroErrFAE a1) (zeroErrFAE a2) 
zeroErrFAE (FSub a1 a2) = FSub (zeroErrFAE a1) (zeroErrFAE a2) 
zeroErrFAE (FMul a1 a2) = FMul (zeroErrFAE a1) (zeroErrFAE a2) 
zeroErrFAE (FDiv a1 a2) = FDiv (zeroErrFAE a1) (zeroErrFAE a2) 
zeroErrFAE (FPow a1 a2) = FPow (zeroErrFAE a1) (zeroErrFAE a2) 
zeroErrFAE (FMod a1 a2) = FMod (zeroErrFAE a1) (zeroErrFAE a2) 
zeroErrFAE (FNeg    a)  = FNeg (zeroErrFAE a)
zeroErrFAE (FFloor  a)  = FFloor (zeroErrFAE a)
zeroErrFAE (FSqrt   a)  = FSqrt (zeroErrFAE a)
zeroErrFAE (FAbs    a)  = FAbs (zeroErrFAE a)
zeroErrFAE (FSin    a)  = FSin (zeroErrFAE a)
zeroErrFAE (FCos    a)  = FCos (zeroErrFAE a)
zeroErrFAE (FArccos a)  = FArccos (zeroErrFAE a)
zeroErrFAE (FArcsin a)  = FArcsin (zeroErrFAE a)
zeroErrFAE (FEFun fun aes) = FEFun fun (map zeroErrFAE aes)
zeroErrFAE (RtoS a) = RtoS (zeroErrAE a)
zeroErrFAE (RtoD a) = RtoD (zeroErrAE a)


