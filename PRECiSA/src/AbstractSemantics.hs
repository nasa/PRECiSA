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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AbstractSemantics where

import AbsPVSLang
import AbstractDomain
import Common.ControlFlow
import Common.DecisionPath
import AbsPVSLang
import qualified Data.List as List
import qualified Data.Set as Set
import Debug.Trace
import Text.PrettyPrint
import Numeric
import Utils
import FPrec
import PPExt
import Foreign.C.Types (CDouble, CFloat)
import Data.Maybe


newtype Iteration = Iter Int deriving (Eq,Show,Num)

type Interpretation = [(FunctionName, (FPrec, [VarId] ,ACebS))]

equivInterp :: Interpretation -> Interpretation -> Bool
equivInterp i1 i2 = Set.fromList (map aux i1) == Set.fromList (map aux i2)
    where
        aux (f, (fp, vars ,acebs)) = (f, (fp, vars, Set.fromList acebs))

errorNotGrowing :: Interpretation -> Interpretation -> Bool
errorNotGrowing [] next = True
errorNotGrowing ((fName, (_,_,acebs)):current) next = localGrowingError && (errorNotGrowing current next)
    where
        localGrowingError = errorNotGrowingFun acebs (third $ fromJust $ lookup fName next)
        third (_,_,t) = t
     

errorNotGrowingFun :: ACebS -> ACebS -> Bool
errorNotGrowingFun current next =
    all (\ aceb -> existsEquivError aceb current) next


existsEquivError :: ACeb -> ACebS -> Bool
existsEquivError aceb iter = result
    where 
        result = any hasEquivError iter
        ACeb{conds = cc, eExpr = ee} = aceb
        hasEquivError aceb' = (equivEExpr ee (eExpr aceb')) && (cc /= conds aceb')

botAceb = ACeb {
            conds  = Cond [(BTrue, FBTrue)],
            rExprs = [] ,
            eExpr  = AE (Int 0),
            decisionPath = root,
            cFlow  = Stable
        }

botInterp :: [Decl] -> Interpretation -- bottom interpretation
botInterp decls = map buildBottomElem decls
    where
        buildBottomElem (Decl fp (NonVarId funName) args _) = (funName, (fp, args, []))


data SemanticConfiguration = SemConf {
    assumeTestStability :: Bool
}

data Env a = Env [(VarName,a)]

emptyEnv :: Env a
emptyEnv = Env []

insertEnv :: VarName -> a -> Env a -> Env a
insertEnv var a (Env env) =
  case lookup var env of 
    Just _ -> error "insertVarEnv: Variable already present in the environment"
    Nothing -> Env $ (var, a):env

addCond :: Condition -> ACeb -> ACeb
addCond (be,fbe) ceb@(ACeb { conds = Cond cs }) = ceb { conds = Cond (map aux cs)}
    where 
        aux (rc,fc) = (simplBExprFix $ And rc be, simplFBExprFix $  FAnd fc fbe)


addCondS :: Condition -> ACebS -> ACebS
addCondS cond cs = map (addCond cond) cs


lub :: (ACebS -> FPrec -> ACeb) -> [ACebS] -> FPrec -> ACebS 
lub fun cs fp = filterCondFalse $ map (\c -> fun c fp) (combos cs)


filterCondFalse :: ACebS -> ACebS
filterCondFalse = filter aux
    where
        isFalseCondition (     b,     fb) = isBExprEquivFalse b || isFBExprEquivFalse fb
        isFalseCondition (     _,      _) = False

        aux ceb@(ACeb { conds = Cond cs }) = not $ all isFalseCondition cs

fixpointSemantics :: Program -> Interpretation -> Iteration -> SemanticConfiguration -> TargetDPs -> Interpretation
fixpointSemantics pgm interp max_ite semConf dps = iterateImmediateConsequence pgm interp max_ite 0 semConf dps

iterateImmediateConsequence :: Program -> Interpretation -> Iteration -> Iteration -> SemanticConfiguration -> TargetDPs -> Interpretation
iterateImmediateConsequence pgm interp max_ite n semConf dps
    | (n == max_ite)                     = widening interp 
    | (equivInterp interp nextIter)      = interp 
    | (errorNotGrowing interp nextIter)  = nextIter 
    | otherwise                          = iterateImmediateConsequence pgm nextIter max_ite (n + 1) semConf dps
        where
            nextIter = immediateConsequence pgm interp semConf dps

widening :: Interpretation -> Interpretation 
widening interp = map convergeToTop interp
    where
        convergeToTop (funName, (fp, args, _)) = (funName, (fp, args, [topAceb]))
        topAceb = ACeb {
            conds  = Cond [(BTrue, FBTrue)],
            rExprs = [] ,
            eExpr  = Infinity,
            decisionPath = root,
            cFlow  = Stable
        }

immediateConsequence :: Program -> Interpretation -> SemanticConfiguration -> TargetDPs -> Interpretation
-- immediate consequence operator on sets of declarations
immediateConsequence (Prog decls) interps semConf dps = foldl (\interp -> \decl -> declSem decl interp dps) interps decls
    where
        declSem :: Decl -> Interpretation -> TargetDPs -> Interpretation
        declSem (Decl fp (NonVarId fun) args stm) interp dps = addDeclInterp fun fp args (stmSem stm interp emptyEnv fp semConf root dps fun) interp

addDeclInterp :: FunctionName -> FPrec -> [VarId] -> ACebS -> Interpretation -> Interpretation                       
addDeclInterp fun fp args sem interp =
    let (l1,l2) = List.partition (isFun fun) interp in
        case l1 of 
          [] -> error $ "addDeclInterp: function " ++ (show fun) ++ " not found."
          [(_, (fp, args, cebs))] -> 
                if hasInfiniteError cebs then
                    l2 ++ [(fun, (fp, args, fun_sem))]
                else
                    l2 ++ [(fun, (fp, args,  unionACebS cebs fun_sem))]
          _ -> error ("addDeclInterp: More than one occurrence of function " ++ fun ++ " in the interpretation.")    
    where
        fun_sem = replaceFun fun args sem
        isFun f1 (f2, (_,_,_)) = (f1 == f2)
        hasInfiniteError cebs = or (map isErrorInfinite cebs)
        replaceFun fun args sem = map (replaceFun' fun args) sem
        replaceFun' fun args ceb = ceb { rExprs = [EFun fun (map id2var args)] }
        id2var (VarId x) = Var x
        isErrorInfinite aceb = ee == Infinity
            where 
                ACeb{ eExpr = ee} = aceb

stmSem :: Stm -> Interpretation -> Env ACebS -> FPrec -> SemanticConfiguration -> LDecisionPath -> TargetDPs -> FunctionName -> ACebS

stmSem (StmExpr aexpr) interp env fp semConf dp dps fun = aexprSem aexpr interp env fp dp fun

stmSem (Let (VarId var) aexpr stm) interp env fp semConf dp dps fun =
  stmSem newStm interp newEnv fp semConf dp dps fun
  where
    newEnv = insertEnv var sem env
    newStm = argsBindStm [VarId var] [aexpr] sem stm      
    sem = aexprSem aexpr interp env fp dp fun

stmSem expr@(Ite fbe stm1 stm2) interp env fp semConf@(SemConf { assumeTestStability = sta }) dp dps fun =
  if sta
    then stableCases
    else case unstableCase of
        Nothing -> stableCases
        Just uc -> uc:stableCases
  where
    be = fbe2be fbe
    sem1 = stmSem stm1 interp env fp semConf (dp ~> True)  dps fun
    sem2 = stmSem stm2 interp env fp semConf (dp ~> False) dps fun

    stableCases'   = addCondS (be,fbe) sem1 ++ addCondS (Not be,FNot fbe) sem2
    unstableCases' = addCondS (be,FNot fbe) (unTestSem fp (representative realEquivalence sem1) (representative fpEquivalence sem2)) ++
                     addCondS (Not be, fbe) (unTestSem fp (representative realEquivalence sem2) (representative fpEquivalence sem1))
    
    isDecPathOfInterest dps (ACeb { decisionPath = dp' })
        = case (lookup fun dps) of 
            Just dpsfun -> existsPrefixInList dp' dpsfun
            Nothing -> error ("function " ++ show fun ++ " not found in Path of Interests.")
    
    (stableOfInterest, stableNotOfInterest) = List.partition (isDecPathOfInterest dps) (filter isStable stableCases')

    stableCases   = if stableNotOfInterest == [] then stableOfInterest else (mergeACebFold stableNotOfInterest):stableOfInterest
    unstableCase  = if unstableCases == [] then Nothing else Just $ mergeACebFold unstableCases
    
    unstableCases = (filter isUnstable (stableCases' ++ unstableCases'))

    representative equiv sem = map head (partition equiv sem)

stmSem expr@(ForLoop i j a f) interp env fp semConf dp dps fun =
    stmSem (StmExpr $ unfoldForLoop expr) interp env fp semConf dp dps fun

unfoldForLoop (ForLoop i j a (NonVarId f))
    | i > j = error "unfoldForLoop: forLoop not valid"
    | i == j = FEFun f [FInt j, a]
    | otherwise = FEFun f [FInt j, unfoldForLoop (ForLoop i (j-1) a (NonVarId f))]


realEquivalence :: ACeb -> ACeb -> Bool
realEquivalence
    ceb1@(ACeb { conds = Cond cs1, rExprs = rs1 })
    ceb2@(ACeb { conds = Cond cs2, rExprs = rs2 })
    = setEq rCs1 rCs2
    where
        rCs1 = map realCond cs1
        rCs2 = map realCond cs2


fpEquivalence :: ACeb -> ACeb -> Bool
fpEquivalence
    ceb1@(ACeb { conds = Cond cs1, rExprs = rs1, eExpr = ee1 })
    ceb2@(ACeb { conds = Cond cs2, rExprs = rs2, eExpr = ee2 })
    = setEq fpCs1 fpCs2
    where
        fpCs1 = map fpCond cs1
        fpCs2 = map fpCond cs2


partition :: (a -> a -> Bool) -> [a] -> [[a]]
partition eqRel [] = []
partition eqRel xs@(x:_) = elems:(partition eqRel rest)
  where
    (elems, rest) = List.partition (eqRel x) xs


makeUnstable :: FPrec -> ACeb -> ACeb -> ACeb
makeUnstable
    fp
    rFlow@(ACeb  { conds = Cond rCs, rExprs = rRs })
    fpFlow@(ACeb { conds = Cond fpCs, rExprs = fpRs, eExpr = fpE, decisionPath = dp})
    = ACeb {
        conds  = Cond cs',
        rExprs = rRs,
        eExpr  = ee',
        decisionPath = dp,
        cFlow  = Unstable
    } where
        ee' = MaxErr [AE $ Add (EE fpE) (Abs (Sub fpR rR)) | fpR <- fpRs, rR <- rRs]
        
        f2r = case fp of
            FPSingle -> StoR
            FPDouble -> DtoR

        cs' = zip (zipWith makeAnd (map realCond rCs) (map realCond fpCs)) (map fpCond fpCs)
        
        makeAnd c1 c2 = simplBExprFix $ And c1 c2


unTestSem :: FPrec -> ACebS -> ACebS -> ACebS
unTestSem fp semR semFP  = map (uncurry $ makeUnstable fp) combinations
    where
        combinations = [(cebR,cebFP) | cebR <- semR, cebFP <- semFP]


--------------------------------------
-- Arithmetic Expressions Semantics --
--------------------------------------

aexprSem :: FAExpr -> Interpretation -> Env ACebS -> FPrec -> LDecisionPath -> FunctionName -> ACebS

aexprSem (RtoS (Int n)) _ _ FPSingle dp fun = [ ACeb {
    conds  = Cond [(BTrue,FBTrue)],
    rExprs = [Int n],
    eExpr  = AE $ Int 0,
    decisionPath = dp,
    cFlow  = Stable
    } ]
aexprSem (RtoS (Int n)) _ _ FPDouble _ _= error "aexprSem: FP precision mismatch"

aexprSem (RtoS (Double n)) _ _ FPSingle dp fun = [ ACeb {
    conds  = Cond [(BTrue,FBTrue)],
    rExprs = [Double n],
    eExpr  = ErrRat $ abs $ toRational ((fromRat n) :: CFloat) - (n :: Rational),
    decisionPath = dp,
    cFlow  = Stable
    } ]
aexprSem (RtoS (Double n)) _ _ FPDouble _ _ = error "aexprSem: FP precision mismatch"

aexprSem (RtoD (Int n)) _ _ FPDouble dp fun = [ ACeb {
    conds  = Cond [(BTrue,FBTrue)],
    rExprs = [Int n],
    eExpr  = AE $ Int 0,
    decisionPath = dp,
    cFlow  = Stable
    } ]
aexprSem (RtoD (Int n)) _ _ FPSingle _ _ = error "aexprSem: FP precision mismatch"

aexprSem (RtoD (Double n)) _ _ FPDouble dp fun = [ ACeb {
    conds  = Cond [(BTrue,FBTrue)],
    rExprs = [Double n],
    eExpr  = ErrRat $ abs $ toRational ((fromRat n) :: CDouble) - (n :: Rational),
    decisionPath = dp,
    cFlow  = Stable
    } ]
aexprSem (RtoD (Double n)) _ _ FPSingle _ _ = error "aexprSem: FP precision mismatch"

aexprSem (RtoS (Neg (Int n))) _ _ FPSingle dp fun = [ ACeb {
    conds  = Cond [(BTrue,FBTrue)],
    rExprs = [Neg (Int n)],
    eExpr  = AE $ Int 0,
    decisionPath = dp,
    cFlow  = Stable
    } ]
aexprSem (RtoS (Neg (Int n))) _ _ FPDouble _ _ = error "aexprSem: FP precision mismatch"

aexprSem (RtoS (Neg (Double n))) _ _ FPSingle dp fun = [ ACeb {
    conds  = Cond [(BTrue,FBTrue)],
    rExprs = [Neg (Double n)],
    eExpr  = ErrRat $ abs $ toRational ((fromRat (-n)) :: Double) - ((-n) :: Rational),
    decisionPath = dp,
    cFlow  = Stable
    } ]
aexprSem (RtoS (Neg (Double n))) _ _ FPDouble _ _ = error "aexprSem: FP precision mismatch"

aexprSem (RtoD (Neg (Int n))) _ _ FPDouble dp fun = [ ACeb {
    conds  = Cond [(BTrue,FBTrue)],
    rExprs = [Neg (Int n)],
    eExpr  = AE $ Int 0,
    decisionPath = dp,
    cFlow  = Stable
    } ]
aexprSem (RtoD (Neg (Int n))) _ _ FPSingle _ _ = error "aexprSem: FP precision mismatch"

aexprSem (RtoD (Neg (Double n))) _ _ FPDouble dp fun = [ ACeb {
    conds  = Cond [(BTrue,FBTrue)],
    rExprs = [Neg (Double n)],
    eExpr  = ErrRat $ abs $ toRational ((fromRat (-n)) :: Double) - ((-n) :: Rational),
    decisionPath = dp,
    cFlow  = Stable
    } ]
aexprSem (RtoD (Neg (Double n))) _ _ FPSingle _ _ = error "aexprSem: FP precision mismatch"

aexprSem (FInt n) _ _ fp dp fun = [ ACeb {
    conds  = Cond [(BTrue,FBTrue)],
    rExprs = [Int n],
    eExpr  = AE $ Int 0,
    decisionPath = dp,
    cFlow  = Stable
    } ]

aexprSem (FDouble n) _ _ fp dp fun = [ ACeb {
    conds  = Cond [(BTrue,FBTrue)],
    rExprs = [Double n],
    eExpr  = ErrRat $ abs $ toRational ((fromRat (n)) :: Double) - ((n) :: Rational),
    decisionPath = dp,
    cFlow  = Stable
    } ]

aexprSem (FVar x) _ (Env env) fp dp fun =
  case (lookup x env) of
    Just a -> a -- variable coming from a Let statment
    Nothing -> [ ACeb {
        conds  = Cond [(BTrue,FBTrue)],
        rExprs = [RealMark x],
        eExpr  = ErrorMark x,
    decisionPath = dp,
        cFlow  = Stable
        } ]

aexprSem (FEFun f actArgs) interp env fp dp fun =
    case (lookup f interp) of
      Just (_, formArgs, funSem) -> semEFun f formArgs actArgs (combos argSem) funSem (length funSem) fp dp
      -- 
      Nothing -> error ("Function " ++ f ++ " not found")
    where
      argSem = map (\arg -> aexprSem arg interp env fp dp fun) actArgs


aexprSem (FAdd a1 a2) interp env fp dp fun =
    lub semAdd [(aexprSem a1 interp env fp dp fun),(aexprSem a2 interp env fp dp fun)] fp
    where
        semAdd :: ACebS -> FPrec -> ACeb
        semAdd [ceb1,ceb2] fp =
            ACeb {
                conds  = Cond [ (simplBExprFix $ And c1 c2, simplFBExprFix $ FAnd g1 g2) |
                                 (c1, g1) <- uncond (conds ceb1), (c2, g2) <- uncond (conds ceb2)],
                rExprs = [Add r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
                eExpr  = MaxErr [ErrAdd r1 (eExpr ceb1) r2 (eExpr ceb2) | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
                decisionPath = decisionPath ceb1,
                cFlow  = Stable
            }

aexprSem (FSub a1 a2) interp env fp dp fun =
    lub semSub [(aexprSem a1 interp env fp dp fun),(aexprSem a2 interp env fp dp fun)] fp
    where
        semSub :: ACebS -> FPrec -> ACeb
        semSub [ceb1,ceb2] fp =
            ACeb {
                conds  = Cond [ (simplBExprFix $ And c1 c2, simplFBExprFix $ FAnd g1 g2) |
                           (c1, g1) <- uncond (conds ceb1), (c2, g2) <- uncond (conds ceb2)],
                rExprs = [Sub r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
                eExpr  = MaxErr [ErrSub r1 (eExpr ceb1) r2 (eExpr ceb2) | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
                decisionPath = decisionPath ceb1,
                cFlow  = Stable
            }



aexprSem ae@(FMul a1 a2) interp env fp dp fun =
    case (pow2Mul ae) of
        Just (Left  (pow2, a)) -> (lub semMulPow2L [(aexprSem pow2 interp env fp dp fun),(aexprSem a interp env fp dp fun)] fp)
        Just (Right (pow2, a)) -> (lub semMulPow2R [(aexprSem a interp env fp dp fun),(aexprSem pow2 interp env fp dp fun)] fp)
        Nothing -> lub semMul [(aexprSem a1 interp env fp dp fun),(aexprSem a2 interp env fp dp fun)] fp
    where
        pow2Mul :: FAExpr -> Maybe (Either (FAExpr,FAExpr) (FAExpr,FAExpr))
        pow2Mul (FMul pow2@(FInt n)         a) | (isPow2 (fromInteger n))  = Just $ Left (pow2, a)
        pow2Mul (FMul pow2@(FDouble n)      a) | (isPow2 (fromRational n)) = Just $ Left (pow2, a)    
        pow2Mul (FMul pow2@(RtoD(Int n))    a) | (isPow2 (fromInteger n))  = Just $ Left (pow2, a)  
        pow2Mul (FMul pow2@(RtoD(Double n)) a) | (isPow2 (fromRational n)) = Just $ Left (pow2, a)  
        pow2Mul (FMul pow2@(RtoS(Int n))    a) | (isPow2 (fromInteger n))  = Just $ Left (pow2, a)  
        pow2Mul (FMul pow2@(RtoS(Double n)) a) | (isPow2 (fromRational n)) = Just $ Left (pow2, a)
        pow2Mul (FMul a         pow2@(FInt n)) | (isPow2 (fromInteger n))  = Just $ Right (pow2, a)
        pow2Mul (FMul a      pow2@(FDouble n)) | (isPow2 (fromRational n)) = Just $ Right (pow2, a)
        pow2Mul (FMul a    pow2@(RtoD(Int n))) | (isPow2 (fromInteger n))  = Just $ Right (pow2, a)
        pow2Mul (FMul a pow2@(RtoD(Double n))) | (isPow2 (fromRational n)) = Just $ Right (pow2, a)
        pow2Mul (FMul a    pow2@(RtoS(Int n))) | (isPow2 (fromInteger n))  = Just $ Right (pow2, a)
        pow2Mul (FMul a pow2@(RtoS(Double n))) | (isPow2 (fromRational n)) = Just $ Right (pow2, a)
        pow2Mul _ = Nothing

        semMul :: ACebS -> FPrec -> ACeb 
        semMul [ceb1,ceb2] fp =
            ACeb {
                conds  = Cond [(simplBExprFix $ And c1 c2, simplFBExprFix $ FAnd g1 g2) |
                           (c1, g1) <- uncond (conds ceb1), (c2, g2) <- uncond (conds ceb2)],
                rExprs = [Mul r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
                eExpr  = MaxErr [ErrMul r1 (eExpr ceb1) r2 (eExpr ceb2) | r1 <- rExprs ceb1, r2 <- rExprs ceb2], 
                decisionPath = decisionPath ceb1,
                cFlow  = Stable
            }

        semMulPow2L :: ACebS -> FPrec -> ACeb
        semMulPow2L [ceb1,ceb2] fp =
            ACeb {
                conds  = Cond [(simplBExprFix $ And c2 (Lt (Int n) (Sub FPrec (FExp (real2fae fp r2)))), g2) |
                           (c2, g2) <- uncond (conds ceb2), r2 <- rExprs ceb2],
                rExprs = [Mul r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
                eExpr  = ErrMulPow2L n (eExpr ceb2),
                decisionPath = decisionPath ceb1,
                cFlow  = Stable
            }
            where
                n = getExp (rExprs ceb1)
                getExp [r] = case r of
                    Int    m -> round $ logBase 2 (fromIntegral m)
                    Double m -> round $ logBase 2 (realToFrac m)

        semMulPow2R :: ACebS -> FPrec -> ACeb
        semMulPow2R [ceb1,ceb2] fp =
            ACeb {
                conds  = Cond [(simplBExprFix $ And c1 (Lt (Int n) (Sub FPrec (FExp (real2fae fp r1)))), g1) |
                           (c1, g1) <- uncond (conds ceb1), r1 <- rExprs ceb1],
                rExprs = [Mul r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
                eExpr  = ErrMulPow2R n (eExpr ceb1),
                decisionPath = decisionPath ceb2,
                cFlow  = Stable
            }
            where
                n = getExp (rExprs ceb2)
                getExp [r] = case r of
                    Int    m -> round $ logBase 2 (fromIntegral m)
                    Double m -> round $ logBase 2 (realToFrac m)

aexprSem (FDiv a1 a2) interp env fp dp fun = lub semDiv [(aexprSem a1 interp env fp dp fun),(aexprSem a2 interp env fp dp fun)] fp
    where
        semDiv :: ACebS -> FPrec -> ACeb 
        semDiv [ceb1,ceb2] fp =
            ACeb {
                conds  = Cond [(simplBExprFix  $ And (And (And (And c1 c2) (Neq r2 (Int 0))) (Neq (f2r (real2fae fp r2)) (Int 0)))
                                                 (Or (Lt (Add r2 (EE (eExpr ceb2))) (Int 0)) (Gt (Sub r2 (EE (eExpr ceb2))) (Int 0))),
                            simplFBExprFix $ FAnd g1 g2) |
                           (c1, g1) <- uncond (conds ceb1), (c2, g2) <- uncond (conds ceb2), r1 <- rExprs ceb1, r2 <- rExprs ceb2],
                rExprs = [Div r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
                eExpr  = MaxErr [ErrDiv r1 (eExpr ceb1) r2 (eExpr ceb2) | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
                decisionPath = decisionPath ceb1,
                cFlow  = Stable
            }
        f2r = case fp of
                FPSingle -> StoR
                FPDouble -> DtoR

aexprSem (FNeg a) interp env fp dp fun = lub semNeg [aexprSem a interp env fp dp fun] fp
    where
        semNeg :: ACebS -> FPrec -> ACeb
        semNeg [ceb] fp = ceb {
                    rExprs = [Neg r | r <- rExprs ceb],
                    eExpr  = MaxErr [ErrNeg r (eExpr ceb)| r <- rExprs ceb]
                }


aexprSem (FAbs a) interp env fp dp fun = lub semAbs [aexprSem a interp env fp dp fun] fp
    where
        semAbs :: ACebS -> FPrec -> ACeb
        semAbs [ceb] fp = ceb {
                    rExprs = [Abs r | r <- rExprs ceb],
                    eExpr  = MaxErr [ErrAbs r (eExpr ceb)| r <- rExprs ceb]
                }

aexprSem (FLn a) interp env fp dp fun = lub semLn [aexprSem a interp env fp dp fun] fp
    where
        semLn :: ACebS -> FPrec -> ACeb
        semLn [ceb] fp = ceb {
                    conds = Cond [(And (And rc (Lt (Int 0)(Sub r (eExpr ceb))))
                                   (Gt (f2r (real2fae fp r)) (Int 0)),
                                   fc) | (rc,fc) <- uncond (conds ceb), r <- rExprs ceb],
                    rExprs = [Ln r | r <- rExprs ceb],
                    eExpr  = MaxErr [ErrLn r (eExpr ceb)| r <- rExprs ceb]
                }
        f2r = case fp of
                FPSingle -> StoR
                FPDouble -> DtoR

aexprSem (FExpo a) interp env fp dp fun = lub semExpo [aexprSem a interp env fp dp fun] fp
    where
        semExpo :: ACebS -> FPrec -> ACeb
        semExpo [ceb] fp = ceb {
                    rExprs = [Expo r | r <- rExprs ceb],
                    eExpr  = MaxErr [ErrExpo r (eExpr ceb)| r <- rExprs ceb]
                }


aexprSem (FFloor a) interp env fp dp fun =
    (lub semFloor [aexprSem a interp env fp dp fun] fp) ++ (lub semFloor0 [aexprSem a interp env fp dp fun] fp)
    where
        semFloor :: ACebS -> FPrec -> ACeb
        semFloor [ceb] fp = ceb {
                    conds = Cond [(And rc (Or (Neq (Floor r) (Floor (Sub r (EE (eExpr ceb)))))
                                         (Neq (Floor r) (Floor (Add r (EE (eExpr ceb)))))),
                              fc) | (rc,fc) <- uncond (conds ceb), r <- rExprs ceb],
                    rExprs = [Floor r | r <- rExprs ceb],
                    eExpr  = MaxErr [ErrFloor r (eExpr ceb)| r <- rExprs ceb]
                }
        semFloor0 :: ACebS -> FPrec -> ACeb
        semFloor0 [ceb] fp = ceb {
                    conds = Cond [(simplBExprFix $ And rc (And (Eq (Floor r) (Floor (Sub r (EE (eExpr ceb)))))
                                          (Eq (Floor r) (Floor (Add r (EE (eExpr ceb)))))),
                              fc) | (rc,fc) <- uncond (conds ceb), r <- rExprs ceb],
                    rExprs = [Floor r | r <- rExprs ceb],
                    eExpr  = MaxErr [ErrFloor0 r (eExpr ceb)| r <- rExprs ceb]
                }

aexprSem (FSqrt a) interp env fp dp fun = lub semSqrt [aexprSem a interp env fp dp fun] fp
    where
        semSqrt :: ACebS -> FPrec -> ACeb
        semSqrt [ceb] fp = ceb {
                    conds = Cond [(simplBExprFix $ And rc (GtE (Sub r (EE (eExpr ceb))) (Int 0)),
                              fc) | (rc,fc) <- uncond (conds ceb), r <- rExprs ceb],
                    rExprs = [Sqrt r | r <- rExprs ceb],
                    eExpr  = MaxErr [ErrSqrt r (eExpr ceb)| r <- rExprs ceb]
                }

aexprSem (FSin a) interp env fp dp fun = lub semSin [aexprSem a interp env fp dp fun] fp
    where
        semSin :: ACebS -> FPrec -> ACeb 
        semSin [ceb] fp = ceb {
                    rExprs = [Sin r | r <- rExprs ceb],
                    eExpr  = MaxErr [ErrSin r (eExpr ceb)| r <- rExprs ceb]
                }

aexprSem (FCos a) interp env fp dp fun = lub semCos [aexprSem a interp env fp dp fun] fp
    where
        semCos :: ACebS -> FPrec -> ACeb 
        semCos [ceb] fp = ceb {
                    rExprs = [Cos r | r <- rExprs ceb],
                    eExpr  = MaxErr [ErrCos r (eExpr ceb)| r <- rExprs ceb]
                }

aexprSem (FAtan a) interp env fp dp fun =
       (lub semAtan   [aexprSem a interp env fp dp fun] fp)
--    ++ (lub semAtanT  [aexprSem a interp env fp dp fun] fp)
    where 
        semAtan :: ACebS -> FPrec -> ACeb 
        semAtan [ceb] fp = ceb {
                    conds = Cond [(simplBExprFix $ And rc (GtE (EE (eExpr ceb)) (Abs r)),
                              fc) | (rc,fc) <- uncond (conds ceb), r <- rExprs ceb],
                    rExprs = [ATan r | r <- rExprs ceb],
                    eExpr  = MaxErr [ErrAtan r (eExpr ceb)| r <- rExprs ceb]
                }

        semAtanT :: ACebS -> FPrec -> ACeb 
        semAtanT [ceb] fp = ceb {
                    conds = Cond [(simplBExprFix $ And rc (Lt (EE (eExpr ceb)) (Abs r)),
                              fc) | (rc,fc) <- uncond (conds ceb), r <- rExprs ceb],
                    rExprs = [ATan r | r <- rExprs ceb],
                    eExpr  = MaxErr [ErrAtanT r (eExpr ceb)| r <- rExprs ceb]
                }

aexprSem (FTan a)  interp env fp dp fun = error "niy: aexprSem tan" 
aexprSem (FAsin a) interp env fp dp fun = error "niy: aexprSem asin"
aexprSem (FAcos a) interp env fp dp fun = error "niy: aexprSem acos"


semEFun :: FunctionName -> [VarId] -> [FAExpr] -> [ACebS] -> ACebS -> Int -> FPrec -> LDecisionPath -> ACebS
semEFun _ _ _ _ [] _ _ _ = []
semEFun fun formArgs actualArgs semArgsCombos semFun@(c:cs) n fp dp =
    (map (aux n c formArgs actualArgs) semArgsCombos)++(semEFun fun formArgs actualArgs semArgsCombos cs n fp dp)
    where
        aux n ceb fa aa argsSem =
            ceb {
                conds  = Cond $ map newcond $ combos [uncond (conds ceb), conjComboArgs],
                rExprs = map (argsBindAExpr fa aa argsSem) (rExprs ceb),
                eExpr  = argsBindEExpr fa aa argsSem (eExpr ceb)
            }
            where
                combosArgs = combos (map (uncond . conds) argsSem)
                conjComboArgs = zip (map condArgs combosArgs) (map guardArgs combosArgs)

                
                newcond [(rc, fpc), (rcargs, fpcargs)] = (simplBExprFix  $ And  (argsBindBExpr fa aa argsSem rc) rcargs,
                    simplFBExprFix $ FAnd (argsBindFBExpr fa aa argsSem fpc) fpcargs)

                condArgs arg  = foldl makeAnd BTrue (map realCond arg)
                guardArgs arg = foldl makeFAnd FBTrue (map fpCond arg)
                valArgs arg  = (map rExprs arg)
                errArgs arg  = (map eExpr arg)
                makeAnd a b = And a b
                makeFAnd a b = FAnd a b


argsBindAExpr :: [VarId] -> [FAExpr] -> ACebS -> AExpr -> AExpr
argsBindAExpr fa aa _   (Var x)      = Var x  
argsBindAExpr fa aa sem (RealMark x) = bindRealMark fa aa x
    where
        bindRealMark :: [VarId] -> [FAExpr] -> String -> AExpr
        bindRealMark [] [] x = Var x
        bindRealMark ((VarId y):ys) (a:as) x | x == y    = (fae2real a)
                                             | otherwise = bindRealMark ys as x   
argsBindAExpr fa aa sem (Add a1 a2)   = Add    (argsBindAExpr  fa aa sem a1) (argsBindAExpr fa aa sem a2)
argsBindAExpr fa aa sem (Sub a1 a2)   = Sub    (argsBindAExpr  fa aa sem a1) (argsBindAExpr fa aa sem a2)
argsBindAExpr fa aa sem (Mul a1 a2)   = Mul    (argsBindAExpr  fa aa sem a1) (argsBindAExpr fa aa sem a2)
argsBindAExpr fa aa sem (Div a1 a2)   = Div    (argsBindAExpr  fa aa sem a1) (argsBindAExpr fa aa sem a2)
argsBindAExpr fa aa sem (Pow a1 a2)   = Pow    (argsBindAExpr  fa aa sem a1) (argsBindAExpr fa aa sem a2)
argsBindAExpr fa aa sem (Mod a1 a2)   = Mod    (argsBindAExpr  fa aa sem a1) (argsBindAExpr fa aa sem a2)
argsBindAExpr fa aa sem (Neg a)       = Neg    (argsBindAExpr  fa aa sem a)
argsBindAExpr fa aa sem (Floor a)     = Floor  (argsBindAExpr  fa aa sem a)
argsBindAExpr fa aa sem (Sqrt a)      = Sqrt   (argsBindAExpr  fa aa sem a)
argsBindAExpr fa aa sem (Abs a)       = Abs    (argsBindAExpr  fa aa sem a)
argsBindAExpr fa aa sem (Sin a)       = Sin    (argsBindAExpr  fa aa sem a)
argsBindAExpr fa aa sem (Cos a)       = Cos    (argsBindAExpr  fa aa sem a)
argsBindAExpr fa aa sem (Tan a)       = Tan    (argsBindAExpr  fa aa sem a)
argsBindAExpr fa aa sem (ACos a)      = ACos   (argsBindAExpr  fa aa sem a)
argsBindAExpr fa aa sem (ASin a)      = ASin   (argsBindAExpr  fa aa sem a)
argsBindAExpr fa aa sem (ATan a)      = ATan   (argsBindAExpr  fa aa sem a)
argsBindAExpr fa aa sem (Ln   a)      = Ln     (argsBindAExpr  fa aa sem a)
argsBindAExpr fa aa sem (Expo a)      = Expo   (argsBindAExpr  fa aa sem a)
argsBindAExpr fa aa sem (SUlp a)      = SUlp   (argsBindAExpr  fa aa sem a)
argsBindAExpr fa aa sem (DUlp a)      = DUlp   (argsBindAExpr  fa aa sem a)
argsBindAExpr fa aa sem (StoR a)      = StoR   (argsBindFAExpr fa aa sem a)
argsBindAExpr fa aa sem (DtoR a)      = DtoR   (argsBindFAExpr fa aa sem a)
argsBindAExpr _ _ _ i@(Int _)         = i
argsBindAExpr _ _ _ d@(Double _)      = d
argsBindAExpr _ _ _ Pi                = Pi
argsBindAExpr fa aa sem (EFun f args) = EFun f (map (argsBindAExpr fa aa sem) args)
argsBindAExpr fa aa sem (EE ee)       = EE (argsBindEExpr fa aa sem ee)
argsBindAExpr fa aa sem FPrec         = FPrec
argsBindAExpr fa aa sem (FExp fae)    = FExp (argsBindFAExpr fa aa sem fae)

argsBindEExpr :: [VarId] -> [FAExpr] -> [ACeb] -> EExpr -> EExpr
argsBindEExpr fa aa sem (ErrRat n)    = ErrRat n
argsBindEExpr fa aa sem (ErrorMark x) = bindErrorMark fa sem x
    where
        bindErrorMark :: [VarId] -> ACebS -> String -> EExpr
        bindErrorMark [] [] x = ErrorMark x
        bindErrorMark ((VarId y):ys) (ceb:cebs) x | x == y    = (eExpr ceb)
                                                  | otherwise = bindErrorMark ys cebs x   
argsBindEExpr fa aa sem (AE ae) = AE $ argsBindAExpr fa aa sem ae
argsBindEExpr fa aa sem (ErrAdd r1 e1 r2 e2) = ErrAdd    (argsBindAExpr fa aa sem r1) (argsBindEExpr fa aa sem e1)
                                                         (argsBindAExpr fa aa sem r2) (argsBindEExpr fa aa sem e2)
argsBindEExpr fa aa sem (ErrSub r1 e1 r2 e2) = ErrSub    (argsBindAExpr fa aa sem r1) (argsBindEExpr fa aa sem e1)
                                                         (argsBindAExpr fa aa sem r2) (argsBindEExpr fa aa sem e2)
argsBindEExpr fa aa sem (ErrMul r1 e1 r2 e2) = ErrMul    (argsBindAExpr fa aa sem r1) (argsBindEExpr fa aa sem e1)
                                                         (argsBindAExpr fa aa sem r2) (argsBindEExpr fa aa sem e2)
argsBindEExpr fa aa sem (ErrDiv r1 e1 r2 e2) = ErrDiv    (argsBindAExpr fa aa sem r1) (argsBindEExpr fa aa sem e1)
                                                         (argsBindAExpr fa aa sem r2) (argsBindEExpr fa aa sem e2)
argsBindEExpr fa aa sem (ErrFloor  r e)      = ErrFloor  (argsBindAExpr fa aa sem r)  (argsBindEExpr fa aa sem e)
argsBindEExpr fa aa sem (ErrFloor0 r e)      = ErrFloor0 (argsBindAExpr fa aa sem r)  (argsBindEExpr fa aa sem e)
argsBindEExpr fa aa sem (ErrSqrt   r e)      = ErrSqrt   (argsBindAExpr fa aa sem r)  (argsBindEExpr fa aa sem e)
argsBindEExpr fa aa sem (ErrNeg    r e)      = ErrNeg    (argsBindAExpr fa aa sem r)  (argsBindEExpr fa aa sem e)
argsBindEExpr fa aa sem (ErrAbs    r e)      = ErrAbs    (argsBindAExpr fa aa sem r)  (argsBindEExpr fa aa sem e)
argsBindEExpr fa aa sem (ErrSin    r e)      = ErrSin    (argsBindAExpr fa aa sem r)  (argsBindEExpr fa aa sem e)
argsBindEExpr fa aa sem (ErrCos    r e)      = ErrCos    (argsBindAExpr fa aa sem r)  (argsBindEExpr fa aa sem e)
argsBindEExpr fa aa sem (ErrAtan   r e)      = ErrAtan   (argsBindAExpr fa aa sem r)  (argsBindEExpr fa aa sem e)
argsBindEExpr fa aa sem (ErrLn     r e)      = ErrLn     (argsBindAExpr fa aa sem r)  (argsBindEExpr fa aa sem e)
argsBindEExpr fa aa sem (ErrExpo   r e)      = ErrExpo   (argsBindAExpr fa aa sem r)  (argsBindEExpr fa aa sem e)
argsBindEExpr fa aa sem (ErrAtanT a e)       = ErrAtanT  (argsBindAExpr fa aa sem a)  (argsBindEExpr fa aa sem e)
argsBindEExpr fa aa sem (ErrMulPow2L n e)    = ErrMulPow2L n (argsBindEExpr fa aa sem e)
argsBindEExpr fa aa sem (ErrMulPow2R n e)    = ErrMulPow2R n (argsBindEExpr fa aa sem e)
argsBindEExpr fa aa sem (HalfUlp a)          = HalfUlp      (argsBindAExpr fa aa sem a)
argsBindEExpr fa aa sem (MaxErr es)          = MaxErr (map (argsBindEExpr fa aa sem) es)
argsBindEExpr _  _  _   Infinity             = Infinity
argsBindEExpr _  _  _   ErrUndefined         = ErrUndefined
argsBindEExpr _  _  _   ee = error $ "argsBindEExpr " ++ show ee


argsBindBExpr :: [VarId] -> [FAExpr] -> ACebS -> BExpr -> BExpr
argsBindBExpr fa aa sem (And e1 e2) = And (argsBindBExpr fa aa sem e1) (argsBindBExpr fa aa sem e2)
argsBindBExpr fa aa sem (Or e1 e2)  = Or  (argsBindBExpr fa aa sem e1) (argsBindBExpr fa aa sem e2)
argsBindBExpr fa aa sem (Not e)     = Not (argsBindBExpr fa aa sem e)
argsBindBExpr fa aa sem (Eq a1 a2)  = Eq  (argsBindAExpr fa aa sem a1) (argsBindAExpr fa aa sem a2)
argsBindBExpr fa aa sem (Neq a1 a2) = Neq (argsBindAExpr fa aa sem a1) (argsBindAExpr fa aa sem a2)
argsBindBExpr fa aa sem (Lt a1 a2)  = Lt  (argsBindAExpr fa aa sem a1) (argsBindAExpr fa aa sem a2)
argsBindBExpr fa aa sem (LtE a1 a2) = LtE (argsBindAExpr fa aa sem a1) (argsBindAExpr fa aa sem a2)
argsBindBExpr fa aa sem (Gt a1 a2)  = Gt  (argsBindAExpr fa aa sem a1) (argsBindAExpr fa aa sem a2)
argsBindBExpr fa aa sem (GtE a1 a2) = GtE (argsBindAExpr fa aa sem a1) (argsBindAExpr fa aa sem a2)
argsBindBExpr fa aa sem BTrue  = BTrue
argsBindBExpr fa aa sem BFalse = BFalse


argsBindFAExpr :: [VarId] -> [FAExpr] -> ACebS -> FAExpr -> FAExpr
argsBindFAExpr fa aa sem (FVar x) = bindVar fa aa x
    where
        bindVar :: [VarId] -> [FAExpr] -> String -> FAExpr
        bindVar [] [] x = FVar x
        bindVar ((VarId y):ys) (fae:as) x | x == y = fae
                                          | otherwise = bindVar ys as x  
argsBindFAExpr fa aa sem (FAdd a1 a2)   = FAdd    (argsBindFAExpr fa aa sem a1) (argsBindFAExpr fa aa sem a2)
argsBindFAExpr fa aa sem (FSub a1 a2)   = FSub    (argsBindFAExpr fa aa sem a1) (argsBindFAExpr fa aa sem a2)
argsBindFAExpr fa aa sem (FMul a1 a2)   = FMul    (argsBindFAExpr fa aa sem a1) (argsBindFAExpr fa aa sem a2)
argsBindFAExpr fa aa sem (FDiv a1 a2)   = FDiv    (argsBindFAExpr fa aa sem a1) (argsBindFAExpr fa aa sem a2)
argsBindFAExpr fa aa sem (FPow a1 a2)   = FPow    (argsBindFAExpr fa aa sem a1) (argsBindFAExpr fa aa sem a2)
argsBindFAExpr fa aa sem (FMod a1 a2)   = FMod    (argsBindFAExpr fa aa sem a1) (argsBindFAExpr fa aa sem a2)
argsBindFAExpr fa aa sem (FNeg a)       = FNeg    (argsBindFAExpr fa aa sem a)
argsBindFAExpr fa aa sem (FFloor a)     = FFloor  (argsBindFAExpr fa aa sem a) 
argsBindFAExpr fa aa sem (FSqrt a)      = FSqrt   (argsBindFAExpr fa aa sem a)
argsBindFAExpr fa aa sem (FAbs a)       = FAbs    (argsBindFAExpr fa aa sem a)
argsBindFAExpr fa aa sem (FSin a)       = FSin    (argsBindFAExpr fa aa sem a)
argsBindFAExpr fa aa sem (FCos a)       = FCos    (argsBindFAExpr fa aa sem a)
argsBindFAExpr fa aa sem (FTan a)       = FTan    (argsBindFAExpr fa aa sem a)
argsBindFAExpr fa aa sem (FAsin a)      = FAsin   (argsBindFAExpr fa aa sem a)
argsBindFAExpr fa aa sem (FAcos a)      = FAcos   (argsBindFAExpr fa aa sem a)
argsBindFAExpr fa aa sem (FAtan a)      = FAtan   (argsBindFAExpr fa aa sem a)
argsBindFAExpr fa aa sem (FLn   a)      = FLn     (argsBindFAExpr fa aa sem a)
argsBindFAExpr fa aa sem (FExpo a)      = FExpo   (argsBindFAExpr fa aa sem a)
argsBindFAExpr _  _ _ i@(FInt _)     = i
argsBindFAExpr _  _ _ d@(FDouble _)  = d
argsBindFAExpr fa aa sem (FEFun f args) = FEFun f (map (argsBindFAExpr fa aa sem) args)
argsBindFAExpr _ _ _ FPi           = FPi
argsBindFAExpr fa aa sem (RtoS a)       = RtoS (argsBindAExpr fa aa sem a)
argsBindFAExpr fa aa sem (RtoD a)       = RtoD (argsBindAExpr fa aa sem a)

argsBindFBExpr :: [VarId] -> [FAExpr] -> ACebS -> FBExpr -> FBExpr
argsBindFBExpr fa aa sem (FAnd e1 e2) = FAnd (argsBindFBExpr fa aa sem e1) (argsBindFBExpr fa aa sem e2)
argsBindFBExpr fa aa sem (FOr e1 e2)  = FOr  (argsBindFBExpr fa aa sem e1) (argsBindFBExpr fa aa sem e2)
argsBindFBExpr fa aa sem (FNot e)     = FNot (argsBindFBExpr fa aa sem e)
argsBindFBExpr fa aa sem (FEq a1 a2)  = FEq  (argsBindFAExpr fa aa sem a1) (argsBindFAExpr fa aa sem a2)
argsBindFBExpr fa aa sem (FNeq a1 a2) = FNeq (argsBindFAExpr fa aa sem a1) (argsBindFAExpr fa aa sem a2)
argsBindFBExpr fa aa sem (FLt a1 a2)  = FLt  (argsBindFAExpr fa aa sem a1) (argsBindFAExpr fa aa sem a2)
argsBindFBExpr fa aa sem (FLtE a1 a2) = FLtE (argsBindFAExpr fa aa sem a1) (argsBindFAExpr fa aa sem a2)
argsBindFBExpr fa aa sem (FGt a1 a2)  = FGt  (argsBindFAExpr fa aa sem a1) (argsBindFAExpr fa aa sem a2)
argsBindFBExpr fa aa sem (FGtE a1 a2) = FGtE (argsBindFAExpr fa aa sem a1) (argsBindFAExpr fa aa sem a2)
argsBindFBExpr fa aa sem FBTrue = FBTrue
argsBindFBExpr fa aa sem FBFalse = FBFalse

argsBindStm :: [VarId] -> [FAExpr] -> ACebS -> Stm -> Stm
argsBindStm fa aa sem (Let x ae stm)     = Let x   (argsBindFAExpr fa aa sem ae) (argsBindStm fa aa sem stm)
argsBindStm fa aa sem (Ite be stmT stmE) = Ite     (argsBindFBExpr fa aa sem be) (argsBindStm fa aa sem stmT) (argsBindStm fa aa sem stmE)
argsBindStm fa aa sem (StmExpr ae)       = StmExpr (argsBindFAExpr fa aa sem ae)

testFilterCondFalse = do
    let reference = [ceb2]
    let res = filterCondFalse [ceb1,ceb2,ceb3]
    if res == reference
        then putStrLn "Ok"
        else putStrLn $ "Error " ++ show res ++ " /= expected " ++ show reference
    where
        ceb1 = dummyACeb { conds = Cond [(BFalse,FBTrue),(BTrue,FBFalse)] }
        ceb2 = dummyACeb { conds = Cond [(BFalse,FBTrue),(BTrue,FBTrue)] }
        ceb3 = dummyACeb { conds = Cond [(BFalse,FBFalse)] }




