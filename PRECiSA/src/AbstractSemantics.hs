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

import AbstractDomain
import Common.ControlFlow
import Common.DecisionPath
import AbsPVSLang
import qualified Data.List as List
import qualified Data.Set as Set
import Numeric
import Utils
import FPrec
import Foreign.C.Types (CDouble, CFloat)
import Data.Maybe
import Translation.Float2Real
import Debug.Trace

data SemanticConfiguration = SemConf {
    assumeTestStability :: Bool,
    mergeUnstables :: Bool
}

newtype Iteration = Iter Int deriving (Eq,Show,Num)

newtype Env a = Env [(VarName,a)] deriving (Show)

type LocalEnv = [(VarName, FAExpr)]

type FunctionInterpretation = (FunName, (FPrec, [Arg] ,ACebS))

type Interpretation = [FunctionInterpretation]

maxRoundOffError :: ACebS -> EExpr
maxRoundOffError acebs = eExpr $ foldl1 mergeACeb acebs

stableConditions :: ACebS -> [Condition]
stableConditions = concatMap (uncond . conds)

semantics :: FunctionInterpretation -> ACebS
semantics (_, (_, _ ,sem)) = sem

functionSemantics :: Interpretation -> FunName -> ACebS
functionSemantics interp f =
    case lookup f interp of
        Just (_, _, funSem) -> funSem
        Nothing -> error ("Function " ++ f ++ " not found")

emptyInterpretation :: Interpretation
emptyInterpretation = []

equivInterp :: Interpretation -> Interpretation -> Bool
equivInterp i1 i2 = setEq (map aux i1) (map aux i2)
    where
        aux (f, (fp, vars ,acebs)) = (f, (fp, vars, Set.fromList acebs))

errorNotGrowing :: Interpretation -> Interpretation -> Bool
errorNotGrowing [] _ = True
errorNotGrowing ((fName, (_,_,acebs)):current) next = localGrowingError && errorNotGrowing current next
    where
        localGrowingError = errorNotGrowingFun acebs (third $ fromJust $ lookup fName next)
        third (_,_,t) = t
     
errorNotGrowingFun :: ACebS -> ACebS -> Bool
errorNotGrowingFun current = all (`existsEquivError` current)

existsEquivError :: ACeb -> ACebS -> Bool
existsEquivError aceb iter = result
    where 
        result = any hasEquivError iter
        ACeb{conds = cc, eExpr = ee} = aceb
        hasEquivError aceb' = equivEExpr ee (eExpr aceb') && cc /= conds aceb'

botInterp :: [Decl] -> Interpretation
botInterp = map buildBottomElem
    where
        buildBottomElem (Decl fp funName args _) = (funName, (fp, args, []))

emptyEnv :: Env a
emptyEnv = Env []

insertEnv :: VarName -> a -> Env a -> Env a
insertEnv var a (Env env) =
  case lookup var env of 
    Just _ -> error "insertVarEnv: Variable already present in the environment"
    Nothing -> Env $ (var, a):env

addCond :: Condition -> ACeb -> ACeb
addCond (be,fbe) ceb@ACeb{ conds = Cond cs } = ceb{conds = Cond (map aux cs)}
    where 
        aux (rc,fc) = (simplBExprFix $ And rc be, simplFBExprFix $  FAnd fc fbe)

addCondS :: Condition -> ACebS -> ACebS
addCondS cond = map (addCond cond)

lub :: (ACebS -> ACeb) -> [ACebS] -> ACebS 
lub fun cs = filterCondFalse $ map fun (combos cs)

filterCondFalse :: ACebS -> ACebS
filterCondFalse = filter aux
    where
        isFalseCondition (b,fb) = isBExprEquivFalse b || isFBExprEquivFalse fb
        aux ACeb{ conds = Cond cs } = not $ all isFalseCondition cs

fixpointSemantics :: Program -> Interpretation -> Iteration -> SemanticConfiguration -> TargetDPs -> Interpretation
fixpointSemantics pgm interp max_ite = iterateImmediateConsequence pgm interp max_ite 0

iterateImmediateConsequence :: Program -> Interpretation -> Iteration -> Iteration -> SemanticConfiguration -> TargetDPs -> Interpretation
iterateImmediateConsequence pgm interp max_ite n semConf dps
    | n == max_ite                    = widening interp 
    | equivInterp interp nextIter     = interp 
    | errorNotGrowing interp nextIter = nextIter 
    | otherwise                       = iterateImmediateConsequence pgm nextIter max_ite (n + 1) semConf dps
        where
            nextIter = immediateConsequence pgm interp semConf dps

widening :: Interpretation -> Interpretation 
widening = map convergeToTop
    where
        convergeToTop (funName, (fp, args, _)) = (funName, (fp, args, [topAceb]))
        topAceb = ACeb {
            conds  = Cond [(BTrue, FBTrue)],
            rExprs = [] ,
            fpExprs = [],
            eExpr  = Infinity,
            decisionPath = root,
            cFlow  = Stable
        }

immediateConsequence :: Program -> Interpretation -> SemanticConfiguration -> TargetDPs -> Interpretation
immediateConsequence decls interps semConf decPaths = foldl (\interp decl -> declSem decl interp decPaths) interps decls
    where
        declSem :: Decl -> Interpretation -> TargetDPs -> Interpretation
        declSem (Decl _ fun _ stm) interp dps = addDeclInterp fun (stmSem stm interp emptyEnv semConf root dps fun) interp

addDeclInterp :: FunName -> ACebS -> Interpretation -> Interpretation                       
addDeclInterp fun sem interp =
    let (l1,l2) = List.partition (isFun fun) interp in
        case l1 of 
          [] -> error $ "addDeclInterp: function " ++ show fun ++ " not found."
          [(_, (fp, args, cebs))] -> 
                if hasInfiniteError cebs then
                    l2 ++ [(fun, (fp, args, replaceInfFun sem))]
                else
                    l2 ++ [(fun, (fp, args,  unionACebS cebs sem))]
          _ -> error ("addDeclInterp: More than one occurrence of function " ++ fun ++ " in the interpretation.")    
    where
        isFun f1 (f2, (_,_,_)) = f1 == f2
        hasInfiniteError = any isErrorInfinite
        replaceInfFun = map replaceInfFun'
        replaceInfFun' ceb =  ceb { rExprs = [Infinity] }
        isErrorInfinite aceb = ee == Infinity
            where 
                ACeb{ eExpr = ee} = aceb

isDecPathOfInterest :: (Eq a, Show a) => a -> [(a, [LDecisionPath])] -> ACeb -> Bool
isDecPathOfInterest fun dps ACeb{ decisionPath = dp' }
    = case lookup fun dps of 
        Just dpsfun -> existsPrefixInList dp' dpsfun
        Nothing -> error ("function " ++ show fun ++ " not found in Path of Interests.")

stmSem :: Stm -> Interpretation -> Env ACebS -> SemanticConfiguration -> LDecisionPath -> TargetDPs -> FunName -> ACebS

stmSem UnstWarning _ _ _ dp _ _ = [ ACeb {
    conds  = Cond [(BTrue,FBTrue)],
    rExprs = [Int 0],
    fpExprs = [FInt 0],
    eExpr  = ErrRat 0,
    decisionPath = dp,
    cFlow  = Stable
    } ]

stmSem (StmExpr aexpr) interp env _ dp _ fun = aexprSem aexpr interp env dp fun

stmSem (Let var t aexpr stm) interp env semConf dp dps fun =
  stmSem newStm interp newEnv semConf dp dps fun
  where
    newEnv = insertEnv var sem env
    newStm = argsBindStm [Arg var t] [aexpr] sem stm      
    sem = aexprSem aexpr interp env dp fun

stmSem (Ite fbe stm1 stm2) interp env semConf@SemConf{ assumeTestStability = sta, mergeUnstables = mu } dp dps fun
  | sta = stableCases
  | mu = case mergedUnstableCase of
            Nothing -> stableCases
            Just uc -> uc : stableCases
  | otherwise = unstableCases ++ stableCases
  where
    be = fbe2be fbe
    sem1 = stmSem stm1 interp env semConf (dp ~> 0)  dps fun
    sem2 = stmSem stm2 interp env semConf (dp ~> 1) dps fun

    sem1Stable = filter isStable sem1
    sem2Stable = filter isStable sem2

    stableCases'   = addCondS (be, fbe) sem1 ++ addCondS (Not be, FNot fbe) sem2
    unstableCases' = addCondS (be,FNot fbe) (unTestSem (representative realEquivalence sem1Stable) (representative fpEquivalence sem2Stable)) ++
                     addCondS (Not be, fbe) (unTestSem (representative realEquivalence sem2Stable) (representative fpEquivalence sem1Stable))
    
    (stableOfInterest, stableNotOfInterest) = List.partition (isDecPathOfInterest fun dps) (filter isStable stableCases')
    
    unstableCases = filter isUnstable (stableCases' ++ unstableCases')

    stableCases   = if null stableNotOfInterest  then stableOfInterest else mergeACebFold stableNotOfInterest:stableOfInterest
    mergedUnstableCase  = if null unstableCases  then Nothing else Just $ mergeACebFold unstableCases

stmSem (ListIte listThen stmElse) interp env semConf@SemConf { assumeTestStability = sta, mergeUnstables = mu } dp dps fun
  | sta = stableCases
  | mu = case mergedUnstableCase of
           Nothing -> stableCases
           Just uc -> uc : stableCases
  | otherwise = unstableCases ++ stableCases
  where
    n = fromIntegral $ length listThen

    semElse = stmSem stmElse interp env semConf (dp ~> n) dps fun

    buildStableCases [] _ neg_bes neg_fbes =
      addCondS (neg_bes, neg_fbes) (stmSem  stmElse interp env semConf (dp ~> n) dps fun)
        
    buildStableCases ((fbe_i,stm_i):restThen) i neg_bes neg_fbes =
      addCondS (And (fbe2be fbe_i) neg_bes, FAnd fbe_i neg_fbes) (stmSem stm_i interp env semConf (dp ~> i) dps fun)
      ++ 
      buildStableCases restThen (i+1) (And (Not (fbe2be fbe_i)) neg_bes) (FAnd (FNot fbe_i) neg_fbes)

    stableCases' = buildStableCases listThen 0 BTrue FBTrue

    (stableOfInterest, stableNotOfInterest) = List.partition (isDecPathOfInterest fun dps) (filter isStable stableCases')

    stableCases = if null stableNotOfInterest then stableOfInterest else mergeACebFold stableNotOfInterest:stableOfInterest

    buildThenCasesSem [] _ = []

    buildThenCasesSem ((_,stm_i):listStm) i =
            stmSem stm_i interp env semConf (dp ~> i) dps fun :
            buildThenCasesSem listStm (i+1)

    semListStable = map (filter isStable) (buildThenCasesSem listThen 0)

    semWithGuards = zip (map fst listThen) semListStable ++ [(FBTrue, semElse)]

    fpUnstableGuards i = FAnd (guards !! i) negGuards
      where
        guards = map fst semWithGuards
        negGuards = foldl FAnd FBTrue (map FNot (take i guards))

    realUnstableGuards i = And (fbe2be $ guards !! i) negGuards
      where
        guards = map fst semWithGuards
        negGuards = foldl And BTrue (map (Not . fbe2be) (take i guards))

    unstableCases' =
            concat [addCondS (realUnstableGuards i, fpUnstableGuards j)
                      (unTestSem (representative realEquivalence (snd (semWithGuards !! i)))
                                 (representative fpEquivalence   (snd (semWithGuards !! j)))) | (i,j) <- pairCombinations (length semWithGuards)]
        
    unstableCases = filter isUnstable (stableCases' ++ unstableCases')
    mergedUnstableCase  = if null unstableCases then Nothing else Just $ mergeACebFold unstableCases


stmSem ForLoop{} _ _ _ _ _ _ = undefined


representative :: (b -> b -> Bool) -> [b] -> [b]
representative equiv sem = map head (partition equiv sem)

realEquivalence :: ACeb -> ACeb -> Bool
realEquivalence
  ACeb { conds = Cond cs1 }
  ACeb { conds = Cond cs2 }
  = setEq rCs1 rCs2
  where
    rCs1 = map realCond cs1
    rCs2 = map realCond cs2


fpEquivalence :: ACeb -> ACeb -> Bool
fpEquivalence
  ACeb { conds = Cond cs1 }
  ACeb { conds = Cond cs2 }
  = setEq fpCs1 fpCs2
  where
    fpCs1 = map fpCond cs1
    fpCs2 = map fpCond cs2

partition :: (a -> a -> Bool) -> [a] -> [[a]]
partition _ [] = []
partition eqRel xs@(x:_) = elems : partition eqRel rest
  where
    (elems, rest) = List.partition (eqRel x) xs


makeUnstable :: ACeb -> ACeb -> ACeb
makeUnstable
  ACeb { conds = Cond rCs,  rExprs = rRs }
  ACeb { conds = Cond fpCs, rExprs = fpRs, fpExprs = fpFs, eExpr = fpE, decisionPath = dp}
  = ACeb {
      conds  = Cond cs',
      rExprs = rRs,
      fpExprs = fpFs,
      eExpr  = ee',
      decisionPath = dp,
      cFlow  = Unstable
  }
  where
    ee' = maxErr [Add fpE (Abs (Sub fpR rR)) | fpR <- fpRs, rR <- rRs]
    cs' = [(rCond, fCond) | rCond <- map realCond rCs, fCond <- map fpCond fpCs]


unTestSem :: ACebS -> ACebS -> ACebS
unTestSem semR semFP  = map (uncurry makeUnstable) combinations
  where
    combinations = [(cebR,cebFP) | cebR <- semR, cebFP <- semFP]

maxErr :: [EExpr] -> EExpr
maxErr [] = error $ "maxError cannot be applied to an empty list."
maxErr [err] = err
maxErr listErr = MaxErr listErr

--------------------------------------
-- Arithmetic Expressions Semantics --
--------------------------------------

aexprSem :: FAExpr -> Interpretation -> Env ACebS -> LDecisionPath -> FunName -> ACebS

aexprSem (RtoS (Rat n)) _ _ dp _ = [ ACeb {
  conds  = Cond [(BTrue,FBTrue)],
  rExprs = [Rat n],
  fpExprs = [FCnst FPSingle n],
  eExpr  = ErrRat $ abs $ toRational (fromRat n :: CFloat) - (n :: Rational),
  decisionPath = dp,
  cFlow  = Stable
  } ]

aexprSem (RtoS (Int n)) _ _ dp _ = [ ACeb {
  conds  = Cond [(BTrue,FBTrue)],
  rExprs = [Int n],
  fpExprs = [FInt n],
  eExpr  = ErrRat 0,
  decisionPath = dp,
  cFlow  = Stable
  } ]

aexprSem (RtoD (Int n)) _ _ dp _ = [ ACeb {
  conds  = Cond [(BTrue,FBTrue)],
  rExprs = [Int n],
  fpExprs = [FInt n],
  eExpr  = ErrRat 0,
  decisionPath = dp,
  cFlow  = Stable
  } ]

aexprSem (RtoD (Rat n)) _ _ dp _ = [ ACeb {
  conds  = Cond [(BTrue,FBTrue)],
  rExprs = [Rat n],
  fpExprs = [FCnst FPDouble n],
  eExpr  = ErrRat $ abs $ toRational (fromRat n :: CDouble) - (n :: Rational),
  decisionPath = dp,
  cFlow  = Stable
  } ]

aexprSem (RtoS (Neg (Int n))) _ _ dp _ = [ ACeb {
  conds  = Cond [(BTrue,FBTrue)],
  rExprs = [Neg (Int n)],
  fpExprs = [FNeg FPSingle (FInt n)],
  eExpr  = ErrRat 0,
  decisionPath = dp,
  cFlow  = Stable
  } ]

aexprSem (RtoS (Neg (Rat n))) _ _ dp _ = [ ACeb {
  conds  = Cond [(BTrue,FBTrue)],
  rExprs = [Neg (Rat n)],
  fpExprs = [FNeg FPSingle (FCnst FPSingle n)],
  eExpr  = ErrRat $ abs $ toRational (fromRat (-n) :: Float) - ((-n) :: Rational),
  decisionPath = dp,
  cFlow  = Stable
  } ]

aexprSem (RtoD (Neg (Int n))) _ _ dp _ = [ ACeb {
  conds  = Cond [(BTrue,FBTrue)],
  rExprs = [Neg (Int n)],
  fpExprs = [FNeg FPDouble (FInt n)],
  eExpr  = ErrRat 0,
  decisionPath = dp,
  cFlow  = Stable
  } ]

aexprSem (RtoD (Neg (Rat n))) _ _ dp _ = [ ACeb {
  conds  = Cond [(BTrue,FBTrue)],
  rExprs = [Neg (Rat n)],
  fpExprs = [FNeg FPDouble (FCnst FPDouble n)],
  eExpr  = ErrRat $ abs $ toRational (fromRat (-n) :: Double) - ((-n) :: Rational),
  decisionPath = dp,
  cFlow  = Stable
  } ]

aexprSem (FInt n) _ _ dp _ = [ ACeb {
  conds  = Cond [(BTrue,FBTrue)],
  rExprs = [Int n],
  fpExprs = [FInt n],
  eExpr  = ErrRat 0,
  decisionPath = dp,
  cFlow  = Stable
  } ]

aexprSem (FCnst FPDouble n) _ _ dp _ = [ ACeb {
  conds  = Cond [(BTrue,FBTrue)],
  rExprs = [Rat n],
  fpExprs = [FCnst FPDouble n],
  eExpr  = ErrRat $ abs $ toRational (fromRat n :: CDouble) - (n :: Rational),
  decisionPath = dp,
  cFlow  = Stable
  } ]

aexprSem (FCnst FPSingle n) _ _ dp _ = [ ACeb {
 conds  = Cond [(BTrue,FBTrue)],
 rExprs = [Rat n],
 fpExprs = [FCnst FPSingle n],
 eExpr  = ErrRat $ abs $ toRational (fromRat n :: CFloat) - (n :: Rational),
 decisionPath = dp,
 cFlow  = Stable
 } ]


aexprSem (FVar fp x) _ (Env env) dp _ =
  fromMaybe
    [ACeb{conds = Cond [(BTrue, FBTrue)], rExprs = [RealMark x],
          fpExprs = [FVar fp x],
          eExpr = if fp == TInt then ErrRat 0 else ErrorMark x fp,
          decisionPath = dp, cFlow = Stable}]
    (lookup x env)

aexprSem (FEFun f _ actArgs) interp env dp fun =
  case lookup f interp of
    Just (_, formArgs, funSem) -> semEFun f formArgs actArgs (combos argSem) funSem (length funSem) dp
    -- 
    Nothing -> error ("Function " ++ f ++ " not found")
  where
    argSem = map (\arg -> aexprSem arg interp env dp fun) actArgs


aexprSem (StoD a) interp env dp fun = collapseSem $ lub semStoD [aexprSem a interp env dp fun]
  where
    semStoD :: ACebS -> ACeb
    semStoD [ceb] = ceb {
                rExprs  = rExprs ceb,
                fpExprs = [StoD f | f <- fpExprs ceb],
                eExpr  = maxErr [ErrStoD r (eExpr ceb)| r <- rExprs ceb]
            }
    semStoD _ = error "ErrStoD: something went wrong"
   
aexprSem (DtoS a) interp env dp fun = collapseSem $ lub semStoD [aexprSem a interp env dp fun]
  where
    semStoD :: ACebS -> ACeb
    semStoD [ceb] = ceb {
                rExprs  = rExprs ceb,
                fpExprs = [DtoS f | f <- fpExprs ceb],
                eExpr  = maxErr [ErrDtoS r (eExpr ceb)| r <- rExprs ceb]
            }
    semStoD _ = error "ErrDtoS: something went wrong"

aexprSem (ItoS a) interp env dp fun = collapseSem $ lub semItoS [aexprSem a interp env dp fun]
  where
    semItoS :: ACebS -> ACeb
    semItoS [ceb] = ceb {
                rExprs  = rExprs ceb,
                fpExprs = [ItoS f | f <- fpExprs ceb],
                eExpr  = maxErr [ErrItoS r (eExpr ceb)| r <- rExprs ceb]
            }
    semItoS _ = error "ErrItoD: something went wrong"

aexprSem (ItoD a) interp env dp fun = collapseSem $ lub semItoD [aexprSem a interp env dp fun]
  where
    semItoD :: ACebS -> ACeb
    semItoD [ceb] = ceb {
                rExprs  = rExprs ceb,
                fpExprs = [ItoD f | f <- fpExprs ceb],
                eExpr  = maxErr [ErrItoD r (eExpr ceb)| r <- rExprs ceb]
            }
    semItoD _ = error "ErrItoD: something went wrong"

aexprSem (FAdd fp a1 a2) interp env dp fun =
  collapseSem $ lub semAdd [aexprSem a1 interp env dp fun, aexprSem a2 interp env dp fun]
  where
    semAdd :: ACebS -> ACeb
    semAdd [ceb1,ceb2] = 
        ACeb {
            conds  = Cond [ (simplBExprFix $ And c1 c2, simplFBExprFix $ FAnd g1 g2) |
                             (c1, g1) <- uncond (conds ceb1), (c2, g2) <- uncond (conds ceb2)],
            rExprs = [Add r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            fpExprs = [FAdd fp f1 f2 | f1 <- fpExprs ceb1, f2 <- fpExprs ceb2],
            eExpr   = maxErr [ErrAdd fp r1 (eExpr ceb1) r2 (eExpr ceb2) | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            decisionPath = root,
            cFlow  = mergeControlFlow (cFlow ceb1) (cFlow ceb2)
        }
    semAdd _ = error "semAdd: something went wrong"

aexprSem (FIAdd a1 a2) interp env dp fun =
  collapseSem $ lub semIAdd [aexprSem a1 interp env dp fun, aexprSem a2 interp env dp fun]
  where
    semIAdd :: ACebS -> ACeb
    semIAdd [ceb1,ceb2] =
        ACeb {
            conds  = Cond [ (simplBExprFix $ And c1 c2, simplFBExprFix $ FAnd g1 g2) |
                             (c1, g1) <- uncond (conds ceb1), (c2, g2) <- uncond (conds ceb2)],
            rExprs = [Add r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            fpExprs = [FIAdd f1 f2 | f1 <- fpExprs ceb1, f2 <- fpExprs ceb2],
            eExpr  = maxErr [ErrAdd TInt r1 (eExpr ceb1) r2 (eExpr ceb2) | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            decisionPath = root,
            cFlow  = mergeControlFlow (cFlow ceb1) (cFlow ceb2)
        }
    semIAdd _ = error "semIAdd: something went wrong"

aexprSem (FSub fp a1 a2) interp env dp fun =
  collapseSem $ lub semSub [aexprSem a1 interp env dp fun, aexprSem a2 interp env dp fun]
  where
    semSub :: ACebS -> ACeb
    semSub [ceb1,ceb2] =
        ACeb {
            conds  = Cond [ (simplBExprFix $ And c1 c2, simplFBExprFix $ FAnd g1 g2) |
                       (c1, g1) <- uncond (conds ceb1), (c2, g2) <- uncond (conds ceb2)],
            rExprs = [Sub r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            fpExprs = [FSub fp f1 f2 | f1 <- fpExprs ceb1, f2 <- fpExprs ceb2],
            eExpr  = maxErr [ErrSub fp r1 (eExpr ceb1) r2 (eExpr ceb2) | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            decisionPath = root,
            cFlow  = mergeControlFlow (cFlow ceb1) (cFlow ceb2)
        }
    semSub _ = error "semSub: something went wrong"

aexprSem (FISub a1 a2) interp env dp fun =
  lub semISub [aexprSem a1 interp env dp fun, aexprSem a2 interp env dp fun]
  where
    semISub :: ACebS -> ACeb
    semISub [ceb1,ceb2] =
        ACeb {
            conds  = Cond [ (simplBExprFix $ And c1 c2, simplFBExprFix $ FAnd g1 g2) |
                       (c1, g1) <- uncond (conds ceb1), (c2, g2) <- uncond (conds ceb2)],
            rExprs = [Sub r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            fpExprs = [FISub f1 f2 | f1 <- fpExprs ceb1, f2 <- fpExprs ceb2],
            eExpr  = maxErr [ErrSub TInt r1 (eExpr ceb1) r2 (eExpr ceb2) | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            decisionPath = root,
            cFlow  = mergeControlFlow (cFlow ceb1) (cFlow ceb2)
        }
    semISub _ = error "semISub: something went wrong"

aexprSem ae@(FMul fp a1 a2) interp env dp fun =
  case pow2Mul ae of
      Just (Left  (pow2, a)) -> collapseSem $ lub semMulPow2L [aexprSem pow2 interp env dp fun, aexprSem a    interp env dp fun]
      Just (Right (pow2, a)) -> collapseSem $ lub semMulPow2R [aexprSem a    interp env dp fun, aexprSem pow2 interp env dp fun]
      Nothing                -> collapseSem $ lub semMul [aexprSem a1 interp env dp fun, aexprSem a2 interp env dp fun]
  where
    pow2Mul :: FAExpr -> Maybe (Either (FAExpr,FAExpr) (FAExpr,FAExpr))
    pow2Mul (FMul _ pow2@(FInt n)         a) | isPow2 (fromInteger  n :: Double) = Just $ Left  (pow2, a)
    pow2Mul (FMul _ pow2@(FCnst _ n)      a) | isPow2 (fromRational n :: Double) = Just $ Left  (pow2, a)   
    pow2Mul (FMul _ a         pow2@(FInt n)) | isPow2 (fromInteger  n :: Double) = Just $ Right (pow2, a)
    pow2Mul (FMul _ a      pow2@(FCnst _ n)) | isPow2 (fromRational n :: Double) = Just $ Right (pow2, a)
    pow2Mul _ = Nothing

    semMul :: ACebS -> ACeb 
    semMul [ceb1,ceb2] =
        ACeb {
            conds  = Cond [(simplBExprFix $ And c1 c2, simplFBExprFix $ FAnd g1 g2) |
                       (c1, g1) <- uncond (conds ceb1), (c2, g2) <- uncond (conds ceb2)],
            rExprs = [Mul r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            fpExprs = [FMul fp f1 f2 | f1 <- fpExprs ceb1, f2 <- fpExprs ceb2],
            eExpr  = maxErr [ErrMul fp r1 (eExpr ceb1) r2 (eExpr ceb2) | r1 <- rExprs ceb1, r2 <- rExprs ceb2], 
            decisionPath = root,
            cFlow  = mergeControlFlow (cFlow ceb1) (cFlow ceb2)
        }
    semMul _ = error "semMul: something went wrong"

    semMulPow2L :: ACebS -> ACeb
    semMulPow2L [ceb1,ceb2] =
        ACeb {
            conds  = Cond [(simplBExprFix $ And c2 (Lt (Int n) (Sub Prec (FExp a2))), g2) |
                       (c2, g2) <- uncond (conds ceb2)],
            rExprs = [Mul r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            fpExprs = [FMul fp f1 f2 | f1 <- fpExprs ceb1, f2 <- fpExprs ceb2],
            eExpr  = ErrMulPow2L fp n (eExpr ceb2),
            decisionPath = root,
            cFlow  = mergeControlFlow (cFlow ceb1) (cFlow ceb2)
        }
        where
            n = getExp (rExprs ceb1)
            getExp [Int m] = round $ logBase (2 :: Double) (fromIntegral m)
            getExp [Rat m] = round $ logBase (2 :: Double) (realToFrac m)
            getExp _ = error "getExp: something went wrong"
    semMulPow2L _ = error "semMul: something went wrong"

    semMulPow2R :: ACebS -> ACeb
    semMulPow2R [ceb1,ceb2] =
        ACeb {
            conds  = Cond [(simplBExprFix $ And c1 (Lt (Int n) (Sub Prec (FExp a1))), g1) |
                       (c1, g1) <- uncond (conds ceb1)],
            rExprs = [Mul r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            fpExprs = [FMul fp f1 f2 | f1 <- fpExprs ceb1, f2 <- fpExprs ceb2],
            eExpr  = ErrMulPow2R fp n (eExpr ceb1),
            decisionPath = root,
            cFlow  = mergeControlFlow (cFlow ceb1) (cFlow ceb2)
        }
        where
            n = getExp (rExprs ceb2)
            getExp [Int m] = round $ logBase (2 :: Double) (fromIntegral m)
            getExp [Rat m] = round $ logBase (2 :: Double) (realToFrac m)
            getExp _ = error "getExp: something went wrong"
    semMulPow2R _ = error "semMul: something went wrong"


aexprSem (FIMul a1 a2) interp env dp fun =
  collapseSem $ lub semIMul [aexprSem a1 interp env dp fun, aexprSem a2 interp env dp fun]
  where
    semIMul :: ACebS -> ACeb
    semIMul [ceb1,ceb2] =
        ACeb {
            conds  = Cond [ (simplBExprFix $ And c1 c2, simplFBExprFix $ FAnd g1 g2) |
                       (c1, g1) <- uncond (conds ceb1), (c2, g2) <- uncond (conds ceb2)],
            rExprs = [Mul r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            fpExprs = [FIMul f1 f2 | f1 <- fpExprs ceb1, f2 <- fpExprs ceb2],
            eExpr  = maxErr [ErrMul TInt r1 (eExpr ceb1) r2 (eExpr ceb2) | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            decisionPath = root,
            cFlow  = mergeControlFlow (cFlow ceb1) (cFlow ceb2)
        }
    semIMul _ = error "aexprSem semIMul: something went wrong"

aexprSem (FFma fp a1 a2 a3) interp env dp fun =
  lub semFma [aexprSem a1 interp env dp fun, aexprSem a2 interp env dp fun, aexprSem a3 interp env dp fun]
  where
    semFma :: ACebS -> ACeb
    semFma [ceb1,ceb2,ceb3] =
        ACeb {
            conds  = Cond [ (simplBExprFix $ And c1 (And c2 c3), simplFBExprFix $ FAnd g1 (FAnd g2 g3)) |
                       (c1, g1) <- uncond (conds ceb1),
                       (c2, g2) <- uncond (conds ceb2),
                       (c3, g3) <- uncond (conds ceb3)],
            rExprs  = [Fma r1 r2 r3 | r1 <- rExprs ceb1, r2 <- rExprs ceb2, r3 <- rExprs ceb3],
            fpExprs = [FFma fp f1 f2 f3 | f1 <- fpExprs ceb1, f2 <- fpExprs ceb2, f3 <- fpExprs ceb3],
            eExpr  = maxErr [ErrFma fp r1 (eExpr ceb1) r2 (eExpr ceb2) r3 (eExpr ceb3) | r1 <- rExprs ceb1, r2 <- rExprs ceb2, r3 <- rExprs ceb3],
            decisionPath = root,
            cFlow  = mergeControlFlow (cFlow ceb1) (mergeControlFlow (cFlow ceb2) (cFlow ceb3))
        }
    semFma _ = error "aexprSem semISub: something went wrong"

aexprSem (FDiv fp a1 a2) interp env dp fun =
  collapseSem $ lub semDiv [aexprSem a1 interp env dp fun, aexprSem a2 interp env dp fun]
  where
    semDiv :: ACebS -> ACeb 
    semDiv [ceb1,ceb2] =
        ACeb {
            conds  = Cond [(simplBExprFix  $  And (And c1 c2) (Neq r2 (Int 0)), -- (Neq (f2r a2) (Int 0))
                                             -- (Or (Lt (Add r2 (EE (eExpr ceb2))) (Int 0)) (Gt (Sub r2 (EE (eExpr ceb2))) (Int 0))),
                        simplFBExprFix $ FAnd (FAnd g1 g2) (FNeq a2 (FInt 0))) |
                       (c1, g1) <- uncond (conds ceb1), (c2, g2) <- uncond (conds ceb2), r2 <- rExprs ceb2],
            rExprs = [Div r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            fpExprs = [FDiv fp f1 f2 | f1 <- fpExprs ceb1, f2 <- fpExprs ceb2],
            eExpr  = maxErr [ErrDiv fp r1 (eExpr ceb1) r2 (eExpr ceb2) | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            decisionPath = root,
            cFlow  = mergeControlFlow (cFlow ceb1) (cFlow ceb2)
        }
    semDiv _ = error "aexprSem semDiv: something went wrong."

aexprSem (FIDiv a1 a2) interp env dp fun =
  collapseSem $ lub semIDiv [aexprSem a1 interp env dp fun, aexprSem a2 interp env dp fun]
  where
    semIDiv :: ACebS -> ACeb 
    semIDiv [ceb1,ceb2] =
        ACeb {
            conds  = Cond [(simplBExprFix  $ And (And (And (And c1 c2) (Neq r2 (Int 0))) (Neq (fae2real a2) (Int 0)))
                                             (Or (Lt (Add r2 (eExpr ceb2)) (Int 0)) (Gt (Sub r2 (eExpr ceb2)) (Int 0))),
                        simplFBExprFix $ FAnd g1 g2) |
                       (c1, g1) <- uncond (conds ceb1), (c2, g2) <- uncond (conds ceb2), r2 <- rExprs ceb2],
            rExprs = [IDiv r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            fpExprs = [FIDiv f1 f2 | f1 <- fpExprs ceb1, f2 <- fpExprs ceb2],
            eExpr  = maxErr [ErrDiv TInt r1 (eExpr ceb1) r2 (eExpr ceb2) | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            decisionPath = root,
            cFlow  = mergeControlFlow (cFlow ceb1) (cFlow ceb2)
        }
    semIDiv _ = error "aexprSem semIDiv: something went wrong"

aexprSem (FItDiv a1 a2) interp env dp fun =
  collapseSem $ lub semItDiv [aexprSem a1 interp env dp fun, aexprSem a2 interp env dp fun]
  where
    semItDiv :: ACebS -> ACeb 
    semItDiv [ceb1,ceb2] =
        ACeb {
            conds  = Cond [(simplBExprFix  $ And (And (And (And c1 c2) (Neq r2 (Int 0))) (Neq (fae2real a2) (Int 0)))
                                             (Or (Lt (Add r2 (eExpr ceb2)) (Int 0)) (Gt (Sub r2 (eExpr ceb2)) (Int 0))),
                        simplFBExprFix $ FAnd g1 g2) |
                       (c1, g1) <- uncond (conds ceb1), (c2, g2) <- uncond (conds ceb2), r2 <- rExprs ceb2],
            rExprs = [ItDiv r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            fpExprs = [FItDiv f1 f2 | f1 <- fpExprs ceb1, f2 <- fpExprs ceb2],
            eExpr  = maxErr [ErrItDiv TInt r1 (eExpr ceb1) r2 (eExpr ceb2) | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            decisionPath = root,
            cFlow  = mergeControlFlow (cFlow ceb1) (cFlow ceb2)
        }
    semItDiv _ = error "aexprSem semItDiv: something went wrong"

aexprSem (FIMod a1 a2) interp env dp fun =
  collapseSem $ lub semIMod [aexprSem a1 interp env dp fun, aexprSem a2 interp env dp fun]
  where
    semIMod :: ACebS -> ACeb 
    semIMod [ceb1,ceb2] =
        ACeb {
            conds  = Cond [(simplBExprFix  $ And (And (And (And c1 c2) (Neq r2 (Int 0))) (Neq (fae2real a2) (Int 0)))
                                             (Or (Lt (Add r2 (eExpr ceb2)) (Int 0)) (Gt (Sub r2 (eExpr ceb2)) (Int 0))),
                        simplFBExprFix $ FAnd g1 g2) |
                       (c1, g1) <- uncond (conds ceb1), (c2, g2) <- uncond (conds ceb2), r2 <- rExprs ceb2],
            rExprs = [IMod r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            fpExprs = [FIMod f1 f2 | f1 <- fpExprs ceb1, f2 <- fpExprs ceb2],
            eExpr  = maxErr [ErrMod TInt r1 (eExpr ceb1) r2 (eExpr ceb2) | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            decisionPath = root,
            cFlow  = mergeControlFlow (cFlow ceb1) (cFlow ceb2)
        }
    semIMod _ = error "aexprSem semIMod: something went wrong"

aexprSem (FItMod a1 a2) interp env dp fun =
  collapseSem $ lub semItMod [aexprSem a1 interp env dp fun, aexprSem a2 interp env dp fun]
  where
    semItMod :: ACebS -> ACeb 
    semItMod [ceb1,ceb2] =
        ACeb {
            conds  = Cond [(simplBExprFix  $ And (And (And (And c1 c2) (Neq r2 (Int 0))) (Neq (fae2real a2) (Int 0)))
                                             (Or (Lt (Add r2 (eExpr ceb2)) (Int 0)) (Gt (Sub r2 (eExpr ceb2)) (Int 0))),
                        simplFBExprFix $ FAnd g1 g2) |
                       (c1, g1) <- uncond (conds ceb1), (c2, g2) <- uncond (conds ceb2), r2 <- rExprs ceb2],
            rExprs = [IMod r1 r2 | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            fpExprs = [FItMod f1 f2 | f1 <- fpExprs ceb1, f2 <- fpExprs ceb2],
            eExpr  = maxErr [ErrItMod TInt r1 (eExpr ceb1) r2 (eExpr ceb2) | r1 <- rExprs ceb1, r2 <- rExprs ceb2],
            decisionPath = root,
            cFlow  = mergeControlFlow (cFlow ceb1) (cFlow ceb2)
        }
    semItMod _ = error "aexprSem semItMod: something went wrong"

aexprSem (FNeg fp a) interp env dp fun = collapseSem $ lub semNeg [aexprSem a interp env dp fun]
  where
    semNeg :: ACebS -> ACeb
    semNeg [ceb] = ceb {
                rExprs  = [Neg     r | r <-  rExprs ceb],
                fpExprs = [FNeg fp f | f <- fpExprs ceb],
                eExpr  = maxErr [ErrNeg fp r (eExpr ceb)| r <- rExprs ceb]
            }
    semNeg _ = error "aexprSem semNeg: something went wrong"


aexprSem (FINeg a) interp env dp fun = collapseSem $ lub semINeg [aexprSem a interp env dp fun]
  where
    semINeg :: ACebS -> ACeb
    semINeg [ceb] = ceb {
                rExprs  = [Neg  r | r <-  rExprs ceb],
                fpExprs = [FINeg f | f <- fpExprs ceb],
                eExpr  = maxErr [ErrNeg TInt r (eExpr ceb)| r <- rExprs ceb]
            }
    semINeg _ = error "aexprSem semINeg: something went wrong"

aexprSem (FAbs fp a) interp env dp fun = collapseSem $ lub semAbs [aexprSem a interp env dp fun]
  where
    semAbs :: ACebS -> ACeb
    semAbs [ceb] = ceb {
                rExprs  = [Abs r  | r <-  rExprs ceb],
                fpExprs = [FAbs fp f | f <- fpExprs ceb],
                eExpr  = maxErr [ErrAbs fp r (eExpr ceb)| r <- rExprs ceb]
            }
    semAbs _ = error "aexprSem semAbs: something went wrong"

aexprSem (FIAbs a) interp env dp fun = collapseSem $ lub semIAbs [aexprSem a interp env dp fun]
  where
    semIAbs :: ACebS -> ACeb
    semIAbs [ceb] = ceb {
                rExprs  = [Abs  r | r <-  rExprs ceb],
                fpExprs = [FIAbs f | f <- fpExprs ceb],
                eExpr  = maxErr [ErrAbs TInt r (eExpr ceb)| r <- rExprs ceb]
            }
    semIAbs _ = error "aexprSem semIAbs: something went wrong"

aexprSem (FLn fp a) interp env dp fun = collapseSem $ lub semLn [aexprSem a interp env dp fun]
  where
    semLn :: ACebS -> ACeb
    semLn [ceb] = ceb {
                conds = Cond [(And (And rc (Lt (Int 0)(Sub r (eExpr ceb))))
                               (Gt (f2r a) (Int 0)),
                               fc) | (rc,fc) <- uncond (conds ceb), r <- rExprs ceb],
                rExprs  = [Ln  r | r <-  rExprs ceb],
                fpExprs = [FLn fp f | f <- fpExprs ceb],
                eExpr  = maxErr [ErrLn fp r (eExpr ceb)| r <- rExprs ceb]
            }
    semLn _ = error "aexprSem semLn: something went wrong"
    f2r = case fp of
            FPSingle -> StoR
            FPDouble -> DtoR
            t -> error $ "f2r not defined for "++ show t

aexprSem (FExpo fp a) interp env dp fun = collapseSem $ lub semExpo [aexprSem a interp env dp fun]
  where
    semExpo :: ACebS -> ACeb
    semExpo [ceb] = ceb {
                rExprs  = [Expo r  | r <-  rExprs ceb],
                fpExprs = [FExpo fp f | f <- fpExprs ceb],
                eExpr  = maxErr [ErrExpo fp r (eExpr ceb)| r <- rExprs ceb]
            }
    semExpo _ = error "aexprSem semExpo: something went wrong"


aexprSem (FFloor fp a) interp env dp fun =
  collapseSem (lub semFloor [aexprSem a interp env dp fun])
  ++
  collapseSem (lub semFloor0 [aexprSem a interp env dp fun])
  where
    semFloor :: ACebS -> ACeb
    semFloor [ceb] = ceb {
                conds = Cond [(And rc (Or (Neq (Floor r) (Floor (Sub r (eExpr ceb))))
                                     (Neq (Floor r) (Floor (Add r (eExpr ceb))))),
                          fc) | (rc,fc) <- uncond (conds ceb), r <- rExprs ceb],
                rExprs = [Floor r | r <- rExprs ceb],
                fpExprs = [FFloor fp f | f <- fpExprs ceb],
                eExpr  = maxErr [ErrFloor fp r (eExpr ceb)| r <- rExprs ceb]
            }
    semFloor _ = error "aexprSem semFloor: something went wrong"

    semFloor0 :: ACebS -> ACeb
    semFloor0 [ceb] = ceb {
                conds = Cond [(simplBExprFix $ And rc (And (Eq (Floor r) (Floor (Sub r (eExpr ceb))))
                                      (Eq (Floor r) (Floor (Add r (eExpr ceb))))),
                          fc) | (rc,fc) <- uncond (conds ceb), r <- rExprs ceb],
                rExprs = [Floor r | r <- rExprs ceb],
                fpExprs = [FFloor fp f | f <- fpExprs ceb],
                eExpr  = maxErr [ErrFloor0 fp r (eExpr ceb)| r <- rExprs ceb]
            }
    semFloor0 _ = error "aexprSem semFloor0: something went wrong"

aexprSem (FSqrt fp a) interp env dp fun = collapseSem $ lub semSqrt [aexprSem a interp env dp fun]
  where
    semSqrt :: ACebS -> ACeb
    semSqrt [ceb] = ceb {
                conds = Cond [(simplBExprFix $ And rc (GtE (Sub r (eExpr ceb)) (Int 0)),
                          fc) | (rc,fc) <- uncond (conds ceb), r <- rExprs ceb],
                rExprs = [Sqrt r | r <- rExprs ceb],
                fpExprs = [FSqrt fp f | f <- fpExprs ceb],
                eExpr  = maxErr [ErrSqrt fp r (eExpr ceb)| r <- rExprs ceb]
            }
    semSqrt _ = error "aexprSem semSqrt: something went wrong"

aexprSem (FSin fp a) interp env dp fun = collapseSem $ lub semSin [aexprSem a interp env dp fun]
  where
      semSin :: ACebS -> ACeb 
      semSin [ceb] = ceb {
                  rExprs = [Sin r | r <- rExprs ceb],
                  fpExprs = [FSin fp f | f <- fpExprs ceb],
                  eExpr  = maxErr [ErrSin fp r (eExpr ceb)| r <- rExprs ceb]
              }
      semSin _ = error "aexprSem semSin: something went wrong"

aexprSem (FCos fp a) interp env dp fun = collapseSem $ lub semCos [aexprSem a interp env dp fun]
  where
      semCos :: ACebS -> ACeb 
      semCos [ceb] = ceb {
                  rExprs = [Cos r | r <- rExprs ceb],
                  fpExprs = [FCos fp f | f <- fpExprs ceb],
                  eExpr  = maxErr [ErrCos fp r (eExpr ceb)| r <- rExprs ceb]
              }
      semCos _ = error "aexprSem semCos: something went wrong"

aexprSem (FAtan fp a) interp env dp fun =
     collapseSem (lub semAtan   [aexprSem a interp env dp fun])
  ++ collapseSem (lub semAtanT  [aexprSem a interp env dp fun])
  where 
      semAtan :: ACebS -> ACeb 
      semAtan [ceb] = ceb {
                  conds = Cond [(simplBExprFix $ And rc (GtE (eExpr ceb) (Abs r)),
                            fc) | (rc,fc) <- uncond (conds ceb), r <- rExprs ceb],
                  rExprs = [ATan r | r <- rExprs ceb],
                  fpExprs = [FAtan fp f | f <- fpExprs ceb],
                  eExpr  = maxErr [ErrAtan fp r (eExpr ceb)| r <- rExprs ceb]
              }
      semAtan _ = error "aexprSem semAtan: something went wrong"

      semAtanT :: ACebS -> ACeb 
      semAtanT [ceb] = ceb {
                  conds = Cond [(simplBExprFix $ And rc (Lt (eExpr ceb) (Abs r)),
                            fc) | (rc,fc) <- uncond (conds ceb), r <- rExprs ceb],
                  rExprs = [ATan r | r <- rExprs ceb],
                  fpExprs = [FAtan fp f | f <- fpExprs ceb],
                  eExpr  = maxErr [ErrAtanT fp r (eExpr ceb)| r <- rExprs ceb]
              }
      semAtanT _ = error "aexprSem semAtanT: something went wrong"

aexprSem fae _ _ _ _ = error $ "niy: aexprSem "++ show fae

semEFun :: FunName -> [Arg] -> [FAExpr] -> [ACebS] -> ACebS -> Int -> LDecisionPath -> ACebS
semEFun _ _ _ _ [] _ _ = []
semEFun fun formArgs actualArgs semArgsCombos (c:cs) n dp =
  map (aux c formArgs actualArgs) semArgsCombos 
  ++
  semEFun fun formArgs actualArgs semArgsCombos cs n dp
  where
    aux ceb fa aa argsSem =
      ceb {
          conds  = Cond $ map newcond $ combos [uncond (conds ceb), conjComboArgs],
          rExprs = map (argsBindAExpr fa aa argsSem) (rExprs ceb),
          eExpr  = argsBindAExpr fa aa argsSem (eExpr ceb)
      }
      where
        combosArgs = combos (map (uncond . conds) argsSem)
        conjComboArgs = zip (map condArgs combosArgs) (map guardArgs combosArgs)           
        newcond [(rc, fpc), (rcargs, fpcargs)] = (simplBExprFix  $ And  (argsBindBExpr fa aa argsSem rc) rcargs,
            simplFBExprFix $ FAnd (argsBindFBExpr fa aa fpc) fpcargs)
        newcond _ = error "semEFun newcond: something went wrong"
  
        condArgs  arg = foldl And  BTrue  (map realCond arg)
        guardArgs arg = foldl FAnd FBTrue (map fpCond   arg) 

collapseSem :: [ACeb] -> [ACeb]
collapseSem sem = collapsedStableCase ++ collapsedUnstableCase
  where
    collapsedStableCase   = if null stableCases   then [] else [mergeACebFold   stableCases]
    collapsedUnstableCase = if null unstableCases then [] else [mergeACebFold unstableCases]
    (stableCases, unstableCases) = List.partition isStable sem

unfoldLocalVars :: LocalEnv -> FAExpr -> FAExpr
unfoldLocalVars env = replaceInFAExpr (\a -> Nothing) (unfoldLocalVars' env)
unfoldLocalVars' env var@(FVar fp x) = unfoldLocalVars env <$> (lookup x env)
unfoldLocalVars' _ a  = Nothing


argsBindAExpr :: [Arg] -> [FAExpr] -> ACebS -> AExpr -> AExpr
argsBindAExpr fa aa _ (RealMark x) = bindRealMark fa aa
  where
    bindRealMark :: [Arg] -> [FAExpr] -> AExpr
    bindRealMark [] [] = Var Real x
    bindRealMark (Arg y _ :ys) (a:as) | y == x    = fae2real a
                                      | otherwise = bindRealMark ys as
    bindRealMark _ _ = error "bindErrorMark: something went wrong"
        
argsBindAExpr _  _  _   (Var fp x)    = Var fp x    
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
argsBindAExpr fa aa _   (StoR a)      = StoR   (argsBindFAExpr fa aa a)
argsBindAExpr fa aa _   (DtoR a)      = DtoR   (argsBindFAExpr fa aa a)
argsBindAExpr _ _ _ i@(Int _)         = i
argsBindAExpr _ _ _ r@(Rat _)         = r
argsBindAExpr fa aa sem (EFun f fp args) = EFun f fp (map (argsBindAExpr fa aa sem) args)
argsBindAExpr _  _  _   Prec          = Prec
argsBindAExpr fa aa _   (FExp fae)    = FExp (argsBindFAExpr fa aa fae)
-- argsBindAExpr _  _  _ a               = error $ "argsBindAExpr not defined for "++ show a

-- argsBindAExpr :: [Arg] -> [FAExpr] -> [ACeb] -> EExpr -> EExpr
argsBindAExpr _ _ _ (ErrRat n)    = ErrRat n
argsBindAExpr fa _ sem (ErrorMark x fp) = bindErrorMark fa sem
    where
        bindErrorMark :: [Arg] -> ACebS -> EExpr
        bindErrorMark [] [] = ErrorMark x fp
        bindErrorMark (Arg y _ :ys) (ceb:cebs) | x == y    = eExpr ceb
                                               | otherwise = bindErrorMark ys cebs
        bindErrorMark _ _ = error "bindErrorMark: something went wrong"


argsBindAExpr fa aa sem (ErrAdd fp r1 e1 r2 e2) = ErrAdd fp (argsBindAExpr fa aa sem r1) (argsBindAExpr fa aa sem e1)
                                                            (argsBindAExpr fa aa sem r2) (argsBindAExpr fa aa sem e2)
argsBindAExpr fa aa sem (ErrSub fp r1 e1 r2 e2) = ErrSub fp (argsBindAExpr fa aa sem r1) (argsBindAExpr fa aa sem e1)
                                                         (argsBindAExpr fa aa sem r2) (argsBindAExpr fa aa sem e2)
argsBindAExpr fa aa sem (ErrMul fp r1 e1 r2 e2) = ErrMul fp (argsBindAExpr fa aa sem r1) (argsBindAExpr fa aa sem e1)
                                                         (argsBindAExpr fa aa sem r2) (argsBindAExpr fa aa sem e2)
argsBindAExpr fa aa sem (ErrDiv fp r1 e1 r2 e2) = ErrDiv fp (argsBindAExpr fa aa sem r1) (argsBindAExpr fa aa sem e1)
                                                         (argsBindAExpr fa aa sem r2) (argsBindAExpr fa aa sem e2)
argsBindAExpr fa aa sem (ErrFloor  fp r e)      = ErrFloor  fp (argsBindAExpr fa aa sem r)  (argsBindAExpr fa aa sem e)
argsBindAExpr fa aa sem (ErrFloor0 fp r e)      = ErrFloor0 fp (argsBindAExpr fa aa sem r)  (argsBindAExpr fa aa sem e)
argsBindAExpr fa aa sem (ErrSqrt   fp r e)      = ErrSqrt   fp (argsBindAExpr fa aa sem r)  (argsBindAExpr fa aa sem e)
argsBindAExpr fa aa sem (ErrNeg    fp r e)      = ErrNeg    fp (argsBindAExpr fa aa sem r)  (argsBindAExpr fa aa sem e)
argsBindAExpr fa aa sem (ErrAbs    fp r e)      = ErrAbs    fp (argsBindAExpr fa aa sem r)  (argsBindAExpr fa aa sem e)
argsBindAExpr fa aa sem (ErrSin    fp r e)      = ErrSin    fp (argsBindAExpr fa aa sem r)  (argsBindAExpr fa aa sem e)
argsBindAExpr fa aa sem (ErrCos    fp r e)      = ErrCos    fp (argsBindAExpr fa aa sem r)  (argsBindAExpr fa aa sem e)
argsBindAExpr fa aa sem (ErrAtan   fp r e)      = ErrAtan   fp (argsBindAExpr fa aa sem r)  (argsBindAExpr fa aa sem e)
argsBindAExpr fa aa sem (ErrLn     fp r e)      = ErrLn     fp (argsBindAExpr fa aa sem r)  (argsBindAExpr fa aa sem e)
argsBindAExpr fa aa sem (ErrExpo   fp r e)      = ErrExpo   fp (argsBindAExpr fa aa sem r)  (argsBindAExpr fa aa sem e)
argsBindAExpr fa aa sem (ErrAtanT fp a e)       = ErrAtanT  fp (argsBindAExpr fa aa sem a)  (argsBindAExpr fa aa sem e)
argsBindAExpr fa aa sem (ErrMulPow2L fp n e)    = ErrMulPow2L fp n (argsBindAExpr fa aa sem e)
argsBindAExpr fa aa sem (ErrMulPow2R fp n e)    = ErrMulPow2R fp n (argsBindAExpr fa aa sem e)
argsBindAExpr fa aa sem (HalfUlp a fp)          = HalfUlp      (argsBindAExpr fa aa sem a) fp
argsBindAExpr fa aa sem (MaxErr es)             = MaxErr (map (argsBindAExpr fa aa sem) es)
argsBindAExpr _  _  _   Infinity                = Infinity
argsBindAExpr _  _  _   ErrUndefined            = ErrUndefined
argsBindAExpr _  _  _   ee = error $ "argsBindAExpr not defined for " ++ show ee


argsBindBExpr :: [Arg] -> [FAExpr] -> ACebS -> BExpr -> BExpr
argsBindBExpr fa aa sem (And e1 e2) = And (argsBindBExpr fa aa sem e1) (argsBindBExpr fa aa sem e2)
argsBindBExpr fa aa sem (Or e1 e2)  = Or  (argsBindBExpr fa aa sem e1) (argsBindBExpr fa aa sem e2)
argsBindBExpr fa aa sem (Not e)     = Not (argsBindBExpr fa aa sem e)
argsBindBExpr fa aa sem (Eq a1 a2)  = Eq  (argsBindAExpr fa aa sem a1) (argsBindAExpr fa aa sem a2)
argsBindBExpr fa aa sem (Neq a1 a2) = Neq (argsBindAExpr fa aa sem a1) (argsBindAExpr fa aa sem a2)
argsBindBExpr fa aa sem (Lt a1 a2)  = Lt  (argsBindAExpr fa aa sem a1) (argsBindAExpr fa aa sem a2)
argsBindBExpr fa aa sem (LtE a1 a2) = LtE (argsBindAExpr fa aa sem a1) (argsBindAExpr fa aa sem a2)
argsBindBExpr fa aa sem (Gt a1 a2)  = Gt  (argsBindAExpr fa aa sem a1) (argsBindAExpr fa aa sem a2)
argsBindBExpr fa aa sem (GtE a1 a2) = GtE (argsBindAExpr fa aa sem a1) (argsBindAExpr fa aa sem a2)
argsBindBExpr _  _  _   BTrue       = BTrue
argsBindBExpr _  _  _   BFalse      = BFalse

replaceLocalVarsInErrExpr :: LocalEnv -> AExpr -> AExpr
replaceLocalVarsInErrExpr localEnv = replaceInAExpr (\a -> Nothing) (replaceLocalVars localEnv) 
  where
    replaceLocalVars localEnv (FVar fp x) = Just $ bindVar localEnv
      where
        bindVar :: LocalEnv -> FAExpr
        bindVar [] = FVar fp x
        bindVar ((y,fae):ys) | y == x = fae
                             | otherwise = bindVar ys
        bindVar _ = error $ "replaceLocalVarsInErrExpr: something went wrong."
    replaceLocalVars _ _          = Nothing

argsBindFAExpr :: [Arg] -> [FAExpr] -> FAExpr -> FAExpr
argsBindFAExpr fa aa = replaceInFAExpr (\a -> Nothing) replaceF
  where
    replaceF (FVar fp x) = Just $ bindVar fa aa
      where
        bindVar :: [Arg] -> [FAExpr] -> FAExpr
        bindVar [] [] = FVar fp x
        bindVar (Arg y _ :ys) (fae:as) | y == x = fae
                                       | otherwise = bindVar ys as
        bindVar args actArgs = error $ "bindVar: something went wrong: \n args: " ++ show args ++ "\n actual args:" ++ show actArgs
    replaceF _           = Nothing

argsBindFBExpr :: [Arg] -> [FAExpr] -> FBExpr -> FBExpr
argsBindFBExpr fa aa (FAnd e1 e2) = FAnd (argsBindFBExpr fa aa e1) (argsBindFBExpr fa aa e2)
argsBindFBExpr fa aa (FOr e1 e2)  = FOr  (argsBindFBExpr fa aa e1) (argsBindFBExpr fa aa e2)
argsBindFBExpr fa aa (FNot e)     = FNot (argsBindFBExpr fa aa e)
argsBindFBExpr fa aa (FEq a1 a2)  = FEq  (argsBindFAExpr fa aa a1) (argsBindFAExpr fa aa a2)
argsBindFBExpr fa aa (FNeq a1 a2) = FNeq (argsBindFAExpr fa aa a1) (argsBindFAExpr fa aa a2)
argsBindFBExpr fa aa (FLt a1 a2)  = FLt  (argsBindFAExpr fa aa a1) (argsBindFAExpr fa aa a2)
argsBindFBExpr fa aa (FLtE a1 a2) = FLtE (argsBindFAExpr fa aa a1) (argsBindFAExpr fa aa a2)
argsBindFBExpr fa aa (FGt a1 a2)  = FGt  (argsBindFAExpr fa aa a1) (argsBindFAExpr fa aa a2)
argsBindFBExpr fa aa (FGtE a1 a2) = FGtE (argsBindFAExpr fa aa a1) (argsBindFAExpr fa aa a2)
argsBindFBExpr fa aa (IsValid ae) = IsValid (argsBindFAExpr fa aa ae)
argsBindFBExpr _  _  FBTrue       = FBTrue
argsBindFBExpr _  _  FBFalse      = FBFalse

argsBindStm :: [Arg] -> [FAExpr] -> ACebS -> Stm -> Stm
argsBindStm _  _  _   UnstWarning          = UnstWarning
argsBindStm fa aa sem (Let x t ae stm)     = Let x t (argsBindFAExpr fa aa ae) (argsBindStm fa aa sem stm)
argsBindStm fa aa sem (Ite be stmT stmE)   = Ite     (argsBindFBExpr fa aa be) (argsBindStm fa aa sem stmT) (argsBindStm fa aa sem stmE)
argsBindStm fa aa sem (ListIte listT stmE) = ListIte (map (\(beT,stmT) -> (argsBindFBExpr fa aa beT, argsBindStm fa aa sem stmT)) listT) (argsBindStm fa aa sem stmE)
argsBindStm fa aa _  (StmExpr ae)         = StmExpr (argsBindFAExpr fa aa ae)
argsBindStm _  _  _  ForLoop{}  = error "ForLoop binding not defined." -- ForLoop i j (argsBindFAExpr fa aa sem fae) f

initErrVars :: Interpretation -> Interpretation
initErrVars = map initErrVarsSem
  where
    initErrVarsSem (funName, (fp, vars, acebs)) = (funName, (fp, vars, map initErrAceb acebs))

removeInfiniteCebS :: Interpretation -> Interpretation
removeInfiniteCebS [] = []
removeInfiniteCebS ((f,(fp,args,cebs)):is) =
  if filteredCebS /= []
  then (f,(fp,args,filteredCebS)):removeInfiniteCebS is
  else removeInfiniteCebS is
  where
    filteredCebS = filter (not . hasInfiniteError) cebs
    hasInfiniteError ACeb{ eExpr = ee} = ee == Infinity

checkProgSize :: [(String, (FPrec,[Arg], ACebS))] -> Int -> Int -> IO ()
checkProgSize [] n maxel | n >= maxel = error "The generated file is too big! Try to run the analysis with the stable tests assumption."
                         | otherwise  = return ()
checkProgSize ((_,(_,_,cebs)):xs) n maxel | length cebs + n >= maxel = error "The generated file is too big! Try to run the analysis with the stable tests assumption."
                                          | otherwise = checkProgSize xs (length cebs + n) maxel





