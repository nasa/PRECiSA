-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AbstractSemantics where

import AbstractDomain
import AbsPVSLang
import Common.ControlFlow
import Common.DecisionPath
import Common.TypesUtils
import qualified Data.List as List
import qualified Data.Set as Set
import Numeric
import Operators
import Utils
import Foreign.C.Types (CDouble, CFloat)
import Data.Maybe
import qualified Data.Map as Map
import Data.Either
import Translation.Float2Real
import Translation.Real2Float

data SemanticConfiguration = SemConf {
    assumeTestStability :: Bool,
    mergeUnstables :: Bool,
    improveError :: Bool,
    unfoldFunCalls :: Bool
}

type Semantics = CollACebS

newtype Iteration = Iter Int deriving (Eq,Show,Num)

type Env a = Map.Map (VarName,ResultField) a

-- newtype Env a = Env [(VarName,a)] deriving (Show)

type FunctionInterpretation = (IsTrans, PVSType, [Arg], Semantics)

type Interpretation = Map.Map FunName FunctionInterpretation

type CollACebS = Map.Map ResultField ACebS

emptyInterp :: Interpretation
emptyInterp = Map.empty

envVars :: Env a -> [VarName]
envVars = map fst . Map.keys

errVarName :: VarName -> VarName
errVarName x = "Err_" ++ x

symbolicError :: Interpretation -> Env ACebS -> FAExpr -> EExpr
symbolicError interp env fae =
  fromMaybe (error "symbolicError: unexpected argument") $
    eExpr $ mergeACebFold $ exprSemantics interp env fae

symbolicErrorStable :: Interpretation -> Env ACebS -> FAExpr -> EExpr
symbolicErrorStable interp env fae =
  fromMaybe (error "symbolicErrorStable: unexpected argument") $
    eExpr $ mergeACebFold $ exprSemanticsStable interp env fae

exprSemanticsStable :: Interpretation -> Env ACebS -> FAExpr -> [ACeb]
exprSemanticsStable interp env fae =
  replaceLetVarsFresh env [] $
    stmSem fae interp env (localVarsNames fae) SemConf{ improveError = False
                                 , assumeTestStability = True
                                 , mergeUnstables = True
                                 , unfoldFunCalls = False} root []

exprSemantics :: Interpretation -> Env ACebS -> FAExpr -> [ACeb]
exprSemantics interp env fae =
  replaceLetVarsFresh env [] $
    stmSem fae interp env (localVarsNames fae) SemConf{ improveError = False
                               , assumeTestStability = False
                               , mergeUnstables = True
                               , unfoldFunCalls = False } root []

maxRoundOffError :: ACebS -> EExpr
maxRoundOffError [] = error "maxRoundOffError: empty list."
maxRoundOffError acebs = fromMaybe (error "maxRoundOffError: unexpected argument") $
  eExpr $ foldl1 mergeACeb acebs

maxRoundOffErrorStable :: ACebS -> EExpr
maxRoundOffErrorStable [] = error "maxRoundOffError: empty list."
maxRoundOffErrorStable acebs = fromMaybe (error "maxRoundOffError: unexpected argument") $
  eExpr $ foldl1 mergeACeb (filter (\c -> cFlow c == Stable) acebs)


stableConditions :: ACebS -> [Condition]
stableConditions = concatMap (uncond . conds)

isPredInterp :: FunctionInterpretation -> Bool
isPredInterp (_,Boolean,_,_) = True
isPredInterp _               = False

isNumericalInterp :: FunctionInterpretation -> Bool
isNumericalInterp = not . isPredInterp

functionSemantics :: FunName -> ResultField -> Interpretation -> ACebS
functionSemantics f resField interp = fromMaybe msg (Map.lookup resField $ frt4 (fromMaybe msg (Map.lookup f interp)))
  where
    msg = error $ "functionSemantics: field " ++ show resField ++ " for function " ++ show f ++ " not found."

functionFormalArgs :: FunName -> Interpretation -> [Arg]
functionFormalArgs f interp = trd4 $ fromMaybe msg (Map.lookup f interp)
  where
    msg = error $ "functionSemantics: function " ++ show f ++ " not found."

emptyInterpretation :: Interpretation
emptyInterpretation = Map.empty

equivInterp :: Interpretation -> Interpretation -> Bool
equivInterp interp1 interp2 = Map.map aux interp1 == Map.map aux interp2
  where
    aux (isTrans, fp, vars , acebs) = (isTrans, fp, vars, Map.map Set.fromList acebs)

errorNotGrowing :: Interpretation -> Interpretation -> Bool
errorNotGrowing current _ | null current = True
errorNotGrowing current next = Map.foldrWithKey funErrorNotGrowing True current
  where
    funErrorNotGrowing funName funInterpCurrent b
      = b && Map.foldrWithKey (fieldErrorNotGrowing funInterpNext) True (frt4 funInterpCurrent)
      where
        funInterpNext = frt4 $ fromMaybe (error $ "errorNotGrowing: function " ++ show funName ++ " not found.")
                        (Map.lookup funName next)
        fieldErrorNotGrowing nextFun fieldName fieldSemCurrent acc = acc && errorNotGrowingFun fieldSemCurrent fieldSemNext
          where
            fieldSemNext = fromMaybe (error $ "errorNotGrowing: field " ++ show fieldName ++ " not found.")
                           (Map.lookup fieldName nextFun)

errorNotGrowingFun :: ACebS -> ACebS -> Bool
errorNotGrowingFun current = all (`existsEquivError` current)

existsEquivError :: ACeb -> ACebS -> Bool
existsEquivError aceb iter = result
  where
    result = any hasEquivError iter
    ACeb{conds = cc, eExpr = ee} = aceb
    hasEquivError aceb' = equivEExpr (fromMaybe (error "existsEquivError: unexpected argument") ee)
                         (fromMaybe (error "existsEquivError: unexpected argument") $ eExpr aceb') && cc /= conds aceb'

botInterp :: [Decl] -> Interpretation
botInterp = foldr insertEmptySem Map.empty
  where
    insertEmptySem (Decl isTrans t funName args _) interp
      = Map.insert funName (isTrans, t, args, botSem) interp
      where
        botSem = Map.insert ResValue [] Map.empty

    insertEmptySem (Pred isTrans _  funName args _) interp
      = Map.insert funName (isTrans, Boolean, args, botSem) interp
      where
        botSem = Map.insert ResValue [] Map.empty

    insertEmptySem (CollDecl isTrans t funName args _) interp
      = Map.insert funName (isTrans, t, args, botSem t) interp
      where
        botSem (Record fieldList) = foldr (insertEmptySemField . fst) Map.empty fieldList
          where
            insertEmptySemField fieldName = Map.insert (ResRecordField fieldName) []
        botSem _ = error "insertEmptySem: type mismatch for CollDecl."

        -- botSem (Tuple idxList) = Map.fromList (zip (map ResTupleIndex [1..]) (map (const []) idxList))
        -- botSem (Record fieldList) = foldr insertEmptySemField Map.empty (map fst fieldList)
        --   where
        --     insertEmptySemField fieldName m = Map.insert (ResRecordField fieldName) [] m

emptyEnv :: Env a
emptyEnv = Map.empty

buildEnv :: SemanticConfiguration -> Interpretation -> [(VarName,FAExpr)] -> Env ACebS
buildEnv config interp locVars = Map.fromList $ map buildEnv' locVars
  where
    buildEnv' (x,expr) = ((x,ResValue),stmSem expr interp emptyEnv (localVarsNames expr) config root [])

insertEnv :: VarName -> ResultField -> a -> Env a -> Env a
insertEnv var field a env =
  case Map.lookup (var,field) env of
    Just _ -> error "insertVarEnv: Variable already present in the environment"
    Nothing -> Map.insert (var,field) a env

addPathCond :: BExpr -> FBExpr -> ACeb -> ACeb
addPathCond be fbe ceb@ACeb{ conds = Conds cs } = ceb{conds = Conds (map aux cs)}
  where
    aux condACeb = condACeb {realPathCond = simplBExprFix $ And be (realPathCond condACeb)
                            ,fpPathCond = simplFBExprFix $ FAnd fbe (fpPathCond condACeb)
                            ,realCond = simplBExprFix (realCond condACeb)
                            ,fpCond = simplFBExprFix (fpCond condACeb)}

addPathCondS :: BExpr -> FBExpr -> ACebS -> ACebS
addPathCondS be fbe = map (addPathCond be fbe)

filterCondFalse :: ACebS -> ACebS
filterCondFalse = filter aux
  where
    aux ACeb{ conds = Conds cs } = not $ all isFalseCond cs

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
widening = Map.map funConvergeToTop
  where
    funConvergeToTop (isTrans,fp, args, sem) = (isTrans,fp, args, Map.map convergeToTop sem)
    convergeToTop _ = [topAceb]

topAceb :: ACeb
topAceb = ACeb {
            conds  = trueConds,
            rExprs = RDeclRes [] ,
            fpExprs = FDeclRes [],
            eExpr  = Just Infinity,
            decisionPath = root,
            cFlow  = Stable
        }

zeroErrAceb :: ACeb
zeroErrAceb = ACeb {
            conds  = trueConds,
            rExprs = RDeclRes [] ,
            fpExprs = FDeclRes [],
            eExpr  = Just $ Int 0,
            decisionPath = root,
            cFlow  = Stable
        }

immediateConsequence :: Program -> Interpretation -> SemanticConfiguration -> TargetDPs -> Interpretation
immediateConsequence decls interps semConf decPaths =
  foldl (\interp decl -> declSem decl interp decPaths semConf) interps decls

declSem :: Decl -> Interpretation -> TargetDPs -> SemanticConfiguration -> Interpretation
declSem (Decl _ _ fun _ stm) interp decPaths semConf =
  addDeclInterp fun ResValue (stmSem stm interp emptyEnv (localVarsNames stm) semConf root dps) interp
  where
    dps = fromMaybe (error $ "declSem: function " ++ fun ++ " not found.") (lookup fun decPaths)

declSem (Pred _ _ fun _ stm) interp decPaths semConf
  = addDeclInterp fun ResValue (bexprStmSem stm interp emptyEnv semConf root dps) interp
  where
    dps = fromMaybe (error $ "declSem: funpredicatection " ++ fun ++ " not found.") (lookup fun decPaths)

declSem (CollDecl _ (Record recordFields) fun _ stm) interp decPaths semConf
  = foldr addRecFieldInterp interp recordFields
  where
    dps = fromMaybe (error $ "declSem: function " ++ fun ++ " not found.") (lookup fun decPaths)
    addRecFieldInterp recField = addDeclInterp fun field sem
      where
        field = ResRecordField $ fst recField
        sem = fromMaybe errMsg $ Map.lookup field $ stmCollSem stm interp emptyEnv semConf root dps
        errMsg = error $ "declSem: Field " ++ show field ++ " not found."

declSem (CollDecl _ (Tuple tupleIdxs) fun _ stm) interp decPaths semConf
  = foldr addRecFieldInterp interp (take (length tupleIdxs) [1..])
    where
      dps = fromMaybe (error $ "declSem: function " ++ fun ++ " not found.") (lookup fun decPaths)
      addRecFieldInterp fieldIdx = addDeclInterp fun idx sem
        where
          idx = ResTupleIndex fieldIdx
          sem = fromMaybe errMsg $  Map.lookup idx $ stmCollSem stm interp emptyEnv semConf root dps
          errMsg = error $ "declSem: Field " ++ show idx ++ " not found."
declSem x _ _ _ = error $ "[declSem] Unhandled case: " ++ show x

-- refactor this with lookup function
-- addDeclInterp :: FunName -> ACebS -> Interpretation -> Interpretation
-- addDeclInterp = undefined
-- addDeclInterp fun sem interp =
--   let (l1,l2) = Map.partition (isFun fun) interp in
--         case l1 of
--           [] -> error $ "addDeclInterp: function " ++ show fun ++ " not found."
--           [(_, (isTrans, fp, args, cebs))] ->
--                 if hasInfiniteError cebs then
--                     l2 ++ [(fun, (isTrans, fp, args, replaceInfFun sem))]
--                 else
--                     l2 ++ [(fun, (isTrans, fp, args,  unionACebS cebs sem))]
--           _ -> error ("addDeclInterp: More than one occurrence of function " ++ fun ++ " in the interpretation.")
--   where
--     isFun f1 (f2, (_,_,_,_)) = f1 == f2
--     hasInfiniteError = any isErrorInfinite
--     replaceInfFun = map replaceInfFun'
--     replaceInfFun' ceb =  ceb { rExprs = RDeclRes [Infinity] }
--     isErrorInfinite aceb = ee == Just Infinity
--       where
--         ACeb{ eExpr = ee} = aceb

addDeclInterp :: FunName -> ResultField -> ACebS -> Interpretation -> Interpretation
addDeclInterp fun field sem = Map.alter updateInterp fun
  where
    updateInterp Nothing = error $ "addDeclInterp: function " ++ show fun ++ " not found."
    updateInterp (Just (isTrans, fp, args, sem')) = Just (isTrans, fp, args, Map.alter updateSem field sem')
    updateSem Nothing = error $ "addDeclInterp: field " ++ show field ++ " for function " ++ show fun ++ " not found."
    updateSem (Just cebs) = Just $ if hasInfiniteError cebs
                                   then replaceInfFun sem
                                   else unionACebS cebs sem
    hasInfiniteError = any isErrorInfinite
    replaceInfFun = map replaceInfFun'
    replaceInfFun' ceb =  ceb { rExprs = RDeclRes [Infinity] }
    isErrorInfinite aceb = ee == Just Infinity
      where
        ACeb{ eExpr = ee} = aceb

representative :: (b -> b -> Bool) -> [b] -> [b]
representative equiv sem = map head (partition equiv sem)

realEquivalence :: ACeb -> ACeb -> Bool
realEquivalence
  ACeb { conds = Conds cs1 }
  ACeb { conds = Conds cs2 }
  = setEq rCs1 rCs2
  where
    rCs1 = map realCond cs1
    rCs2 = map realCond cs2

fpEquivalence :: ACeb -> ACeb -> Bool
fpEquivalence
  ACeb { conds = Conds cs1 }
  ACeb { conds = Conds cs2 }
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
  ACeb { conds = Conds rCs,  rExprs = rRs }
  ACeb { conds = Conds fpCs, rExprs = fpRs, fpExprs = fpFs, eExpr = fpE, decisionPath = dp}
  = ACeb {
      conds  = Conds cs',
      rExprs = rRs,
      fpExprs = fpFs,
      eExpr  = case fpE of
                 Just e -> Just $ maxErr $ [BinaryOp AddOp e (UnaryOp AbsOp (BinaryOp SubOp fpR rR))
                                 | fpR <- rDeclRes fpRs, rR <- rDeclRes rRs]
                 Nothing -> Nothing,
      decisionPath = dp,
      cFlow  = Unstable
  }
  where
    cs' = [Cond {realPathCond = realPathCond rC
                ,fpPathCond = fpPathCond fpC
                ,realCond = And (realCond rC) (realCond fpC)
                ,fpCond = fpCond fpC} | rC <- rCs , fpC <- fpCs]


unTestSem :: ACebS -> ACebS -> ACebS
unTestSem semR semFP  = map (uncurry makeUnstable) combinations
  where
    combinations = [(cebR,cebFP) | cebR <- semR, cebFP <- semFP]

maxErr :: [EExpr] -> EExpr
maxErr [] = error "maxError cannot be applied to an empty list."
maxErr [err] = err
maxErr listErr = MaxErr listErr

---------------------------
-- Expressions Semantics --
---------------------------

intSem :: Integer -> LDecisionPath -> [ACeb]
intSem n dp = [ ACeb {
  conds  = trueConds,
  rExprs = RDeclRes [Int n],
  fpExprs = FDeclRes [FInt n],
  eExpr  = Just $ ErrRat 0,
  decisionPath = dp,
  cFlow  = Stable
  } ]

fpSem :: PVSType -> Rational -> LDecisionPath -> [ACeb]
fpSem fp n dp = [ ACeb {
  conds  = trueConds,
  rExprs = RDeclRes [Rat n],
  fpExprs = FDeclRes [FCnst fp n],
  eExpr  = Just $ ErrRat $ abs $ rat - (n :: Rational),
  decisionPath = dp,
  cFlow  = Stable
  } ]
    where
      rat = case fp of
              FPDouble -> toRational (fromRat n :: CDouble)
              FPSingle -> toRational (fromRat n :: CFloat)
              _ -> error "fpSem: unexpected type."

condDenomNotZero :: PVSType -> ACeb -> ACeb -> Conditions
condDenomNotZero fp ceb1 ceb2 =
  Conds [Cond {realPathCond = simplBExprFix $ And (realPathCond cond1) (realPathCond cond2)
              ,fpPathCond = simplFBExprFix  $ FAnd (fpPathCond cond1) (fpPathCond cond2)
              ,realCond = simplBExprFix $ And (And (realCond cond1) (realCond cond2))
                                              (Rel  Neq  r2  (Int 0))
              ,fpCond = simplFBExprFix $ FAnd (FAnd (fpCond cond1) (fpCond cond2))
                                              (if isIntFAExpr a2
                                               then FRel Neq a2 (FInt 0)
                                               else FRel Neq a2 (TypeCast TInt fp (FInt 0)))}
        | cond1 <- uncond (conds ceb1),
          cond2 <- uncond (conds ceb2),
          r2 <- rDeclRes $ rExprs ceb2,
          a2 <- fDeclRes $ fpExprs ceb2]

conditionUnOp :: Operators.UnOp -> PVSType -> ACeb -> Conditions
conditionUnOp SqrtOp _ ceb =
  Conds [Cond {realPathCond = realPathCond cond
              ,fpPathCond = fpPathCond cond
              ,realCond = simplBExprFix $ And (realCond cond)
                          (Rel GtE (BinaryOp SubOp r
                          (fromMaybe (error "conditionUnOp SqrtOp: unexpected argument.") $ eExpr ceb)) (Int 0))
              ,fpCond = fpCond cond}
          | cond <- uncond (conds ceb), r <- rDeclRes $ rExprs ceb]

conditionUnOp LnOp fp ceb =
  Conds [Cond {realPathCond = realPathCond cond
              ,fpPathCond = fpPathCond cond
              ,realCond = simplBExprFix $ And (And (realCond cond) (Rel Lt (Int 0)
                                          (BinaryOp SubOp r
                                          (fromMaybe (error "conditionUnOp LnOp: unexpected argument.") $ eExpr ceb))))
                                                                   (Rel Gt (FromFloat fp a) (Int 0))
              ,fpCond = fpCond cond}
        | cond <- uncond (conds ceb), r <- rDeclRes$ rExprs ceb, a <- fDeclRes $ fpExprs ceb]

conditionUnOp _ _ ceb = conds ceb

conditionBinOp :: Operators.BinOp -> PVSType -> ACeb -> ACeb -> Conditions
conditionBinOp DivOp   fp ceb1 ceb2 = condDenomNotZero fp ceb1 ceb2
conditionBinOp IDivOp  fp ceb1 ceb2 = condDenomNotZero fp ceb1 ceb2
conditionBinOp ItDivOp fp ceb1 ceb2 = condDenomNotZero fp ceb1 ceb2
conditionBinOp ModOp   fp ceb1 ceb2 = condDenomNotZero fp ceb1 ceb2
conditionBinOp ItModOp fp ceb1 ceb2 = condDenomNotZero fp ceb1 ceb2
conditionBinOp       _  _ ceb1 ceb2 =
  Conds [Cond {realPathCond = simplBExprFix $ And (realPathCond cond1) (realPathCond cond2)
              ,fpPathCond = simplFBExprFix  $ FAnd (fpPathCond cond1) (fpPathCond cond2)
              ,realCond = simplBExprFix $ And (realCond cond1) (realCond cond2)
              ,fpCond = simplFBExprFix $ FAnd (fpCond cond1) (fpCond cond2)}
        | cond1 <- uncond (conds ceb1), cond2 <- uncond (conds ceb2)]

semUnOp :: SemanticConfiguration -> Operators.UnOp -> PVSType -> [ACeb] -> LDecisionPath -> [ACeb]
semUnOp config op fp semOp1 dp =
  (if improveError config then id else collapseSem) $
    filterCondFalse $ map makeCeb semOp1
  where
    makeCeb ceb1 = ACeb {
        conds  = conditionUnOp op fp ceb1,
        rExprs  = RDeclRes [UnaryOp   op    r1 | r1 <- rDeclRes $ rExprs ceb1],
        fpExprs = FDeclRes [UnaryFPOp op fp f1 | f1 <- fDeclRes $ fpExprs ceb1],
        eExpr   = Just $ maxErr [ErrUnOp op fp r1 (fromMaybe (error "semUnOp: unexpected argument.") $ eExpr ceb1) | r1 <- rDeclRes $ rExprs ceb1],
        decisionPath = dp,
        cFlow  = cFlow ceb1
    }

semBinOp :: SemanticConfiguration -> Operators.BinOp -> PVSType -> [ACeb] -> [ACeb] -> LDecisionPath -> [ACeb]
semBinOp config op fp semOp1 semOp2 dp =
  (if improveError config then id else collapseSem) $
    filterCondFalse $ map makeCeb (combos [semOp1,semOp2])
  where
    makeCeb [ceb1, ceb2] = ACeb {
        conds  = conditionBinOp op fp ceb1 ceb2,
        rExprs  = RDeclRes [BinaryOp   op r1 r2 | r1 <- rDeclRes $ rExprs ceb1, r2 <- rDeclRes $ rExprs ceb2],
        fpExprs = FDeclRes [BinaryFPOp op fp f1 f2 | f1 <- fDeclRes $ fpExprs ceb1, f2 <- fDeclRes $ fpExprs ceb2],
        eExpr   = Just $ maxErr [ErrBinOp op fp r1 (fromMaybe (error "semBinOp: unexpected argument.") $ eExpr ceb1)
                                                r2 (fromMaybe (error "semBinOp: unexpected argument.") $ eExpr ceb2)
                                | r1 <- rDeclRes $  rExprs ceb1, r2 <- rDeclRes $ rExprs ceb2],
        decisionPath = dp,
        cFlow  = mergeControlFlow (cFlow ceb1) (cFlow ceb2)
    }
    makeCeb _ = error "makeCeb: somethinh went wrong"

isDecPathOfInterest :: [LDecisionPath] -> LDecisionPath -> Bool
isDecPathOfInterest dps dp = existsPrefixInList dp dps

isDecPathOfInterestACeb :: [LDecisionPath] -> ACeb -> Bool
isDecPathOfInterestACeb dps ACeb{ decisionPath = dp } = isDecPathOfInterest dps dp

notSubDecPathOfInterest :: [LDecisionPath] -> LDecisionPath -> Bool
notSubDecPathOfInterest dps dp = not (isDecPathOfInterest dps (dp ~> 0)) &&
                                 not (isDecPathOfInterest dps (dp ~> 1))

stableCasesIteSem :: [LDecisionPath]
                  -> ACebS
                  -> ACebS
                  -> FBExpr
                  -> ACebS

stableCasesIteSem dps semThen semElse fbe | null stableNotOfInterest = stableOfInterest
                                          | otherwise = mergeACebFold stableNotOfInterest:stableOfInterest
  where
    be = fbe2be fbe
    (stableOfInterest, stableNotOfInterest) = List.partition (isDecPathOfInterestACeb dps)
                                                             (filter isStable sem)
    sem = addPathCondS be fbe semThen ++
          addPathCondS (Not be) (FNot fbe) semElse

unstableCasesIteSem :: ACebS -> ACebS -> FBExpr -> ACebS
unstableCasesIteSem semThen semElse fbe = filter isUnstable (stableCases ++ unstableCases)
  where
    be = fbe2be fbe
    semThenStable = filter isStable semThen
    semElseStable = filter isStable semElse
    unstableCases = addPathCondS be (FNot fbe)
                             (unTestSem (representative realEquivalence semThenStable)
                                        (representative fpEquivalence   semElseStable))
                    ++
                    addPathCondS (Not be) fbe
                             (unTestSem (representative realEquivalence semElseStable)
                                        (representative fpEquivalence   semThenStable))
    stableCases = addPathCondS be fbe semThen ++
                  addPathCondS (Not be) (FNot fbe) semElse

semIte :: Bool -> Bool -> [LDecisionPath] -> ACebS -> ACebS -> FBExpr -> ACebS
semIte sta mu dps semThen semElse fbe | sta = stableCases
                                      | mu = case mergedUnstableCase of
                                                Nothing -> stableCases
                                                Just uc -> uc : stableCases
                                      | otherwise = unstableCases ++ stableCases
  where
    stableCases   = stableCasesIteSem dps semThen semElse fbe
    unstableCases = unstableCasesIteSem semThen semElse fbe
    mergedUnstableCase  = if null unstableCases then Nothing else Just $ mergeACebFold unstableCases

semCollIte :: Bool -> Bool -> [LDecisionPath] -> CollACebS -> CollACebS -> FBExpr -> CollACebS
semCollIte sta mu dps semThen semElse fbe = Map.mapWithKey semIteField semThen
  where
    semIteField field semThenField = semIte sta mu dps semThenField (semElseField field) fbe
    semElseField fieldName = fromMaybe (errMsg fieldName) $ Map.lookup fieldName semElse
    errMsg fieldName = error $ "semCollIte: Field " ++ show fieldName ++ " not found."

semCollIteList :: Bool -> Bool -> [LDecisionPath] -> [(FBExpr,CollACebS)] -> CollACebS -> CollACebS
semCollIteList sta mu dps listSemThen = Map.mapWithKey semIteListField
  where
    semIteListField field = semIteList sta mu dps (listSemFieldThen field)
    listSemFieldThen fieldName
      = map (\(be,sem) -> (be, fromMaybe (errMsg fieldName) $ Map.lookup fieldName sem)) listSemThen
    errMsg fieldName = error $ "semCollIte: Field " ++ show fieldName ++ " not found."

stableCasesListIte :: [LDecisionPath] -> ACebS -> ACebS
stableCasesListIte dps stableCases = if null stableNotOfInterest
                                             then stableOfInterest
                                             else mergeACebFold stableNotOfInterest:stableOfInterest
  where
    (stableOfInterest, stableNotOfInterest) = List.partition (isDecPathOfInterestACeb dps)
                                                             (filter isStable stableCases)

unstableCasesListIte :: ACebS -> [(FBExpr,ACebS)] -> ACebS -> ACebS
unstableCasesListIte  unstableCases' listSemThen semElse = unstableCases' ++ filter isUnstable unstableCases
  where
    semListStable = map (filter isStable . snd) listSemThen
    semWithGuards = zip (map fst listSemThen) semListStable ++ [(FBTrue, semElse)]

    fpUnstableGuards i = FAnd (guards !! i) negGuards
      where
        guards = map fst semWithGuards
        negGuards = foldl FAnd FBTrue (map FNot (take i guards))

    realUnstableGuards i = And (fbe2be $ guards !! i) negGuards
      where
        guards = map fst semWithGuards
        negGuards = foldl And BTrue (map (Not . fbe2be) (take i guards))

    unstableCases = concat [addPathCondS (realUnstableGuards i) (fpUnstableGuards j)
                      (unTestSem (representative realEquivalence (snd (semWithGuards !! i)))
                                 (representative fpEquivalence   (snd (semWithGuards !! j)))) | (i,j) <- pairCombinations (length semWithGuards)]

semIteList :: Bool -> Bool -> [LDecisionPath] -> [(FBExpr,ACebS)] -> ACebS -> ACebS
semIteList sta mu dps listSemThen semElse | sta = stableCases
                                          | mu = case mergedUnstableCase of
                                                  Nothing -> stableCases
                                                  Just uc -> uc : stableCases
                                          | otherwise = unstableCases ++ stableCases
  where
    (basicStable, basicUnstable) = List.partition isStable (addCondsToSem listSemThen (0::Int) BTrue FBTrue)

    addCondsToSem [] _ neg_bes neg_fbes = addPathCondS neg_bes neg_fbes semElse
    addCondsToSem ((fbe_i,sem_i):restThen) i neg_bes neg_fbes =
      addPathCondS (And (fbe2be fbe_i) neg_bes) (FAnd fbe_i neg_fbes) sem_i
      ++
      addCondsToSem restThen (i+1) (And (Not (fbe2be fbe_i)) neg_bes) (FAnd (FNot fbe_i) neg_fbes)

    stableCases   = stableCasesListIte   dps basicStable
    unstableCases = unstableCasesListIte     basicUnstable listSemThen semElse
    mergedUnstableCase = if null unstableCases then Nothing else Just $ mergeACebFold unstableCases

replaceLetVarsFresh :: Env ACebS -> [Arg] -> ACebS -> ACebS
replaceLetVarsFresh env formArgs = map replaceLetVarsFresh'
  where
    replaceLetVarsFresh' aceb =
      aceb {
        conds   = renameVarsConds subs (conds aceb),
        rExprs  = renameVarsRResult subs (rExprs aceb),
        fpExprs = renameVarsFResult subs (fpExprs aceb),
        eExpr   = fmap (renameVarsAExpr subs) (eExpr aceb)
      }
      where
        subs = buildLetInSubs (envVars env ++ map argName formArgs) (fpExprs aceb)
        buildLetInSubs vars exprs =  map (buildLetInPair vars (0 :: Integer) . fst) (localVarsFResult exprs)
        buildLetInPair vars n x = if x ++ "__" ++ show n `elem` vars
                                  then buildLetInPair vars (n + 1) x
                                  else (x,x ++ "__" ++ show n)

addLetElem2Env :: Interpretation
               -> SemanticConfiguration
               -> LDecisionPath
               -> [LDecisionPath]
               -> Env ACebS
               -> FLetElem
               -> Env ACebS
addLetElem2Env interp config dp dps accEnv (var,_,aexpr) = insertEnv var ResValue sem accEnv
  where
    sem = stmSem aexpr interp accEnv (localVarsNames aexpr) config dp dps


bexprStmSem :: FBExprStm
            -> Interpretation
            -> Env ACebS
            -> SemanticConfiguration
            -> LDecisionPath
            -> [LDecisionPath]
            -> ACebS

bexprStmSem (BLet [] _) _ _ _ _ _ = error "stmSem: something went wrong, let-in with empty list of assigments."

bexprStmSem (BLet (letElem:rest) stm) interp env config dp dps
  | isArithExpr (exprFLetElem letElem) =
    [ACeb{
       conds = simplifyConditions $ mergeConds (varBindConds [var] [expr] [cebExpr] $ conds cebStm) (conds cebExpr)
      ,rExprs  = RPredRes [RBLet [realLetElem realExprLetElem] realExpr | realExpr <- rPredRes $ rExprs cebStm]
      ,fpExprs = FPredRes [BLet  [letElem] fpExpr | fpExpr <- fPredRes $ fpExprs cebStm]
      ,eExpr   = Nothing
      ,decisionPath = decisionPath cebStm
      ,cFlow = mergeControlFlow (cFlow cebStm) (cFlow cebExpr)
    }
    | cebStm  <- bexprStmSem newStm interp env config dp dps
    , cebExpr <- stmSem expr interp env (localVarsNames expr) config dp dps
    , realExprLetElem <- rDeclRes $ rExprs cebExpr
    ]
  | otherwise = []
  where
    var  = varFLetElem  letElem
    expr = exprFLetElem letElem
    realLetElem re = LetElem {letVar  = var
                          ,letType = if typeFLetElem letElem == TInt then TInt else Real
                          ,letExpr = re}
    newStm = if null rest then stm else BLet rest stm

bexprStmSem (BIte fbe stmThen stmElse) interp env config@SemConf{ assumeTestStability = sta
                                                                , mergeUnstables = mu } dp dps =
  semIte sta mu dps semThen semElse fbe
  where
    semThen = bexprStmSem stmThen interp env config (dp ~> 0) dps
    semElse = bexprStmSem stmElse interp env config (dp ~> 1) dps

bexprStmSem (BListIte listThen stmElse) interp env config@SemConf{ assumeTestStability = sta, mergeUnstables = mu } dp dps = semIteList sta mu dps listSemThen semElse
  where
    n = fromIntegral $ length listThen
    semElse = bexprStmSem stmElse interp env config (dp ~> n) dps
    listSemThen = buildThenCasesSem listThen 0
    buildThenCasesSem [] _ = []
    buildThenCasesSem ((be_i,stm_i):listStm) i = (be_i,bexprStmSem stm_i interp env config (dp ~> i) dps) :
                                                  buildThenCasesSem listStm (i+1)

bexprStmSem (BExpr FBTrue) _ _ _ dp _ = [ACeb {
  conds  = trueConds,
  rExprs = RPredRes [RBExpr BTrue] ,
  fpExprs = FPredRes [BExpr FBTrue],
  eExpr  = Nothing,
  decisionPath = dp,
  cFlow  = Stable
  }]

bexprStmSem (BExpr FBFalse) _ _ _ dp _ = [ACeb {
  conds  = trueConds,
  rExprs = RPredRes [RBExpr BFalse] ,
  fpExprs = FPredRes [BExpr FBFalse],
  eExpr  = Nothing,
  decisionPath = dp,
  cFlow  = Stable
  }]

bexprStmSem (BExpr fbe@(FOr be1 be2)) interp env config dp dps = [ACeb {
  conds  = mergeConds (conds ceb1) (conds ceb2),
  rExprs = RPredRes [RBExpr rbe] ,
  fpExprs = FPredRes [BExpr fbe],
  eExpr  = Nothing,
  decisionPath = dp,
  cFlow  = Stable
  } | ceb1 <- bexprStmSem (BExpr be1) interp env config dp dps
    , ceb2 <- bexprStmSem (BExpr be2) interp env config dp dps]
  where
    rbe = fbe2be fbe

bexprStmSem (BExpr fbe@(FAnd be1 be2)) interp env config dp dps = [ACeb {
  conds  = mergeConds (conds ceb1) (conds ceb2),
  rExprs = RPredRes [RBExpr rbe] ,
  fpExprs = FPredRes [BExpr fbe],
  eExpr  = Nothing,
  decisionPath = dp,
  cFlow  = Stable
  } | ceb1 <- bexprStmSem (BExpr be1) interp env config dp dps
    , ceb2 <- bexprStmSem (BExpr be2) interp env config dp dps]
  where
    rbe = fbe2be fbe

bexprStmSem (BExpr fbe@(FNot be)) interp env config dp dps = [ACeb {
  conds  = conds ceb,
  rExprs = RPredRes [RBExpr rbe] ,
  fpExprs = FPredRes [BExpr fbe],
  eExpr  = Nothing,
  decisionPath = dp,
  cFlow  = Stable
  } | ceb <- bexprStmSem (BExpr be) interp env config dp dps]
  where
    rbe = fbe2be fbe

bexprStmSem (BExpr fbe@(FRel _ ae1 ae2)) interp env config dp dps = [ACeb {
  conds  = mergeConds (conds ceb1) (conds ceb2),
  rExprs = RPredRes [RBExpr rbe] ,
  fpExprs = FPredRes [BExpr fbe],
  eExpr  = Nothing,
  decisionPath = dp,
  cFlow  = Stable
  } | ceb1 <- stmSem ae1 interp env (localVarsNames ae1) config dp dps
    , ceb2 <- stmSem ae2 interp env (localVarsNames ae2) config dp dps]
  where
    rbe = fbe2be fbe

bexprStmSem (BExpr (FEPred _ _ p actArgs)) interp env config dp dps =
  case Map.lookup p interp of
    Just (_, _, formArgs, funSem) -> semEFun p ResValue formArgs actArgs
                                             (combos argSem)
                                             (replaceLetVarsFresh env formArgs funSemRes)
                                             (length funSem) dp
      where
        funSemRes = fromMaybe errMsg $ Map.lookup ResValue funSem
        errMsg = error $ "bexprStmSem: predicate " ++ p ++ "not found in semantics."
    --
    Nothing -> error ("Function " ++ p ++ " not found")
  where
    argSem = map (\arg -> stmSem arg interp env (localVarsNames arg) config dp dps) actArgs


bexprStmSem (BExpr be) _ _ _ _ _ = error $ "bexprStmSem not defined for " ++ show be ++ "."

bexprStmSem  BUnstWarning _ _ _ _ _ = [zeroErrAceb]

stmCollSem :: CollFAExpr
           -> Interpretation
           -> Env ACebS
           -> SemanticConfiguration
           -> LDecisionPath
           -> [LDecisionPath]
           -> CollACebS

stmCollSem (CLet [] _) _ _ _ _ _ = error "stmCollSem: empty variable list in let-in statement."

stmCollSem (CLet (letElem:rest) stm) interp env config dp dps
  | isArithExpr (exprFLetElem letElem) = Map.map aux (stmCollSem newStm interp env config dp dps)
  where
    aux semField = [ACeb{
       conds = simplifyConditions $ mergeConds (varBindConds [var] [expr] [cebExpr] $ conds cebStm) (conds cebExpr)
      ,rExprs  = RDeclRes [RLet [realLetElem realExprLetElem] realExpr | realExpr <- rDeclRes $ rExprs cebStm]
      ,fpExprs = FDeclRes [Let [letElem] fpExpr | fpExpr <- fDeclRes $ fpExprs cebStm]
      ,eExpr   = Just $ RLet [realLetElem realExprLetElem, errorLetElem cebExpr]
                             (replaceInAExpr (replaceErrorMarks var) (const Nothing)
                              $ fromMaybe (error "stmSem: unexpected argument.") $ eExpr cebStm)
      ,decisionPath = decisionPath cebStm
      ,cFlow = mergeControlFlow (cFlow cebStm) (cFlow cebExpr)
      }
      | cebStm  <- semField
      , cebExpr <- stmSem expr interp env (localVarsNames expr) config dp dps
      , realExprLetElem <- rDeclRes $ rExprs cebExpr
      ]
    var  = varFLetElem  letElem
    expr = exprFLetElem letElem
    replaceErrorMarks vName (ErrorMark x ResValue _) | x==vName = Just $ Var Real (errVarName x)
    replaceErrorMarks _ _ = Nothing
    realLetElem re = LetElem {letVar  = var
                          ,letType = if typeFLetElem letElem == TInt then TInt else Real
                          ,letExpr = re}
    errorLetElem ceb = LetElem {letVar  = errVarName var
                               ,letType = Real
                               ,letExpr = fromMaybe (error "stmSem let: unexpected argument") $ eExpr ceb}
    newStm = if null rest then stm else CLet rest stm

stmCollSem (CLet (letElem:rest) stm) interp env config dp dps = stmCollSem newStm interp newEnv config dp dps
  where
    newStm = varBindLetCollFAExpr [letElem] (if null rest then stm else CLet rest stm)
    newEnv = addLetElem2Env interp config dp dps env letElem

stmCollSem (CIte fbe stm1 stm2) interp env config@SemConf{assumeTestStability = sta, mergeUnstables = mu} dp dps
  = semCollIte sta mu dps semThen semElse fbe
    where
      semThen = stmCollSem stm1 interp env config (dp ~> 0) dps
      semElse = stmCollSem stm2 interp env config (dp ~> 1) dps

stmCollSem (CListIte listThen stmElse) interp env config@SemConf{assumeTestStability = sta, mergeUnstables = mu} dp dps
  = semCollIteList sta mu dps listSemThen semElse
    where
      n = fromIntegral $ length listThen
      semElse = stmCollSem stmElse interp env config (dp ~> n) dps
      listSemThen = buildThenCasesSem listThen 0
      buildThenCasesSem [] _ = []
      buildThenCasesSem ((be_i,stm_i):listStm) i = (be_i,stmCollSem stm_i interp env config (dp ~> i) dps) :
                                                    buildThenCasesSem listStm (i+1)

stmCollSem (RecordExpr fieldList) interp env config dp dps = Map.fromList $ map aux (filter (isLeft . snd) fieldList)
  where
    aux (field, Left expr)  = (ResRecordField field, stmSem expr interp env (localVarsNames expr) config dp dps)
    aux x = error $ "[stmCollSem.aux] Unhandled case: " ++ show x

stmCollSem (TupleExpr exprList) interp env config dp dps
  = Map.fromList $ zip (map ResTupleIndex [1..]) (map (\ expr -> stmSem expr interp env (localVarsNames expr) config dp dps) (lefts exprList))

stmCollSem (ArrayUpdate {}) _ _ _ _ _ = error "stmCollSem: niy for ArrayUpdate"

stmCollSem (ArrayCollUpdate {}) _ _ _ _ _ = error "stmCollSem: niy for ArrayCollUpdate"

stmCollSem (CollFun _isTrans f _t actArgs) interp env config dp dps
  = case Map.lookup f interp of
    Just (_, _, formArgs, funSem) -> Map.mapWithKey (funSemField formArgs) funSem
    Nothing -> error ("Function " ++ f ++ " not found.")
  where
    funSemField formArgs field acebs
      = semEFun f field formArgs actArgs
                 (combos argSem)
                 (replaceLetVarsFresh env formArgs acebs)
                 (length acebs) dp
      -- TODO optimization
      -- = if (unfoldFunCalls config)
      --     then semEFun f field formArgs actArgs
      --            (combos argSem)
      --            (replaceLetVarsFresh env formArgs acebs)
      --            (length acebs) dp
      --     else [ ACeb {
      --            conds  = trueConds,
      --            rExprs = RDeclRes [fae2real fexpr],
      --            fpExprs = FDeclRes [fexpr],
      --            eExpr  = Just $ ErrFun f field actArgs,
      --            decisionPath = dp,
      --            cFlow  = Stable
      --          } ]
    argSem = map (\arg -> stmSem arg interp env (localVarsNames arg) config dp dps) actArgs

stmCollSem (CollVar _t _x) _interp _env _config _dp _dps = undefined

stmSem :: FAExpr
       -> Interpretation
       -> Env ACebS
       -> [VarName]
       -> SemanticConfiguration
       -> LDecisionPath
       -> [LDecisionPath]
       -> ACebS

stmSem (FInt n)             _ _ _ _ dp _ = intSem n dp
stmSem (FCnst fp n)         _ _ _ _ dp _ = fpSem fp n dp
stmSem (FInterval fp lb ub) _ _ _ _ dp _ = [ ACeb {
  conds  = trueConds,
  rExprs = RDeclRes [Interval lb ub],
  fpExprs = FDeclRes [FInterval fp lb ub],
  eExpr  = Just $ HalfUlp (Rat (max lb ub)) fp,
  decisionPath = dp,
  cFlow  = Stable
  } ]
stmSem (ToFloat _ (Int n)) _ _ _ _ dp _ = intSem n dp

stmSem (ToFloat fp (Rat n)) _ _ _ _ dp _ = fpSem fp n dp

stmSem (ToFloat fp (UnaryOp NegOp (Int n))) _ _ _ _ dp _ = [ ACeb {
  conds  = trueConds,
  rExprs = RDeclRes [UnaryOp NegOp (Int n)],
  fpExprs = FDeclRes [UnaryFPOp NegOp fp (FInt n)],
  eExpr  = Just $ ErrRat 0,
  decisionPath = dp,
  cFlow  = Stable
  } ]

stmSem (ToFloat fp (UnaryOp NegOp (Rat n))) _ _ _ _ dp _ = [ ACeb {
  conds  = trueConds,
  rExprs  = RDeclRes [UnaryOp   NegOp (Rat n)],
  fpExprs = FDeclRes [UnaryFPOp NegOp fp (FCnst fp n)],
  eExpr  = Just $ ErrRat $ abs $ rat - ((-n) :: Rational),
  decisionPath = dp,
  cFlow  = Stable
  } ]
    where
      rat = case fp of
            FPDouble -> toRational (fromRat (-n) :: CDouble)
            FPSingle -> toRational (fromRat (-n) :: CFloat)
            _ -> error "fpSem: unexpected type."

stmSem (TypeCast _ _ (FInt n)) _ _ _ _ dp _ =
  [ ACeb {
    conds  = trueConds,
    rExprs = RDeclRes [Int n],
    fpExprs = FDeclRes [FInt n],
    eExpr  = Just $ ErrRat 0,
    decisionPath = dp,
    cFlow  = Stable
  } ]

stmSem (TypeCast _ toType (FCnst fromType n)) _ _ _ _ dp _ =
  [ ACeb {
    conds   = trueConds,
    rExprs  = RDeclRes [Rat n],
    fpExprs = FDeclRes [TypeCast fromType toType (FCnst fromType n)],
    eExpr   = Just $ ErrRat $ errRat + abs (ratTo - ratFrom),
    decisionPath = dp,
    cFlow  = Stable
  } ]
    where
      errRat = abs $ ratFrom - (n :: Rational)
      ratFrom = case fromType of
              FPDouble -> toRational (fromRat n :: CDouble)
              FPSingle -> toRational (fromRat n :: CFloat)
              _ -> error "fpSem: unexpected type."
      ratTo = case toType of
              FPDouble -> toRational (fromRat ratFrom :: CDouble)
              FPSingle -> toRational (fromRat ratFrom :: CFloat)
              _ -> error "fpSem: unexpected type."

stmSem (TypeCast fp1 fp2 a) interp env locVars config dp dps =
  (if improveError config then id else collapseSem) $
  filterCondFalse $ map makeCebCast (stmSem a interp env locVars config dp dps)
    where
      makeCebCast :: ACeb -> ACeb
      makeCebCast ceb = ceb {
                  rExprs  = RDeclRes $ rDeclRes $ rExprs ceb,
                  fpExprs = FDeclRes [TypeCast fp1 fp2 f | f <- fDeclRes $ fpExprs ceb],
                  eExpr  = Just $ maxErr [ErrCast fp1 fp2 r (fromMaybe (error "stmSem TypeCast: unexpected argument")
                                $ eExpr ceb)| r <- rDeclRes $ rExprs ceb]
              }

stmSem (FVar fp x) _ env locVars _ dp _ =
  fromMaybe
    [ACeb{conds = trueConds,
          rExprs = RDeclRes [if x `elem` locVars then Var Real x else RealMark x ResValue],
          fpExprs = FDeclRes [FVar fp x],
          eExpr = Just $ if fp == TInt then ErrRat 0 else ErrorMark x ResValue fp,
          decisionPath = dp, cFlow = Stable}]
    (Map.lookup (x,ResValue) env)

stmSem (FArrayElem fp x _) _ env locVars _ dp _ =
  fromMaybe
    [ACeb{conds = trueConds,
          rExprs  = RDeclRes [if x `elem` locVars then Var Real x else RealMark x ResValue],
          fpExprs = FDeclRes [FVar fp x],
          eExpr = Just $ if fp == TInt then ErrRat 0 else ErrorMark x ResValue fp,
          decisionPath = dp, cFlow = Stable}]
    (Map.lookup (x,ResValue) env)

stmSem (FTupleElem t x idx) _interp env _locVars _config dp _dps =
  fromMaybe
    [ACeb{conds = trueConds,
          rExprs  = RDeclRes [RealMark x (ResTupleIndex idx)],
          fpExprs = FDeclRes [FTupleElem t x idx],
          eExpr = Just $ if t == TInt then ErrRat 0 else ErrorMark x (ResTupleIndex idx) t,
          decisionPath = dp, cFlow = Stable}]
    (Map.lookup (x,ResTupleIndex idx) env)

stmSem (FRecordElem t x field) _interp env _locVars _config dp _dps =
  fromMaybe
    [ACeb{conds = trueConds,
          rExprs  = RDeclRes [RealMark x (ResRecordField field)],
          fpExprs = FDeclRes [FRecordElem t x field],
          eExpr = Just $ if t == TInt then ErrRat 0 else ErrorMark x (ResRecordField field) t,
          decisionPath = dp, cFlow = Stable}]
    (Map.lookup (x,ResRecordField field) env)

stmSem (FListElem fp x idx) _interp env _locVars _config dp _dps =
  fromMaybe
    [ACeb{conds = trueConds,
          rExprs = RDeclRes [ListElem (fp2realType fp) x (fae2real idx)],
          fpExprs = FDeclRes [FListElem fp x idx],
          eExpr = Just $ if fp == TInt then ErrRat 0 else ErrorMark x ResValue fp,
          decisionPath = dp, cFlow = Stable}]
    (Map.lookup (x,ResValue) env)

stmSem fexpr@(FEFun _ f field fp actArgs) interp env locVars config dp dps =
  case Map.lookup f interp of
    Just (_, _, formArgs, funSem) ->
      case Map.lookup field funSem of
        Just acebs ->
          if unfoldFunCalls config
          then semEFun f field formArgs actArgs
                 (combos argSem)
                 (replaceLetVarsFresh env formArgs acebs)
                 (length funSem) dp
          else [ ACeb {
                 conds  = trueConds,
                 rExprs = RDeclRes [fae2real fexpr],
                 fpExprs = FDeclRes [fexpr],
                 eExpr  = Just $ ErrFun f fp field actArgs (map fae2real actArgs) argsErrs,
                 decisionPath = dp,
                 cFlow  = Stable
               } ]
        Nothing -> error ("Field " ++ show field ++ " for function " ++ f ++ " not found.")
    Nothing -> error ("Function " ++ f ++ " not found.")
  where
    argSem = map (\arg -> stmSem arg interp env locVars config dp dps) actArgs
    argsErrs = map (\arg -> MaxErr $ map errorExpr (stmSem arg interp env (localVarsNames arg) config dp dps)) actArgs
    errorExpr = fromMaybe (error "computeErrorAExpr: unexpected argument.") . eExpr

stmSem (FMap fp f l) interp env _ config dp dps
  = stmSem (FEFun False f ResValue fp [FVar fp l]) interp env [] config dp dps

-- foldr --
stmSem (FFold fp f l n ae0) interp env locVars config dp dps
  = stmSem unfoldedFold interp env locVars config dp dps
  where
    unfoldedFold = foldr buildFun ae0 (replicate (fromIntegral n) (FVar fp l))
    buildFun _listElem ae = FEFun False f ResValue fp [FVar fp l, ae]

stmSem (BinaryFPOp MulOp TInt a1 a2) interp env locVars config dp dps =
  (if improveError config then id else collapseSem) $
  filterCondFalse $ semBinOp config MulOp TInt (stmSem a1 interp env locVars config dp dps)
                                               (stmSem a2 interp env locVars config dp dps) dp

stmSem ae@(BinaryFPOp MulOp fp a1 a2) interp env locVars config dp dps =
  (if improveError config then id else collapseSem) $
  case pow2Mul ae of
      Just (Left  (pow2, a)) -> filterCondFalse $ map (semMulPow2  True) (combos [stmSem pow2 interp env locVars config dp dps, stmSem a    interp env locVars config dp dps])
      Just (Right (pow2, a)) -> filterCondFalse $ map (semMulPow2 False) (combos [stmSem a interp env locVars config dp dps, stmSem pow2 interp env locVars config dp dps])
      Nothing -> filterCondFalse $ semBinOp config MulOp fp (stmSem a1 interp env locVars config dp dps)
                                                            (stmSem a2 interp env locVars config dp dps) dp
  where
    pow2Mul :: FAExpr -> Maybe (Either (FAExpr,FAExpr) (FAExpr,FAExpr))
    pow2Mul (BinaryFPOp MulOp _ pow2@(FInt n)         a) | isPow2 (fromInteger  n :: Double) = Just $ Left  (pow2, a)
    pow2Mul (BinaryFPOp MulOp _ pow2@(FCnst _ n)      a) | isPow2 (fromRational n :: Double) = Just $ Left  (pow2, a)
    pow2Mul (BinaryFPOp MulOp _ a         pow2@(FInt n)) | isPow2 (fromInteger  n :: Double) = Just $ Right (pow2, a)
    pow2Mul (BinaryFPOp MulOp _ a      pow2@(FCnst _ n)) | isPow2 (fromRational n :: Double) = Just $ Right (pow2, a)
    pow2Mul _ = Nothing

    semMulPow2 :: Bool -> ACebS -> ACeb
    semMulPow2 left [ceb1,ceb2] =
      ACeb {
        conds = if left
                then Conds [Cond {
                  realPathCond = realPathCond cond
                 ,fpPathCond = fpPathCond cond
                 ,realCond = simplBExprFix $ And (realCond cond) (Rel Lt (Int n) (BinaryOp SubOp (Prec fp) (FExp a2)))
                 ,fpCond = fpPathCond cond}
                | cond <- uncond (conds ceb2)]
                else Conds [Cond {
                  realPathCond = realPathCond cond
                 ,fpPathCond = fpPathCond cond
                 ,realCond = simplBExprFix $ And (realCond cond) (Rel Lt (Int n) (BinaryOp SubOp (Prec fp) (FExp a1)))
                 ,fpCond = fpPathCond cond}
                | cond <- uncond (conds ceb1)],
        rExprs  = RDeclRes [BinaryOp   MulOp    r1 r2 | r1 <- rDeclRes $ rExprs  ceb1, r2 <- rDeclRes $ rExprs ceb2],
        fpExprs = FDeclRes [BinaryFPOp MulOp fp f1 f2 | f1 <- fDeclRes $ fpExprs ceb1, f2 <- fDeclRes $ fpExprs ceb2],
        eExpr   = Just $ if left then ErrMulPow2L fp n (fromMaybe (error "semMulPow2: unexpected argument.")
                                                       $ eExpr ceb2)
                         else ErrMulPow2R fp n (fromMaybe (error "semMulPow2: unexpected argument.")
                                               $ eExpr ceb1),
        decisionPath = root,
        cFlow  = mergeControlFlow (cFlow ceb1) (cFlow ceb2)
      }
      where
        n = if left then getExp (rDeclRes $ rExprs ceb1) else getExp (rDeclRes $ rExprs ceb2)
        getExp [Int m] = round $ logBase (2 :: Double) (fromIntegral m)
        getExp [Rat m] = round $ logBase (2 :: Double) (realToFrac m)
        getExp _ = error "getExp: something went wrong"

    semMulPow2 _ cebs = error $ "semMulPow2: something went wrong\n ceb1 = " ++ show cebs ++ "\n"

stmSem (BinaryFPOp SubOp TInt a1 a2) interp env locVars config dp dps =
  (if improveError config then id else collapseSem) $
  filterCondFalse $ semBinOp config SubOp TInt (stmSem a1 interp env locVars config dp dps)
                                               (stmSem a2 interp env locVars config dp dps) dp

stmSem (BinaryFPOp SubOp fp a1 a2) interp env locVars config dp dps =
  if improveError config
  then
    filterCondFalse $ concatMap semSubImproved (combos [stmSem a1 interp env locVars config dp dps
                                                       ,stmSem a2 interp env locVars config dp dps])
  else
    filterCondFalse $ semBinOp config SubOp fp (stmSem a1 interp env locVars config dp dps)
                                               (stmSem a2 interp env locVars config dp dps) dp
  where
    semSubImproved [ceb1,ceb2] =
      [ACeb {
        conds = Conds [Cond {realPathCond = simplBExprFix $ And (realPathCond cond1) (realPathCond cond2)
                            ,fpPathCond = simplFBExprFix  $ FAnd (fpPathCond cond1) (fpPathCond cond2)
                            ,realCond = simplBExprFix $
                                         And (And (Rel LtE (BinaryOp DivOp r2 (Int 2)) r1)
                                         (Rel LtE r1 (BinaryOp MulOp r2 (Int 2)))) (And (realCond cond1) (realCond cond2))
                            ,fpCond = simplFBExprFix $ FAnd (fpCond cond1) (fpCond cond2)}
                      | cond1 <- uncond (conds ceb1), cond2 <- uncond (conds ceb2),
                        r1 <- rDeclRes $ rExprs ceb1, r2 <- rDeclRes $ rExprs ceb2],
            rExprs  = RDeclRes [BinaryOp   SubOp    r1 r2 | r1 <- rDeclRes $ rExprs  ceb1
                                                          , r2 <- rDeclRes $ rExprs ceb2],
            fpExprs = FDeclRes [BinaryFPOp SubOp fp f1 f2 | f1 <- fDeclRes $ fpExprs ceb1
                                                          , f2 <- fDeclRes $ fpExprs ceb2],
            eExpr   = Just $ maxErr [ErrSubSternenz fp r1 (fromMaybe (error "stmSem BinaryFPOp: unexpected argument.") $ eExpr ceb1) r2 (fromMaybe (error "stmSem BinaryFPOp: unexpected argument.") $ eExpr ceb2)
                                    | r1 <- rDeclRes $ rExprs ceb1
                                    , r2 <- rDeclRes $ rExprs ceb2],
            decisionPath = dp,
            cFlow  = mergeControlFlow (cFlow ceb1) (cFlow ceb2)
        }
      ,ACeb {
            conds  = Conds [Cond {realPathCond = simplBExprFix $ And (realPathCond cond1) (realPathCond cond2)
                                 ,fpPathCond = simplFBExprFix  $ FAnd (fpPathCond cond1) (fpPathCond cond2)
                                 ,realCond = simplBExprFix $
                                        And (Or (Rel Gt (BinaryOp DivOp r2 (Int 2)) r1)
                                                (Rel Gt r1 (BinaryOp MulOp r2 (Int 2)))) (And (realCond cond1) (realCond cond2))
                                 ,fpCond = simplFBExprFix $ FAnd (fpCond cond1) (fpCond cond2)}
                           | cond1 <- uncond (conds ceb1), cond2 <- uncond (conds ceb2),
                                      r1 <- rDeclRes $ rExprs  ceb1, r2 <- rDeclRes $ rExprs ceb2],
            rExprs  = RDeclRes [BinaryOp SubOp r1 r2 | r1 <- rDeclRes $ rExprs  ceb1
                                                     , r2 <- rDeclRes $ rExprs ceb2],
            fpExprs = FDeclRes [BinaryFPOp SubOp fp f1 f2 | f1 <- fDeclRes $ fpExprs ceb1
                                                          , f2 <- fDeclRes $ fpExprs ceb2],
            eExpr   = Just $ maxErr [ErrBinOp SubOp fp r1 (fromMaybe (error "stmSem BinaryFPOp: unexpected argument.")
                                    $ eExpr ceb1) r2 (fromMaybe (error "stmSem BinaryFPOp: unexpected argument.") $ eExpr ceb2)
                                    | r1 <- rDeclRes $ rExprs ceb1
                                    , r2 <- rDeclRes $ rExprs ceb2],
            decisionPath = dp,
            cFlow  = mergeControlFlow (cFlow ceb1) (cFlow ceb2)
        }]
    semSubImproved _ = error "stmSem SubOp: something went wrong, perhaps the semantics of the operands is empty."

stmSem (BinaryFPOp op fp a1 a2) interp env locVars config dp dps =
  (if improveError config then id else collapseSem) $
  semBinOp config op fp (stmSem a1 interp env locVars config dp dps) (stmSem a2 interp env locVars config dp dps) dp

stmSem (UnaryFPOp FloorOp TInt a) interp env locVars config dp dps =
  (if improveError config then id else collapseSem) $
  filterCondFalse $ map makeCeb (stmSem a interp env locVars config dp dps)
  where
    makeCeb ceb1 =
      ACeb {
        conds  = conds ceb1,
        rExprs  = RDeclRes [UnaryOp   FloorOp    r1 | r1 <- rDeclRes $ rExprs ceb1],
        fpExprs = FDeclRes [UnaryFPOp FloorOp TInt f1 | f1 <- fDeclRes $  fpExprs ceb1],
        eExpr   = Just $ maxErr [ErrUnOp FloorOp TInt r1 (fromMaybe (error "stmSem FloorOp: unexpected argument.") $ eExpr ceb1) | r1 <- rDeclRes $ rExprs ceb1],
        decisionPath = dp,
        cFlow  = cFlow ceb1}

stmSem (UnaryFPOp FloorOp fp a) interp env locVars config dp dps =
  if improveError config
  then filterCondFalse $ concatMap makeCeb semOperand
  else semUnOp config FloorOp fp semOperand dp
  where
    semOperand = stmSem a interp env locVars config dp dps
    makeCeb ceb1 =
      [ACeb {
        conds = Conds [Cond {realPathCond = realPathCond cond
                            ,fpPathCond = fpPathCond cond
                            ,realCond = simplBExprFix $
                                        And (realCond cond) (Or (Rel Neq (UnaryOp FloorOp r) (UnaryOp FloorOp (BinaryOp SubOp r (fromMaybe (error "UnaryFPOp FloorOp: unexpected argument.") $ eExpr ceb1))))
                                        (Rel Neq (UnaryOp FloorOp r) (UnaryOp FloorOp (BinaryOp AddOp r (fromMaybe (error "UnaryFPOp FloorOp: unexpected argument.") $ eExpr ceb1)))))
                            ,fpCond = fpCond cond}
                           | cond <- uncond (conds ceb1), r <- rDeclRes $ rExprs ceb1],
        rExprs  = RDeclRes [UnaryOp   FloorOp    r1 | r1 <- rDeclRes $ rExprs ceb1],
        fpExprs = FDeclRes [UnaryFPOp FloorOp fp f1 | f1 <- fDeclRes $ fpExprs ceb1],
        eExpr   = Just $ maxErr [ErrUnOp FloorOp fp r1 (fromMaybe (error "UnaryFPOp FloorOp: unexpected argument.") $ eExpr ceb1) | r1 <- rDeclRes $ rExprs ceb1],
        decisionPath = dp,
        cFlow  = cFlow ceb1}
      ,ACeb {
        conds = Conds [Cond {realPathCond = realPathCond cond
                            ,fpPathCond = fpPathCond cond
                            ,realCond = simplBExprFix $
                                        And (realCond cond) (And (Rel Eq (UnaryOp FloorOp r) (UnaryOp FloorOp (BinaryOp SubOp r (fromMaybe (error "UnaryFPOp FloorOp: unexpected argument.") $ eExpr ceb1))))
                                        (Rel Eq (UnaryOp FloorOp r) (UnaryOp FloorOp (BinaryOp AddOp r (fromMaybe (error "UnaryFPOp FloorOp: unexpected argument.") $ eExpr ceb1)))))
                            ,fpCond = fpCond cond}
                           | cond <- uncond (conds ceb1), r <- rDeclRes $ rExprs ceb1],
        rExprs  = RDeclRes [UnaryOp FloorOp r1 | r1 <- rDeclRes $ rExprs ceb1],
        fpExprs = FDeclRes [UnaryFPOp FloorOp fp f1 | f1 <- fDeclRes $ fpExprs ceb1],
        eExpr   = Just $ maxErr [ErrFloorNoRound fp r1 (fromMaybe (error "UnaryFPOp FloorOp: unexpected argument.") $ eExpr ceb1) | r1 <- rDeclRes $ rExprs ceb1],
        decisionPath = dp,
        cFlow  = cFlow ceb1}
      ]

stmSem (UnaryFPOp AtanOp fp a) interp env locVars config dp dps =
  semUnOp config AtanOp fp (stmSem a interp env locVars config dp dps) dp

stmSem (UnaryFPOp TanOp fp a) interp env locVars config dp dps =
  semBinOp config DivOp fp (stmSem (UnaryFPOp SinOp fp a) interp env locVars config dp dps)
                           (stmSem (UnaryFPOp CosOp fp a) interp env locVars config dp dps) dp

stmSem (UnaryFPOp op fp a) interp env locVars config dp dps =
  semUnOp config op fp (stmSem a interp env locVars config dp dps) dp

stmSem (FFma fp a1 a2 a3) interp env locVars config dp dps =
  (if improveError config then id else collapseSem) $
  filterCondFalse $ map semFma (combos [stmSem a1 interp env locVars config dp dps
                                       ,stmSem a2 interp env locVars config dp dps
                                       ,stmSem a3 interp env locVars config dp dps])
  where
    semFma :: ACebS -> ACeb
    semFma [ceb1,ceb2,ceb3] =
        ACeb {
            conds  = Conds [Cond {realPathCond = simplBExprFix $ And (realPathCond cond1)
                                                 (And (realPathCond cond2) (realPathCond cond3))
                                 ,fpPathCond = simplFBExprFix $ FAnd (fpPathCond cond1)
                                                 (FAnd (fpPathCond cond2) (fpPathCond cond3))
                                 ,realCond = simplBExprFix $ And (realCond cond1)
                                                 (And (realCond cond2) (realCond cond3))
                                 ,fpCond = simplFBExprFix $ FAnd (fpCond cond1)
                                                 (FAnd (fpCond cond2) (fpCond cond3))}
                      |cond1 <- uncond (conds ceb1),
                       cond2 <- uncond (conds ceb2),
                       cond3 <- uncond (conds ceb3)],
            rExprs  = RDeclRes [BinaryOp AddOp r1 (BinaryOp MulOp r2 r3) | r1 <- rDeclRes $ rExprs  ceb1
                                                                         , r2 <- rDeclRes $ rExprs  ceb2
                                                                         , r3 <- rDeclRes $ rExprs  ceb3],
            fpExprs = FDeclRes [FFma fp f1 f2 f3 | f1 <- fDeclRes $ fpExprs ceb1
                                                 , f2 <- fDeclRes $ fpExprs ceb2
                                                 , f3 <- fDeclRes $ fpExprs ceb3],
            eExpr  = Just $ maxErr [ErrFma fp r1 (fromMaybe (error "semFma: unexpected argument.") $ eExpr ceb1)
                                              r2 (fromMaybe (error "semFma: unexpected argument.") $ eExpr ceb2)
                                              r3 (fromMaybe (error "semFma: unexpected argument.") $ eExpr ceb3)
                            | r1 <- rDeclRes $ rExprs ceb1
                            , r2 <- rDeclRes $ rExprs ceb2
                            , r3 <- rDeclRes $ rExprs ceb3],
            decisionPath = dp,
            cFlow  = mergeControlFlow (cFlow ceb1) (mergeControlFlow (cFlow ceb2) (cFlow ceb3))
        }
    semFma _ = error "stmSem semISub: something went wrong"

stmSem UnstWarning _ _ _ _ dp _ = [ ACeb {
    conds  = trueConds,
    rExprs = RDeclRes [Int 0],
    fpExprs = FDeclRes [FInt 0],
    eExpr  = Just $ ErrRat 0,
    decisionPath = dp,
    cFlow  = Stable
    } ]

stmSem (Let [] _) _ _ _ _ _ _ = error "stmSem: empty variable list in let-in statement."

stmSem (Let (letElem:rest) stm) interp env locVars config dp dps
  | isArithExpr (exprFLetElem letElem) =
    [ACeb{
       conds = simplifyConditions $ mergeConds (varBindConds [var] [expr] [cebExpr] $ conds cebStm) (conds cebExpr)
      ,rExprs  = RDeclRes [RLet [realLetElem realExprLetElem] realExpr | realExpr <- rDeclRes $ rExprs cebStm]
      ,fpExprs = FDeclRes [Let [letElem] fpExpr | fpExpr <- fDeclRes $ fpExprs cebStm]
      ,eExpr   = Just $ RLet [realLetElem realExprLetElem, errorLetElem cebExpr]
                             (replaceInAExpr (replaceErrorMarks var) (const Nothing)
                              $ fromMaybe (error "stmSem: unexpected argument.") $ eExpr cebStm)
      ,decisionPath = decisionPath cebStm
      ,cFlow = mergeControlFlow (cFlow cebStm) (cFlow cebExpr)
    }
    | cebStm  <- stmSem newStm interp env locVars config dp dps
    , cebExpr <- stmSem expr interp env locVars config dp dps
    , realExprLetElem <- rDeclRes $ rExprs cebExpr
    ]
  where
    var  = varFLetElem  letElem
    expr = exprFLetElem letElem
    replaceErrorMarks vName (ErrorMark x ResValue _) | x==vName = Just $ Var Real (errVarName x)
    replaceErrorMarks _ _ = Nothing
    realLetElem re = LetElem {letVar  = var
                          ,letType = if typeFLetElem letElem == TInt then TInt else Real
                          ,letExpr = re}
    errorLetElem ceb = LetElem {letVar  = errVarName var
                               ,letType = Real
                               ,letExpr = fromMaybe (error "stmSem let: unexpected argument") $ eExpr ceb}
    newStm = if null rest then stm else Let rest stm

stmSem (Let (letElem:rest) stm) interp env locVars config dp dps = stmSem newStm interp newEnv locVars config dp dps
  where
    newStm = varBindLetFAExpr [letElem] (if null rest then stm else Let rest stm)
    newEnv = addLetElem2Env interp config dp dps env letElem

stmSem (Ite fbe stm1 stm2) interp env locVars config@SemConf{ assumeTestStability = sta, mergeUnstables = mu } dp dps =
  semIte sta mu dps semThen semElse fbe
  where
    semThen = stmSem stm1 interp env locVars config (dp ~> 0) dps
    semElse = stmSem stm2 interp env locVars config (dp ~> 1) dps

stmSem (ListIte listThen stmElse) interp env locVars config@SemConf { assumeTestStability = sta, mergeUnstables = mu } dp dps = semIteList sta mu dps listSemThen semElse
  where
    n = fromIntegral $ length listThen
    semElse = stmSem stmElse interp env locVars config (dp ~> n) dps
    listSemThen = buildThenCasesSem listThen 0
    buildThenCasesSem [] _ = []
    buildThenCasesSem ((be_i,stm_i):listStm) i = (be_i,stmSem stm_i interp env locVars config (dp ~> i) dps) :
                                                 buildThenCasesSem listStm (i+1)

stmSem forloop@(ForLoop _ _ (FInt _) (FInt _) _ _ forBody) _ _ _ _ _ _
  | isIntFAExpr forBody = [ACeb {
            conds  = trueConds,
            rExprs = RDeclRes [fae2real forloop] ,
            fpExprs = FDeclRes [forloop],
            eExpr  = Just $ Int 0,
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem forloop@(ForLoop _ _ (ToFloat _ (Int _)) (ToFloat _ (Int _)) _ _ forBody) _ _ _ _ _ _
  | isIntFAExpr forBody = [ACeb {
            conds  = trueConds,
            rExprs = RDeclRes [fae2real forloop] ,
            fpExprs = FDeclRes [forloop],
            eExpr  = Just $ Int 0,
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem (ForLoop {}) _ _ _ _ _ _ =
  error "stmSem: generic for loop not suported yet."

stmSem fae _ _ _ _ _ _ = error $ "stmSem: niy for " ++ show fae

semEFun :: FunName -> ResultField -> [Arg] -> [FAExpr] -> [ACebS] -> ACebS -> Int -> LDecisionPath -> ACebS
semEFun _ _ _ _ _ [] _ _ = []
semEFun fun field formArgs actualArgs semArgsCombos (c:cs) n dp =
  map (aux c formArgs actualArgs) semArgsCombos
  ++
  semEFun fun field formArgs actualArgs semArgsCombos cs n dp
  where
    aux ceb fa aa argsSem =
      ceb {conds  = Conds $ map newcond $ combos [uncond (conds ceb), conjComboArgs],
           rExprs = case rExprs ceb of
                      RDeclRes decls -> RDeclRes $ map (argsBindAExpr fa aa argsSem) decls
                      RPredRes decls -> RPredRes $ map (argsBindBExprStm fa aa argsSem) decls,
           eExpr  = case eExpr ceb of
                      Just e -> Just $ argsBindAExpr fa aa argsSem e
                      Nothing -> Nothing,
           cFlow  = mergeControlFlowList (cFlow ceb : map cFlow argsSem)
          }
      where
        combosArgs = combos (map (uncond . conds) argsSem)
        conjComboArgs = map toCond $ List.zip4 (map realPathCondArgs combosArgs)
                                               (map fpPathCondArgs combosArgs)
                                               (map realCondArgs combosArgs)
                                               (map fpCondArgs combosArgs)
        newcond [cond, condArg] =
          Cond {realPathCond = simplBExprFix $ And (argsBindBExpr fa aa argsSem (realPathCond cond)) (realPathCond condArg)
               ,fpPathCond = simplFBExprFix $ FAnd (argsBindFBExpr fa aa (fpPathCond cond)) (fpPathCond condArg)
               ,realCond = simplBExprFix $ And (argsBindBExpr fa aa argsSem (realCond cond)) (realCond condArg)
               ,fpCond = simplFBExprFix $ FAnd (argsBindFBExpr fa aa (fpCond cond)) (fpCond condArg)}
        newcond _ = error "semEFun newcond: something went wrong"
        realPathCondArgs  []  = BTrue
        realPathCondArgs arg = foldl1 And (map realPathCond arg)
        fpPathCondArgs [] = FBTrue
        fpPathCondArgs arg = foldl1 FAnd (map fpPathCond arg)
        realCondArgs  []  = BTrue
        realCondArgs arg = foldl1 And (map realCond arg)
        fpCondArgs [] = FBTrue
        fpCondArgs arg = foldl1 FAnd (map fpCond arg)

collapseSem :: [ACeb] -> [ACeb]
collapseSem sem = collapsedStableCase ++ collapsedUnstableCase
  where
    collapsedStableCase   = [mergeACebFold   stableCases | not (null stableCases)]
    collapsedUnstableCase = [mergeACebFold unstableCases | not (null unstableCases)]
    (stableCases, unstableCases) = List.partition isStable sem

unfoldLocalVars :: LocalEnv -> FAExpr -> FAExpr
unfoldLocalVars env = replaceInFAExpr (const Nothing) (unfoldLocalVars' env)

unfoldLocalVars' :: LocalEnv -> FAExpr -> Maybe FAExpr
unfoldLocalVars' env (FVar _ x) = unfoldLocalVars env <$> lookupFLetElem x env
unfoldLocalVars' _ _ = Nothing

varBindAExprReal :: [VarName] -> [AExpr] -> ACebS -> AExpr -> AExpr
varBindAExprReal fa aa _ (RealMark x ResValue) = bindRealMark fa aa
  where
    bindRealMark :: [VarName] -> [AExpr] -> AExpr
    bindRealMark [] [] = Var Real x
    bindRealMark (y:ys) (a:as) | y == x    = a
                               | otherwise = bindRealMark ys as
    bindRealMark _ _ = error "bindErrorMark: something went wrong"
varBindAExprReal fa aa _ (Var Real x) = bindRealVar fa aa
  where
    bindRealVar :: [VarName] -> [AExpr] -> AExpr
    bindRealVar [] [] = Var Real x
    bindRealVar (y:ys) (a:as) | y == x    =  a
                              | otherwise = bindRealVar ys as
    bindRealVar _ _ = error "bindErrorMark: something went wrong"
varBindAExprReal fa aa sem (EFun f field fp args) = EFun f field fp (map (varBindAExprReal fa aa sem) args)
varBindAExprReal fa aa sem (ArrayElem fp v aes) = ArrayElem fp v (map (varBindAExprReal fa aa sem) aes)
varBindAExprReal fa aa sem (ListElem fp v a) = ListElem fp v (varBindAExprReal fa aa sem a)
varBindAExprReal  _  _   _ (TupleElem fp v a) = TupleElem fp v a
varBindAExprReal  _  _   _ (RecordElem fp v a) = RecordElem fp v a
varBindAExprReal fa aa sem (UnaryOp  op a    ) = UnaryOp  op (varBindAExprReal fa aa sem a)
varBindAExprReal fa aa sem (BinaryOp op a1 a2) = BinaryOp op (varBindAExprReal fa aa sem a1)
                                                             (varBindAExprReal fa aa sem a2)
varBindAExprReal fa aa _   (FromFloat fp a) = FromFloat fp (varBindFAExpr fa (map (real2fpAexpr False False fp []) aa) a)
varBindAExprReal fa aa sem (ErrFun f fp field args rargs argErrs)
  = ErrFun f fp field (map (varBindFAExpr fa (map (real2fpAexpr False False fp []) aa)) args)
                      (map (varBindAExprReal fa aa sem) rargs)
                      (map (varBindAExprReal fa aa sem) argErrs)
varBindAExprReal fa _ sem (ErrorMark x field fp) = bindErrorMark fa sem
    where
        bindErrorMark :: [VarName] -> ACebS -> EExpr
        bindErrorMark [] [] = ErrorMark x field fp
        bindErrorMark (y:ys) (ceb:cebs) | x == y    = fromMaybe (error "varBindAExprReal: unexpected argument.") $ eExpr ceb
                                        | otherwise = bindErrorMark ys cebs
        bindErrorMark _ _ = error "bindErrorMark: something went wrong"
varBindAExprReal fa aa sem (ErrBinOp op fp r1 e1 r2 e2) = ErrBinOp op fp (varBindAExprReal fa aa sem r1)
                                                                         (varBindAExprReal fa aa sem e1)
                                                                         (varBindAExprReal fa aa sem r2)
                                                                         (varBindAExprReal fa aa sem e2)
varBindAExprReal fa aa sem (ErrSubSternenz fp r1 e1 r2 e2) = ErrSubSternenz fp (varBindAExprReal fa aa sem r1)
                                                                         (varBindAExprReal fa aa sem e1)
                                                                         (varBindAExprReal fa aa sem r2)
                                                                         (varBindAExprReal fa aa sem e2)
varBindAExprReal fa aa sem (ErrFma fp r1 e1 r2 e2 r3 e3) = ErrFma fp (varBindAExprReal fa aa sem r1)
                                                                         (varBindAExprReal fa aa sem e1)
                                                                         (varBindAExprReal fa aa sem r2)
                                                                         (varBindAExprReal fa aa sem e2)
                                                                         (varBindAExprReal fa aa sem r3)
                                                                         (varBindAExprReal fa aa sem e3)
varBindAExprReal fa aa sem (ErrUnOp op fp r e) = ErrUnOp op  fp (varBindAExprReal fa aa sem r)  (varBindAExprReal fa aa sem e)
varBindAExprReal fa aa sem (ErrFloorNoRound fp r e) = ErrFloorNoRound fp (varBindAExprReal fa aa sem r)
                                                                         (varBindAExprReal fa aa sem e)
varBindAExprReal fa aa sem (ErrCast fp1 fp2 r e)   = ErrCast fp1 fp2  (varBindAExprReal fa aa sem r)  (varBindAExprReal fa aa sem e)
varBindAExprReal fa aa sem (ErrMulPow2L fp n e)    = ErrMulPow2L fp n (varBindAExprReal fa aa sem e)
varBindAExprReal fa aa sem (ErrMulPow2R fp n e)    = ErrMulPow2R fp n (varBindAExprReal fa aa sem e)
varBindAExprReal fa aa sem (HalfUlp a fp)          = HalfUlp      (varBindAExprReal fa aa sem a) fp
varBindAExprReal fa aa sem (MaxErr es)             = MaxErr (map (varBindAExprReal fa aa sem) es)
varBindAExprReal fa aa sem (Max es)                = Max (map (varBindAExprReal fa aa sem) es)
varBindAExprReal fa aa sem (Min es)                = Min (map (varBindAExprReal fa aa sem) es)
varBindAExprReal fa aa sem (RLet letElems ae)  = RLet (map varBindAExprLetElem letElems)
                                                  (varBindAExprReal fa aa sem ae)
  where
    varBindAExprLetElem letElem = letElem { letExpr =  varBindAExprReal fa aa sem (letExpr letElem)}
varBindAExprReal fa aa sem  (RIte be aeThen aeElse) = RIte (varBindBExprReal fa aa sem be)
                                                       (varBindAExprReal fa aa sem aeThen)
                                                       (varBindAExprReal fa aa sem aeElse)
varBindAExprReal fa aa sem  (RListIte listThen aeElse) = RListIte (map (varBindlistThen fa aa sem) listThen)
                                                              (varBindAExprReal fa aa sem aeElse)
  where
    varBindlistThen fa' aa' sem' (be,ae) = (varBindBExprReal fa' aa' sem' be, varBindAExprReal fa' aa' sem' ae)
varBindAExprReal fa aa sem  (RForLoop t idx idxStart idxEnd acc initAcc forBody)
  = RForLoop t idx (varBindAExprReal fa aa sem idxStart)
               (varBindAExprReal fa aa sem idxEnd)
               acc
               (varBindAExprReal fa aa sem initAcc)
               (varBindAExprReal fa aa sem forBody)
varBindAExprReal _ _ _ a = a

varBindBExprReal :: [VarName] -> [AExpr] -> ACebS -> BExpr -> BExpr
varBindBExprReal _  _  _   BTrue       = BTrue
varBindBExprReal _  _  _   BFalse      = BFalse
varBindBExprReal fa aa sem (And e1 e2) = And (varBindBExprReal fa aa sem e1) (varBindBExprReal fa aa sem e2)
varBindBExprReal fa aa sem (Or e1 e2)  = Or  (varBindBExprReal fa aa sem e1) (varBindBExprReal fa aa sem e2)
varBindBExprReal fa aa sem (Not e)     = Not (varBindBExprReal fa aa sem e)
varBindBExprReal fa aa sem (Rel rel a1 a2)  = Rel rel (varBindAExprReal fa aa sem a1) (varBindAExprReal fa aa sem a2)
varBindBExprReal fa aa sem (EPred f args) = EPred f (map (varBindAExprReal fa aa sem) args)

varBindCond :: [VarName] -> [FAExpr] -> ACebS -> Condition -> Condition
varBindCond varNames exprs sem cond = Cond {realPathCond = varBindBExpr varNames exprs sem (realPathCond cond)
                                           ,fpPathCond = varBindFBExpr varNames exprs (fpPathCond cond)
                                           ,realCond = varBindBExpr varNames exprs sem (realCond cond)
                                           ,fpCond = varBindFBExpr varNames exprs (fpCond cond)}

varBindConds :: [VarName] -> [FAExpr] -> ACebS -> Conditions -> Conditions
varBindConds varNames exprs sem (Conds cs) = Conds (map (varBindCond varNames exprs sem) cs)

replaceLocalVarsInErrExpr :: LocalEnv -> AExpr -> AExpr
replaceLocalVarsInErrExpr localEnv = replaceInAExpr (const Nothing) replaceLocalVars
  where
    replaceLocalVars (FVar fp x) = Just $ bindVar localEnv
      where
        bindVar :: LocalEnv -> FAExpr
        bindVar [] = FVar fp x
        bindVar ((y,_,fae):ys) | y == x = fae
                               | otherwise = bindVar ys
    replaceLocalVars _ = Nothing

varBindAExpr :: [VarName] -> [FAExpr] -> ACebS -> AExpr -> AExpr
varBindAExpr fa aa _ (RealMark x field) = bindRealMark fa aa
  where
    bindRealMark :: [VarName] -> [FAExpr] -> AExpr
    bindRealMark [] _ = RealMark x field --Var Real x
    bindRealMark (y:ys) (a:as) | y == x    = fae2real a
                               | otherwise = bindRealMark ys as
    bindRealMark _ _ = error "bindErrorMark: something went wrong."
varBindAExpr fa aa _ (Var Real x) = bindRealMark fa aa
  where
    bindRealMark :: [VarName] -> [FAExpr] -> AExpr
    bindRealMark [] [] = Var Real x
    bindRealMark (y:ys) (a:as) | y == x    = fae2real a
                               | otherwise = bindRealMark ys as
    bindRealMark _ _ = error "bindErrorMark: something went wrong"
varBindAExpr _  _  _   i@(Int _)         = i
varBindAExpr _  _  _   r@(Rat _)         = r
varBindAExpr fa aa sem (EFun f field fp args) = EFun f field fp (map (varBindAExpr fa aa sem) args)
varBindAExpr fa aa sem (ArrayElem t v idxs) = ArrayElem t v (map (varBindAExpr fa aa sem) idxs)
varBindAExpr fa aa sem (ListElem t v idx) = ListElem t v (varBindAExpr fa aa sem idx)
varBindAExpr _  _  _   (TupleElem t v idx) = TupleElem t v idx
varBindAExpr _  _  _   (RecordElem t v idx) = RecordElem t v idx
varBindAExpr _  _  _   (Prec fp)  = Prec fp
varBindAExpr _  _  _   (ErrRat n) = ErrRat n
varBindAExpr _  _  _   (Var fp x) = Var fp x
varBindAExpr fa aa _   (FExp a) = FExp (varBindFAExpr fa aa a)
varBindAExpr fa aa sem (UnaryOp  op a    ) = UnaryOp  op (varBindAExpr  fa aa sem a)
varBindAExpr fa aa sem (BinaryOp op a1 a2) = BinaryOp op (varBindAExpr  fa aa sem a1) (varBindAExpr fa aa sem a2)
varBindAExpr fa aa _   (FromFloat fp a)      = FromFloat fp   (varBindFAExpr fa aa a)
varBindAExpr fa aa sem (ErrFun f fp field args rargs argErrs) = ErrFun f fp field (map (varBindFAExpr fa aa) args)
                                                                            (map (varBindAExpr fa aa sem) rargs)
                                                                            (map (varBindAExpr fa aa sem) argErrs)
varBindAExpr fa _ sem (ErrorMark x field fp) = bindErrorMark fa sem
  where
    bindErrorMark :: [VarName] -> ACebS -> EExpr
    bindErrorMark [] [] = ErrorMark x field fp
    bindErrorMark (y:ys) (ceb:cebs) | x == y    = fromMaybe (error "varBindAExpr: unexpected argument.")
                                                  $ eExpr ceb
                                    | otherwise = bindErrorMark ys cebs
    bindErrorMark _ _ = error "bindErrorMark: something went wrong"
varBindAExpr fa aa sem (ErrBinOp op fp r1 e1 r2 e2) = ErrBinOp op fp (varBindAExpr fa aa sem r1)
                                                                     (varBindAExpr fa aa sem e1)
                                                                     (varBindAExpr fa aa sem r2)
                                                                     (varBindAExpr fa aa sem e2)
varBindAExpr fa aa sem (ErrSubSternenz fp r1 e1 r2 e2) = ErrSubSternenz fp (varBindAExpr fa aa sem r1)
                                                                     (varBindAExpr fa aa sem e1)
                                                                     (varBindAExpr fa aa sem r2)
                                                                     (varBindAExpr fa aa sem e2)
varBindAExpr fa aa sem (ErrUnOp op fp r e) = ErrUnOp op fp (varBindAExpr fa aa sem r)
                                                           (varBindAExpr fa aa sem e)
varBindAExpr fa aa sem (ErrFloorNoRound fp r e) = ErrFloorNoRound fp (varBindAExpr fa aa sem r)
                                                                     (varBindAExpr fa aa sem e)
varBindAExpr fa aa sem (ErrFma fp r1 e1 r2 e2 r3 e3) = ErrFma fp (varBindAExpr fa aa sem r1)
                                                                 (varBindAExpr fa aa sem e1)
                                                                 (varBindAExpr fa aa sem r2)
                                                                 (varBindAExpr fa aa sem e2)
                                                                 (varBindAExpr fa aa sem r3)
                                                                 (varBindAExpr fa aa sem e3)
varBindAExpr fa aa sem (ErrCast fp1 fp2 r e) = ErrCast fp1 fp2 (varBindAExpr fa aa sem r)
                                                               (varBindAExpr fa aa sem e)
varBindAExpr fa aa sem (ErrMulPow2L fp n e)    = ErrMulPow2L fp n (varBindAExpr fa aa sem e)
varBindAExpr fa aa sem (ErrMulPow2R fp n e)    = ErrMulPow2R fp n (varBindAExpr fa aa sem e)
varBindAExpr fa aa sem (HalfUlp a fp)          = HalfUlp      (varBindAExpr fa aa sem a) fp
varBindAExpr fa aa sem (Max es)                = Max (map (varBindAExpr fa aa sem) es)
varBindAExpr fa aa sem (Min es)                = Min (map (varBindAExpr fa aa sem) es)
varBindAExpr fa aa sem (MaxErr es)             = MaxErr (map (varBindAExpr fa aa sem) es)
varBindAExpr _  _  _   Infinity                = Infinity
varBindAExpr _  _  _   RUnstWarning            = RUnstWarning
varBindAExpr fa aa sem (RLet letElems ae)  = RLet (map varBindAExprLetElem letElems)
                                                  (varBindAExpr fa aa sem ae)
  where
    varBindAExprLetElem letElem = letElem { letExpr =  varBindAExpr fa aa sem (letExpr letElem)}
varBindAExpr fa aa sem  (RIte be aeThen aeElse) = RIte (varBindBExpr fa aa sem be)
                                                       (varBindAExpr fa aa sem aeThen)
                                                       (varBindAExpr fa aa sem aeElse)
varBindAExpr fa aa sem  (RListIte listThen aeElse) = RListIte (map (varBindlistThen fa aa sem) listThen)
                                                              (varBindAExpr fa aa sem aeElse)
  where
    varBindlistThen fa' aa' sem' (be,ae) = (varBindBExpr fa' aa' sem' be, varBindAExpr fa' aa' sem' ae)
varBindAExpr fa aa sem  (RForLoop t idx idxStart idxEnd acc initAcc forBody)
  = RForLoop t idx (varBindAExpr fa aa sem idxStart)
               (varBindAExpr fa aa sem idxEnd)
               acc
               (varBindAExpr fa aa sem initAcc)
               (varBindAExpr fa aa sem forBody)
varBindAExpr _ _ _ x = error $ "[varBindAExpr] Unhandled case: " ++ show x

varBindFAExpr :: [VarName] -> [FAExpr] -> FAExpr -> FAExpr
varBindFAExpr fa aa = replaceInFAExpr (const Nothing) replaceFVar
  where
    replaceFVar (FVar fp x) = Just $ bindVar fa aa
      where
        bindVar :: [VarName] -> [FAExpr] -> FAExpr
        bindVar [] _ = FVar fp x
        bindVar (y:ys) (fae:as) | y == x = fae
                                       | otherwise = bindVar ys as
        bindVar args actArgs = error $ "bindVar: something went wrong: \n args: " ++ show args ++ "\n actual args:" ++ show actArgs
    replaceFVar _           = Nothing

varBindCollFAExpr :: [VarName] -> [FAExpr] -> CollFAExpr -> CollFAExpr
varBindCollFAExpr fa aa = replaceInCollFAExpr (const Nothing) replaceFVar
  where
    replaceFVar (FVar fp x) = Just $ bindVar fa aa
      where
        bindVar :: [VarName] -> [FAExpr] -> FAExpr
        bindVar [] _ = FVar fp x
        bindVar (y:ys) (fae:as) | y == x = fae
                                       | otherwise = bindVar ys as
        bindVar args actArgs = error $ "bindVar: something went wrong: \n args: " ++ show args ++ "\n actual args:" ++ show actArgs
    replaceFVar _           = Nothing

varBindBExpr :: [VarName] -> [FAExpr] -> ACebS -> BExpr -> BExpr
varBindBExpr _  _  _   BTrue       = BTrue
varBindBExpr _  _  _   BFalse      = BFalse
varBindBExpr fa aa sem (And e1 e2) = And (varBindBExpr fa aa sem e1) (varBindBExpr fa aa sem e2)
varBindBExpr fa aa sem (Or e1 e2)  = Or  (varBindBExpr fa aa sem e1) (varBindBExpr fa aa sem e2)
varBindBExpr fa aa sem (Not e)     = Not (varBindBExpr fa aa sem e)
varBindBExpr fa aa sem (Rel rel a1 a2) = Rel rel (varBindAExpr fa aa sem a1) (varBindAExpr fa aa sem a2)
varBindBExpr fa aa sem (EPred f args) = EPred f (map (varBindAExpr fa aa sem) args)

varBindBExprStm :: [VarName] -> [FAExpr] -> ACebS -> BExprStm -> BExprStm
varBindBExprStm fa aa sem (RBLet letElems stm) = RBLet (map argsBindLetElem letElems) (varBindBExprStm fa aa sem stm)
  where
    argsBindLetElem lElem = lElem {letExpr = varBindAExpr fa aa sem (letExpr lElem)}

varBindBExprStm fa aa sem (RBIte be stmThen stmElse)  = RBIte (varBindBExpr fa aa sem be)
                                                              (varBindBExprStm fa aa sem stmThen)
                                                              (varBindBExprStm fa aa sem stmElse)

varBindBExprStm fa aa sem (RBListIte listThen stmElse) = RBListIte (map argsBindThenBranch listThen)
                                                                   (varBindBExprStm fa aa sem stmElse)
  where
    argsBindThenBranch (beThen,stmThen) = (varBindBExpr fa aa sem beThen, varBindBExprStm fa aa sem stmThen)

varBindBExprStm fa aa sem (RBExpr be) = RBExpr $ varBindBExpr fa aa sem be

varBindFBExpr :: [VarName] -> [FAExpr] -> FBExpr -> FBExpr
varBindFBExpr _  _  FBTrue       = FBTrue
varBindFBExpr _  _  FBFalse      = FBFalse
varBindFBExpr _  _  v@(BStructVar _) = v
varBindFBExpr fa aa (FAnd be1 be2) = FAnd (varBindFBExpr fa aa be1) (varBindFBExpr fa aa be2)
varBindFBExpr fa aa (FOr be1 be2)  = FOr  (varBindFBExpr fa aa be1) (varBindFBExpr fa aa be2)
varBindFBExpr fa aa (FNot be)      = FNot (varBindFBExpr fa aa be)
varBindFBExpr fa aa (FRel rel a1 a2)  = FRel rel  (varBindFAExpr fa aa a1) (varBindFAExpr fa aa a2)
varBindFBExpr fa aa (IsValid ae)    = IsValid (varBindFAExpr fa aa ae)
varBindFBExpr fa aa (FEPred isTrans predAbs f args) = FEPred isTrans predAbs f (map (varBindFAExpr fa aa) args)
varBindFBExpr fa aa (BIsValid be)   = BIsValid $ varBindFBExpr fa aa be
varBindFBExpr fa aa (BValue be)     = BValue $ varBindFBExpr fa aa be

varBindFBExprStm :: [VarName] -> [FAExpr] -> FBExprStm -> FBExprStm
varBindFBExprStm fa aa (BLet letElems stm) = BLet (map argsBindLetElem letElems) (varBindFBExprStm fa aa stm)
  where
    argsBindLetElem (x,t,expr) = (x,t,varBindFAExpr fa aa expr)

varBindFBExprStm fa aa (BIte be stmThen stmElse)  = BIte (varBindFBExpr    fa aa be)
                                                          (varBindFBExprStm fa aa stmThen)
                                                          (varBindFBExprStm fa aa stmElse)

varBindFBExprStm fa aa (BListIte listThen stmElse) = BListIte (map argsBindThenBranch listThen)
                                                               (varBindFBExprStm fa aa stmElse)
  where
    argsBindThenBranch (beThen,stmThen) = (varBindFBExpr fa aa beThen, varBindFBExprStm fa aa stmThen)
varBindFBExprStm fa aa (BExpr be) = BExpr $ varBindFBExpr fa aa be
varBindFBExprStm _ _ BUnstWarning = BUnstWarning

argsBindAExprReal :: [Arg] -> [AExpr] -> ACebS -> AExpr -> AExpr
argsBindAExprReal args = varBindAExprReal (map argName args)

argsBindBExprReal :: [Arg] -> [AExpr] -> ACebS -> BExpr -> BExpr
argsBindBExprReal args = varBindBExprReal (map argName args)

argsBindAExpr :: [Arg] -> [FAExpr] -> ACebS -> AExpr -> AExpr
argsBindAExpr args = varBindAExpr (map argName args)

argsBindFAExpr :: [Arg] -> [FAExpr] -> FAExpr -> FAExpr
argsBindFAExpr args = varBindFAExpr (map argName args)

argsBindBExpr :: [Arg] -> [FAExpr] -> ACebS -> BExpr -> BExpr
argsBindBExpr args = varBindBExpr (map argName args)

argsBindBExprStm :: [Arg] -> [FAExpr] -> ACebS -> BExprStm -> BExprStm
argsBindBExprStm args = varBindBExprStm (map argName args)

argsBindFBExpr :: [Arg] -> [FAExpr] -> FBExpr -> FBExpr
argsBindFBExpr args = varBindFBExpr (map argName args)

argsBindFBExprStm :: [Arg] -> [FAExpr] -> FBExprStm -> FBExprStm
argsBindFBExprStm args = varBindFBExprStm (map argName args)

varBindLetFAExpr :: [(VarName, PVSType, FAExpr)] -> FAExpr -> FAExpr
varBindLetFAExpr letElems = varBindFAExpr (map fst3 letElems) (map trd3 letElems)

varBindLetCollFAExpr :: [(VarName, PVSType, FAExpr)] -> CollFAExpr -> CollFAExpr
varBindLetCollFAExpr letElems = varBindCollFAExpr (map fst3 letElems) (map trd3 letElems)

varBindLetFBExprStm :: [(VarName, PVSType, FAExpr)] -> FBExprStm -> FBExprStm
varBindLetFBExprStm letElems = varBindFBExprStm (map fst3 letElems) (map trd3 letElems)

varBindLetFBExpr :: [(VarName, PVSType, FAExpr)] -> FBExpr -> FBExpr
varBindLetFBExpr letElems = varBindFBExpr (map fst3 letElems) (map trd3 letElems)

initErrVars :: Interpretation -> Interpretation
initErrVars = Map.map initErrVarsSem
  where
    initErrVarsSem (isTrans, fp, vars, acebs) = (isTrans,fp, vars, Map.map (map initErrAceb) acebs)

-- removeInfiniteCebS :: Interpretation -> Interpretation
-- removeInfiniteCebS [] = []
-- removeInfiniteCebS ((f,(isTrans,fp,args,cebs)):is) =
--   if filteredCebS /= []
--   then (f,(isTrans,fp,args,filteredCebS)):removeInfiniteCebS is
--   else removeInfiniteCebS is
--   where
--     filteredCebS = filter (not . hasInfiniteError) cebs
--     hasInfiniteError ACeb{ eExpr = ee} | isNothing ee = False
--                                        | otherwise = fromJust ee == Infinity

removeInfiniteCebS :: Interpretation -> Interpretation
removeInfiniteCebS = Map.filter hasAtLeastANotInfCeb
  where
    hasAtLeastANotInfCeb (_isTrans,_fp,_args,sem) = not $ null sem
    _removeInfiniteInSem (isTrans,fp,args,cebs) = (isTrans,fp,args,notInfCebS)
      where
        notInfCebS = filter (not . hasInfiniteError) cebs
        hasInfiniteError ACeb{ eExpr = ee} | isNothing ee = False
                                       | otherwise = fromJust ee == Infinity

unfoldSemantics :: Interpretation -> Interpretation
unfoldSemantics sem = Map.map (unfoldFunCallInSem sem) sem

unfoldFunCallInSem :: Interpretation -> FunctionInterpretation -> FunctionInterpretation
unfoldFunCallInSem interp (isTrans, fp, args, sem) = (isTrans, fp, args, unfoldedSem)
  where
    unfoldedSem = Map.map (unfoldFunCallInCebS interp) sem

unfoldFunCallInCebS :: Interpretation -> ACebS -> ACebS
unfoldFunCallInCebS interp = map (unfoldFunCallInCeb interp)

unfoldFunCallInCeb :: Interpretation -> ACeb -> ACeb
unfoldFunCallInCeb interp aceb =
  aceb {conds  = Conds (elimDuplicates $ concatMap (unfoldFunCallsInCond interp) cs),
        eExpr  = (Just . unfoldFunCallInEExprRec interp) =<< ee
       }
    where
      ACeb {conds = Conds cs, eExpr = ee} = aceb

------------ TO DO --------------------------------------
-- unfold real and fp condition at the same time ----------
unfoldFunCallsInCond :: Interpretation -> Condition -> [Condition]
unfoldFunCallsInCond interp cond = elimDuplicates $
  filter (not . isFalseCond) $ unfoldFunCallsInCond' interp (reverse funCallsF) (reverse funCallsR) [cond]
  where
    funCallsF = funCallListFBExpr (fpCond cond) ++ funCallListFBExpr (fpPathCond cond)
    funCallsR = funCallListBExpr  (realCond cond) ++ funCallListBExpr (realPathCond cond)

unfoldFunCallsInCond' :: Interpretation -> [FAExpr] -> [AExpr] -> [Condition] -> [Condition]
unfoldFunCallsInCond' _ [] [] conditions = conditions

unfoldFunCallsInCond' interp [] (call@(EFun fun field _ actArgs):calls) conditions =
  unfoldFunCallsInCond' interp [] calls unfoldedConds
  where
    unfoldedConds = concatMap (unfoldRFunCall call actArgs formArgs sem) conditions
    formArgs = functionFormalArgs fun interp
    sem = functionSemantics fun field interp

unfoldFunCallsInCond' interp (call@(FEFun isTrans fun field fp actArgs):calls) funCallsR conditions =
  unfoldFunCallsInCond' interp calls funCallsR' unfoldedConds
  where
    unfoldedConds = concatMap (unfoldFpFunCall isTrans fun field fp actArgs formArgs realCall funCallsR sem) conditions
    sem = functionSemantics fun field interp
    formArgs = functionFormalArgs fun interp
    realCall = replaceInAExpr realMark2Var (const Nothing) (fae2real call)
    funCallsR' = List.delete realCall funCallsR

unfoldFunCallsInCond' _ (ae:_) _ _ = error $ "unfoldFunCallsInCond': function call expected but got " ++ show ae ++ "."
unfoldFunCallsInCond' _ [] (_:_) _ = error "unfoldFunCallsInCond': something went wrong, real and fp function call lists have a different number of elements."

unfoldRFunCall :: AExpr -> [AExpr] -> [Arg] -> [ACeb] -> Condition -> [Condition]
unfoldRFunCall call actArgs formArgs sem cond = map aux sem
  where
    aux ceb = cond {realCond = simplBExprFix $ And (argsBindBExprReal formArgs actArgs sem (realConds $ conds ceb))
                           (listOr $ map (\rExpr -> replaceInBExpr (replaceRealFunCall rExpr) (const Nothing) (realCond cond))  (rDeclRes $ rExprs ceb))
                        ,realPathCond = simplBExprFix $ And (argsBindBExprReal formArgs actArgs sem (realPathConds $ conds ceb))
                           (listOr $ map (\rExpr -> replaceInBExpr (replaceRealFunCall rExpr) (const Nothing) (realPathCond cond))  (rDeclRes $ rExprs ceb))}
    replaceRealFunCall expr = replaceR call (argsBindAExprReal  formArgs actArgs sem expr)

unfoldFpFunCall :: IsTrans -> FunName -> ResultField -> PVSType -> [FAExpr] -> [Arg] -> AExpr -> [AExpr] -> [ACeb] -> Condition -> [Condition]
unfoldFpFunCall isTrans fun field fp actArgs formArgs realCall funCallsR sem cond =
  concatMap makeConditions sem
  where
    makeConditions ceb = map (makeCondition (rDeclRes $ rExprs ceb) (fDeclRes $ fpExprs ceb)) (uncond $ conds ceb)
    makeCondition realExprs fExprs condFun =
      Cond {realPathCond = if realCall `notElem` funCallsR then realPathCond cond
                           else
                           simplBExprFix $ And (argsBindBExprReal formArgs realActArgs sem (realPathCond condFun))
                           (listOr $ map (\rExpr -> replaceInBExpr (replaceRealFunCall rExpr) (const Nothing) (realPathCond cond)) realExprs)
           ,fpPathCond = simplFBExprFix $ FAnd (argsBindFBExpr formArgs actArgs (fpPathCond condFun))
                            (listFOr $ map (\fpExpr -> replaceInFBExpr (return Nothing) (replaceFPFunCall fpExpr) (fpPathCond cond)) fExprs)
           ,realCond = if realCall `notElem` funCallsR then realCond cond
                       else
                       simplBExprFix $ And (argsBindBExprReal formArgs realActArgs sem (realCond condFun))
                           (listOr $ map (\rExpr -> replaceInBExpr (replaceRealFunCall rExpr) (const Nothing) (realCond cond)) realExprs)
           ,fpCond = simplFBExprFix $ FAnd (argsBindFBExpr formArgs actArgs (fpCond condFun))
                            (listFOr $ map (\fpExpr -> replaceInFBExpr (return Nothing) (replaceFPFunCall fpExpr) (fpCond cond)) fExprs)}
    realActArgs  = map (replaceInAExpr realMark2Var (const Nothing) . fae2real) actArgs
    fpCall   = FEFun isTrans fun field fp actArgs
    replaceRealFunCall expr = replaceR realCall (argsBindAExprReal  formArgs realActArgs sem expr)
    replaceFPFunCall   expr = replaceF fpCall   (argsBindFAExpr     formArgs actArgs     expr)

replaceF :: FAExpr -> FAExpr -> FAExpr -> Maybe FAExpr
replaceF call expr subexpr | call == subexpr = Just expr
                           | otherwise       = Nothing

replaceR :: AExpr -> AExpr -> AExpr -> Maybe AExpr
replaceR call expr subexpr | call == subexpr = Just expr
                           | otherwise       = Nothing

realMark2Var :: AExpr -> Maybe AExpr
realMark2Var (RealMark x ResValue) = Just (Var Real x)
realMark2Var _ = Nothing

unfoldFunCallInEExprRec :: Interpretation -> EExpr -> EExpr
unfoldFunCallInEExprRec interp be = if null funCalls
  then be
  else unfoldFunCallInEExprRec interp (unfoldFunCallInEExpr interp funCalls be)
  where
    funCalls = funCallListAExpr be

unfoldFunCallInEExpr :: Interpretation -> [AExpr] -> EExpr -> EExpr
unfoldFunCallInEExpr _ [] be = be
unfoldFunCallInEExpr interp (call@(EFun fun field _ actArgs):calls) be = unfoldFunCallInEExpr interp calls unfoldedFun
  where
    unfoldedFun = maxErr $ map (\expr -> replaceInAExpr (replaceR call (argsBindAExprReal formArgs actArgs sem expr)) (const Nothing) be) rexprs
    -- unfoldedFun = listOr $ map (\expr -> replaceInBExpr (replaceR call expr) (const Nothing) be) rexprs
    rexprs = concatMap (rDeclRes . rExprs) sem
    formArgs = functionFormalArgs fun interp
    sem = functionSemantics fun field interp
unfoldFunCallInEExpr _ (expr:_) _ = error $ "unfoldFunCallInEExpr: " ++ show expr ++ " is not a function call."
