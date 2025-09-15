-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module AbstractDomain where

import AbsPVSLang
import qualified Data.Set as Set
import PPExt
import Prelude hiding ((<>))
import Common.ControlFlow
import Common.DecisionPath


data Condition = Cond {
    realPathCond :: BExpr,
    fpPathCond   :: FBExpr,
    realCond     :: BExpr,
    fpCond       :: FBExpr
} deriving (Show,Eq,Ord,Read)

newtype Conditions = Conds [Condition]
    deriving (Show,Eq,Ord,Read)

data RResult = RDeclRes [AExpr] | RPredRes [BExprStm]
    deriving (Show,Eq,Ord,Read)

data FResult = FDeclRes [FAExpr] | FPredRes [FBExprStm]
    deriving (Show,Eq,Ord,Read)

data ACeb = ACeb {
    conds        :: Conditions,
    rExprs       :: RResult,
    fpExprs      :: FResult,
    eExpr        :: Maybe EExpr,
    decisionPath :: LDecisionPath,
    cFlow        :: ControlFlow
} deriving (Show,Eq,Ord,Read)

type ACebS = [ACeb]


rDeclRes :: RResult -> [AExpr]
rDeclRes (RDeclRes exprs) = exprs
rDeclRes (RPredRes _) = error "rDeclRes: argument is not a declaration."

rPredRes :: RResult -> [BExprStm]
rPredRes (RPredRes exprs) = exprs
rPredRes (RDeclRes _) = error "rPredRes: argument is not a predicate."

fDeclRes :: FResult -> [FAExpr]
fDeclRes (FDeclRes exprs) = exprs
fDeclRes (FPredRes _) = error "fDeclRes: argument is not a declaration."

fPredRes :: FResult -> [FBExprStm]
fPredRes (FPredRes exprs) = exprs
fPredRes (FDeclRes _) = error "fPredRes: argument is not a predicate."

trueCond :: Condition
trueCond = Cond {realPathCond = BTrue
                ,fpPathCond = FBTrue
                ,realCond = BTrue
                ,fpCond = FBTrue}

toCond :: (BExpr, FBExpr, BExpr, FBExpr) -> Condition
toCond (rpc, fpc, rc, fc) = Cond {realPathCond = rpc
                                 ,fpPathCond = fpc
                                 ,realCond = rc
                                 ,fpCond = fc}

trueConds :: Conditions
trueConds = Conds [trueCond]

uncond :: Conditions -> [Condition]
uncond (Conds c) = c

realConds :: Conditions -> BExpr
realConds (Conds cs) = listOr $ map realCond cs

realPathConds :: Conditions -> BExpr
realPathConds (Conds cs) = listOr $ map realPathCond cs

fpConds :: Conditions -> FBExpr
fpConds (Conds cs) = listFOr $ map fpCond cs

fpPathConds :: Conditions -> FBExpr
fpPathConds (Conds cs) = listFOr $ map fpPathCond cs

simplifyCondition :: Condition -> Condition
simplifyCondition cond = Cond {realPathCond = simplBExprFix  (realPathCond cond)
                              ,fpPathCond = simplFBExprFix (fpPathCond cond)
                              ,realCond = simplBExprFix  (realCond cond)
                              ,fpCond = simplFBExprFix (fpCond cond)}

simplifyConditions :: Conditions -> Conditions
simplifyConditions (Conds cs) = removeTrueConds $ Conds (map simplifyCondition cs)

removeTrueConds :: Conditions -> Conditions
removeTrueConds (Conds cs) = Conds $ if null res then [trueCond] else res
  where
    res = filter (/= trueCond) cs

isFalseCond :: Condition -> Bool
isFalseCond cond = isBExprEquivFalse (And (realPathCond cond) (realCond cond))
                || isFBExprEquivFalse (FAnd (fpPathCond cond) (fpCond cond))


mergeConds :: Conditions -> Conditions -> Conditions
mergeConds (Conds cs1) (Conds cs2) =
   if null cs1' && null cs2'
   then trueConds
   else Conds $ Set.toList $ Set.union (Set.fromList cs1') (Set.fromList cs2')
  where
    cs1' = filter (/= trueCond) cs1
    cs2' = filter (/= trueCond) cs2

mergeListConds :: [Conditions] -> Conditions
mergeListConds = foldl1 mergeConds

mergeRExprs :: RResult -> RResult -> RResult
mergeRExprs (RDeclRes res1) (RDeclRes res2) = RDeclRes $ Set.toList $ Set.union (Set.fromList res1) (Set.fromList res2)
mergeRExprs (RPredRes res1) (RPredRes res2) = RPredRes $ Set.toList $ Set.union (Set.fromList res1) (Set.fromList res2)
mergeRExprs res1 res2 = error $ "mergeRExprs: mismatching arguments: " ++ show res1 ++ " and " ++ show res2

mergeFpExprs :: FResult -> FResult -> FResult
mergeFpExprs (FDeclRes fpes1) (FDeclRes fpes2) = FDeclRes $ Set.toList $ Set.union (Set.fromList fpes1) (Set.fromList fpes2)
mergeFpExprs (FPredRes fpes1) (FPredRes fpes2) = FPredRes $ Set.toList $ Set.union (Set.fromList fpes1) (Set.fromList fpes2)
mergeFpExprs _ _ = error "mergeFpExprs: mismatching arguments."

unionACebS :: ACebS -> ACebS -> ACebS
unionACebS s1 s2 = Set.toList (Set.union (Set.fromList s1) (Set.fromList s2))

mergeErr :: Maybe EExpr -> Maybe EExpr -> Maybe EExpr
mergeErr (Just ee1) (Just ee2) = Just $ MaxErr [ee1,ee2]
mergeErr Nothing Nothing = Nothing
mergeErr e1 e2 = error $ "mergeErr: unexpected arguments. e1 = " ++ show e1 ++ "e2 = "++ show e2

mergeACeb :: ACeb -> ACeb -> ACeb
mergeACeb aceb1 aceb2 =
    ACeb {
        conds  = mergeConds cs1 cs2,
        rExprs = mergeRExprs re1 re2,
        fpExprs = mergeFpExprs fpe1 fpe2,
        eExpr  = mergeErr ee1 ee2,
        decisionPath = maxCommonPrefix dp1 dp2,
        cFlow  = mergeControlFlow cf1 cf2
    }
    where
        ACeb { conds = cs1, rExprs = re1, fpExprs = fpe1, eExpr = ee1, decisionPath = dp1, cFlow = cf1} = aceb1
        ACeb { conds = cs2, rExprs = re2, fpExprs = fpe2, eExpr = ee2, decisionPath = dp2, cFlow = cf2} = aceb2

mergeACebFold :: ACebS -> ACeb
mergeACebFold [] = error "mergeACebFold: empty list."
mergeACebFold list = foldl1 mergeACeb list

isStable :: ACeb -> Bool
isStable ACeb { cFlow = c } = c == Stable

isUnstable :: ACeb -> Bool
isUnstable ACeb { cFlow = c } = c == Unstable

initErrAceb :: ACeb -> ACeb
initErrAceb aceb =
    aceb {
        conds = initErrCond cc,
        eExpr = case ee of
                  Just e -> Just $ initAExpr e
                  Nothing -> Nothing
    }
    where
        ACeb { conds = cc, eExpr = ee } = aceb

initErrCond :: Conditions -> Conditions
initErrCond (Conds cs) = Conds $ map initErrBExprs cs
    where
        initErrBExprs cond = Cond {realPathCond = initBExpr (realPathCond cond)
                                  ,fpPathCond = initFBExpr (fpPathCond cond)
                                  ,realCond = initBExpr (realCond cond)
                                  ,fpCond = initFBExpr (fpCond cond)}

ppCond :: Condition -> Doc
ppCond cond = prettyDoc (And (realPathCond cond) (realCond cond))
             <+> text "AND" <+>
             prettyDoc (FAnd (fpPathCond cond) (fpCond cond))

isTrueCondition :: Conditions -> Bool
isTrueCondition (Conds cs) = all isTrueCond cs

isTrueCond :: Condition -> Bool
isTrueCond cond = simplifyCondition cond == trueCond

instance PPExt Conditions where
    prettyDoc (Conds c) = hsep $ punctuate (text " OR") (map (parens . ppCond) c)

instance PPExt ACeb where
  prettyDoc ACeb { conds  = cs, rExprs = re,  eExpr = ee, cFlow = c, decisionPath = LDP dp}
      = prettyDoc cs <+> text "=>" <+> text "error =" <> prettyMaybe ee
        <+> text "real expr =" <+> prettyResult re (text "\n")
        <+> text "flow =" <+> prettyDoc c
        <+> text "decisionPath =" <+> (text . show) dp
    where
      prettyResult (RDeclRes rExpr) =  prettyList rExpr
      prettyResult (RPredRes rExpr) =  prettyList rExpr
      prettyMaybe (Just e) = prettyDoc e
      prettyMaybe Nothing = text "Nothing"

localVarsFResult :: FResult -> [(String, FAExpr)]
localVarsFResult (FDeclRes exprs) = concatMap localVars exprs
localVarsFResult (FPredRes exprs) = concatMap localVarsBExprStm exprs

renameVarsConds :: VarSubs -> Conditions -> Conditions
renameVarsConds subs (Conds cs) = Conds $ map renameVarsCond cs
  where
    renameVarsCond cond = cond {
      realPathCond = renameVarsBExpr  subs (realPathCond cond),
      fpPathCond   = renameVarsFBExpr subs (fpPathCond cond),
      realCond     = renameVarsBExpr  subs (realCond cond),
      fpCond       = renameVarsFBExpr subs (fpCond cond)}

renameVarsFResult :: VarSubs -> FResult -> FResult
renameVarsFResult subs (FDeclRes exprs) = FDeclRes $ map (renameVarsFAExpr subs) exprs
renameVarsFResult subs (FPredRes exprs) = FPredRes $ map (renameVarsFBExprStm subs) exprs

renameVarsRResult :: VarSubs -> RResult -> RResult
renameVarsRResult subs (RDeclRes exprs) = RDeclRes $ map (renameVarsAExpr subs) exprs
renameVarsRResult subs (RPredRes exprs) = RPredRes $ map (renameVarsBExprStm subs) exprs
