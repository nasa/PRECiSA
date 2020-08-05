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

type Condition = (BExpr,FBExpr)

newtype Conditions = Cond [Condition]
    deriving (Eq,Ord, Show)

data ACeb = ACeb {
    conds        :: Conditions,
    rExprs       :: [AExpr],
    fpExprs      :: [FAExpr],
    eExpr        :: EExpr,
    decisionPath :: LDecisionPath,
    cFlow        :: ControlFlow
} deriving (Show,Eq,Ord)

type ACebS = [ACeb]

uncond :: Conditions -> [Condition]
uncond (Cond c) = c

realCond :: Condition -> BExpr
realCond (rc,_) = rc

realConds :: Conditions -> BExpr
realConds (Cond cs) = listOr $ map realCond cs

fpCond :: Condition -> FBExpr
fpCond (_,fc) = fc

fpConds :: Conditions -> FBExpr
fpConds (Cond cs) = listFOr $ map fpCond cs

simplifyCondition :: Condition -> Condition
simplifyCondition (be,fbe) = (simplBExprFix be, simplFBExprFix fbe)

simplifyConditions :: Conditions -> Conditions
simplifyConditions (Cond cs) = removeTrueConds $ Cond ( map simplifyCondition cs) 

removeTrueConds :: Conditions -> Conditions
removeTrueConds (Cond cs) = Cond $ if null res then [(BTrue,FBTrue)] else res
  where
    res = filter (\a -> a /= (BTrue, FBTrue)) cs

isConditionInconsistent :: Condition -> Bool
isConditionInconsistent (be,fbe) = isBExprEquivFalse be || isFBExprEquivFalse fbe

mergeConds :: Conditions -> Conditions -> Conditions
mergeConds (Cond cs1) (Cond cs2) = Cond $ Set.toList $ Set.union (Set.fromList cs1) (Set.fromList cs2)

mergeListConds :: [Conditions] -> Conditions
mergeListConds = foldl1 mergeConds 

mergeRExprs :: [AExpr] -> [AExpr] -> [AExpr]
mergeRExprs res1 res2 = Set.toList $ Set.union (Set.fromList res1) (Set.fromList res2)

mergeFpExprs :: [FAExpr] -> [FAExpr] -> [FAExpr]
mergeFpExprs fpes1 fpes2 = Set.toList $ Set.union (Set.fromList fpes1) (Set.fromList fpes2)

unionACebS :: ACebS -> ACebS -> ACebS
unionACebS s1 s2 = Set.toList (Set.union (Set.fromList s1) (Set.fromList s2))

mergeACeb :: ACeb -> ACeb -> ACeb
mergeACeb aceb1 aceb2 =
    ACeb {
        conds  = mergeConds cs1 cs2,
        rExprs = mergeRExprs re1 re2,
        fpExprs = mergeFpExprs fpe1 fpe2,
        eExpr  = MaxErr [ee1,ee2],
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
        eExpr = initAExpr ee
    }
    where
        ACeb { conds = cc, eExpr = ee } = aceb

initErrCond :: Conditions -> Conditions
initErrCond (Cond cs) = Cond $ map initErrBExprs cs
    where
        initErrBExprs (be, fbe) = (initBExpr be, initFBExpr fbe)

ppCond :: Condition -> Doc
ppCond (rc,fc) = prettyDoc rc  <+> text "AND" <+> prettyDoc fc

instance PPExt Conditions where
    prettyDoc (Cond c) = hsep $ punctuate (text " OR") (map (parens . ppCond) c)

instance PPExt ACeb where
    prettyDoc ACeb { conds  = cs, rExprs = re,  eExpr = ee, cFlow = c, decisionPath = LDP dp}
        = prettyDoc cs <+> text "=>" <+> text "error =" <> prettyDoc ee
          <+> text "real expr =" <+> prettyList re (text "\n")
          <+> text "flow =" <+> prettyDoc c
          <+> text "decisionPath =" <+> (text . show) dp 
