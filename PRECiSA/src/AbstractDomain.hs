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

module AbstractDomain where

import AbsPVSLang
import qualified Data.List as List
import qualified Data.Set as Set
import PPExt
import FPrec
import Common.ControlFlow
import Common.DecisionPath


type Condition = (BExpr,FBExpr)

newtype Conditions = Cond [Condition]
    deriving (Show,Eq,Ord)


uncond :: Conditions -> [Condition]
uncond (Cond c) = c

cond :: [Condition] -> Conditions
cond c = Cond c

realCond :: Condition -> BExpr
realCond (rc,fc) = rc

fpCond :: Condition -> FBExpr
fpCond (rc,fc) = fc

andCond :: Condition -> Condition -> Condition
andCond (rc1, fc1) (rc2, fc2) = (And rc1 rc2, FAnd fc1 fc2)

orCond :: Condition -> Condition -> Condition
orCond (rc1, fc1) (rc2, fc2) = (Or rc1 rc2, FOr fc1 fc2)

mergeConds :: Conditions -> Conditions -> Conditions
mergeConds (Cond cs1) (Cond cs2) = Cond $ cs1++cs2

data ACeb = ACeb {
    conds        :: Conditions,
    rExprs       :: [AExpr],
    eExpr        :: EExpr,
    decisionPath :: LDecisionPath,
    cFlow        :: ControlFlow
} deriving (Show,Eq,Ord)

type ACebS = [ACeb]

unionACebS :: ACebS -> ACebS -> ACebS
unionACebS s1 s2 = Set.toList (Set.union (Set.fromList s1) (Set.fromList s2))


mergeACeb :: ACeb -> ACeb -> ACeb
mergeACeb aceb1 aceb2 =
    ACeb {
        conds  = mergeConds cs1 cs2,
        rExprs = re1 ++ re2,
        eExpr  = MaxErr [ee1,ee2],
        decisionPath = maxCommonPrefix dp1 dp2,
        cFlow  = if cf1 == cf2 then cf1 else error $ "mergeACeb: trying to merging conditional bounds of different type"
    }
    where
        ACeb { conds = cs1, rExprs = re1, eExpr = ee1, decisionPath = dp1, cFlow = cf1} = aceb1
        ACeb { conds = cs2, rExprs = re2, eExpr = ee2, decisionPath = dp2, cFlow = cf2} = aceb2

mergeACebFold :: ACebS -> ACeb
mergeACebFold acebs = foldl1 mergeACeb acebs

isStable :: ACeb -> Bool
isStable ACeb { cFlow = c } = c == Stable

isUnstable :: ACeb -> Bool
isUnstable ACeb { cFlow = c } = c == Unstable



initErrAceb aceb = 
    aceb {
        conds = initErrCond cc,
        eExpr = initErrExpr ee
    }
    where
        ACeb { conds = cc, eExpr = ee } = aceb


-- PrettyPrint Instances --

ppCond :: FPrec -> Condition -> Doc
ppCond fp (rc,fc) = prettyDocWith fp rc  <+> text "AND" <+> prettyDocWith fp fc

instance PPExt Conditions where
    prettyDoc _ = error "prettyDoc Conditions undefined, fp needed" 
    prettyDocWith fp (Cond c) = hsep $ punctuate (text " OR") (map (parens . ppCond fp) c)


instance PPExt ACeb where
    prettyDoc ACeb { conds  = cs, rExprs = re,  eExpr = ee, cFlow = c, decisionPath = LDP dp}
        = prettyDoc cs <+> text "=>" <+> text "error =" <> prettyDoc ee
          <+> text "real expr =" <+> prettyList re (text "\n")
          <+> text "flow =" <+> prettyDoc c
          <+> text "decisionPath =" <+> (text . show) dp 

    prettyDocWith fp ACeb { conds  = cs, rExprs = re,  eExpr = ee, cFlow = c, decisionPath = LDP dp}
        = prettyDocWith fp cs <+> text "=>" <+> text "error =" <> prettyDocWith fp ee
          <+> text "real expr =" <+> prettyListWith fp (text "\n") re 
          <+> text "flow =" <+> prettyDoc c
          <+> text "decisionPath =" <+> (text . show) dp 


dummyACeb = ACeb {
    conds  = Cond [],
    rExprs = [],
    eExpr  = ErrRat (toRational 3),
    decisionPath = root,
    cFlow  = Stable
}


initErrCond :: Conditions -> Conditions
initErrCond (Cond conds) = Cond $ map initErrBExprs conds
    where
        initErrBExprs (be, fbe) = (initBExpr be, initFBExpr fbe)






