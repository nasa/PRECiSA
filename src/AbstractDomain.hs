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


{-#LANGUAGE GADTs #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}

module AbstractDomain where

import AbsPVSLang
import qualified Data.List as List
import qualified Data.Set as Set 
import PPExt


---------------------
-- Interpretations --
---------------------

type Interp = [(String, (FPrec, [VarId], CebS))] 

botInterp :: Interp -- bottom interpretation
botInterp = []


---------------------
-- Abstract domain --
---------------------

type CebS = [Ceb]
type Ceb = (BExpr, FBExpr, FAExpr, AExpr, EExpr, PVStree) 

condR :: Ceb -> BExpr
condR (cond, _, _, _, _, _) = cond

condFP :: Ceb -> FBExpr
condFP (_, fcond, _, _, _, _) = fcond

exprFP :: Ceb -> FAExpr
exprFP (_, _, expr, _, _, _) = expr

exprR :: Ceb -> AExpr
exprR (_, _, _, vals, _, _) = vals

exprErr :: Ceb -> EExpr
exprErr (_, _, _, _, err, _) = err

pvsTree :: Ceb -> PVStree
pvsTree (_, _, _, _, _, pvs) = pvs


--------------------
-- PVS proof tree --
--------------------

data PVStree = SIntR
             | SDoubleR
             | SPiR
             | SVarR String
             | SFunR String Int [EExpr] [AExpr] [FAExpr] [PVStree]
             | SAddR EExpr EExpr
                     AExpr AExpr
                     FAExpr FAExpr
                     PVStree PVStree
             | SSubR EExpr EExpr
                     AExpr AExpr
                     FAExpr FAExpr
                     PVStree PVStree
             | SMulR EExpr EExpr
                     AExpr AExpr
                     FAExpr FAExpr
                     PVStree PVStree
             | SDivR EExpr EExpr
                     AExpr AExpr
                     FAExpr FAExpr
                     PVStree PVStree
             | SNegR    EExpr AExpr FAExpr PVStree
             | SFloorR  EExpr AExpr FAExpr PVStree
             | SFloor0R  EExpr AExpr FAExpr PVStree
             | SSqrtR   EExpr AExpr FAExpr PVStree
             | SAbsR    EExpr AExpr FAExpr PVStree
             | SSinR    EExpr AExpr FAExpr PVStree
             | SCosR    EExpr AExpr FAExpr PVStree
             | SArcsinR EExpr AExpr FAExpr PVStree
             | SArccosR EExpr AExpr FAExpr PVStree
             | SUnTest  EExpr AExpr AExpr AExpr PVStree -- unstable test
             -------------------------------------------------------
             | DIntR
             | DDoubleR
             | DPiR
             | DVarR String
             | DFunR String Int [EExpr] [AExpr] [FAExpr] [PVStree]
             | DAddR EExpr EExpr
                     AExpr AExpr
                     FAExpr FAExpr
                     PVStree PVStree
             | DSubR EExpr EExpr
                     AExpr AExpr
                     FAExpr FAExpr
                     PVStree PVStree
             | DMulR EExpr EExpr
                     AExpr AExpr
                     FAExpr FAExpr
                     PVStree PVStree
             | DDivR EExpr EExpr
                     AExpr AExpr
                     FAExpr FAExpr
                     PVStree PVStree
             | DNegR    EExpr AExpr FAExpr PVStree
             | DFloorR  EExpr AExpr FAExpr PVStree
             | DFloor0R  EExpr AExpr FAExpr PVStree
             | DSqrtR   EExpr AExpr FAExpr PVStree
             | DAbsR    EExpr AExpr FAExpr PVStree
             | DSinR    EExpr AExpr FAExpr PVStree
             | DCosR    EExpr AExpr FAExpr PVStree
             | DArcsinR EExpr AExpr FAExpr PVStree
             | DArccosR EExpr AExpr FAExpr PVStree
             | DUnTest  EExpr AExpr AExpr AExpr PVStree -- unstable test
    deriving (Show)             


-------------------
-- aux functions --
-------------------

combos :: [[t]] -> [[t]]
-- returns all the possible combination for a lists of lists
-- (ex combos [[1,2,3],[4,5]] = [1,4],[1,5],[2,4],[2,5],[3,4],[3,5])
combos [] = [[]]
combos ([]:ls) = []
combos ((h:t):ls) = map (h:) (combos ls) ++ combos (t:ls)


-----------------------
-- PPExt instances --
-----------------------

lemmas :: String -> Int -> Doc
lemmas f n = hsep (punctuate (text " ") (lemmas' f n n))
  where
    lemmas' f n 0 = []
    lemmas' f n m = (text "\"" <> text f <> text "_" <> int (n-m) <> text "\""):(lemmas' f n (m-1))

instance PPExt (PVStree) where
    prettyDoc SIntR = text "%|- (eval-formulas +)"
    prettyDoc SDoubleR  = text "%|- (eval-formulas +)"
    prettyDoc SPiR = text "%|- (eval-formulas +)"
    prettyDoc (SVarR x) = text "%|- (propax)"    
    prettyDoc (SUnTest e r1 r2 f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"abs_dist_other\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-   " <> text "\"" <> prAExpr f FPSingle <> text "\""
        $$ text "%|-   " <> text "\"" <> prAExpr r1 FPSingle <> text "\""
        $$ text "%|-   " <> text "\"" <> prAExpr r2 FPSingle <> text "\""
        $$ text "%|-   " <> text "\"" <> prEExpr e FPSingle <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (SFunR fun n es is fs ts) =
        text "%|- (then"
        $$ text "%|- (branch-lemmas" <+> (parens $ lemmas fun n)
        $$ text "%|- (then"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ vcat (map (\e -> text "%|-    " <> text "\"" <> prEExpr e FPSingle <> text "\"") es)
        $$ vcat (map (\i -> text "%|-    " <> text "\"" <> prAExpr i FPSingle <> text "\"") is)
        $$ vcat (map (\f -> text "%|-    " <> text "\"" <> prFAExpr f FPSingle <> text "\"") fs)
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ vcat (map prettyDoc ts)
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))))"
    prettyDoc (SAddR e1 e2 i1 i2 f1 f2 t1 t2) =
        text "%|- (then"
        $$ text "%|- (lemma \"Sadd_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e1 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prEExpr e2 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i1 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i2 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f1 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f2 FPSingle <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t1
        $$ prettyDoc t2
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (SSubR e1 e2 i1 i2 f1 f2 t1 t2) =
        text "%|- (then"
        $$ text "%|- (lemma \"Ssub_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e1 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prEExpr e2 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i1 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i2 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f1 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f2 FPSingle <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t1
        $$ prettyDoc t2
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (SMulR e1 e2 i1 i2 f1 f2 t1 t2) =
        text "%|- (then"
        $$ text "%|- (lemma \"Smul_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e1 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prEExpr e2 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i1 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i2 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f1 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f2 FPSingle <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t1
        $$ prettyDoc t2   
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (SDivR e1 e2 i1 i2 f1 f2 t1 t2) =
        text "%|- (then"
        $$ text "%|- (lemma \"Sdiv_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e1 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prEExpr e2 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i1 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i2 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f1 FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f2 FPSingle <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t1
        $$ prettyDoc t2
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (SNegR e i f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"Sneg_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f FPSingle <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (SFloorR e i f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"Sfloor_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f FPSingle <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (SFloor0R e i f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"Sfloor_t_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f FPSingle <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (SSqrtR e i f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"Ssqrt_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i FPSingle <> text "\"" 
        $$ text "%|-    " <> text "\"" <> prFAExpr f FPSingle <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (SAbsR e i f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"Sabs_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f FPSingle <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (SSinR e i f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"Ssin_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i FPSingle <> text "\"" 
        $$ text "%|-    " <> text "\"" <> prFAExpr f FPSingle <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (SCosR e i f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"Scos_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i FPSingle <> text "\""  
        $$ text "%|-    " <> text "\"" <> prFAExpr f FPSingle <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (SArcsinR e i f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"Sarcsin_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f FPSingle <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (SArccosR e i f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"Sarccos_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i FPSingle <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f FPSingle <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    --------------------------------------------------------
    prettyDoc DIntR = text "%|- (eval-formulas +)"
    prettyDoc DDoubleR  = text "%|- (eval-formulas +)"
    prettyDoc DPiR = text "%|- (eval-formulas +)"
    prettyDoc (DVarR x) = text "%|- (propax)"
    prettyDoc (DUnTest e r1 r2 f t) = 
        text "%|- (then"
        $$ text "%|- (lemma \"abs_dist_other\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prAExpr f  FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr r1 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr r2 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prEExpr e FPDouble <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (DFunR fun n es is fs ts) =
        text "%|- (then"
        $$ text "%|- (branch-lemmas" <+> (parens $ lemmas fun n)
        $$ text "%|- (then"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ vcat (map (\e -> text "%|-    " <> text "\"" <> prEExpr e FPDouble <> text "\"") es)
        $$ vcat (map (\i -> text "%|-    " <> text "\"" <> prAExpr i FPDouble <> text "\"") is)
        $$ vcat (map (\f -> text "%|-    " <> text "\"" <> prFAExpr f FPDouble <> text "\"") fs)
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ vcat (map prettyDoc ts)
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))))"
    prettyDoc (DAddR e1 e2 i1 i2 f1 f2 t1 t2) =
        text "%|- (then"
        $$ text "%|- (lemma \"Dadd_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e1 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prEExpr e2 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i1 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i2 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f1 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f2 FPDouble <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t1
        $$ prettyDoc t2
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (DSubR e1 e2 i1 i2 f1 f2 t1 t2) =
        text "%|- (then"
        $$ text "%|- (lemma \"Dsub_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e1 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prEExpr e2 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i1 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i2 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f1 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f2 FPDouble <> text "\"" 
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t1
        $$ prettyDoc t2
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (DMulR e1 e2 i1 i2 f1 f2 t1 t2) =
        text "%|- (then"
        $$ text "%|- (lemma \"Dmul_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e1 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prEExpr e2 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i1 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i2 FPDouble <> text "\"" 
        $$ text "%|-    " <> text "\"" <> prFAExpr f1 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f2 FPDouble <> text "\"" 
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t1
        $$ prettyDoc t2
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (DDivR e1 e2 i1 i2 f1 f2 t1 t2) =
        text "%|- (then"
        $$ text "%|- (lemma \"Ddiv_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e1 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prEExpr e2 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i1 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i2 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f1 FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f2 FPDouble <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t1
        $$ prettyDoc t2
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"       
    prettyDoc (DNegR e i f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"Dneg_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f FPDouble <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (DFloorR e i f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"Dfloor_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i FPDouble <> text "\"" 
        $$ text "%|-    " <> text "\"" <> prFAExpr f FPDouble <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t  
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (DFloor0R e i f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"Dfloor_t_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i FPDouble <> text "\"" 
        $$ text "%|-    " <> text "\"" <> prFAExpr f FPDouble <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t  
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (DSqrtR e i f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"Dsqrt_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f FPDouble <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (DAbsR e i f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"Dabs_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i FPDouble <> text "\"" 
        $$ text "%|-    " <> text "\"" <> prFAExpr f FPDouble <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (DSinR e i f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"Dsin_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i FPDouble <> text "\"" 
        $$ text "%|-    " <> text "\"" <> prFAExpr f FPDouble <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (DCosR e i f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"Dcos_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i FPDouble <> text "\"" 
        $$ text "%|-    " <> text "\"" <> prFAExpr f FPDouble <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (DArcsinR e i f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"Darcsin_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i FPDouble <> text "\""  
        $$ text "%|-    " <> text "\"" <> prFAExpr f FPDouble <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"
    prettyDoc (DArccosR e i f t) =
        text "%|- (then"
        $$ text "%|- (lemma \"Darccos_aerr\")"
        $$ text "%|- (let ((new-label (freshname \"l\")))"
        $$ text "%|-    (then (label new-label -1)"
        $$ text "%|- (branch (with-tccs (inst new-label"
        $$ text "%|-    " <> text "\"" <> prEExpr e FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prAExpr i FPDouble <> text "\""
        $$ text "%|-    " <> text "\"" <> prFAExpr f FPDouble <> text "\""
        $$ text "%|- ))" 
        $$ text "%|- ((branch"
        $$ text "%|- (split -1)"
        $$ text "%|- ("
        $$ text "%|- (then (aerr-assert) (fail))"
        $$ prettyDoc t
        $$ text "%|- (assert-condition)"
        $$ text "%|- ))"
        $$ text "%|- (assert-condition))))))"




