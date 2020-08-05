-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
 
-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
 
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
    
    
module MapSpecLangAST
where

import Parser.ParRawSpecLang
import Parser.LexRawSpecLang
import AbsRawSpecLang
import AbsSpecLang
import AbsPVSLang
import ErrM
import Data.Maybe

rawparserSpec :: String -> Err AbsRawSpecLang.Spec
rawparserSpec = pSpec . tokens

raw2Spec :: [Decl] -> AbsRawSpecLang.Spec -> AbsSpecLang.Spec
raw2Spec decls (AbsRawSpecLang.Specification sbs) = AbsSpecLang.Spec (map (raw2SpecBind decls) sbs)

makeDecl :: String -> [AbsRawSpecLang.VarBind] -> [Decl] -> AbsSpecLang.SpecBind
makeDecl f varBinds decls = AbsSpecLang.SpecBind f (map (raw2varBind args) varBinds)
    where 
        (_,args,_) = fromMaybe (error $ "makeDecl: function " ++ show f ++ "not found in " ++ show decls ++ ".") (findInDecls f decls)

raw2SpecBind :: [Decl] -> AbsRawSpecLang.SpecBind -> AbsSpecLang.SpecBind
raw2SpecBind decls (AbsRawSpecLang.SpecBindN (Id f) _ varBinds) = makeDecl f varBinds decls
raw2SpecBind decls (AbsRawSpecLang.SpecBind0 (Id f)   varBinds) = makeDecl f varBinds decls

raw2varBind :: [Arg] -> AbsRawSpecLang.VarBind -> AbsSpecLang.VarBind
raw2varBind args (AbsRawSpecLang.VarSpec (Id x) lb ub) = AbsSpecLang.VarBind x fp (raw2LBound lb) (raw2UBound ub)
    where
       fp = fromMaybe (error $ "runFunction: arg " ++ show x ++ " not found.") (lookup x (map mapArg2Pair args))

raw2LBound :: AbsRawSpecLang.LBound -> AbsSpecLang.LBound
raw2LBound (AbsRawSpecLang.LBInt n) = AbsSpecLang.LBInt n
raw2LBound (AbsRawSpecLang.LBNegInt n) = AbsSpecLang.LBInt (-n)
raw2LBound (AbsRawSpecLang.LBDouble rat) = AbsSpecLang.LBDouble (toRational rat)
raw2LBound (AbsRawSpecLang.LBNegDouble rat) = AbsSpecLang.LBDouble (toRational (-rat))
raw2LBound AbsRawSpecLang.LInf = AbsSpecLang.LInf

raw2UBound :: AbsRawSpecLang.UBound -> AbsSpecLang.UBound
raw2UBound (AbsRawSpecLang.UBInt n) = AbsSpecLang.UBInt n
raw2UBound (AbsRawSpecLang.UBNegInt n) = AbsSpecLang.UBInt (-n)
raw2UBound (AbsRawSpecLang.UBDouble rat) = AbsSpecLang.UBDouble (toRational rat)
raw2UBound (AbsRawSpecLang.UBNegDouble rat) = AbsSpecLang.UBDouble (toRational (-rat))
raw2UBound AbsRawSpecLang.UInf = AbsSpecLang.UInf
