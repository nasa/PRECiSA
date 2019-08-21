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
raw2Spec decls (AbsRawSpecLang.Spec sbs) = AbsSpecLang.Spec (map (raw2SpecBind decls) sbs)

raw2SpecBind :: [Decl] -> AbsRawSpecLang.SpecBind -> AbsSpecLang.SpecBind
raw2SpecBind decls (AbsRawSpecLang.SpecBind f _ varBinds) = AbsSpecLang.SpecBind f (map (raw2varBind args) varBinds)
    where 
        (_,args,_) = findInDecls f decls

raw2varBind :: [Arg] -> AbsRawSpecLang.VarBind -> AbsSpecLang.VarBind
raw2varBind args (AbsRawSpecLang.VarBind x lb ub) = AbsSpecLang.VarBind x fp (raw2LBound lb) (raw2UBound ub)
    where
       fp = fromJust $ lookup x (map mapArg2Pair args)

raw2LBound :: AbsRawSpecLang.LBound -> AbsSpecLang.LBound
raw2LBound (AbsRawSpecLang.LBInt n) = AbsSpecLang.LBInt n
raw2LBound (AbsRawSpecLang.LBDouble rat) = AbsSpecLang.LBDouble rat
raw2LBound AbsRawSpecLang.LInf = AbsSpecLang.LInf

raw2UBound :: AbsRawSpecLang.UBound -> AbsSpecLang.UBound
raw2UBound (AbsRawSpecLang.UBInt n) = AbsSpecLang.UBInt n
raw2UBound (AbsRawSpecLang.UBDouble rat) = AbsSpecLang.UBDouble rat
raw2UBound AbsRawSpecLang.UInf = AbsSpecLang.UInf
