module SMT.PrettyPrinter where

import AbstractDomain
import AbsPVSLang
import AbsSpecLang
import PPExt
--import Text.PrettyPrint
import FPrec
import Prelude hiding ((<>))
import qualified Common.ShowRational as Rat

baseRVarName:: String
baseRVarName  = "Real_"

prettySMTVarId :: VarName -> Doc
prettySMTVarId var = text var <> text "[real]"

prettySMTVarIds :: [VarName] -> Doc
prettySMTVarIds vars = hsep $ punctuate comma (map prettySMTVarId vars)

prettySMTFVarId :: VarName -> FPrec -> Doc
prettySMTFVarId var fp = text var <> text "[" <> prettySMTFPrec fp <> text "]"

prettySMTFVarIds :: [(VarName,FPrec)] -> Doc
prettySMTFVarIds vars = hsep $ punctuate comma (map (uncurry prettySMTFVarId) vars)

prettySMTVarBinds :: [VarBind] -> Doc
prettySMTVarBinds varBinds = hsep $ punctuate comma (map prettySMTVarBind varBinds)

prettySMTVarBind :: VarBind -> Doc
prettySMTVarBind (VarBind var fp LInf UInf) =
    text var <> text "[" <> prettySMTFPrec fp <> text "]"
prettySMTVarBind (VarBind var fp lb ub) =
    text var <> text "[" <> prettySMTFPrec fp <> text "] ="
    <+> text "["<> prettySMTLBound lb <> semi <> prettySMTUBound ub <> text "]"

prettySMTVarBindsReal :: [VarBind] -> Doc
prettySMTVarBindsReal varBinds = hsep $ punctuate comma (map prettySMTVarBindReal varBinds)

prettySMTVarBindReal :: VarBind -> Doc
prettySMTVarBindReal (VarBind var _ LInf UInf) =
    text baseRVarName <> text var <> text "[real]"
prettySMTVarBindReal (VarBind var _ lb ub) =
    text baseRVarName <> text var <> text "[real] ="
    <+> text "["<> prettySMTLBound lb <> semi <> prettySMTUBound ub <> text "]"

prettySMTUBound :: UBound -> Doc
prettySMTUBound (UBInt n) = prettySMTInt n
prettySMTUBound (UBDouble rat) = prettySMTRat rat
prettySMTUBound UInf = text "inf"

prettySMTLBound :: LBound -> Doc
prettySMTLBound (LBInt n) = prettySMTInt n
prettySMTLBound (LBDouble rat) = prettySMTRat rat
prettySMTLBound LInf = text "inf"

prettySMTInt :: Integer -> Doc
prettySMTInt n = integer n <> text ".0"

prettySMTRat :: Rational -> Doc
prettySMTRat rat = text $ Rat.showRational Nothing rat -- double $ ((fromRat rat) :: Double) --

prettySMTFPrec :: FPrec -> Doc
prettySMTFPrec FPSingle = text "single"
prettySMTFPrec FPDouble = text "double"
prettySMTFPrec _ = error "prettySMTFPrec: unexpected value" 

prettySMT :: Conditions -> Doc
prettySMT (Cond [])       = text "false"
prettySMT (Cond [c])      = prettySMTCondition c
prettySMT (Cond listCond) = text "or" <> parens (vcat $ punctuate comma (map prettySMTCondition' listCond))

prettySMTCondition :: Condition -> Doc
prettySMTCondition (BTrue ,  FBTrue) = empty
prettySMTCondition (BFalse, FBFalse) = text "false"
prettySMTCondition (BTrue ,     fbe) = prettySMTfbexpr fbe
prettySMTCondition (be    ,  FBTrue) = prettySMTbexpr  be
prettySMTCondition (be    ,     fbe) = prettySMTbexpr  be $$ prettySMTfbexpr fbe

prettySMTCondition' :: Condition -> Doc
prettySMTCondition' (BTrue ,  FBTrue) = empty
prettySMTCondition' (BFalse, FBFalse) = text "false"
prettySMTCondition' (BTrue ,     fbe) = prettySMTfbexpr fbe
prettySMTCondition' (be    ,  FBTrue) = prettySMTbexpr  be
prettySMTCondition' (be    ,     fbe) = text "and" <> parens (prettySMTbexpr' be <> comma <+> prettySMTfbexpr' fbe)


prettySMTbexpr :: BExpr -> Doc
prettySMTbexpr (Or  be1 be2) = text "or"  <> parens (prettySMTbexpr' be1 <+> comma <+> prettySMTbexpr' be2)
prettySMTbexpr (And be1 be2) = prettySMTbexpr be1 $$ prettySMTbexpr be2
prettySMTbexpr (Not be)      = text "not" <> parens (prettySMTbexpr' be)
prettySMTbexpr (Eq  ae1 ae2) = prettySMTaexpr ae1 <+> text "="   <+> prettySMTaexpr ae2
prettySMTbexpr (Neq ae1 ae2) = prettySMTaexpr ae1 <+> text "!="  <+> prettySMTaexpr ae2
prettySMTbexpr (Lt  ae1 ae2) = prettySMTaexpr ae1 <+> text "<"   <+> prettySMTaexpr ae2
prettySMTbexpr (LtE ae1 ae2) = prettySMTaexpr ae1 <+> text "<="  <+> prettySMTaexpr ae2
prettySMTbexpr (Gt  ae1 ae2) = prettySMTaexpr ae1 <+> text ">"   <+> prettySMTaexpr ae2
prettySMTbexpr (GtE ae1 ae2) = prettySMTaexpr ae1 <+> text ">="  <+> prettySMTaexpr ae2
prettySMTbexpr BTrue  = text "true"
prettySMTbexpr BFalse = text "false"

prettySMTbexpr' :: BExpr -> Doc
prettySMTbexpr' (And be1 be2) = text "and" <> parens (prettySMTbexpr' be1 <> comma <+> prettySMTbexpr' be2)
prettySMTbexpr' fbe = prettySMTbexpr fbe

prettySMTfbexpr :: FBExpr -> Doc
prettySMTfbexpr (FOr  be1 be2) = text "or"  <> parens (prettySMTfbexpr' be1 <> comma <+> prettySMTfbexpr' be2)
prettySMTfbexpr (FAnd be1 be2) = prettySMTfbexpr be1 $$ prettySMTfbexpr be2
prettySMTfbexpr (FNot be)      = text "not" <> parens (prettySMTfbexpr' be)
prettySMTfbexpr (FEq  ae1 ae2) = prettySMTfaexpr ae1 <+> text "="   <+> prettySMTfaexpr ae2
prettySMTfbexpr (FNeq ae1 ae2) = prettySMTfaexpr ae1 <+> text "!="  <+> prettySMTfaexpr ae2
prettySMTfbexpr (FLt  ae1 ae2) = prettySMTfaexpr ae1 <+> text "<"   <+> prettySMTfaexpr ae2
prettySMTfbexpr (FLtE ae1 ae2) = prettySMTfaexpr ae1 <+> text "<="  <+> prettySMTfaexpr ae2
prettySMTfbexpr (FGt  ae1 ae2) = prettySMTfaexpr ae1 <+> text ">"   <+> prettySMTfaexpr ae2
prettySMTfbexpr (FGtE ae1 ae2) = prettySMTfaexpr ae1 <+> text ">="  <+> prettySMTfaexpr ae2
prettySMTfbexpr FBTrue         = text "true"
prettySMTfbexpr FBFalse        = text "false"
prettySMTfbexpr (IsValid _)    = error "prettySMTfbexpr not defined for IsValid."

prettySMTfbexpr' :: FBExpr -> Doc
prettySMTfbexpr' (FAnd be1 be2) = text "and" <> parens (prettySMTfbexpr' be1 <+> comma <+> prettySMTfbexpr' be2)
prettySMTfbexpr' fbe = prettySMTfbexpr fbe


prettySMTfaexpr :: FAExpr -> Doc
prettySMTfaexpr (FAdd _ ae1 ae2) = text "+FP" <+> prettySMTfaexpr ae1 <+> prettySMTfaexpr ae2
prettySMTfaexpr (FSub _ ae1 ae2) = text "-FP" <+> prettySMTfaexpr ae1 <+> prettySMTfaexpr ae2
prettySMTfaexpr (FMul _ ae1 ae2) = text "*FP" <+> prettySMTfaexpr ae1 <+> prettySMTfaexpr ae2
prettySMTfaexpr (FDiv _ ae1 ae2) = text "/FP" <+> prettySMTfaexpr ae1 <+> prettySMTfaexpr ae2
prettySMTfaexpr (FNeg _ ae) = text "-" <+> prettySMTfaexpr ae
prettySMTfaexpr (FAbs _ ae) = text "abs" <> parens (prettySMTfaexpr ae)
prettySMTfaexpr (FCnst _ d) = prettySMTRat d
prettySMTfaexpr (FVar _ x) = text x
prettySMTfaexpr (FInt i) = integer i <> text ".0"
prettySMTfaexpr (RtoD (Int i)) = integer i <> text ".0"
prettySMTfaexpr (RtoS (Int i)) = integer i <> text ".0"
prettySMTfaexpr (RtoD (Rat d)) = prettySMTRat d
prettySMTfaexpr (RtoS (Rat d)) = prettySMTRat d
prettySMTfaexpr fa = error $ "prettySMTfaexpr niy: " ++ show fa

prettySMTaexpr :: AExpr -> Doc
prettySMTaexpr (Add ae1 ae2) = prettySMTaexpr ae1 <+> text "+" <+> prettySMTaexpr ae2
prettySMTaexpr (Sub ae1 ae2) = prettySMTaexpr ae1 <+> text "-" <+> prettySMTaexpr ae2
prettySMTaexpr (Mul ae1 ae2) = prettySMTaexpr ae1 <+> text "*" <+> prettySMTaexpr ae2
prettySMTaexpr (Div ae1 ae2) = prettySMTaexpr ae1 <+> text "/" <+> prettySMTaexpr ae2
prettySMTaexpr (Neg ae) = text "-" <+> prettySMTaexpr ae
prettySMTaexpr (Abs ae) = text "abs" <> parens (prettySMTaexpr ae)
prettySMTaexpr (Rat d) = prettySMTRat d
prettySMTaexpr (Var _ x) = text x
prettySMTaexpr (Int i) = integer i <> text ".0"
prettySMTaexpr (RealMark x) = text $ baseRVarName ++ x
prettySMTaexpr (DtoR (FVar _ x)) = text x
prettySMTaexpr (StoR (FVar _ x)) = text x
prettySMTaexpr (DtoR (FInt i)) = integer i <> text ".0"
prettySMTaexpr (StoR (FInt i)) = integer i <> text ".0"
prettySMTaexpr (DtoR (FCnst _ d)) = prettySMTRat d
prettySMTaexpr ae = error $ "prettySMTaexpr: " ++ show ae ++ "not supported yet."


