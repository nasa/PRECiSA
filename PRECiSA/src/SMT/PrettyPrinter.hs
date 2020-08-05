-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
 
-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
 
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
    
    
module SMT.PrettyPrinter where

import AbstractDomain
import AbsPVSLang
import AbsSpecLang
import PPExt
import PVSTypes
import Prelude hiding ((<>))
import qualified Common.ShowRational as Rat
import Operators
import Common.TypesUtils

baseRVarName:: String
baseRVarName  = "Real_"

prettySMTVarId :: VarName -> Doc
prettySMTVarId var = text var <> text "[real]"

prettySMTVarIds :: [VarName] -> Doc
prettySMTVarIds vars = hsep $ punctuate comma (map prettySMTVarId vars)

prettySMTFVarId :: VarName -> PVSType -> Doc
prettySMTFVarId var fp = text var <> text "[" <> prettySMTPVSType fp <> text "]"

prettySMTFVarIds :: [(VarName,PVSType)] -> Doc
prettySMTFVarIds vars = hsep $ punctuate comma (map (uncurry prettySMTFVarId) vars)

prettySMTVarBinds :: [VarBind] -> Doc
prettySMTVarBinds varBinds = hsep $ punctuate comma (map prettySMTVarBind varBinds)

prettySMTVarBind :: VarBind -> Doc
prettySMTVarBind (VarBind var fp LInf UInf) =
    text var <> text "[" <> prettySMTPVSType fp <> text "]"
prettySMTVarBind (VarBind var fp lb ub) =
    text var <> text "[" <> prettySMTPVSType fp <> text "] ="
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

prettySMTPVSType :: PVSType -> Doc
prettySMTPVSType FPSingle = text "single"
prettySMTPVSType FPDouble = text "double"
prettySMTPVSType _ = error "prettySMTPVSType: unexpected value" 

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
prettySMTbexpr BTrue  = text "true"
prettySMTbexpr BFalse = text "false"
prettySMTbexpr (Or  be1 be2) = text "or"  <> parens (prettySMTbexpr' be1 <+> comma <+> prettySMTbexpr' be2)
prettySMTbexpr (And be1 be2) = prettySMTbexpr be1 $$ prettySMTbexpr be2
prettySMTbexpr (Not be)      = text "not" <> parens (prettySMTbexpr' be)
prettySMTbexpr (Rel Eq  ae1 ae2) = prettySMTaexpr ae1 <+> text "="   <+> prettySMTaexpr ae2
prettySMTbexpr (Rel Neq ae1 ae2) = prettySMTaexpr ae1 <+> text "!="  <+> prettySMTaexpr ae2
prettySMTbexpr (Rel Lt  ae1 ae2) = prettySMTaexpr ae1 <+> text "<"   <+> prettySMTaexpr ae2
prettySMTbexpr (Rel LtE ae1 ae2) = prettySMTaexpr ae1 <+> text "<="  <+> prettySMTaexpr ae2
prettySMTbexpr (Rel Gt  ae1 ae2) = prettySMTaexpr ae1 <+> text ">"   <+> prettySMTaexpr ae2
prettySMTbexpr (Rel GtE ae1 ae2) = prettySMTaexpr ae1 <+> text ">="  <+> prettySMTaexpr ae2
prettySMTbexpr be = error $ "prettySMTbexpr: " ++ show be ++ "not supported yet."

prettySMTbexpr' :: BExpr -> Doc
prettySMTbexpr' (And be1 be2) = text "and" <> parens (prettySMTbexpr' be1 <> comma <+> prettySMTbexpr' be2)
prettySMTbexpr' fbe = prettySMTbexpr fbe

prettySMTfbexpr :: FBExpr -> Doc
prettySMTfbexpr FBTrue         = text "true"
prettySMTfbexpr FBFalse        = text "false"
prettySMTfbexpr (FOr  be1 be2) = text "or"  <> parens (prettySMTfbexpr' be1 <> comma <+> prettySMTfbexpr' be2)
prettySMTfbexpr (FAnd be1 be2) = prettySMTfbexpr be1 $$ prettySMTfbexpr be2
prettySMTfbexpr (FNot be)      = text "not" <> parens (prettySMTfbexpr' be)
prettySMTfbexpr (FRel Eq  ae1 ae2) = prettySMTfaexpr ae1 <+> text "="   <+> prettySMTfaexpr ae2
prettySMTfbexpr (FRel Neq ae1 ae2) = prettySMTfaexpr ae1 <+> text "!="  <+> prettySMTfaexpr ae2
prettySMTfbexpr (FRel Lt  ae1 ae2) = prettySMTfaexpr ae1 <+> text "<"   <+> prettySMTfaexpr ae2
prettySMTfbexpr (FRel LtE ae1 ae2) = prettySMTfaexpr ae1 <+> text "<="  <+> prettySMTfaexpr ae2
prettySMTfbexpr (FRel Gt  ae1 ae2) = prettySMTfaexpr ae1 <+> text ">"   <+> prettySMTfaexpr ae2
prettySMTfbexpr (FRel GtE ae1 ae2) = prettySMTfaexpr ae1 <+> text ">="  <+> prettySMTfaexpr ae2
prettySMTfbexpr (IsValid _)    = error "prettySMTfbexpr not defined for IsValid."
prettySMTfbexpr be = error $ "prettySMTfbexpr: " ++ show be ++ "not supported yet."

prettySMTfbexpr' :: FBExpr -> Doc
prettySMTfbexpr' (FAnd be1 be2) = text "and" <> parens (prettySMTfbexpr' be1 <+> comma <+> prettySMTfbexpr' be2)
prettySMTfbexpr' fbe = prettySMTfbexpr fbe


prettySMTfaexpr :: FAExpr -> Doc
prettySMTfaexpr (BinaryFPOp AddOp _ ae1 ae2) = text "+FP" <+> prettySMTfaexpr ae1 <+> prettySMTfaexpr ae2
prettySMTfaexpr (BinaryFPOp SubOp _ ae1 ae2) = text "-FP" <+> prettySMTfaexpr ae1 <+> prettySMTfaexpr ae2
prettySMTfaexpr (BinaryFPOp MulOp _ ae1 ae2) = text "*FP" <+> prettySMTfaexpr ae1 <+> prettySMTfaexpr ae2
prettySMTfaexpr (BinaryFPOp DivOp _ ae1 ae2) = text "/FP" <+> prettySMTfaexpr ae1 <+> prettySMTfaexpr ae2
prettySMTfaexpr (UnaryFPOp  NegOp _ ae) = text "-" <+> prettySMTfaexpr ae
prettySMTfaexpr (UnaryFPOp  AbsOp _ ae) = text "abs" <> parens (prettySMTfaexpr ae)
prettySMTfaexpr (FCnst _ d) = prettySMTRat d
prettySMTfaexpr (FVar _ x) = text x
prettySMTfaexpr (FInt i) = integer i <> text ".0"
prettySMTfaexpr (ToFloat FPDouble (Int i)) = integer i <> text ".0"
prettySMTfaexpr (ToFloat FPSingle (Int i)) = integer i <> text ".0"
prettySMTfaexpr (ToFloat FPDouble (Rat d)) = prettySMTRat d
prettySMTfaexpr (ToFloat FPSingle (Rat d)) = prettySMTRat d
prettySMTfaexpr fa = error $ "prettySMTfaexpr niy: " ++ show fa

prettySMTaexpr :: AExpr -> Doc
prettySMTaexpr (Rat d) = prettySMTRat d
prettySMTaexpr (Var _ x) = text x
prettySMTaexpr (Int i) = integer i <> text ".0"
prettySMTaexpr (RealMark x) = text $ baseRVarName ++ x

prettySMTaexpr (BinaryOp AddOp ae1 ae2) = prettySMTaexpr ae1 <+> text "+" <+> prettySMTaexpr ae2
prettySMTaexpr (BinaryOp SubOp ae1 ae2) = prettySMTaexpr ae1 <+> text "-" <+> prettySMTaexpr ae2
prettySMTaexpr (BinaryOp MulOp ae1 ae2) = prettySMTaexpr ae1 <+> text "*" <+> prettySMTaexpr ae2
prettySMTaexpr (BinaryOp DivOp ae1 ae2) = prettySMTaexpr ae1 <+> text "/" <+> prettySMTaexpr ae2
prettySMTaexpr (UnaryOp  NegOp ae) = text "-" <+> prettySMTaexpr ae
prettySMTaexpr (UnaryOp  AbsOp ae) = text "abs" <> parens (prettySMTaexpr ae)

prettySMTaexpr (FromFloat _ (FVar _  x)) = text x
prettySMTaexpr (FromFloat _ (FInt    i)) = integer i <> text ".0"
prettySMTaexpr (FromFloat _ (FCnst _ d)) = prettySMTRat d
prettySMTaexpr ae = error $ "prettySMTaexpr: " ++ show ae ++ "not supported yet."


