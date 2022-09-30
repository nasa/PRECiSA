-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
 
-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
 
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
  

module FramaC.CLang where

import Common.TypesUtils
import qualified Common.ShowRational as Rat
import Data.Maybe (fromMaybe)
import FramaC.Types
import qualified FramaC.ACSLlang as ACSL (Pred(..)) 
import Operators
import Prelude hiding ((<>))
import PPExt

type FunName = String
type PredName = String

data  Arg = Arg Type VarName
  deriving (Eq, Ord, Show, Read) 

data Decl = Decl Type FunName [Arg] [Stm]
  deriving (Eq, Ord, Show, Read) 

data Stm = VarDecl           Type VarName
         | VarDeclAssign     Type VarName AExpr 
         | VarAssign         Type VarName AExpr
         | VarDeclAssignBool Type VarName BExpr 
         | VarAssignBool          VarName BExpr
         | Ite BExpr [Stm] [Stm]
         | ListIte [(BExpr,[Stm])] [Stm]
         | ForLoop VarName AExpr AExpr [Stm]
         | Return AExpr
         | ReturnBool BExpr
         | ACSL [ACSL.Pred]
  deriving (Eq, Ord, Show, Read) 

data AExpr = IntCnst Integer
           | FPCnst FPFormat Rational
           | ErrorCnst Double
           | Var Type VarName
           | RealMark VarName
           | TypeCast Type Type AExpr
           | EFun FunName Type [AExpr]
           | ArrayElem Type VarName AExpr
           | BinaryOp  BinOp Type AExpr AExpr
           | UnaryOp   UnOp  Type AExpr
           | Min [AExpr]
           | Max [AExpr]
           | Value AExpr
           | IteExpr  BExpr AExpr AExpr
           | ForLoopExpr VarName AExpr AExpr AExpr
           | Some Type (Either AExpr BExpr)
           | None Type
  deriving (Eq, Ord, Show, Read) 

data BExpr = BTrue
           | BFalse
           | Not BExpr
           | Or  BExpr BExpr
           | And BExpr BExpr
           | Rel RelOp AExpr AExpr
           | IsValid AExpr
           | BIsValid BExpr
           | FEPred PredName [AExpr]
           | BValue BExpr
           | BVar Type VarName
  deriving (Eq, Ord, Show, Read) 

replaceInBExpr :: (AExpr -> Maybe AExpr) -> (BExpr -> Maybe BExpr) -> BExpr -> BExpr
replaceInBExpr aef bef expr = fromMaybe (replaceInBExpr' expr) (bef expr)
  where
    replaceInBExpr' (Or  be1 be2)     = Or  (replaceInBExpr aef bef be1) (replaceInBExpr aef bef be2)
    replaceInBExpr' (And be1 be2)     = And (replaceInBExpr aef bef be1) (replaceInBExpr aef bef be2)
    replaceInBExpr' (Not be)          = Not (replaceInBExpr aef bef be)
    replaceInBExpr' (Rel op  ae1 ae2) = Rel op  (replaceInAExpr aef bef ae1) (replaceInAExpr aef bef ae2)
    replaceInBExpr' (IsValid ae)      = IsValid  (replaceInAExpr aef bef ae)
    replaceInBExpr' (BIsValid be)     = BIsValid (replaceInBExpr aef bef be)
    replaceInBExpr' (FEPred f args)   = FEPred f (map (replaceInAExpr aef bef) args)
    replaceInBExpr' BTrue  = BTrue
    replaceInBExpr' BFalse = BFalse
    replaceInBExpr' (BValue be) = BValue (replaceInBExpr aef bef be)
    replaceInBExpr' (BVar t x) = BVar t x

replaceInAExpr :: (AExpr -> Maybe AExpr) -> (BExpr -> Maybe BExpr) -> AExpr -> AExpr
replaceInAExpr aef bef expr = fromMaybe (replaceInAExpr' expr) (aef expr)
  where
    replaceInAExpr' :: AExpr -> AExpr
    replaceInAExpr' (TypeCast fromType toType ae) = TypeCast fromType toType (replaceInAExpr aef bef ae)
    replaceInAExpr' (EFun f t aes) = EFun f t (map (replaceInAExpr aef bef) aes)
    replaceInAExpr' (ArrayElem t v ae) = ArrayElem t v (replaceInAExpr aef bef ae)
    replaceInAExpr' (BinaryOp op t ae1 ae2) = BinaryOp  op t (replaceInAExpr aef bef ae1) (replaceInAExpr aef bef ae2)
    replaceInAExpr' (UnaryOp  op t ae) = UnaryOp  op t (replaceInAExpr aef bef ae)
    replaceInAExpr' (Min aes) = Min (map (replaceInAExpr aef bef) aes)
    replaceInAExpr' (Max aes) = Max (map (replaceInAExpr aef bef) aes)
    replaceInAExpr' (Value ae) = Value (replaceInAExpr aef bef ae)
    replaceInAExpr' (IteExpr be thenExpr elseExpr) = IteExpr (replaceInBExpr aef bef be) (replaceInAExpr aef bef thenExpr) (replaceInAExpr aef bef elseExpr)
    replaceInAExpr' (ForLoopExpr idx idxStart idxEnd body) = ForLoopExpr idx (replaceInAExpr aef bef idxStart) (replaceInAExpr aef bef idxEnd) (replaceInAExpr aef bef body)
    replaceInAExpr' (Some t (Left  ae)) = Some t (Left  $ replaceInAExpr aef bef ae)
    replaceInAExpr' (Some t (Right be)) = Some t (Right $ replaceInBExpr aef bef be)
    replaceInAExpr' ae = ae

getValueFunCallsBExpr :: BExpr -> BExpr
getValueFunCallsBExpr (Not be) = Not (getValueFunCallsBExpr be)
getValueFunCallsBExpr (Or  be1 be2) = Or  (getValueFunCallsBExpr be1) (getValueFunCallsBExpr be2)
getValueFunCallsBExpr (And be1 be2) = And (getValueFunCallsBExpr be1) (getValueFunCallsBExpr be2)
getValueFunCallsBExpr (Rel Eq  ae1 ae2) = Rel Eq  (getValueFunCallsAExpr ae1) (getValueFunCallsAExpr ae2)
getValueFunCallsBExpr (Rel Neq ae1 ae2) = Rel Neq (getValueFunCallsAExpr ae1) (getValueFunCallsAExpr ae2)
getValueFunCallsBExpr (Rel Lt  ae1 ae2) = Rel Lt  (getValueFunCallsAExpr ae1) (getValueFunCallsAExpr ae2)
getValueFunCallsBExpr (Rel LtE ae1 ae2) = Rel LtE (getValueFunCallsAExpr ae1) (getValueFunCallsAExpr ae2)
getValueFunCallsBExpr (Rel Gt  ae1 ae2) = Rel Gt  (getValueFunCallsAExpr ae1) (getValueFunCallsAExpr ae2)
getValueFunCallsBExpr (Rel GtE ae1 ae2) = Rel GtE (getValueFunCallsAExpr ae1) (getValueFunCallsAExpr ae2)
getValueFunCallsBExpr be@(FEPred _ _) = BValue be
getValueFunCallsBExpr be =  be

getValueFunCallsAExpr :: AExpr -> AExpr
getValueFunCallsAExpr = replaceInAExpr getValueFunCallAExpr (Just . getValueFunCallsBExpr)
  where
    getValueFunCallAExpr fun@EFun{} = Just $ Value fun
    getValueFunCallAExpr _ = Nothing

instance PPExt Arg where
  prettyDoc (Arg (Array t _) x) = prettyDoc t <+> text "*" <> text x
  prettyDoc (Arg  Real x) = text "real" <+> text x
  prettyDoc (Arg  Int  x) = text "int"  <+> text x
  prettyDoc (Arg (Float SinglePrec) x) = text "single" <+> text (x ++ "_single")
  prettyDoc (Arg (Float DoublePrec) x) = text "double" <+> text (x ++ "_double")
  prettyDoc arg = error $ "prettyDoc Arg: not defined for " ++ show arg

instance PPExt AExpr where
  prettyDoc (IntCnst  i) = integer i 
  prettyDoc (FPCnst _ rat) = text $ Rat.showFloatC rat
  prettyDoc (ErrorCnst err) = prettyErrorHex err
  prettyDoc (EFun f fp args) = text f <> text "_fp" <+> parens (docListComma $ map prettyDoc args)
  prettyDoc (Value (EFun f fp args)) = text f <> text "_fp" <+> parens (docListComma $ map prettyDoc args) <> text ".value"
  prettyDoc (Value ae) = prettyDoc ae <> text ".value"
  prettyDoc (Var t x) = printVarName t x
  prettyDoc (ArrayElem _ v idxExpr) = text v <> text "[" <> prettyDoc idxExpr <> text "]"
  prettyDoc (Some Int (Left expr)) = text "some" <> parens (prettyDoc expr)
  prettyDoc (Some (Float SinglePrec) (Left expr)) = text "someFloat" <> parens (prettyDoc expr)
  prettyDoc (Some (Float DoublePrec) (Left expr)) = text "someDouble" <> parens (prettyDoc expr)
  prettyDoc (Some Boolean (Right expr)) = text "someBool" <> parens (prettyDoc expr)
  prettyDoc (Some t ae) = error $ "prettyDoc AExpr Some not defined for type " ++ show t ++ " and expr " ++ show ae ++ "."
  prettyDoc (None  Int) = text "none()"
  prettyDoc (None (Float SinglePrec)) = text "noneFloat()"
  prettyDoc (None (Float DoublePrec)) = text "noneDouble()"
  prettyDoc (None Boolean) = text "noneBool()"
  prettyDoc (None _) = text "none()"
  -- prettyDoc (ToFloat _ (Int i)) = integer i <> text ".0"
  -- prettyDoc (ToFloat _ (Rat rat)) = text $ Rat.showRational Nothing rat
  prettyDoc (TypeCast _ (Float DoublePrec) ae) = text "(double)" <> parens (prettyDoc ae)
  prettyDoc (TypeCast _ (Float SinglePrec) ae) = text "(single)" <> parens (prettyDoc ae)
  prettyDoc (BinaryOp AddOp   _ ae1 ae2) = parens $ prettyDoc ae1 <+> text "+" <+> prettyDoc ae2
  prettyDoc (BinaryOp SubOp   _ ae1 ae2) = parens $ prettyDoc ae1 <+> text "-" <+> prettyDoc ae2
  prettyDoc (BinaryOp MulOp   _ ae1 ae2) = parens $ prettyDoc ae1 <+> text "*" <+> prettyDoc ae2
  prettyDoc (BinaryOp DivOp   _ ae1 ae2) = parens $ prettyDoc ae1 <+> text "/" <+> prettyDoc ae2
  prettyDoc (BinaryOp IDivOp  _ ae1 ae2) = parens $ prettyDoc ae1 <+> text "/" <+> prettyDoc ae2
  prettyDoc (BinaryOp ItDivOp _ ae1 ae2) = parens $ prettyDoc ae1 <+> text "/" <+> prettyDoc ae2
  prettyDoc (BinaryOp ModOp   _ ae1 ae2) = parens $ prettyDoc ae1 <+> text "%" <+> prettyDoc ae2
  prettyDoc (BinaryOp PowOp   _ ae1 ae2) = text "pow" <> parens (prettyDoc ae1 <+> comma <+> prettyDoc ae2)
  prettyDoc (UnaryOp  NegOp   _ ae) = text "-"    <+> parens (prettyDoc ae)
  prettyDoc (UnaryOp  AbsOp   (Float _) ae) = text "fabs"   <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  AbsOp   _ ae) = text "abs"   <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  FloorOp _ ae) = text "floor" <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  SqrtOp  _ ae) = text "sqrt"  <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  SinOp   _ ae) = text "sin"   <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  CosOp   _ ae) = text "cos"   <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  TanOp   _ ae) = text "floor" <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  AcosOp  _ ae) = text "asin"  <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  AsinOp  _ ae) = text "acos"  <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  AtanOp  _ ae) = text "acos"  <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  LnOp    _ ae) = text "log"   <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  ExpoOp  _ ae) = text "exp"   <> parens (prettyDoc ae)
  prettyDoc (IteExpr be thenExpr elseExpr) = prettyDoc be <+> text "?" <+> prettyDoc thenExpr <+> colon <+> prettyDoc elseExpr <> semi
  prettyDoc (Min _) = error "prettyDoc: min not defined"
  prettyDoc (Max _) = error "prettyDoc: max not defined" 
  -- prettyDoc FFma{} = error "prettyDoc: fused multiply add not defined"
  prettyDoc ae = error $ "prettyDoc: case " ++ show ae ++ " niy."

instance PPExt BExpr where
  prettyDoc (Or  be1 be2) = parens (prettyDoc be1) <+> text "||" <+> parens (prettyDoc be2)
  prettyDoc (And be1 be2) = parens (prettyDoc be1) <+> text "&&" <+> parens (prettyDoc be2)
  prettyDoc (Not be)      = text "!" <> parens (prettyDoc be)
  prettyDoc (Rel Eq  ae1 ae2) = prettyDoc ae1 <+> text "==" <+> prettyDoc ae2
  prettyDoc (Rel Neq ae1 ae2) = prettyDoc ae1 <+> text "!=" <+> prettyDoc ae2
  prettyDoc (Rel Lt  ae1 ae2) = prettyDoc ae1 <+> text "<"  <+> prettyDoc ae2
  prettyDoc (Rel LtE ae1 ae2) = prettyDoc ae1 <+> text "<=" <+> prettyDoc ae2
  prettyDoc (Rel Gt  ae1 ae2) = prettyDoc ae1 <+> text ">"  <+> prettyDoc ae2
  prettyDoc (Rel GtE ae1 ae2) = prettyDoc ae1 <+> text ">=" <+> prettyDoc ae2
  prettyDoc (IsValid (Value ae))   = prettyDoc ae  <> text ".isValid"
  prettyDoc (IsValid ae)   = prettyDoc ae  <> text ".isValid"
  prettyDoc (BIsValid be)   = prettyDoc be  <> text ".isValid"
  prettyDoc (FEPred p args) = text p <> parens (docListComma $ map prettyDoc args)
  prettyDoc (BValue be) = prettyDoc be <> text ".value"
  prettyDoc (BVar _ x) = text x
  prettyDoc BTrue  = text "true"
  prettyDoc BFalse = text "false"

prettyInstrList :: [Stm] -> Doc
prettyInstrList list = vcat (map (\stm -> prettyDoc stm <> semi) list)

printVarName :: Type -> VarName -> Doc
printVarName t@(Float _) x = text x <> text "_" <> prettyDoc t
printVarName (MaybeStruct t@(Float _)) x = printVarName t x
printVarName _           x = text x

instance PPExt Stm where
  prettyDoc (VarDecl       t x     ) = prettyDoc t <+> printVarName t x <> semi
  prettyDoc (VarDeclAssign t x expr) = prettyDoc t <+> printVarName t x
                                           <+> equals <+> prettyDoc expr <> semi
  prettyDoc (VarDeclAssignBool t x expr) = prettyDoc t <+> text x <+> equals <+> prettyDoc expr <> semi
  prettyDoc (VarAssign         t x expr) = printVarName t x <+> equals <+> prettyDoc expr <> semi
  prettyDoc (VarAssignBool       x expr) = text x <+> equals <+> prettyDoc expr <> semi
  prettyDoc (Ite bexpr stmThen stmElse) =
    text "if"
    <+> parens (prettyDoc bexpr)
    $$  text "{"
    <+> prettyListNewLine stmThen
    $$  text "} else {"
    <+> prettyListNewLine stmElse
    $$  text "}"
  prettyDoc (ListIte [] _) = error "prettyDoc ListIte: empty thenList."
  prettyDoc (ListIte ((beThen,stmThen):thenList) stmElse) =
    text "if"
    <+> parens (prettyDoc beThen) $$ text "{"
    <+> prettyInstrList stmThen
    $$ text "}"
    $$ vcat (map (\(be,stm) -> text "else if" <+> parens (prettyDoc be)
    $$ text "{" <+> prettyInstrList stm <+> text "}") thenList) 
    $$  text "else {"
    <+> prettyListNewLine stmElse
    $$  text "}"
  prettyDoc (ForLoop idx idxStart idxEnd body) =
    text "for" <> parens (text idx <> text "="  <> prettyDoc idxStart <> text ";"
    <+> text idx <> text "<=" <> prettyDoc idxEnd   <> text ";"
    <+> text idx <> text "++" ) <+> text "{"
    $$ prettyInstrList body
    $$ text "}"
  prettyDoc (Return expr) = text "return" <+> prettyDoc expr <> semi
  prettyDoc (ReturnBool expr) = text "return" <+> prettyDoc expr <> semi
  prettyDoc (ACSL acsl) = text "/*@"
                        $$ vcat (map prettyDoc acsl)
                        $$ text "@*/"

instance PPExt Decl where
  prettyDoc (Decl returnType f args body) = prettyDoc returnType
                                        <+> text f
                                        <+> parens (docListComma $ map prettyDoc args)
                                        <+> text "{"
                                        $$  nest 2 (prettyListNewLine body)
                                        $$  text "}"