-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
 
-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
 
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.

  
module FramaC.ACSLlang where

import Prelude hiding ((<>))
import PPExt
import Operators
import FramaC.ACSLTypes
import Common.TypesUtils
import Common.ShowRational(showFloatC)
import Data.Maybe (fromMaybe)
import Data.List.Extra (stripSuffix)

type FunName  = String
type PredName = String
type Size = Integer
type IsRec = Bool

data  Arg = Arg Type VarName
  deriving (Eq, Ord, Show, Read) 

type LetElem  = (VarName, Type, AExpr)
type FLetElem = (VarName, Type,FAExpr)

data FAExpr = FInt  Integer
            | FCnst Type Rational
            | FEFun String Type [FAExpr]
            | FVar  Type VarName
            | FArrayElem Type (Maybe ArraySize) VarName FAExpr
            | FTypeCast Type Type FAExpr
            | ToFloat  Type AExpr
            | FValue FAExpr
            | FResult
            | BinaryFPOp BinOp Type FAExpr FAExpr
            | UnaryFPOp  UnOp  Type FAExpr
            | FFma Type FAExpr FAExpr FAExpr
            | FMin [FAExpr]
            | FMax [FAExpr]
            | FLet [FLetElem] FAExpr
            | FIte FBExpr FAExpr FAExpr
            | FListIte [(FBExpr, FAExpr)] FAExpr
            | ForLoop Type FAExpr FAExpr FAExpr VarName VarName FAExpr
  deriving (Eq, Ord, Show, Read) 

data FBExprStm
  = BLet [FLetElem] FBExprStm
  | BIte FBExpr FBExprStm FBExprStm
  | BListIte [(FBExpr,FBExprStm)] FBExprStm
  | BExpr FBExpr
  deriving (Eq, Ord, Read, Show) 

data FBExpr = FBTrue
            | FBFalse
            | FOr  FBExpr FBExpr
            | FAnd FBExpr FBExpr
            | FNot FBExpr
            | FRel RelOp FAExpr FAExpr
            | FBetween FAExpr FAExpr FAExpr
            | FEPred PredAbs PredName [FAExpr]
  deriving (Eq, Ord, Show, Read) 

data AExpr = IntCnst Integer
           | RatCnst Rational
           | Var Type VarName
           | RealMark VarName
           | ErrorCnst Double
           | Result
           | EFun FunName Type [AExpr]
           | ArrayElem VarName  AExpr
           | FromFloat FPFormat AExpr
           | TypeCast Type Type AExpr
           | BinaryOp BinOp AExpr AExpr
           | UnaryOp  UnOp  AExpr
           | Min [AExpr]
           | Max [AExpr]
           | MaxErr [AExpr]
           | Value AExpr
          --  | Let VarName Type AExpr AExpr
           | Let [LetElem] AExpr
           | Ite BExpr  AExpr AExpr
           | ListIte [(BExpr, AExpr)] AExpr
           | HalfUlp FPFormat AExpr
           | ErrRat Rational
           | ErrorMark VarName FPFormat
           | ErrUnOp  UnOp Bool Type AExpr AExpr
           | ErrBinOp BinOp Type AExpr AExpr AExpr AExpr
           | ErrCast Type Type AExpr AExpr
           | ErrMulPow2R FPFormat Integer AExpr
           | ErrMulPow2L FPFormat Integer AExpr
  deriving (Eq, Ord, Show, Read) 

data BExprStm
  = RBLet [LetElem] BExprStm
  | RBIte BExpr BExprStm BExprStm
  | RBListIte [(BExpr,BExprStm)] BExprStm
  | RBExpr BExpr
  deriving (Eq, Ord, Read, Show) 

data BExpr = BTrue
           | BFalse
           | Not BExpr
           | Or  BExpr BExpr
           | And BExpr BExpr
           | Rel RelOp AExpr AExpr
           | Between AExpr AExpr AExpr
           | EPred PredName [AExpr] 
  deriving (Eq, Ord, Show, Read) 

data Pred = PredBExpr  BExpr
          | PredFBExpr FBExpr
          | PredNot Pred
          | PredAnd Pred Pred
          | PredOr  Pred Pred
          | Implies Pred Pred
          | Iff Pred Pred
          | Forall [(String,Type)] Pred
          | Exists [(String,Type)] Pred
          | IsValid AExpr
          | PredIte BExpr Pred Pred
          | PredLet  VarName AExpr Pred
          | FPredLet Type VarName FAExpr Pred
          | Pred PredName [AExpr]
          | Assigns [VarName]
          | Ensures  Pred
          | Requires Pred
          | ErrorDiseq FAExpr AExpr  AExpr
          | FErrorDiseq FAExpr AExpr FAExpr
          | LoopVariant AExpr
          | LoopInvariant Pred
          | LoopAssigns [VarName]
          | AExprPred AExpr
          | FAExprPred FAExpr
          | IsFiniteFP FAExpr
  deriving (Eq, Ord, Show, Read) 

data Decl = Decl IsRec Type FunName [Arg] AExpr
          | RPred FunName [Arg] BExprStm
  deriving (Eq, Ord, Show, Read) 

data FPDecl = FPDecl IsRec Type FunName [Arg] FAExpr
            | FPPred FunName [Arg] FBExprStm
  deriving (Eq, Ord, Show, Read) 

data PredDecl = PredDecl FunName [Arg] Pred
  deriving (Eq, Ord, Show, Read) 

format :: Type -> FPFormat
format (Float fp) = fp
format t = error $ "format: not defined for " ++ show t

arg2var :: Arg -> AExpr
arg2var (Arg t x) = Var t x

arg2fpvar :: Arg -> FAExpr
arg2fpvar (Arg t x) = FVar t x

arg2fpvarWithType :: Type -> Arg -> FAExpr
arg2fpvarWithType _  (Arg Int x) = FVar Int x
arg2fpvarWithType fp (Arg _   x) = FVar fp x

arg2varWithType :: Type -> Arg -> AExpr
arg2varWithType _ (Arg Int x) = Var Int x
arg2varWithType fp (Arg _   x) = Var fp x

printBinOpErrorACSL ::  String -> AExpr -> AExpr -> AExpr -> AExpr -> Doc
printBinOpErrorACSL nameErrFun r1 e1 r2 e2 =
    text nameErrFun <> (text "(" <>  prettyDoc r1 <> comma <+> prettyDoc e1 <> comma
                                 <+> prettyDoc r2 <> comma <+> prettyDoc e2 <> text ")")

printUnaryOpErrorACSL ::  String -> AExpr -> AExpr -> Doc
printUnaryOpErrorACSL nameErrFun r e = text nameErrFun <> (text "(" <>  prettyDoc r <> comma
                                                                    <+> prettyDoc e <> text ")")

prettyLetElem :: (PPExt b) => (String, Type, b) -> Doc
prettyLetElem (x, t, expr) = text "\\let"
                         <+> prVarName x t
                         <+> text "="
                         <+> prettyDoc expr
                         <+> text ";"

prVarName :: String -> Type -> Doc
prVarName v (Float fp) = text v <> text "_" <> prettyDoc fp 
prVarName v _ = text v

instance PPExt Arg where
  prettyDoc (Arg (Array t _) x) = prettyDoc t <+> text "*" <> text x
  prettyDoc (Arg  Real x) = text "real" <+> text x
  prettyDoc (Arg  Int  x) = text "integer"  <+> text x
  prettyDoc (Arg (Float SinglePrec) x) = text "single" <+> text (x ++ "_single")
  prettyDoc (Arg (Float DoublePrec) x) = text "double" <+> text (x ++ "_double")
  prettyDoc arg = error $ "prettyDoc Arg: not implemented for " ++ show arg

instance PPExt FAExpr where
    prettyDoc (FInt i) = integer i
    prettyDoc (FCnst _ d) = parens $ text $ showFloatC d
    prettyDoc (FVar t x) = prVarName x t
    -- prettyDoc (FVar _ x) = text x
    prettyDoc  FResult = text "\\result"
    prettyDoc (FValue (FVar _ x)) = text x <> text ".value"
    prettyDoc (FValue FResult)    = text "\\result.value"
    prettyDoc (FValue e) = parens (prettyDoc e) <> text ".value"
    prettyDoc (FArrayElem _ _ v idx) = text v <> text "[" <> prettyDoc idx <> text "]"
    prettyDoc (FEFun f _ []) = text f
    prettyDoc (FEFun f _ args) = text f <> parens (hsep $ punctuate comma $ map prettyDoc args)
    --
    prettyDoc (ToFloat  (Float SinglePrec)          a) = prettyDoc a --prettyDocUnaryOp "RtoS"   a
    prettyDoc (ToFloat  (Float DoublePrec)          a) = prettyDoc a --prettyDocUnaryOp "RtoD"   a
    --
    prettyDoc (FTypeCast _ (Float DoublePrec) a) = text "(double)" <+> parens (prettyDoc a)
    prettyDoc (FTypeCast _ (Float SinglePrec) a) = text "(float)"  <+> parens (prettyDoc a)
    --
    prettyDoc (UnaryFPOp NegOp   Int     a) = prettyDocUnaryOp "Ineg"   a
    prettyDoc (UnaryFPOp NegOp   (Float SinglePrec) a) = prettyDocUnaryOp "Sneg"   a
    prettyDoc (UnaryFPOp NegOp   (Float DoublePrec) a) = prettyDocUnaryOp "Dneg"   a
    prettyDoc (UnaryFPOp AbsOp   Int     a) = prettyDocUnaryOp "Iabs"   a
    prettyDoc (UnaryFPOp AbsOp   (Float SinglePrec) a) = prettyDocUnaryOp "Sabs"   a
    prettyDoc (UnaryFPOp AbsOp   (Float DoublePrec) a) = prettyDocUnaryOp "Dabs"   a
    prettyDoc (UnaryFPOp FloorOp (Float SinglePrec) a) = prettyDocUnaryOp "Sfloor" a
    prettyDoc (UnaryFPOp FloorOp (Float DoublePrec) a) = prettyDocUnaryOp "Dfloor" a
    prettyDoc (UnaryFPOp SqrtOp  (Float SinglePrec) a) = prettyDocUnaryOp "Ssqrt"  a
    prettyDoc (UnaryFPOp SqrtOp  (Float DoublePrec) a) = prettyDocUnaryOp "Dsqrt"  a
    prettyDoc (UnaryFPOp SinOp   (Float SinglePrec) a) = prettyDocUnaryOp "Ssin"   a
    prettyDoc (UnaryFPOp SinOp   (Float DoublePrec) a) = prettyDocUnaryOp "Dsin"   a
    prettyDoc (UnaryFPOp CosOp   (Float SinglePrec) a) = prettyDocUnaryOp "Scos"   a
    prettyDoc (UnaryFPOp CosOp   (Float DoublePrec) a) = prettyDocUnaryOp "Dcos"   a
    prettyDoc (UnaryFPOp TanOp   (Float SinglePrec) a) = prettyDocUnaryOp "Stan"   a
    prettyDoc (UnaryFPOp TanOp   (Float DoublePrec) a) = prettyDocUnaryOp "Dtan"   a
    prettyDoc (UnaryFPOp AsinOp  (Float SinglePrec) a) = prettyDocUnaryOp "Sasin"  a
    prettyDoc (UnaryFPOp AsinOp  (Float DoublePrec) a) = prettyDocUnaryOp "Dasin"  a
    prettyDoc (UnaryFPOp AcosOp  (Float SinglePrec) a) = prettyDocUnaryOp "Sacos"  a
    prettyDoc (UnaryFPOp AcosOp  (Float DoublePrec) a) = prettyDocUnaryOp "Dacos"  a
    prettyDoc (UnaryFPOp AtanOp  (Float SinglePrec) a) = prettyDocUnaryOp "Satan"  a
    prettyDoc (UnaryFPOp AtanOp  (Float DoublePrec) a) = prettyDocUnaryOp "Datan"  a
    prettyDoc (UnaryFPOp LnOp    (Float SinglePrec) a) = prettyDocUnaryOp "Sln"    a
    prettyDoc (UnaryFPOp LnOp    (Float DoublePrec) a) = prettyDocUnaryOp "Dln"    a
    prettyDoc (UnaryFPOp ExpoOp  (Float SinglePrec) a) = prettyDocUnaryOp "Sexp"   a
    prettyDoc (UnaryFPOp ExpoOp  (Float DoublePrec) a) = prettyDocUnaryOp "Dexp"   a
    --
    prettyDoc (BinaryFPOp AddOp   Int     a1 a2) = prettyDocBinaryOp "Iadd"  a1 a2
    prettyDoc (BinaryFPOp AddOp   (Float SinglePrec) a1 a2) = prettyDocBinaryOp "Sadd"  a1 a2
    prettyDoc (BinaryFPOp AddOp   (Float DoublePrec) a1 a2) = prettyDocBinaryOp "Dadd"  a1 a2
    prettyDoc (BinaryFPOp SubOp   Int     a1 a2) = prettyDocBinaryOp "Isub"  a1 a2
    prettyDoc (BinaryFPOp SubOp   (Float SinglePrec) a1 a2) = prettyDocBinaryOp "Ssub"  a1 a2
    prettyDoc (BinaryFPOp SubOp   (Float DoublePrec) a1 a2) = prettyDocBinaryOp "Dsub"  a1 a2
    prettyDoc (BinaryFPOp MulOp   Int     a1 a2) = prettyDocBinaryOp "Imul"  a1 a2
    prettyDoc (BinaryFPOp MulOp   (Float SinglePrec) a1 a2) = prettyDocBinaryOp "Smul"  a1 a2
    prettyDoc (BinaryFPOp MulOp   (Float DoublePrec) a1 a2) = prettyDocBinaryOp "Dmul"  a1 a2
    prettyDoc (BinaryFPOp DivOp   Int     a1 a2) = prettyDocBinaryOp "Idiv"  a1 a2
    prettyDoc (BinaryFPOp DivOp   (Float SinglePrec) a1 a2) = prettyDocBinaryOp "Sdiv"  a1 a2
    prettyDoc (BinaryFPOp DivOp   (Float DoublePrec) a1 a2) = prettyDocBinaryOp "Ddiv"  a1 a2
    prettyDoc (BinaryFPOp ItDivOp Int     a1 a2) = prettyDocBinaryOp "Itdiv" a1 a2
    prettyDoc (BinaryFPOp ModOp   Int     a1 a2) = prettyDocBinaryOp "Imod"  a1 a2
    prettyDoc (BinaryFPOp ModOp   (Float SinglePrec) a1 a2) = prettyDocBinaryOp "Smod"  a1 a2
    prettyDoc (BinaryFPOp ModOp   (Float DoublePrec) a1 a2) = prettyDocBinaryOp "Dmod"  a1 a2
    prettyDoc (BinaryFPOp ItModOp Int     a1 a2) = prettyDocBinaryOp "Itmod" a1 a2
    prettyDoc (BinaryFPOp PowOp   Int     a1 a2) = prettyDocBinaryOp "Ipow"  a1 a2
    prettyDoc (BinaryFPOp PowOp   _        a1 a2) = prettyDoc a1 <+> text "^" <+> lparen <> prettyDoc a2 <> rparen
    --
    prettyDoc (FFma   (Float SinglePrec) a1 a2 a3) = text "Sfma" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2 <> comma <+> prettyDoc a3)
    prettyDoc (FFma   (Float DoublePrec) a1 a2 a3) = text "Dfma" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2 <> comma <+> prettyDoc a3)
    --
    prettyDoc (FMin as) = text "min" <> parens (hsep $ punctuate comma $ map prettyDoc as)
    prettyDoc (FMax as) = text "min" <> parens (hsep $ punctuate comma $ map prettyDoc as)
    --
    prettyDoc (FLet letElems stm) = vcat (map prettyLetElem letElems)
                                       $$ parens (prettyDoc stm)
    prettyDoc (FIte be stmThen stmElse) =
      prettyDoc be
      <+> text "?"
      <+> parens (prettyDoc stmThen)
      <+> text ":"
      <+> parens (prettyDoc stmElse)
    prettyDoc (FListIte [] _) = error "prettyDoc RListIte: empty stmThen list"
    prettyDoc (FListIte ((beThen,stmThen):thenList) stmElse) =
      parens (prettyDoc beThen)
      $$ text "?"
      <+> prettyDoc stmThen <+> text ":"
      $$ vcat (map (\(be,stm) -> parens (prettyDoc be)
      $$ text "?" <+> prettyDoc stm <+> text ":") thenList)
      $$ prettyDoc stmElse
    prettyDoc ForLoop{} = text "TO DO For Loop"
    --
    prettyDoc ee = error $ "prettyDoc FAExpr for " ++ show ee ++ " not implemented yet."

instance PPExt AExpr where
  prettyDoc (IntCnst n) = integer n 
  prettyDoc (RatCnst r) = text $ showRational r
  prettyDoc (Var t x) = prVarName x t
  prettyDoc  Result = text "\\result"
  prettyDoc (ErrorCnst err) = prettyErrorHex err
  prettyDoc (RealMark x) = text x
  prettyDoc (FromFloat _ (Var _ x)) = text x
  prettyDoc (Value (Var _ x)) = text x <> text ".value"
  prettyDoc (Value Result)    = text "\\result.value"
  prettyDoc (EFun f _ args) = text f <> parens (docListComma $ map prettyDoc args)
  prettyDoc (ArrayElem v idx) = text v <> text "[" <> prettyDoc idx <> text "]"
  prettyDoc (BinaryOp AddOp   ae1 ae2) = parens $ prettyDoc ae1 <+> text "+" <+> prettyDoc ae2
  prettyDoc (BinaryOp SubOp   ae1 ae2) = parens $ prettyDoc ae1 <+> text "-" <+> prettyDoc ae2
  prettyDoc (BinaryOp MulOp   ae1 ae2) = parens $ prettyDoc ae1 <+> text "*" <+> prettyDoc ae2
  prettyDoc (BinaryOp DivOp   ae1 ae2) = parens $ prettyDoc ae1 <+> text "/" <+> prettyDoc ae2
  prettyDoc (BinaryOp ModOp   ae1 ae2) = parens $ prettyDoc ae1 <+> text "%" <+> prettyDoc ae2
  prettyDoc (BinaryOp PowOp   ae1 ae2) = text "\\pow" <+> parens (prettyDoc ae1 <+> comma <+> prettyDoc ae2)
  prettyDoc (UnaryOp  NegOp   ae) = text "-"       <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  FloorOp ae) = text "\\floor" <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  SqrtOp  ae) = text "\\sqrt"  <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  AbsOp   ae) = text "\\abs"   <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  SinOp   ae) = text "\\sin"   <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  CosOp   ae) = text "\\cos"   <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  TanOp   ae) = text "\\tan"   <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  AsinOp  ae) = text "\\asin"  <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  AcosOp  ae) = text "\\acos"  <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  AtanOp  ae) = text "\\atan"  <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  LnOp    ae) = text "\\log"   <> parens (prettyDoc ae)
  prettyDoc (UnaryOp  ExpoOp  ae) = text "\\exp"   <> parens (prettyDoc ae)
  prettyDoc (Min [ae]) = prettyDoc ae
  prettyDoc (Min [ae1,ae2]) = text "\\min" <> parens (prettyDoc ae1 <+> comma <+> prettyDoc ae2)
  prettyDoc (Min (ae1:aes)) = text "\\min" <> parens (prettyDoc ae1 <+> comma <+> prettyDoc (Min aes))
  prettyDoc (Max [ae]) = prettyDoc ae
  prettyDoc (Max [ae1,ae2]) = text "\\max" <> parens (prettyDoc ae1 <+> comma <+> prettyDoc ae2)
  prettyDoc (Max (ae1:aes)) = text "\\max" <> parens (prettyDoc ae1 <+> comma <+> prettyDoc (Max aes))
  prettyDoc (MaxErr[ae]) = prettyDoc ae
  prettyDoc (MaxErr [ae1,ae2]) = text "\\max" <> parens (prettyDoc ae1 <+> comma <+> prettyDoc ae2)
  prettyDoc (MaxErr (ae1:aes)) = text "\\max" <> parens (prettyDoc ae1 <+> comma <+> prettyDoc (MaxErr aes))
  --------------------------------------------------------------------------------------------------------------
  prettyDoc (Let letElems stm) = vcat (map prettyLetElem letElems)
                                       $$ parens (prettyDoc stm)

  prettyDoc (Ite be stmThen stmElse) =
        prettyDoc be
    <+> text "?"
    <+> parens (prettyDoc stmThen)
    <+> text ":"
    <+> parens (prettyDoc stmElse)

  prettyDoc (ListIte [] _) = error "prettyDoc RListIte: empty stmThen list"
  prettyDoc (ListIte ((beThen,stmThen):thenList) stmElse) =
      parens (prettyDoc beThen)
    $$ text "?"
    <+> prettyDoc stmThen <+> text ":"
    $$ vcat (map (\(be,stm) -> parens (prettyDoc be)
    $$ text "?" <+> prettyDoc stm <+> text ":") thenList)
    $$ prettyDoc stmElse
  -----------------------------------------------------------------------------------------------------------------------
  prettyDoc (HalfUlp SinglePrec ae) = text "ulp_sp" <> (text "(" <> prettyDoc ae <> text ")/2")
  prettyDoc (HalfUlp DoublePrec ae) = text "ulp_dp" <> (text "(" <> prettyDoc ae <> text ")/2")
  prettyDoc (ErrRat rat) = text $ show rat
  prettyDoc (ErrorMark x _) = text "E_" <> text x
  prettyDoc (ErrBinOp AddOp   (Float SinglePrec) ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errAdd_sp"   ae1 ee1 ae2 ee2
  prettyDoc (ErrBinOp AddOp   (Float DoublePrec) ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errAdd_dp"   ae1 ee1 ae2 ee2
  prettyDoc (ErrBinOp AddOp   Int                ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errAdd_i"   ae1 ee1 ae2 ee2
  prettyDoc (ErrBinOp SubOp   (Float SinglePrec) ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errSub_sp"   ae1 ee1 ae2 ee2
  prettyDoc (ErrBinOp SubOp   (Float DoublePrec) ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errSub_dp"   ae1 ee1 ae2 ee2
  prettyDoc (ErrBinOp SubOp   Int                ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errSub_i"   ae1 ee1 ae2 ee2
  prettyDoc (ErrBinOp MulOp   (Float SinglePrec) ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errMul_sp"   ae1 ee1 ae2 ee2
  prettyDoc (ErrBinOp MulOp   (Float DoublePrec) ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errMul_dp"   ae1 ee1 ae2 ee2
  prettyDoc (ErrBinOp MulOp   Int                ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errMul_i"   ae1 ee1 ae2 ee2
  prettyDoc (ErrBinOp DivOp   (Float SinglePrec) ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errDiv_sp"   ae1 ee1 ae2 ee2
  prettyDoc (ErrBinOp DivOp   (Float DoublePrec) ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errDiv_dp"   ae1 ee1 ae2 ee2
  prettyDoc (ErrBinOp DivOp   Int                ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errDiv_i"   ae1 ee1 ae2 ee2
  prettyDoc (ErrBinOp ModOp   (Float SinglePrec) ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errMod_sp"   ae1 ee1 ae2 ee2
  prettyDoc (ErrBinOp ModOp   (Float DoublePrec) ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errMod_dp"   ae1 ee1 ae2 ee2
  prettyDoc (ErrBinOp ModOp   Int                ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errMod_i"   ae1 ee1 ae2 ee2
  prettyDoc (ErrBinOp ItDivOp (Float SinglePrec) ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errItDiv_sp" ae1 ee1 ae2 ee2
  prettyDoc (ErrBinOp ItDivOp (Float DoublePrec) ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errItDiv_dp" ae1 ee1 ae2 ee2
  prettyDoc (ErrBinOp ItModOp (Float SinglePrec) ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errItMod_sp" ae1 ee1 ae2 ee2
  prettyDoc (ErrBinOp ItModOp (Float DoublePrec) ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errItMod_dp" ae1 ee1 ae2 ee2
  prettyDoc (ErrUnOp FloorOp False (Float SinglePrec) ae ee) = printUnaryOpErrorACSL "errFloor_sp"  ae ee
  prettyDoc (ErrUnOp FloorOp False (Float DoublePrec) ae ee) = printUnaryOpErrorACSL "errFloor_dp"  ae ee
  prettyDoc (ErrUnOp FloorOp True  (Float SinglePrec) ae ee) = printUnaryOpErrorACSL "errFloor0_sp" ae ee
  prettyDoc (ErrUnOp FloorOp True  (Float DoublePrec) ae ee) = printUnaryOpErrorACSL "errFloor0_dp" ae ee
  prettyDoc (ErrUnOp SqrtOp  False (Float SinglePrec) ae ee) = printUnaryOpErrorACSL "errSqrt_sp"   ae ee
  prettyDoc (ErrUnOp SqrtOp  False (Float DoublePrec) ae ee) = printUnaryOpErrorACSL "errSqrt_dp"   ae ee
  prettyDoc (ErrUnOp NegOp   False (Float SinglePrec) ae ee) = printUnaryOpErrorACSL "errNeg_sp"    ae ee
  prettyDoc (ErrUnOp NegOp   False (Float DoublePrec) ae ee) = printUnaryOpErrorACSL "errNeg_dp"    ae ee
  prettyDoc (ErrUnOp NegOp   False Int                ae ee) = printUnaryOpErrorACSL "errNeg_i"    ae ee
  prettyDoc (ErrUnOp AbsOp   False (Float SinglePrec) ae ee) = printUnaryOpErrorACSL "errAbs_sp"    ae ee
  prettyDoc (ErrUnOp AbsOp   False (Float DoublePrec) ae ee) = printUnaryOpErrorACSL "errAbs_dp"    ae ee
  prettyDoc (ErrUnOp SinOp   False (Float SinglePrec) ae ee) = printUnaryOpErrorACSL "errSin_sp"    ae ee
  prettyDoc (ErrUnOp SinOp   False (Float DoublePrec) ae ee) = printUnaryOpErrorACSL "errSin_dp"    ae ee
  prettyDoc (ErrUnOp CosOp   False (Float SinglePrec) ae ee) = printUnaryOpErrorACSL "errCos_sp"    ae ee
  prettyDoc (ErrUnOp CosOp   False (Float DoublePrec) ae ee) = printUnaryOpErrorACSL "errCos_dp"    ae ee
  prettyDoc (ErrUnOp TanOp   False (Float SinglePrec) ae ee) = printUnaryOpErrorACSL "errTan_sp"    ae ee
  prettyDoc (ErrUnOp TanOp   False (Float DoublePrec) ae ee) = printUnaryOpErrorACSL "errTan_dp"    ae ee
  prettyDoc (ErrUnOp AsinOp  False (Float SinglePrec) ae ee) = printUnaryOpErrorACSL "errAsin_sp"   ae ee
  prettyDoc (ErrUnOp AsinOp  False (Float DoublePrec) ae ee) = printUnaryOpErrorACSL "errAsin_dp"   ae ee
  prettyDoc (ErrUnOp AcosOp  False (Float SinglePrec) ae ee) = printUnaryOpErrorACSL "errAcos_sp"   ae ee
  prettyDoc (ErrUnOp AcosOp  False (Float DoublePrec) ae ee) = printUnaryOpErrorACSL "errAcos_dp"   ae ee
  prettyDoc (ErrUnOp AtanOp  False (Float SinglePrec) ae ee) = printUnaryOpErrorACSL "errAtan_sp"   ae ee
  prettyDoc (ErrUnOp AtanOp  False (Float DoublePrec) ae ee) = printUnaryOpErrorACSL "errAtan_dp"   ae ee
  prettyDoc (ErrUnOp AtanOp  True  (Float SinglePrec) ae ee) = printUnaryOpErrorACSL "errAtanT_sp"  ae ee
  prettyDoc (ErrUnOp AtanOp  True  (Float DoublePrec) ae ee) = printUnaryOpErrorACSL "errAtanT_dp"  ae ee
  prettyDoc (ErrUnOp LnOp    False (Float SinglePrec) ae ee) = printUnaryOpErrorACSL "errLn_sp"     ae ee
  prettyDoc (ErrUnOp LnOp    False (Float DoublePrec) ae ee) = printUnaryOpErrorACSL "errLn_sp"     ae ee
  prettyDoc (ErrUnOp ExpoOp  False (Float SinglePrec) ae ee) = printUnaryOpErrorACSL "errExpo_sp"   ae ee
  prettyDoc (ErrUnOp ExpoOp  False (Float DoublePrec) ae ee) = printUnaryOpErrorACSL "errExpo_dp"   ae ee
  prettyDoc (ErrCast (Float SinglePrec) (Float DoublePrec) ae ee) = printUnaryOpErrorACSL "errStoD" ae ee
  prettyDoc (ErrCast (Float DoublePrec) (Float SinglePrec) ae ee) = printUnaryOpErrorACSL "errDtoS" ae ee
  prettyDoc (ErrCast Int (Float SinglePrec) ae ee) = printUnaryOpErrorACSL "errItoS" ae ee
  prettyDoc (ErrCast Int (Float DoublePrec) ae ee) = printUnaryOpErrorACSL "errItoD" ae ee
  prettyDoc (ErrMulPow2R SinglePrec n ee) = text "ErrMulPow2R_sp" <> (text "(" <> integer n <> comma <+> prettyDoc ee <> text ")")
  prettyDoc (ErrMulPow2R DoublePrec n ee) = text "ErrMulPow2R_dp" <> (text "(" <> integer n <> comma <+> prettyDoc ee <> text ")")
  prettyDoc (ErrMulPow2L SinglePrec n ee) = text "ErrMulPow2L_sp" <> (text "(" <> integer n <> comma <+> prettyDoc ee <> text ")")
  prettyDoc (ErrMulPow2L DoublePrec n ee) = text "ErrMulPow2L_dp" <> (text "(" <> integer n <> comma <+> prettyDoc ee <> text ")")
  prettyDoc  ae = error $ "prettyACSLaexpr not defined for " ++ show ae

instance PPExt FBExprStm where
  prettyDoc (BLet letElems stm) = vcat (map prettyLetElem letElems)
                                       $$ parens (prettyDoc stm)
  prettyDoc (BIte be stmThen stmElse) = 
        prettyDoc be
    <+> text "?"
    <+> parens (prettyDoc stmThen)
    <+> text ":"
    <+> parens (prettyDoc stmElse)
  prettyDoc (BListIte [] _) = error "prettyDoc BListIte: empty stmThen list"
  prettyDoc (BListIte ((beThen,stmThen):thenList) stmElse) =
      parens (prettyDoc beThen)
    $$ text "?"
    <+> prettyDoc stmThen <+> text ":"
    $$ vcat (map (\(be,stm) -> parens (prettyDoc be)
    $$ text "?" <+> prettyDoc stm <+> text ":") thenList)
    $$ prettyDoc stmElse
  prettyDoc (BExpr be) = prettyDoc be

instance PPExt FBExpr where
    prettyDoc (FOr  be1 be2) = parens $ prettyDoc be1 <+> text "||" <+> prettyDoc be2
    prettyDoc (FAnd be1 be2) = parens $ prettyDoc be1 <+> text "&&" <+> prettyDoc be2
    prettyDoc (FNot     be) = text "!" <+> parens (prettyDoc be)
    prettyDoc (FRel Eq a1 a2) = parens $ prettyDoc a1 <+> text "=="  <+> prettyDoc a2
    prettyDoc (FRel Neq a1 a2) = parens $ prettyDoc a1 <+> text "!=" <+> prettyDoc a2
    prettyDoc (FRel Lt  a1 a2) = parens $ prettyDoc a1 <+> text "<"  <+> prettyDoc a2
    prettyDoc (FRel LtE a1 a2) = parens $ prettyDoc a1 <+> text "<=" <+> prettyDoc a2
    prettyDoc (FRel Gt  a1 a2) = parens $ prettyDoc a1 <+> text ">"  <+> prettyDoc a2
    prettyDoc (FRel GtE a1 a2) = parens $ prettyDoc a1 <+> text ">=" <+> prettyDoc a2
    prettyDoc (FBetween lb a ub) = parens $ prettyDoc lb <+> text "<=" <+> prettyDoc a <+> text "<=" <+> prettyDoc ub
    prettyDoc (FEPred Original p args) = text p <> parens (docListComma $ map prettyDoc args)
    prettyDoc (FEPred TauPlus  p args) = text p <> text "_tauplus"  <> parens (docListComma $ map prettyDoc args)
    prettyDoc (FEPred TauMinus p args) = text p <> text "_tauminus" <> parens (docListComma $ map prettyDoc args)
    prettyDoc       FBTrue = text "\\true"
    prettyDoc      FBFalse = text "\\false"

instance PPExt BExprStm where
  prettyDoc (RBLet letElems stm) = vcat (map prettyLetElem letElems)
                                       $$ parens (prettyDoc stm)
  prettyDoc (RBIte be stmThen stmElse) = 
        prettyDoc be
    <+> text "?"
    <+> parens (prettyDoc stmThen)
    <+> text ":"
    <+> parens (prettyDoc stmElse)
  prettyDoc (RBListIte [] _) = error "prettyDoc RBListIte: empty stmThen list"
  prettyDoc (RBListIte ((beThen,stmThen):thenList) stmElse) =
      parens (prettyDoc beThen)
    $$ text "?"
    <+> prettyDoc stmThen <+> text ":"
    $$ vcat (map (\(be,stm) -> parens (prettyDoc be)
    $$ text "?" <+> prettyDoc stm <+> text ":") thenList)
    $$ prettyDoc stmElse
  prettyDoc (RBExpr be) = prettyDoc be

instance PPExt BExpr where  
  prettyDoc  BTrue = text "\\true"
  prettyDoc  BFalse = text "\\false"
  prettyDoc (Not p) = text "!" <+> parens (prettyDoc p)
  prettyDoc (Or  p1 p2) = parens $ prettyDoc p1 <+> text "||"  <+> prettyDoc p2
  prettyDoc (And p1 p2) = parens $ prettyDoc p1 <+> text "&&"  <+> prettyDoc p2
  prettyDoc (Rel Eq  a1 a2) = parens $ prettyDoc a1 <+> text "=="  <+> prettyDoc a2
  prettyDoc (Rel Neq a1 a2) = parens $ prettyDoc a1 <+> text "!="  <+> prettyDoc a2
  prettyDoc (Rel Lt  a1 a2) = parens $ prettyDoc a1 <+> text "<"   <+> prettyDoc a2
  prettyDoc (Rel LtE a1 a2) = parens $ prettyDoc a1 <+> text "<="  <+> prettyDoc a2
  prettyDoc (Rel Gt  a1 a2) = parens $ prettyDoc a1 <+> text ">"   <+> prettyDoc a2
  prettyDoc (Rel GtE a1 a2) = parens $ prettyDoc a1 <+> text ">="  <+> prettyDoc a2
  prettyDoc (EPred p args) = text p <> parens (docListComma $ map prettyDoc args)
  prettyDoc (Between a lb ub) = parens $ prettyDoc lb <+> text "<=" <+> prettyDoc a <+> text "<=" <+> prettyDoc ub

instance PPExt Pred where
  prettyDoc (PredBExpr  be) = prettyDoc be
  prettyDoc (PredFBExpr be) = prettyDoc be
  prettyDoc (PredNot p) = text "!" <+> parens (prettyDoc p)
  prettyDoc (PredOr  p1 p2) = parens $ prettyDoc p1 <+> text "||"  <+> prettyDoc p2
  prettyDoc (PredAnd p1 p2) = parens $ prettyDoc p1 <+> text "&&"  <+> prettyDoc p2
  prettyDoc (Implies p1 p2) = parens $ prettyDoc p1 $$ text "==>" <+> parens (prettyDoc p2)
  prettyDoc (Iff p1 p2) = prettyDoc p1 $$ text "<==>" <+> parens (prettyDoc p2)
  prettyDoc (Forall varList p) = text "\\forall" <+> prettyDocVarList varList <> semi <+> parens (prettyDoc p)
  prettyDoc (Exists varList p) = text "\\exists" <+> prettyDocVarList varList <> semi <+> parens (prettyDoc p)
  prettyDoc (IsValid a) = prettyDoc a <> text ".isValid"
  prettyDoc (Assigns [])      = text "assigns \\nothing;"
  prettyDoc (Assigns varList) = text "assigns" <+> docListComma (map text varList) <+> semi
  prettyDoc (Pred p args) = text p <> parens (docListComma $ map prettyDoc args)
  prettyDoc (Ensures  p) = text "ensures"  <+> prettyDoc p <+> semi
  prettyDoc (Requires p) = text "requires" <+> prettyDoc p <+> semi
  prettyDoc (PredLet  x   expr p) = text "\\let" <+> text x <+> text "="
                                    <+> prettyDoc expr <> semi $$ parens (prettyDoc p)
  prettyDoc (FPredLet t x expr p) = text "\\let" <+> prVarName x t <+> text "="
                                    <+> prettyDoc expr <> semi $$ parens (prettyDoc p) 
  prettyDoc (ErrorDiseq fpexpr expr err) = text "\\abs" <> parens (prettyDoc fpexpr <+> text "-" <+> prettyDoc expr) <+> text "<=" <+> prettyDoc err
  prettyDoc (FErrorDiseq fpexpr expr err) = text "\\abs" <> parens (prettyDoc fpexpr <+> text "-" <+> prettyDoc expr) <+> text "<=" <+> prettyDoc err
  prettyDoc (LoopAssigns   vars) = text "loop assigns"   <+> docListComma (map text vars) <+> semi
  prettyDoc (LoopVariant   expr) = text "loop variant"   <+> prettyDoc expr <+> semi
  prettyDoc (LoopInvariant expr) = text "loop invariant" <+> prettyDoc expr<+> semi
  prettyDoc (AExprPred ae) = prettyDoc ae
  prettyDoc (FAExprPred ae) = prettyDoc ae
  prettyDoc (IsFiniteFP ae) = text "\\is_finite" <> parens (prettyDoc ae)
  prettyDoc be = error $ "prettyDoc Pred not defined for " ++ show be

prettyDocVarList :: [(String,Type)] -> Doc
prettyDocVarList = docListComma . map prettyDocVarWithType
  where
    prettyDocVarWithType (x,t) = prettyDoc t <+> text x 

instance PPExt Decl where
  prettyDoc (Decl isRec t f args body) =
       text "axiomatic real_function_" <> text f <+> text "{"
    $$ text "logic"
    <+> prettyDoc t
    <+> text f <> isRecDoc 
    <+> parens (docListComma $ map prettyDoc  args)
    <+> text "="
    $$ prettyDoc body <> text ";"
    $$ text "}"
    where
      isRecDoc = if isRec then text "{L}" else emptyDoc
  prettyDoc (RPred f args body) =
       text "axiomatic real_pred_" <> text f <+> text "{"
    $$ text "logic boolean"
    <+> text f 
    <+> parens (docListComma $ map prettyDoc  args)
    <+> text "="
    $$ prettyDoc body <> text ";"
    $$ text "}"

instance PPExt FPDecl where
  prettyDoc (FPDecl isRec t f args body) =
       text "axiomatic fp_function_" <> text (fromMaybe f (stripSuffix "_fp" f)) <+> text "{"
    $$ text "logic"
    <+> prettyDoc t
    <+> text f <> isRecDoc 
    <+> parens (docListComma $ map prettyDoc  args)
    <+> text "="
    $$ prettyDoc body <> text ";"
    $$ text "}"
    where
      isRecDoc = if isRec then text "{L}" else emptyDoc
  prettyDoc (FPPred f args body) =
       text "axiomatic fp_pred_" <> text (fromMaybe f (stripSuffix "_fp" f)) <+> text "{"
    $$ text "logic boolean"
    <+> text f 
    <+> parens (docListComma $ map prettyDoc  args)
    <+> text "="
    $$ prettyDoc body <> text ";"
    $$ text "}"

instance PPExt PredDecl where
  prettyDoc (PredDecl f args body) =
        text "predicate"
    <+> text f
    <+> parens (docListComma $ map prettyDoc args)
    <+> text "="
    $$ prettyDoc body <> text ";"

prettyACSL :: PPExt a => a -> Doc
prettyACSL a = text "/*@"
             $$ prettyDoc a 
             $$ text "*/"

annotateACSL :: Doc -> Doc
annotateACSL doc = text "/*@"
              $$ doc
              $$ text "*/"