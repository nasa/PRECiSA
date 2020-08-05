-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
 
-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
 
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.

module AbsRawRealPVSLang where

newtype Id = Id String
  deriving (Eq, Ord, Show, Read)

data ElsIf = ElsIf Expr Expr
  deriving (Eq, Ord, Show, Read)

data LetElem = LetElem Id Expr | LetElemType Id Type Expr
  deriving (Eq, Ord, Show, Read)

data Expr
    = Let [LetElem] Expr
    | Or Expr Expr
    | And Expr Expr
    | Not Expr
    | Eq Expr Expr
    | Neq Expr Expr
    | Lt Expr Expr
    | LtE Expr Expr
    | Gt Expr Expr
    | GtE Expr Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Neg Expr
    | If Expr Expr Expr
    | ListIf Expr Expr [ElsIf] Expr
    | For Type Expr Expr Expr Id Expr Expr Id Type Expr
    | Pow Expr Expr
    | Floor Expr
    | Sqrt Expr
    | Abs Expr
    | Sin Expr
    | Cos Expr
    | Tan Expr
    | ASin Expr
    | ACos Expr
    | ATan Expr
    | Ln Expr
    | Exp Expr
    | Mod1 Expr Expr
    | Mod2 Expr Expr
    | RtoS Expr
    | RtoD Expr
    | ItoS Expr
    | ItoD Expr
    | FCallN Id [Expr]
    | Pi1
    | Pi2
    | Int Integer
    | Rat Double
    | Var Id
    | UnstWarning
    | BTrue
    | BFalse
  deriving (Eq, Ord, Show, Read)

data Type
    = TypeReal
    | TypeInt
    | TypeInteger
    | TypePosNat
    | TypeBool
    | TypeBelow Integer
    | TypeArrayInteger Type
    | TypeArrayInt Type
    | TypeArrayBelow Expr Type
  deriving (Eq, Ord, Show, Read)

data Subrange = SubrageType Integer Integer
  deriving (Eq, Ord, Show, Read)

data Arg
    = FArg [Id] Type
    | FArgSubrange [Id] Subrange
    | FArgGuard [Id] Type Expr
  deriving (Eq, Ord, Show, Read)

data Args = FArgs [Arg] | FArgsNoType [Id]
  deriving (Eq, Ord, Show, Read)

data Decl
    = DeclN Id Args Type Expr
    | DeclRec Id Args Type Expr
    | Decl0 Id Type Expr
  deriving (Eq, Ord, Show, Read)

data Imp = LibImp [Id]
  deriving (Eq, Ord, Show, Read)

data VarDecl = VarDeclaration Id Type
  deriving (Eq, Ord, Show, Read)

data Program
    = Prog Id Imp [VarDecl] [Decl] Id | ProgImp Id [VarDecl] [Decl] Id
  deriving (Eq, Ord, Show, Read)

