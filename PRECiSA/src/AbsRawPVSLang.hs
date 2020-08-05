-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
 
-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
 
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.

module AbsRawPVSLang where

newtype Id = Id String
  deriving (Eq, Ord, Show, Read)

data ElsIf = ElsIf Expr Expr
  deriving (Eq, Ord, Show, Read)

data LetElem = LetElem Id Expr | LetElemType Id FPtype Expr
  deriving (Eq, Ord, Show, Read)

data Expr
    = Let [LetElem] Expr
    | FOr Expr Expr
    | FAnd Expr Expr
    | FNot Expr
    | FEq Expr Expr
    | FNeq Expr Expr
    | FLt Expr Expr
    | FLtE Expr Expr
    | FGt Expr Expr
    | FGtE Expr Expr
    | ExprAdd Expr Expr
    | ExprSub Expr Expr
    | ExprMul Expr Expr
    | ExprDiv Expr Expr
    | ExprNeg Expr
    | If Expr Expr Expr
    | ListIf Expr Expr [ElsIf] Expr
    | For FPtype Expr Expr Expr Id Expr Expr Id FPtype Expr
    | ExprPow Expr Expr
    | Floor Expr
    | Sqrt Expr
    | Abs Expr
    | Sin Expr
    | Cos Expr
    | Tan Expr
    | ASin Expr
    | ACos Expr
    | ATan Expr
    | Mod Expr Expr
    | Ln Expr
    | Exp Expr
    | SAdd Expr Expr
    | DAdd Expr Expr
    | IAdd Expr Expr
    | SSub Expr Expr
    | DSub Expr Expr
    | ISub Expr Expr
    | SMul Expr Expr
    | DMul Expr Expr
    | IMul Expr Expr
    | SDiv Expr Expr
    | DDiv Expr Expr
    | IDiv Expr Expr
    | SNeg Expr
    | DNeg Expr
    | INeg Expr
    | SFloor Expr
    | DFloor Expr
    | SSqrt Expr
    | DSqrt Expr
    | SAbs Expr
    | DAbs Expr
    | IAbs Expr
    | SSin Expr
    | DSin Expr
    | SCos Expr
    | DCos Expr
    | STan Expr
    | DTan Expr
    | SAcos Expr
    | DAcos Expr
    | SAsin Expr
    | DAsin Expr
    | SAtan Expr
    | DAtan Expr
    | SMod Expr Expr
    | DMod Expr Expr
    | IMod Expr Expr
    | SLn Expr
    | DLn Expr
    | SExp Expr
    | DExp Expr
    | StoR Expr
    | DtoR Expr
    | RtoS Double
    | RtoD Double
    | NegRtoS Double
    | NegRtoD Double
    | IntRtoS Integer
    | IntRtoD Integer
    | IntNegRtoS Integer
    | IntNegRtoD Integer
    | ItoS Integer
    | ItoD Integer
    | FCallN Id [Expr]
    | ExprId Id
    | FInt Integer
    | Rat Double
    | UnstWarning
    | FBTrue
    | FBFalse
  deriving (Eq, Ord, Show, Read)

data FPtype
    = FPtype_int
    | FPtype_integer
    | FPtype_unb_single
    | FPtype_unb_double
    | FPtype_unb_pos_single
    | FPtype_unb_pos_double
    | FPtype_unb_nz_single
    | FPtype_unb_nz_double
    | FPtype_bool
  deriving (Eq, Ord, Show, Read)

data Subrange = SubrageType Integer Integer
  deriving (Eq, Ord, Show, Read)

data Arg
    = FArg [Id] FPtype
    | FArgSubrange [Id] Subrange
    | FArgGuard [Id] FPtype Expr
  deriving (Eq, Ord, Show, Read)

data Args = FArgs [Arg] | FArgsNoType [Id]
  deriving (Eq, Ord, Show, Read)

data Decl = DeclN Id Args FPtype Expr | Decl0 Id FPtype Expr
  deriving (Eq, Ord, Show, Read)

data Imp = LibImp [Id]
  deriving (Eq, Ord, Show, Read)

data VarDecl = VarDeclaration Id FPtype
  deriving (Eq, Ord, Show, Read)

data Program
    = Prog Id Imp [VarDecl] [Decl] Id | ProgImp Id [VarDecl] [Decl] Id
  deriving (Eq, Ord, Show, Read)

