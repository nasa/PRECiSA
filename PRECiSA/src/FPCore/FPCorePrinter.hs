module FPCore.FPCorePrinter where

import AbstractDomain
import AbsPVSLang
import AbsSpecLang
import Data.Ratio
import PPExt
import PVSTypes
import Prelude hiding ((<>))
import qualified Common.ShowRational as Rat
import Operators
import Common.TypesUtils

fpcprintProgram :: Program -> Spec -> Doc
fpcprintProgram [decl] spec = fpcprintDecl decl spec
fpcprintProgram p _ = error $ "fpcprintProgram: only programs with one function declaration can be converted to FPCore"

fpcprintDecl :: Decl -> Spec -> Doc
fpcprintDecl (Decl _ _ sym args fae) spec = hcat [text $ "(FPCore " ++ sym ++ " (", hsep $ map fpcprintArg args, text ") ", fpcprintSpec spec, text " ", fpcprintFAExpr fae, text ")"]
fpcprintDecl d _ = error $ "fpcprintDecl: " ++ show d ++ " not supported yet."

fpcprintSpec :: Spec -> Doc
fpcprintSpec (Spec []) = text ""
fpcprintSpec (Spec [specbind]) = fpcprintSpecBind specbind
fpcprintSpec _ = error $ "fpcprintSpec: cannot have more than one specbind"

fpcprintSpecBind :: SpecBind -> Doc
fpcprintSpecBind (SpecBind _ [var]) =
  hcat [text ":pre ", fpcprintVarBind var]
fpcprintSpecBind (SpecBind _ vars) =
  hcat [text ":pre (and ", hsep $ map fpcprintVarBind vars,    text ")"]

fpcprintVarBind :: VarBind -> Doc
fpcprintVarBind (VarBind name _ lb ub) =
  hcat $ [text "(<= ", fpcprintLBound lb,
    text $ " " ++ name ++ " ", fpcprintUBound ub, text ")"]

fpcprintLBound :: LBound -> Doc
fpcprintLBound (LBInt i) = text $ show i
fpcprintLBound (LBDouble r) = text $ (show $ numerator r) ++ "/" ++ (show $ denominator r)
fpcprintLBound LInf = text ""

fpcprintUBound :: UBound -> Doc
fpcprintUBound (UBInt i) = text $ show i
fpcprintUBound (UBDouble r) = text $ (show $ numerator r) ++ "/" ++ (show $ denominator r)
fpcprintUBound UInf = text ""

fpcprintArg :: Arg -> Doc
fpcprintArg (Arg name _) = text name

fpcprintFAExpr :: FAExpr -> Doc
fpcprintFAExpr (FInt i) = text $ show i
fpcprintFAExpr (FCnst _ r) = text $ (show $ numerator r) ++ "/" ++ (show $ denominator r)
fpcprintFAExpr (FVar _ name) = text name
fpcprintFAExpr (TypeCast _ _ fae) = fpcprintFAExpr fae
fpcprintFAExpr (ToFloat _ ae) = fpcprintAExpr ae
fpcprintFAExpr (Value fae) =  fpcprintFAExpr fae
fpcprintFAExpr (BinaryFPOp op _ fae1 fae2) =
  hcat [text "(", fpcprintBinOp op,
    text " ", fpcprintFAExpr fae1, text " ",
    fpcprintFAExpr fae2, text ")"]
fpcprintFAExpr (UnaryFPOp op _ fae) =
  hcat [text "(", fpcprintUnOp op, text " ", fpcprintFAExpr fae, text ")"]
fpcprintFAExpr (FFma _ fae1 fae2 fae3) = hcat [text "(fma ", fpcprintFAExpr fae1, text " ", fpcprintFAExpr fae2, text " ", fpcprintFAExpr fae3, text " )"]
fpcprintFAExpr (FMin faes) = hcat $ [text "(fmin ", hsep (map fpcprintFAExpr faes), text ")"]
fpcprintFAExpr (FMax faes) = hcat $ [text "(fmax ", hsep (map fpcprintFAExpr faes), text ")"]
fpcprintFAExpr (Let lelems fae) = hcat $ [text "(let (", hsep (map fpcprintFLetElem lelems), text ")", fpcprintFAExpr fae, text ")"]
fpcprintFAExpr (Ite fbe fae1 fae2) = hcat $ [text "(if ", fpcprintFBExpr fbe, text " ", fpcprintFAExpr fae1, text " ",  fpcprintFAExpr fae2, text ")"]
fpcprintFAExpr fae = error $ "fpcprintFAExpr: " ++ show fae ++ " not supported yet."

fpcprintAExpr :: AExpr -> Doc
fpcprintAExpr (BinaryOp op ae1 ae2) =
  hcat [text "(", fpcprintBinOp op,
    text " ", fpcprintAExpr ae1, text " ",
    fpcprintAExpr ae2, text ")"]
fpcprintAExpr (UnaryOp op ae) =
  hcat [text "(", fpcprintUnOp op, text " ", fpcprintAExpr ae, text ")"]
fpcprintAExpr (FromFloat _ fae) = fpcprintFAExpr fae
fpcprintAExpr (Int i) = text $ show i
fpcprintAExpr (Rat r) = text $ (show $ numerator r) ++ "/" ++ (show $ denominator r)
fpcprintAExpr (Var _ name) = text name
fpcprintAExpr (FExp fae) = fpcprintFAExpr fae
fpcprintAExpr ae = error $ "fpcprintAExpr: " ++ show ae ++ " not supported yet."

fpcprintFBExpr :: FBExpr -> Doc
fpcprintFBExpr FBTrue = text "TRUE"
fpcprintFBExpr FBFalse = text "FALSE"
fpcprintFBExpr (FOr fbe1 fbe2) = hcat $ [text "(or ", fpcprintFBExpr fbe1, text " ", fpcprintFBExpr fbe2, text ")"]
fpcprintFBExpr (FAnd fbe1 fbe2) = hcat $ [text "(and ", fpcprintFBExpr fbe1, text " ", fpcprintFBExpr fbe2, text ")"]
fpcprintFBExpr (FNot fbe) = hcat $ [text "(not ", fpcprintFBExpr fbe, text ")"]
fpcprintFBExpr (FRel op fae1 fae2) = hcat $ [text "(", fpcprintFRel op, text " ", fpcprintFAExpr fae1, text " ", fpcprintFAExpr fae2, text ")"]
fpcprintFBExpr fbe = error $ "fpcprintFBExpr: " ++ show fbe ++ " not supported yet."

fpcprintFRel :: RelOp -> Doc
fpcprintFRel Eq  = text "=="
fpcprintFRel Neq = text "!="
fpcprintFRel Lt  = text "<"
fpcprintFRel LtE = text "<="
fpcprintFRel Gt  = text ">"
fpcprintFRel GtE = text ">="

fpcprintFLetElem :: FLetElem -> Doc
fpcprintFLetElem (name, _, fae) = hcat $ [text "[", text name, text " ", fpcprintFAExpr fae, text "]"]

fpcprintUnOp :: UnOp -> Doc
fpcprintUnOp NegOp    = text "-"
fpcprintUnOp FloorOp  = text "floor"
fpcprintUnOp SqrtOp   = text "sqrt"
fpcprintUnOp AbsOp    = text "abs"
fpcprintUnOp SinOp    = text "sin"
fpcprintUnOp CosOp    = text "cos"
fpcprintUnOp TanOp    = text "tan"
fpcprintUnOp AcosOp   = text "acos"
fpcprintUnOp AsinOp   = text "asin"
fpcprintUnOp AtanOp   = text "atan"
fpcprintUnOp LnOp     = text "log"
fpcprintUnOp ExpoOp   = text "exp"

fpcprintBinOp :: BinOp -> Doc
fpcprintBinOp AddOp   = text "+"
fpcprintBinOp SubOp   = text "-"
fpcprintBinOp MulOp   = text "*"
fpcprintBinOp DivOp   = text "/"
fpcprintBinOp PowOp   = text "pow"
fpcprintBinOp ModOp   = text "fmod"
fpcprintBinOp op
  = error $ "fpcprintBinOp: " ++ show op ++ " not supported yet"

fpcprintLetElem :: LetElem -> Doc
fpcprintLetElem le = error $ "fpcprintLetElem: " ++ show le ++ " not supported yet."
