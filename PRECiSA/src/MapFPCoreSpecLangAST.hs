module MapFPCoreSpecLangAST
where

import Data.Ratio
import Parser.ParRawSpecLang
import Parser.LexRawSpecLang
import AbsFPCoreLang
import AbsPVSLang (ResultField(..))
import AbsSpecLang
import ErrM
import Data.Maybe
import PVSTypes


fpcore2Spec :: FPCore -> Spec
fpcore2Spec (FProgram (Symbol s) _ props _) = Spec [(SpecBind s (concat $ map fpcoreProp2VarBinds props))]
fpcore2Spec (FProgramSymbless _ props _) = Spec [(SpecBind "f" (concat $ map fpcoreProp2VarBinds props))]

fpcoreProp2VarBinds :: Property -> [VarBind]
fpcoreProp2VarBinds (Prop (Symbol "pre") (DExpr expr)) = fpcoreExpr2VarBinds expr
fpcoreProp2VarBinds _ = []

fpcoreExpr2VarBinds :: Expr -> [VarBind]
fpcoreExpr2VarBinds (ExOp AndOp e1 exprs) = concat $ map fpcoreExpr2VarBinds (e1:exprs)
fpcoreExpr2VarBinds (ExOp LTEOp (ExNum n1) [(ExSym (Symbol s)), (ExNum n2)]) = [(VarBind s ResValue FPDouble (LBDouble $ fpcoreNum2Rational n1) (UBDouble $ fpcoreNum2Rational n2))]

fpcoreNum2Rational :: AbsFPCoreLang.Number -> Data.Ratio.Rational
fpcoreNum2Rational (NRat (Rational s)) = (read (map parseRat s) :: Data.Ratio.Rational)
  where
    parseRat '/' = '%'
    parseRat c   = c
fpcoreNum2Rational (NDecNum (DecNum s)) = (toRational (read s :: Double))
fpcoreNum2Rational _ = error $ "Not yet implemented"
