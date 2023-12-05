-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module MapFPCoreLangAST
where

import AbsFPCoreLang
import AbsPVSLang
import Common.TypesUtils
import Data.Maybe(fromMaybe)
import Data.Ratio
import ErrM
import PVSTypes
import Numeric
import qualified Operators as Op
import Parser.ParFPCoreLang
import Parser.LexFPCoreLang
import Utils (fst3,snd3)

type VarTypeEnv = [(String, PVSTypes.PVSType)]
type FunTypeEnv = [(String, PVSTypes.PVSType)]

fpcore2Prog :: AbsFPCoreLang.FPCore -> AbsPVSLang.Program
fpcore2Prog (AbsFPCoreLang.FProgram  (Symbol name) args props expr) = [fpcore2Decl [(name, FPDouble)] name args expr]
fpcore2Prog (AbsFPCoreLang.FProgramSymbless args props expr) = [fpcore2Decl [("f", FPDouble)] "f" args expr]

-- Every FPCore program maps to one PVS declaration.
fpcore2Decl :: FunTypeEnv -> String -> [AbsFPCoreLang.Argument] -> AbsFPCoreLang.Expr -> AbsPVSLang.Decl
fpcore2Decl fenv name fpcArgs fpcExpr = Decl False FPDouble name args expr
  where
    args = fpcore2Args fpcArgs
    env = map mapArg2Pair args
    expr = fpcore2FAExpr env fenv fpcExpr

fpcore2Args :: [AbsFPCoreLang.Argument] -> [AbsPVSLang.Arg]
fpcore2Args fpcargs = concatMap fpcore2Arg fpcargs

-- Right now we ignore dimension and properties
fpcore2Arg :: AbsFPCoreLang.Argument -> [AbsPVSLang.Arg]
fpcore2Arg (ASym symbol)      = [fpcoreSym2Arg symbol]
fpcore2Arg (ASymDim symbol _) = [fpcoreSym2Arg symbol]
fpcore2Arg (AProp _ symbol _) = [fpcoreSym2Arg symbol]

fpcoreSym2Arg :: AbsFPCoreLang.Symbol -> AbsPVSLang.Arg
fpcoreSym2Arg (Symbol s) = AbsPVSLang.Arg s FPDouble 

-- While, For, Tensor, not fully supported yet.
fpcore2FAExpr :: VarTypeEnv -> FunTypeEnv -> AbsFPCoreLang.Expr -> AbsPVSLang.FAExpr
fpcore2FAExpr env fenv (ExNum n) = fpcoreNum2Expr n
fpcore2FAExpr env fenv (ExConst c) = fpcoreConst2Expr c
fpcore2FAExpr env fenv (ExSym (Symbol s)) = FVar FPDouble s
fpcore2FAExpr env fenv (ExOp op ex1 exl) = fpcoreOpCall2Expr env fenv op (ex1 : exl)
fpcore2FAExpr env fenv (ExIf ex1 ex2 ex3) = Ite (fpcore2FBExpr env fenv ex1) (fpcore2FAExpr env fenv ex2) (fpcore2FAExpr env fenv ex3)
fpcore2FAExpr env fenv (ExLet letElems stm) -- Should bind simultaneously but not sure if it does
  = AbsPVSLang.Let letList (fpcore2FAExpr newenv fenv stm)
  where
    (newenv,letList) = foldr aux_fold (env,[]) letElems
    aux_fold letElem (accEnv,elems) =  (env',elems ++ [newLetElem])
      where
        newLetElem = fpcore2LetElem accEnv fenv letElem
        env' = (fst3 newLetElem, snd3 newLetElem):accEnv

fpcore2FAExpr env fenv (ExLetStar letElems stm) -- TODO: Currently has same behavior as regular let
  = process env fenv letElems stm
  where
    process env fenv [(SymExPair (Symbol name) e)] stm = Let [(name, FPDouble, fpcore2FAExpr env fenv e)] (fpcore2FAExpr env fenv stm)
    process env fenv ((SymExPair (Symbol name) e):lelems) stm = Let [(name, FPDouble, fpcore2FAExpr env fenv e)] (process env fenv lelems stm)
-- = fpcore2LetStar2FAExpr env fenv letElems stm

fpcore2FAExpr _ _ _ = error $ "unsupported expression"

fpcore2LetElem :: VarTypeEnv -> FunTypeEnv -> AbsFPCoreLang.SymEx -> AbsPVSLang.FLetElem
fpcore2LetElem env fenv (AbsFPCoreLang.SymExPair (Symbol sym) ex) = (sym, FPDouble, fpcore2FAExpr env fenv ex)

-- TODO: Implementation
fpcoreOpCall2Expr :: VarTypeEnv -> FunTypeEnv -> AbsFPCoreLang.Operation -> [AbsFPCoreLang.Expr] -> AbsPVSLang.FAExpr
fpcoreOpCall2Expr env fenv PlusOp [ex1, ex2] = 
  AbsPVSLang.BinaryFPOp Op.AddOp FPDouble (fpcore2FAExpr env fenv ex1) (fpcore2FAExpr env fenv ex2)
fpcoreOpCall2Expr env fenv MinusOp [ex1, ex2] = 
  AbsPVSLang.BinaryFPOp Op.SubOp FPDouble (fpcore2FAExpr env fenv ex1) (fpcore2FAExpr env fenv ex2)
fpcoreOpCall2Expr env fenv MinusOp [fae] = 
  AbsPVSLang.UnaryFPOp Op.NegOp  FPDouble (fpcore2FAExpr env fenv fae)
fpcoreOpCall2Expr env fenv MulOp [ex1, ex2] = 
  AbsPVSLang.BinaryFPOp Op.MulOp FPDouble (fpcore2FAExpr env fenv ex1) (fpcore2FAExpr env fenv ex2)
fpcoreOpCall2Expr env fenv DivOp [ex1, ex2] = 
  AbsPVSLang.BinaryFPOp Op.DivOp FPDouble (fpcore2FAExpr env fenv ex1) (fpcore2FAExpr env fenv ex2)
fpcoreOpCall2Expr env fenv FabsOp [fae] =
  AbsPVSLang.UnaryFPOp Op.AbsOp   FPDouble (fpcore2FAExpr env fenv fae)
fpcoreOpCall2Expr env fenv FmaOp [ex1, ex2, ex3] = 
  FFma FPDouble (fpcore2FAExpr env fenv ex1) (fpcore2FAExpr env fenv ex2) (fpcore2FAExpr env fenv ex3)
fpcoreOpCall2Expr env fenv ExpOp [fae] =
  AbsPVSLang.UnaryFPOp Op.ExpoOp  FPDouble (fpcore2FAExpr env fenv fae)
fpcoreOpCall2Expr env fenv Exp2Op [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv Expm1Op [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv LogOp [fae] =
  AbsPVSLang.UnaryFPOp Op.LnOp   FPDouble (fpcore2FAExpr env fenv fae)
fpcoreOpCall2Expr env fenv Log10Op [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv Log2Op [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv Log1pOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv PowOp [ex1, ex2] = 
  AbsPVSLang.BinaryFPOp Op.PowOp FPDouble (fpcore2FAExpr env fenv ex1) (fpcore2FAExpr env fenv ex2)
fpcoreOpCall2Expr env fenv SqrtOp [fae] =
  AbsPVSLang.UnaryFPOp Op.SqrtOp   FPDouble (fpcore2FAExpr env fenv fae)
fpcoreOpCall2Expr env fenv CbrtOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv HypotOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv SinOp [fae] =
  AbsPVSLang.UnaryFPOp Op.SinOp   FPDouble (fpcore2FAExpr env fenv fae)
fpcoreOpCall2Expr env fenv CosOp [fae] =
  AbsPVSLang.UnaryFPOp Op.CosOp   FPDouble (fpcore2FAExpr env fenv fae)
fpcoreOpCall2Expr env fenv TanOp [fae] =
  AbsPVSLang.UnaryFPOp Op.TanOp   FPDouble (fpcore2FAExpr env fenv fae)
fpcoreOpCall2Expr env fenv AsinOp [fae] = 
  AbsPVSLang.UnaryFPOp Op.AsinOp   FPDouble (fpcore2FAExpr env fenv fae)
fpcoreOpCall2Expr env fenv AcosOp [fae] = 
  AbsPVSLang.UnaryFPOp Op.AcosOp   FPDouble (fpcore2FAExpr env fenv fae)
fpcoreOpCall2Expr env fenv AtanOp [fae] = 
  AbsPVSLang.UnaryFPOp Op.AtanOp   FPDouble (fpcore2FAExpr env fenv fae)
fpcoreOpCall2Expr env fenv Atan2Op [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv SinhOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv CoshOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv TanhOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv AsinhOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv AcoshOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv AtanhOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv ErfOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv ErfcOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv TgammaOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv LgammaOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv CeilOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv FloorOp [fae] =
  AbsPVSLang.UnaryFPOp Op.FloorOp   FPDouble (fpcore2FAExpr env fenv fae)
fpcoreOpCall2Expr env fenv FmodOp [ex1, ex2] = 
  AbsPVSLang.BinaryFPOp Op.ModOp FPDouble (fpcore2FAExpr env fenv ex1) (fpcore2FAExpr env fenv ex2)
fpcoreOpCall2Expr env fenv RemainderOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv FmaxOp faes =
  FMax $ map (fpcore2FAExpr env fenv) faes
fpcoreOpCall2Expr env fenv FminOp faes = 
  FMin $ map (fpcore2FAExpr env fenv) faes
fpcoreOpCall2Expr env fenv FdimOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv CopysignOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv TruncOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv RoundOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr env fenv NearbyintOp [ex1, ex2] = error $ "Not yet implemented"
fpcoreOpCall2Expr _ _ _ _ = error $ "Illegal cast from boolean operation to arithmetic"

-- TODO: Implementation
fpcoreOpCall2BExpr :: VarTypeEnv -> FunTypeEnv -> AbsFPCoreLang.Operation -> [AbsFPCoreLang.Expr] -> AbsPVSLang.FBExpr
fpcoreOpCall2BExpr env fenv LTOp faes = process env fenv faes
  where
    process env fenv [fae1,fae2]      = AbsPVSLang.FRel Op.Lt  (fpcore2FAExpr env fenv fae1) (fpcore2FAExpr env fenv fae2)
    process env fenv (fae1:fae2:faes) = AbsPVSLang.FAnd (AbsPVSLang.FRel Op.Lt  (fpcore2FAExpr env fenv fae1) (fpcore2FAExpr env fenv fae2)) (process env fenv (fae2:faes))
fpcoreOpCall2BExpr env fenv GTOp faes = process env fenv faes
  where
    process env fenv [fae1,fae2]      = AbsPVSLang.FRel Op.Gt  (fpcore2FAExpr env fenv fae1) (fpcore2FAExpr env fenv fae2)
    process env fenv (fae1:fae2:faes) = AbsPVSLang.FAnd (AbsPVSLang.FRel Op.Gt  (fpcore2FAExpr env fenv fae1) (fpcore2FAExpr env fenv fae2)) (process env fenv (fae2:faes))
fpcoreOpCall2BExpr env fenv LTEOp faes = process env fenv faes
  where
    process env fenv [fae1,fae2]      = AbsPVSLang.FRel Op.LtE  (fpcore2FAExpr env fenv fae1) (fpcore2FAExpr env fenv fae2)
    process env fenv (fae1:fae2:faes) = AbsPVSLang.FAnd (AbsPVSLang.FRel Op.LtE  (fpcore2FAExpr env fenv fae1) (fpcore2FAExpr env fenv fae2)) (process env fenv (fae2:faes))
fpcoreOpCall2BExpr env fenv GTEOp faes = process env fenv faes
  where
    process env fenv [fae1,fae2]      = AbsPVSLang.FRel Op.GtE  (fpcore2FAExpr env fenv fae1) (fpcore2FAExpr env fenv fae2)
    process env fenv (fae1:fae2:faes) = AbsPVSLang.FAnd (AbsPVSLang.FRel Op.GtE  (fpcore2FAExpr env fenv fae1) (fpcore2FAExpr env fenv fae2)) (process env fenv (fae2:faes))
fpcoreOpCall2BExpr env fenv EqualOp faes = process env fenv faes
  where
    process env fenv [fae1,fae2]      = AbsPVSLang.FRel Op.Eq (fpcore2FAExpr env fenv fae1) (fpcore2FAExpr env fenv fae2)
    process env fenv (fae1:fae2:faes) = AbsPVSLang.FAnd (AbsPVSLang.FRel Op.Eq (fpcore2FAExpr env fenv fae1) (fpcore2FAExpr env fenv fae2)) (process env fenv (fae2:faes))
fpcoreOpCall2BExpr env fenv NEqualOp [fae1, fae2] = AbsPVSLang.FRel Op.Neq  (fpcore2FAExpr env fenv fae1) (fpcore2FAExpr env fenv fae2)
fpcoreOpCall2BExpr env fenv AndOp fbes = process env fenv fbes
  where
    process env fenv [fbe1,fbe2]      = AbsPVSLang.FAnd (fpcore2FBExpr env fenv fbe1) (fpcore2FBExpr env fenv fbe2)
    process env fenv (fbe:fbes) = AbsPVSLang.FAnd (fpcore2FBExpr env fenv fbe) (process env fenv fbes)
fpcoreOpCall2BExpr env fenv OrOp fbes = process env fenv fbes
  where
    process env fenv [fbe1,fbe2]      = AbsPVSLang.FOr (fpcore2FBExpr env fenv fbe1) (fpcore2FBExpr env fenv fbe2)
    process env fenv (fbe:fbes) = AbsPVSLang.FOr (fpcore2FBExpr env fenv fbe) (process env fenv fbes)
fpcoreOpCall2BExpr env fenv NotOp [fbe] = AbsPVSLang.FNot  (fpcore2FBExpr env fenv fbe)
fpcoreOpCall2BExpr env fenv IsfiniteOp [fae1, fae2] = error $ "Not yet implemented"
fpcoreOpCall2BExpr env fenv IsinfOp [fae1, fae2] = error $ "Not yet implemented"
fpcoreOpCall2BExpr env fenv IsnanOp [fae1, fae2] = error $ "Not yet implemented"
fpcoreOpCall2BExpr env fenv IsnormalOp [fae1, fae2] = error $ "Not yet implemented"
fpcoreOpCall2BExpr env fenv SignbitOp [fae1, fae2] = error $ "Not yet implemented"
fpcoreOpCall2BExpr _ _ _ _ = error $ "Illegal cast from arithmetic operation to boolean"

-- TODO: Implementation
fpcoreConst2Expr :: AbsFPCoreLang.Constant -> AbsPVSLang.FAExpr
fpcoreConst2Expr EConst = error $ "EConst Not yet implemented"
fpcoreConst2Expr LOG2EConst = error $ "LOG2EConst Not yet implemented"
fpcoreConst2Expr LOG10EConst = error $ "LOG10EConst Not yet implemented"
fpcoreConst2Expr LN2Const = error $ "LN2Const Not yet implemented"
fpcoreConst2Expr LN10Const = error $ "LN10Const Not yet implemented"
fpcoreConst2Expr PIConst = error $ "PIConst Not yet implemented"
fpcoreConst2Expr PI_2Const = error $ "PI_2Const Not yet implemented"
fpcoreConst2Expr PI_4Const = error $ "PI_4Const Not yet implemented"
fpcoreConst2Expr M_1_PIConst = error $ "M_1_PIConst Not yet implemented"
fpcoreConst2Expr M_2_PIConst = error $ "M_2_PIConst Not yet implemented"
fpcoreConst2Expr M_2_SQRTPIConst = error $ "M_2_SQRTPIConst Not yet implemented"
fpcoreConst2Expr SQRT2Const = error $ "SQRT2Const Not yet implemented"
fpcoreConst2Expr SQRT1_2Const = error $ "SQRT1_2Const Not yet implemented"
fpcoreConst2Expr INFINITYConst = error $ "INFINITYConst Not yet implemented"
fpcoreConst2Expr NANConst = error $ "NANConst Not yet implemented"
fpcoreConst2Expr _ = error $ "Illegal cast from boolean constant to arithmetic"

fpcoreConst2BExpr :: AbsFPCoreLang.Constant -> AbsPVSLang.FBExpr
fpcoreConst2BExpr TRUEConst = AbsPVSLang.FBTrue
fpcoreConst2BExpr FALSEConst = AbsPVSLang.FBFalse
fpcoreConst2BExpr _ = error $ "Illegal cast from arithmetic constant to boolean"

-- TODO: Implementation
fpcoreNum2Expr :: AbsFPCoreLang.Number -> AbsPVSLang.FAExpr
fpcoreNum2Expr (NRat (Rational s)) = FCnst FPDouble (read (map parseRat s) :: Data.Ratio.Rational)
  where
    parseRat '/' = '%'
    parseRat c   = c
fpcoreNum2Expr (NDecNum (DecNum s)) = FCnst FPDouble (toRational (read s :: Double))
fpcoreNum2Expr _ = error $ "Not yet implemented"

-- TODO: Implementation
fpcoreNum2Double :: AbsFPCoreLang.Number -> Double
fpcoreNum2Double _ = error $ "Not yet implemented"

-- TODO: Implementation
fpcore2FBExpr :: VarTypeEnv -> FunTypeEnv -> AbsFPCoreLang.Expr -> AbsPVSLang.FBExpr
fpcore2FBExpr env fenv (ExOp op ex1 exl) = fpcoreOpCall2BExpr env fenv op (ex1 : exl)
-- fpcore2FBExpr env fenv (ExIf ex1 ex2 ex3) = BIte (fpcore2FBExpr env fenv ex1) (fpcore2FBExpr env fenv ex2) (fpcore2FBExpr env fenv ex3)
fpcore2FBExpr _ _ _ = error $ "unsupported expression"

either2Err :: Either String a -> Err a
either2Err (Left s) = Bad s
either2Err (Right a) = Ok a

fpcoreparserPVS :: String -> Err AbsFPCoreLang.FPCore
fpcoreparserPVS = either2Err . pFPCore . tokens
