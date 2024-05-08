-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module Translation.Float2Real
  ( fbe2be,
    fae2real,
    fae2real',
    fp2realProg,
  )
where

import AbsPVSLang
import PVSTypes
import Operators
import Data.Function (fix)
import Common.TypesUtils (VarName)

fp2realVarName :: VarName -> VarName
fp2realVarName x = "r_"++x

fp2realFunName :: FunName -> FunName
fp2realFunName f = f++"_real"

fp2realArg :: Arg -> Arg
fp2realArg (Arg x t)     = Arg (fp2realVarName x) (fp2realType t)

fp2realProg :: Program -> RProgram
fp2realProg = map fp2realDecl

fp2realDecl :: Decl -> RDecl
fp2realDecl (Decl _ fp f args stm)
  = RDecl retType (fp2realFunName f) (map fp2realArg args) (fae2real stm)
    where
        retType = fp2realType fp
fp2realDecl (Pred _ _ f args stm)
  = RPred (fp2realFunName f) (map fp2realArg args) (beStm2beStm stm)
fp2realDecl (RecordDecl _ t f args stm)
  = RRecordDecl (fp2realType t) (fp2realFunName f) (map fp2realArg args) (faeColl2real stm)
fp2realDecl (TupleDecl _ t f args stm)
  = RTupleDecl (fp2realType t) (fp2realFunName f) (map fp2realArg args) (faeColl2real stm)

faeColl2real :: CollFAExpr -> CollAExpr
faeColl2real (CLet letElems expr)
  = RCLet (map fp2letElem letElems) (faeColl2real expr)
faeColl2real (CIte be thenExpr elseExpr)
  = RCIte (fbe2be be) (faeColl2real thenExpr) (faeColl2real elseExpr)
faeColl2real (CListIte thenExprs elseExpr)
  = RCListIte (map (\(be,expr) -> (fbe2be be,faeColl2real expr)) thenExprs) (faeColl2real elseExpr)
faeColl2real (RecordExpr fieldExprs)
  = RRecordExpr (map (\(field,expr) -> (field,fae2real expr)) fieldExprs)
faeColl2real (TupleExpr exprs)
  = RTupleExpr (map fae2real exprs)
faeColl2real (CollFun _ f t args) = RCollFun f (fp2realType t) (map fae2real args)
faeColl2real (CollVar t x) = RCollVar (fp2realType t) x

beStm2beStm :: FBExprStm -> BExprStm
beStm2beStm (BLet letElems beStm) = RBLet (map fp2letElem letElems) (beStm2beStm beStm)
beStm2beStm (BIte be stmThen stmElse) = RBIte (fbe2be be) (beStm2beStm stmThen) (beStm2beStm stmElse)
beStm2beStm (BListIte listThen stmElse) = RBListIte (map fp2realItePair listThen) (beStm2beStm stmElse)
  where
    fp2realItePair (fbe, stm) = (fbe2be fbe, beStm2beStm stm)
beStm2beStm (BExpr be) = RBExpr (fbe2be be)
beStm2beStm BUnstWarning = error "beStm2beStm not defined for BUnstWarning."

fp2letElem :: FLetElem -> LetElem
fp2letElem (x,_,ae) = LetElem {letVar = x, letType = Real, letExpr = fae2real ae}

fbe2be :: FBExpr -> BExpr
fbe2be FBTrue       = BTrue
fbe2be FBFalse      = BFalse
fbe2be (FOr b1 b2)  = Or  (fbe2be b1) (fbe2be b2)
fbe2be (FAnd b1 b2) = And (fbe2be b1) (fbe2be b2)
fbe2be (FNot b)     = Not (fbe2be b)
fbe2be (FRel rel a1 a2) = Rel rel (fae2real a1) (fae2real a2)
fbe2be (FEPred _ _ f args)  = EPred f (map fae2real args)
fbe2be (IsValid _) = BTrue
fbe2be be = error $ "fbe2be not defined for " ++ show be ++ "."

fae2real :: FAExpr -> AExpr
fae2real = fix fae2real_rec

fae2real_rec :: (FAExpr -> AExpr) -> FAExpr -> AExpr
fae2real_rec _ (FVar _ x)         = Var Real x --RealMark x ResValue --
fae2real_rec _ (FInt n)           = Int n
fae2real_rec _ (FCnst fp n) = FromFloat fp (FCnst fp n)
fae2real_rec _ (FInterval _ lb ub) = Interval lb ub
fae2real_rec f (TypeCast _ _ ae)      = f ae
fae2real_rec _ (ToFloat  _     ae)      = ae
fae2real_rec _ (FMap  fp fun l) = RMap (fp2realType fp) fun l
fae2real_rec f (FFold fp fun l n ae) = RFold (fp2realType fp) fun l n (f ae)
fae2real_rec f (UnaryFPOp  op _ ae1)     = UnaryOp  op (f ae1)
fae2real_rec f (BinaryFPOp op _ ae1 ae2) = BinaryOp op (f ae1) (f ae2)
fae2real_rec f (FFma _ a1 a2 a3)  = BinaryOp AddOp (f a1) (BinaryOp MulOp (f a2) (f a3))
fae2real_rec f (FEFun _ f' field TInt args)  = EFun f' field TInt (map f args)
fae2real_rec f (FEFun _ f' field _ args)  = EFun f' field Real (map f args)
fae2real_rec f (FMin as)          = Min (map f as)
fae2real_rec f (FMax as)          = Max (map f as)
fae2real_rec f (FArrayElem fp size v idxs) = ArrayElem (fp2realType fp) size v (map f idxs)
fae2real_rec f (FTupleElem t x idx) = TupleElem (fp2realType t) x idx
fae2real_rec f (FRecordElem t x field) = RecordElem (fp2realType t) x field
fae2real_rec f (FListElem t x expr) = ListElem t x (f expr)
fae2real_rec _ UnstWarning        = RUnstWarning
fae2real_rec f (Let letElems stm) = RLet (map fp2realLetElem letElems) (f stm)
  where
    fp2realLetElem (x,t,fae) = LetElem {letVar = x, letType = fp2realType t, letExpr = f fae}
    -- fp2realLetElem (x,t,fae) = LetElem {letVar = fp2realVarName x, letType = fp2realType t, letExpr = f fae}
fae2real_rec f (Ite fbe stm1 stm2)        = RIte (fbe2be fbe) (f stm1) (f stm2)
fae2real_rec f (ListIte thenList stmElse) = RListIte (map fp2realItePair thenList) (f stmElse)
  where
    fp2realItePair (fbe, stm) = (fbe2be fbe, f stm)
fae2real_rec f (ForLoop retType startIdx endIdx initValueAcc idx acc forBody) =
  RForLoop newRetType (f startIdx) (f endIdx) (f initValueAcc) (fp2realVarName idx) (fp2realVarName acc) (f forBody)
  where
    newRetType = if retType == TInt
                   then TInt
                   else Real
fae2real_rec _ ae = error $ "fae2real_rec not defined for " ++ show ae

fae2real' :: FAExpr -> AExpr
fae2real' = fix fae2real_rec'

fae2real_rec' :: (FAExpr -> AExpr) -> FAExpr -> AExpr
fae2real_rec' _ (FVar TInt x)       = Var TInt x
fae2real_rec' _ (FVar _ x)          = Var Real x
fae2real_rec' _ (FInt n)            = Int n
fae2real_rec' f (TypeCast _ _ ae)      = f ae
fae2real_rec' _ (ToFloat  _     ae) = ae
fae2real_rec' _ (FCnst fp n) = FromFloat fp (FCnst fp n)
fae2real_rec' f (UnaryFPOp  op _ ae1)     = UnaryOp  op (f ae1)
fae2real_rec' f (BinaryFPOp op _ ae1 ae2) = BinaryOp op (f ae1) (f ae2)
fae2real_rec' f (FEFun _ f' field TInt args)  = EFun f' field TInt (map f args)
fae2real_rec' f (FEFun _ f' field _ args)  = EFun f' field Real (map f args)
fae2real_rec' f (FMin as)          = Min (map f as)
fae2real_rec' f (FMax as)          = Max (map f as)
fae2real_rec' f (FArrayElem TInt size v idxs) = ArrayElem TInt size v (map f idxs)
fae2real_rec' f (FArrayElem _    size v idxs) = ArrayElem Real size v (map f idxs)
-------------------------
fae2real_rec' _ UnstWarning                = RUnstWarning

fae2real_rec' f (Let letElems stm) = RLet (map fp2realLetElem letElems) (f stm)
  where
    fp2realLetElem (x,t,fae) = LetElem {letVar = fp2realVarName x, letType = fp2realType t, letExpr = f fae}
fae2real_rec' f (Ite fbe stm1 stm2)        = RIte (fbe2be' fbe) (f stm1) (f stm2)
fae2real_rec' f (ListIte thenList stmElse) = RListIte (map fp2realItePair thenList) (f stmElse)
    where
        fp2realItePair (fbe, stm) = (fbe2be' fbe, f stm)
fae2real_rec' f (ForLoop retType startIdx endIdx initValueAcc idx acc forBody) =
  RForLoop newRetType (f startIdx) (f endIdx) (f initValueAcc) idx acc (f forBody)
  where
    newRetType = if retType == TInt
                   then TInt
                   else Real
fae2real_rec' _ ae = error $ "fae2real_rec not defined for " ++ show ae

fbe2be' :: FBExpr -> BExpr
fbe2be' FBTrue       = BTrue
fbe2be' FBFalse      = BFalse
fbe2be' (FOr b1 b2)  = Or  (fbe2be' b1) (fbe2be' b2)
fbe2be' (FAnd b1 b2) = And (fbe2be' b1) (fbe2be' b2)
fbe2be' (FNot b)     = Not (fbe2be' b)
fbe2be' (FRel rel a1 a2) = Rel rel (fae2real' a1) (fae2real' a2)
fbe2be' (FEPred _ _ f args)  = EPred f (map fae2real' args)
fbe2be' (IsValid _) = BTrue
fbe2be' be = error $ "fbe2be' not defined for " ++ show be ++ "."