-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module Translation.Real2Float where

import AbsPVSLang
import PVSTypes
import Operators
import Utils(isInt)
import Common.TypesUtils
import Data.Maybe (fromMaybe)

type NormBoolExpr = Bool

real2fpArg :: PVSType -> Arg -> Arg
real2fpArg  _ (Arg x TInt) = Arg x TInt
real2fpArg fp (Arg x Real) = Arg x fp
real2fpArg _  (Arg x (Array TInt size)) = Arg x (Array TInt size)
real2fpArg fp (Arg x (Array Real size)) = Arg x (Array fp size)
real2fpArg _  (Arg _ t) = error $ "real2fpArg not defined for " ++ show t

real2fpProg :: NormBoolExpr -> PVSType -> RProgram -> Program
real2fpProg normBE fp prog = map (real2fpDecl normBE fp prog) prog

real2fpDecl :: NormBoolExpr -> PVSType -> RProgram -> RDecl -> Decl
real2fpDecl normBE fp prog (RDecl retType f xs stm)
  | retType == TInt = Decl True TInt f (map (real2fpArg fp) xs)
                                       (real2fpAexpr normBE True fp prog stm)
  | otherwise       = Decl True fp   f (map (real2fpArg fp) xs)
                                       (real2fpAexpr normBE False fp prog stm)
real2fpDecl normBE fp prog (RPred f xs expr) = Pred True Original f (map (real2fpArg fp) xs)
                                                             (real2fpBexprStm normBE fp prog expr)

real2fpBexprStm :: NormBoolExpr -> PVSType -> RProgram -> BExprStm -> FBExprStm
real2fpBexprStm normBE fp prog (RBLet letElems stm) = BLet (map (real2letElem normBE fp prog) letElems)
                                                           (real2fpBexprStm normBE fp prog stm)
real2fpBexprStm normBE fp prog (RBIte be thenStm elseStm) = BIte (real2fpBexpr normBE False fp prog be)
                                                     (real2fpBexprStm normBE fp prog thenStm)
                                                     (real2fpBexprStm normBE fp prog elseStm)
real2fpBexprStm normBE fp prog (RBListIte listThen elseStm) =  BListIte (map real2fpItePair listThen)
                                                            (real2fpBexprStm normBE fp prog elseStm)
  where
    real2fpItePair (fbe, stm) = (real2fpBexpr normBE False fp prog fbe, real2fpBexprStm normBE fp prog stm)
real2fpBexprStm normBE fp prog (RBExpr be) = BExpr (real2fpBexpr normBE False fp prog be)

real2letElem :: NormBoolExpr -> PVSType -> RProgram -> LetElem -> FLetElem
real2letElem normBE fp prog letElem | t == TInt || isIntAExpr ae = (x, TInt, real2fpAexpr normBE True fp prog ae)
                             | otherwise                  = (x, fp,   real2fpAexpr normBE False fp prog ae)
  where
    t  = letType letElem
    ae = letExpr letElem
    x  = letVar  letElem

real2fpBexpr :: NormBoolExpr -> Bool -> PVSType -> RProgram -> BExpr -> FBExpr
real2fpBexpr _ _ _ _ BTrue  = FBTrue
real2fpBexpr _ _ _ _ BFalse = FBFalse
real2fpBexpr normBE isCast fp prog (Or  b1 b2) = FOr  (real2fpBexpr normBE isCast fp prog b1)
                                                            (real2fpBexpr normBE isCast fp prog b2)
real2fpBexpr normBE isCast fp prog (And b1 b2) = FAnd (real2fpBexpr normBE isCast fp prog b1)
                                                            (real2fpBexpr normBE isCast fp prog b2)
real2fpBexpr normBE isCast fp prog (Not b)     = FNot (real2fpBexpr normBE isCast fp prog b)
real2fpBexpr normBE _ fp prog (Rel rel  a1 a2) = real2fpRel normBE fp prog rel a1 a2
real2fpBexpr normBE isCast fp prog (EPred f args) = FEPred True Original f (real2fpActArgs normBE isCast fp prog formArgs args)
  where
    formArgs = realDeclArgs $ fromMaybe (error $ "real2fpAexpr: function " ++ f ++ " not found in program.")
                            (findInRealProg f prog)

real2fpRel :: NormBoolExpr -> PVSType -> RProgram -> RelOp -> AExpr -> AExpr -> FBExpr
real2fpRel False fp prog rel ae1 ae2
  | isIntAExpr ae1 && isIntAExpr ae2 = FRel rel (real2fpAexpr False True fp prog ae1)
                                                (real2fpAexpr False True fp prog ae2)
  | isIntAExpr ae1 && not (isIntAExpr ae2) = FRel rel (TypeCast TInt fp (real2fpAexpr False True fp prog ae1))
                                                      (real2fpAexpr False False fp prog ae2)
  | isIntAExpr ae2 && not (isIntAExpr ae1) = FRel rel (real2fpAexpr False False fp prog ae1)
                                                      (TypeCast TInt fp (real2fpAexpr False True fp prog ae2))
  | otherwise = FRel rel (real2fpAexpr False False fp prog ae1)
                         (real2fpAexpr False False fp prog ae2)

real2fpRel True fp prog rel ae1 ae2
  | isIntAExpr ae1 && isIntAExpr ae2 && (isZeroAExpr ae1 || isZeroAExpr ae2)
    = FRel rel (real2fpAexpr True True fp prog ae1)
               (real2fpAexpr True True fp prog ae2)
  | isIntAExpr ae1 && isIntAExpr ae2
    = FRel rel (BinaryFPOp SubOp fp (real2fpAexpr True True fp prog ae1)
                                    (real2fpAexpr True True fp prog ae2)) (FInt 0)
  | isIntAExpr ae1 && not (isIntAExpr ae2) && (isZeroAExpr ae1 || isZeroAExpr ae2)
    = FRel rel (TypeCast TInt fp (real2fpAexpr True True fp prog ae1))
               (real2fpAexpr True False fp prog ae2)
  | isIntAExpr ae1 && not (isIntAExpr ae2)
    = FRel rel (BinaryFPOp SubOp fp (TypeCast TInt fp (real2fpAexpr True True fp prog ae1))
                                    (real2fpAexpr True False fp prog ae2)) (FInt 0)
  | isIntAExpr ae2 && not (isIntAExpr ae1) && (isZeroAExpr ae1 || isZeroAExpr ae2)
    = FRel rel (real2fpAexpr True False fp prog ae1)
               (TypeCast TInt fp (real2fpAexpr True True fp prog ae2))
  | isIntAExpr ae2 && not (isIntAExpr ae1)
    = FRel rel (BinaryFPOp SubOp fp (real2fpAexpr True False fp prog ae1)
                                    (TypeCast TInt fp (real2fpAexpr True True fp prog ae2))) (FInt 0)
  | (isZeroAExpr ae1 || isZeroAExpr ae2) = FRel rel (real2fpAexpr True False fp prog ae1)
                               (real2fpAexpr True False fp prog ae2)
  | otherwise = FRel rel (BinaryFPOp SubOp fp (real2fpAexpr True False fp prog ae1)
                                              (real2fpAexpr True False fp prog ae2)) (FInt 0)

real2fpAexpr :: NormBoolExpr -> Bool -> PVSType -> RProgram -> AExpr -> FAExpr
real2fpAexpr normBE False fp prog ae | fp/=TInt && isIntAExpr ae = TypeCast TInt fp $ real2fpAexpr normBE True fp prog ae
real2fpAexpr _ _ _ _ (Int n) = FInt n
real2fpAexpr _ _     TInt _ (Rat r) | isInt r = FInt $ floor r
real2fpAexpr _ True  _    _ (Rat r) | isInt r = FInt $ floor r
real2fpAexpr _ _     fp   _ (Rat r) = ToFloat fp (Rat r)
real2fpAexpr _ _ fp _ (Interval lb ub) = FInterval fp lb ub
real2fpAexpr _ _    TInt  _ (Var TInt x) = FVar TInt x
real2fpAexpr _ True _     _ (Var TInt x) = FVar TInt x
real2fpAexpr _ _    fp    _ (Var _    x) = FVar fp   x
real2fpAexpr _ _    fp    _ (RealMark x ResValue) = FVar fp   x
real2fpAexpr normBE _    TInt  prog (ArrayElem TInt size v idxs)
  = FArrayElem TInt size v (map (real2fpAexpr normBE True TInt prog) idxs)
real2fpAexpr normBE True fp prog (ArrayElem _    size v idxs)
  = FArrayElem fp size v (map (real2fpAexpr normBE True TInt prog) idxs)
real2fpAexpr _ _    fp    _ (FromFloat fp' ae) = ToFloat fp (FromFloat fp' ae)
real2fpAexpr normBE isCast fp prog (EFun f field TInt args) = FEFun True f field TInt $
  if null prog
    then map (real2fpAexpr normBE isCast fp prog) args
    else (real2fpActArgs normBE isCast fp prog formArgs args)
  where
    formArgs = realDeclArgs $ fromMaybe (error $ "real2fpAexpr: function " ++ f ++ " not found in program.")
                            (findInRealProg f prog)
real2fpAexpr normBE isCast fp prog (EFun f field _ args)    = FEFun True f field fp $
  if null prog
    then map (real2fpAexpr normBE isCast fp prog) args
    else (real2fpActArgs normBE isCast fp prog formArgs args)
  where
    formArgs = realDeclArgs $ fromMaybe (error $ "real2fpAexpr: function " ++ f ++ " not found in program.")
                            (findInRealProg f prog)
real2fpAexpr normBE isCast fp prog (UnaryOp  op ae1)  = UnaryFPOp op fp' (real2fpAexpr normBE isCast fp prog ae1)
  where
    fp' = unaryOpPVSType fp ae1
real2fpAexpr normBE isCast fp prog (BinaryOp op ae1 ae2) = BinaryFPOp op fp' (real2fpAexpr normBE isCast fp prog ae1)
                                                                      (real2fpAexpr normBE isCast fp prog ae2)
  where
    fp' = binaryOpPVSType fp ae1 ae2
real2fpAexpr normBE isCast fp prog (RLet letElems stm) = Let (map (real2letElem normBE fp prog) letElems)
                                                      (real2fpAexpr normBE isCast fp prog stm)
real2fpAexpr normBE isCast fp prog (RIte be thenStm elseStm) = Ite (real2fpBexpr normBE isCast fp prog be)
                                                       (real2fpAexpr normBE isCast fp prog thenStm)
                                                       (real2fpAexpr normBE isCast fp prog elseStm)
real2fpAexpr normBE isCast fp prog (RListIte thenList stmElse) = ListIte (map real2fpItePair thenList)
                                                             (real2fpAexpr normBE isCast fp prog stmElse)
  where
    real2fpItePair (fbe, stm) = (real2fpBexpr normBE isCast fp prog fbe, real2fpAexpr normBE isCast fp prog stm)
real2fpAexpr normBE isCast fp prog (RForLoop retType startIdx endIdx initValueAcc idx acc forBody) =
  ForLoop retType (real2fpAexpr normBE isCast fp prog startIdx)
                             (real2fpAexpr normBE isCast fp prog endIdx)
                             (real2fpAexpr normBE isCast fp prog initValueAcc)
                             idx acc
                             (real2fpAexpr normBE isCast fp prog forBody)
real2fpAexpr _ _ _  _ RUnstWarning               = UnstWarning
real2fpAexpr _ _ _ _ ae = error $ "real2fpAexpr not defined for " ++ show ae

real2fpActArgs :: NormBoolExpr -> Bool -> PVSType -> RProgram -> [Arg] -> [AExpr] -> [FAExpr]
real2fpActArgs _ _ _ _ [] [] = []
real2fpActArgs normBE isCast fp prog ((Arg x TInt):formArgs) (expr:actArgs)
  | isIntAExpr expr = (real2fpAexpr normBE isCast TInt prog expr):(real2fpActArgs normBE isCast fp prog formArgs actArgs)
  | otherwise = error $ "real2fpActArgs: type mismatch. Argument "
                        ++ x ++ " is of type integer, but expression"
                        ++ show expr ++ " is not."
real2fpActArgs normBE isCast fp prog ((Arg _ _):formArgs) (expr:actArgs)
  | isIntAExpr expr = ((TypeCast TInt fp (real2fpAexpr normBE True fp prog expr)):real2fpActArgs normBE isCast fp prog formArgs actArgs)
  | otherwise       = ((real2fpAexpr normBE isCast fp prog expr):real2fpActArgs normBE isCast fp prog formArgs actArgs)
real2fpActArgs _ _ _ _ _ _ = error $ "real2fpActArgs: argument list size mismatch."