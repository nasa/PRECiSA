-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module AbsPVSLangTest where

import Test.Tasty
import Test.Tasty.HUnit
import AbsPVSLang
import PVSTypes
import Operators
import Common.TypesUtils
import UtilsTest (fromDouble2Rat)
import Data.Set (fromList)


testAbsPVSLang = testGroup "AbsPVSLang"
   [simplFAExpr__tests
   ,rewriteEquivEExpr__tests
   ,equivEExpr__tests
   ,replaceVarWithAExpr__tests
   ,substituteInAExpr__tests
   ,initErrorMark__tests
   ,initAExpr__tests
   ,initBExpr__tests
   ,funCallListFAExpr__tests
   ,funCallListFBExpr__tests
   ,funCallListAExpr__tests
   ,funCallListBExpr__tests
   ,noRoundOffErrorInAExpr__tests
   ,varList__tests
   ,equivModuloIndex__tests
   ,localVars__tests
   ,localVarsBExpr__tests
   ,localVarsBExprStm__tests
   ,forIndexes__tests
   ,funCallList__tests
   ,unfoldForLoop__tests
   ,isBExprEquivFalse__tests
   ,isFBExprEquivFalse__tests
   ,makeFPDeclRecursive__tests
   ,isArithExpr__tests
   ,isListArithExprs__tests
   ,listLetElems__tests
   ,removeLetInFAExpr__tests
   ,hasConditionals__tests
   ,hasConditionalsBExpr__tests
   ,predCallListFBExprStmWithCond__tests
   ,simplFRel__tests
   ,simplBExprFix__tests
   ,isIntAExpr__tests
   ,simplAExpr__tests
   ,renameVar__tests
   ,renameFVar__tests
   ,renameVarsAExpr__tests
   ,renameVarsFAExpr__tests
   ,renameVarsFBExprStm__tests
   ,renameVarsBExprStm__tests
   ,subExpressions__tests
   ,unfoldLetIn__tests
   ]


subExpressions__tests = testGroup "subExpressions__tests" $ [
  subExpressions__test1
 ,subExpressions__test2
 ,subExpressions__test3
 ,subExpressions__test4
 ,subExpressions__test5
 ,subExpressions__test6
 ,subExpressions__test7
 ,subExpressions__test8
 ,subExpressions__test9
 ,subExpressions__test10
 ,subExpressions__test11
 ,subExpressions__test12
 ,subExpressions__test13
 ]


setEquiv :: (Ord a, Show a) => [a] -> [a] -> IO()
setEquiv set1 set2 = fromList set1 @?= fromList set2

subExpressions__test1 = testCase "subExpressions Let" $
  subExpressions (Let [("x",TInt,FInt 3)] (FVar TInt "x"))
  `setEquiv`
  [Let [("x",TInt,FInt 3)] (FVar TInt "x"),FInt 3, FVar TInt "x"]

subExpressions__test2 = testCase "subExpressions Let repetition" $
  subExpressions (Let [("x",TInt,FInt 3)] (FInt 3))
  `setEquiv`
  [Let [("x",TInt,FInt 3)] (FInt 3),FInt 3]

subExpressions__test3 = testCase "subExpressions Ite" $
  subExpressions (Ite (FRel Eq (FVar TInt "x") (FInt 4)) (FVar TInt "x") (FVar TInt "z"))
  `setEquiv`
  [Ite (FRel Eq (FVar TInt "x") (FInt 4)) (FVar TInt "x") (FVar TInt "z")
  ,FInt 4
  ,FVar TInt "x"
  ,FVar TInt "z"]

subExpressions__test4 = testCase "subExpressions Ite repetition" $
  subExpressions (Ite (FRel Eq (FVar TInt "x") (FInt 4)) (FVar TInt "x") (FVar TInt "x"))
  `setEquiv`
  [Ite (FRel Eq (FVar TInt "x") (FInt 4)) (FVar TInt "x") (FVar TInt "x")
  ,FVar TInt "x"
  ,FInt 4
  ,FVar TInt "x"
  ,FVar TInt "x"]

subExpressions__test5 = testCase "subExpressions Binary Op" $
  subExpressions (BinaryFPOp MulOp FPDouble (FVar TInt "x") (FVar TInt "x"))
  `setEquiv`
  [BinaryFPOp MulOp FPDouble (FVar TInt "x") (FVar TInt "x")
  ,FVar TInt "x"
  ,FVar TInt "x"]

subExpressions__test6 = testCase "subExpressions Binary Op repetition" $
  subExpressions (BinaryFPOp MulOp FPDouble (FVar TInt "x") (FVar TInt "z"))
  `setEquiv`
  [BinaryFPOp MulOp FPDouble (FVar TInt "x") (FVar TInt "z")
  ,FVar TInt "x"
  ,FVar TInt "z"]

subExpressions__test7 = testCase "subExpressions Unary Op" $
  subExpressions (UnaryFPOp NegOp FPDouble (FVar TInt "x"))
  `setEquiv`
  [UnaryFPOp NegOp FPDouble (FVar TInt "x")
  ,FVar TInt "x"]

subExpressions__test8 = testCase "subExpressions Unary Op repetition" $
  subExpressions (UnaryFPOp NegOp FPDouble (FVar TInt "x"))
  `setEquiv`
  [UnaryFPOp NegOp FPDouble (FVar TInt "x"),FVar TInt "x"]

subExpressions__test9 = testCase "subExpressions EFun" $
  subExpressions (FEFun True "f" ResValue TInt [FVar TInt "y"])
  `setEquiv`
  [FEFun True "f" ResValue TInt [FVar TInt "y"], FVar TInt "y"]

subExpressions__test10 = testCase "subExpressions ArrayElem" $
  subExpressions (FArrayElem TInt (Just $ ArraySizeInt 3) "y" [(FVar TInt "x")])
  `setEquiv`
  [FArrayElem TInt (Just $ ArraySizeInt 3) "y" [(FVar TInt "x")]
  ,FVar TInt "x"]

subExpressions__test11 = testCase "subExpressions ListIte" $
  subExpressions (ListIte [(FRel Eq (FVar TInt "x") (FInt 4),FVar TInt "b")] (FVar TInt "x"))
  `setEquiv`
  [ListIte [(FRel Eq (FVar TInt "x") (FInt 4),FVar TInt "b")] (FVar TInt "x")
  ,FVar TInt "b"
  ,FVar TInt "x"
  ,FInt 4]

subExpressions__test12 = testCase "subExpressions FMin" $
  subExpressions (FMin [(FVar TInt "y")])
  `setEquiv`
  [FMin [(FVar TInt "y")], FVar TInt "y"]

subExpressions__test13 = testCase "subExpressions FMax" $
  subExpressions  (FMax [(FVar TInt "y")])
  `setEquiv`
  [FMax [(FVar TInt "y")], FVar TInt "y"]


renameVar__tests = testGroup "renameVar__tests" $ [
  renameVar__test1
 ,renameVar__test2
 ,renameVar__test3
 ,renameVar__test4
 ,renameVar__test5
 ,renameVar__test6
 ,renameVar__test7
 ,renameVar__test8
 ,renameVar__test9
 ,renameVar__test10
 ]

renameVar__test1 = testCase "renameVar RLet" $
  renameVar [("x","y")] (RLet [LetElem {letVar = "x",
                                       letType = TInt,
                                       letExpr = Int 3}] (Var TInt "x"))
  @?=
  Just (RLet [LetElem {letVar = "y",
                        letType = TInt,
                        letExpr = Int 3}] (Var TInt "y"))

renameVar__test2 = testCase "renameVar RLet no match" $
  renameVar [("x","y")] (RLet [LetElem {letVar = "z",
                                       letType = TInt,
                                       letExpr = Int 3}] (Var TInt "z"))
  @?=
  Just (RLet [LetElem {letVar = "z",
                        letType = TInt,
                        letExpr = Int 3}] (Var TInt "z"))

renameVar__test3 = testCase "renameVar Var" $
  renameVar [("x","y")] (Var TInt "x")
  @?=
  Just (Var TInt "y")

renameVar__test4 = testCase "renameVar Var no match" $
  renameVar [("z","y")] (Var TInt "x")
  @?=
  Nothing

renameVar__test5 = testCase "renameVar RealMark" $
  renameVar [("x","y")] (RealMark "x" ResValue)
  @?=
  Just (RealMark "y" ResValue)

renameVar__test6 = testCase "renameVar RealMark no match" $
  renameVar [("z","y")] (RealMark "x" ResValue)
  @?=
  Nothing

renameVar__test7 = testCase "renameVar ErrorMark" $
  renameVar [("x","y")] (ErrorMark "x" ResValue TInt)
  @?=
  Just (ErrorMark "y" ResValue TInt)

renameVar__test8 = testCase "renameVar ErrorMark no match" $
  renameVar [("z","y")] (ErrorMark "x" ResValue TInt)
  @?=
  Nothing

renameVar__test9 = testCase "renameVar RLet nested" $
  renameVar [("x","z")] (RLet [LetElem {letVar = "x",
                                       letType = TInt,
                                       letExpr = Int 3}]
                                       (RLet [LetElem {letVar = "y",
                                                       letType = TInt,
                                                       letExpr = Var TInt "x"}] (Var TInt "y")))
  @?=
  Just (RLet [LetElem {letVar = "z",
                       letType = TInt,
                       letExpr = Int 3}]
                       (RLet [LetElem {letVar = "y",
                                        letType = TInt,
                                        letExpr = Var TInt "z"}] (Var TInt "y")))

renameVar__test10 = testCase "renameVar RLet nested nomatch" $
  renameVar [("a","z")] (RLet [LetElem {letVar = "x",
                                       letType = TInt,
                                       letExpr = Int 3}]
                                       (RLet [LetElem {letVar = "y",
                                                       letType = TInt,
                                                       letExpr = Var TInt "x"}] (Var TInt "y")))
  @?=
  Just (RLet [LetElem {letVar = "x",
                                       letType = TInt,
                                       letExpr = Int 3}]
                                       (RLet [LetElem {letVar = "y",
                                                       letType = TInt,
                                                       letExpr = Var TInt "x"}] (Var TInt "y")))

renameFVar__tests = testGroup "renameFVar__tests" $ [
  renameFVar__test1
 ,renameFVar__test2
 ,renameFVar__test3
 ,renameFVar__test4
 ,renameFVar__test5
 ,renameFVar__test6
 ]

renameFVar__test1 = testCase "renameFVar RLet" $
  renameFVar [("x","y")] (Let [("x",TInt,FInt 3)] (FVar TInt "x"))
  @?=
  Just (Let [("y",TInt,FInt 3)] (FVar TInt "y"))

renameFVar__test2 = testCase "renameVar RLet no match" $
  renameFVar [("x","y")] (Let [("z",TInt,FInt 3)] (FVar TInt "z"))
  @?=
  Just (Let [("z",TInt,FInt 3)] (FVar TInt "z"))

renameFVar__test3 = testCase "renameVar Var" $
  renameFVar [("x","y")] (FVar TInt "x")
  @?=
  Just (FVar TInt "y")

renameFVar__test4 = testCase "renameVar Var no match" $
  renameFVar [("z","y")] (FVar TInt "x")
  @?=
  Nothing

renameFVar__test5 = testCase "renameFVar RLet nested" $
  renameFVar [("x","y")] (Let [("x",TInt,FInt 3)] (FVar TInt "x"))
  @?=
  Just (Let [("y",TInt,FInt 3)] (FVar TInt "y"))

renameFVar__test6 = testCase "renameVar RLet nested no match" $
  renameFVar [("x","y")] (Let [("z",TInt,FInt 3)] (FVar TInt "z"))
  @?=
  Just (Let [("z",TInt,FInt 3)] (FVar TInt "z"))


renameVarsAExpr__tests = testGroup "renameVarsAExpr__tests" $ [
  renameVarsAExpr__test1
 ,renameVarsAExpr__test2
 ,renameVarsAExpr__test3
 ,renameVarsAExpr__test4
 ,renameVarsAExpr__test5
 ,renameVarsAExpr__test6
 ,renameVarsAExpr__test7
 ,renameVarsAExpr__test8
 ,renameVarsAExpr__test9
 ,renameVarsAExpr__test10
 ,renameVarsAExpr__test11
 ,renameVarsAExpr__test12
 ,renameVarsAExpr__test12
 ,renameVarsAExpr__test13
 ,renameVarsAExpr__test14
 ,renameVarsAExpr__test15
 ,renameVarsAExpr__test16
 ,renameVarsAExpr__test17
 ,renameVarsAExpr__test18
 ,renameVarsAExpr__test19
 ,renameVarsAExpr__test20
 ]

renameVarsAExpr__test1 = testCase "renameVarsAExpr RLet" $
  renameVarsAExpr [("x","y")] (RLet [LetElem {letVar = "x",
                                              letType = TInt,
                                              letExpr = Int 3}] (Var TInt "x"))
  @?=
  RLet [LetElem {letVar = "y",
                 letType = TInt,
                 letExpr = Int 3}] (Var TInt "y")

renameVarsAExpr__test2 = testCase "renameVarsAExpr RLet no match" $
  renameVarsAExpr [("x","y")] (RLet [LetElem {letVar = "z",
                                              letType = TInt,
                                              letExpr = Int 3}] (Var TInt "z"))
  @?=
  RLet [LetElem {letVar = "z",
                 letType = TInt,
                 letExpr = Int 3}] (Var TInt "z")

renameVarsAExpr__test3 = testCase "renameVarsAExpr RIte" $
  renameVarsAExpr [("x","y")] (RIte (Rel Eq (Var TInt "x") (Int 4)) (Var TInt "x") (Var TInt "z"))
  @?=
  (RIte (Rel Eq (Var TInt "y") (Int 4)) (Var TInt "y") (Var TInt "z"))

renameVarsAExpr__test4 = testCase "renameVarsAExpr RIte no match" $
  renameVarsAExpr [("a","y")] (RIte (Rel Eq (Var TInt "x") (Int 4)) (Var TInt "x") (Var TInt "z"))
  @?=
  (RIte (Rel Eq (Var TInt "x") (Int 4)) (Var TInt "x") (Var TInt "z"))

renameVarsAExpr__test5 = testCase "renameVarsAExpr Binary Op" $
  renameVarsAExpr [("x","y")] (BinaryOp MulOp (Var TInt "x") (Var TInt "z"))
  @?=
  (BinaryOp MulOp (Var TInt "y") (Var TInt "z"))

renameVarsAExpr__test6 = testCase "renameVarsAExpr Binary Op no match" $
  renameVarsAExpr [("a","y")] (BinaryOp MulOp (Var TInt "x") (Var TInt "z"))
  @?=
  (BinaryOp MulOp (Var TInt "x") (Var TInt "z"))

renameVarsAExpr__test7 = testCase "renameVarsAExpr Unary Op" $
  renameVarsAExpr [("x","y")] (UnaryOp NegOp (Var TInt "x"))
  @?=
  (UnaryOp NegOp (Var TInt "y"))

renameVarsAExpr__test8 = testCase "renameVarsAExpr Unary Op no match" $
  renameVarsAExpr [("a","y")] (UnaryOp NegOp (Var TInt "x"))
  @?=
  (UnaryOp NegOp (Var TInt "x"))

renameVarsAExpr__test9 = testCase "renameVarsAExpr EFun" $
  renameVarsAExpr [("x","y")] (EFun "f" ResValue TInt [Var TInt "x"])
  @?=
  EFun "f" ResValue TInt [Var TInt "y"]

renameVarsAExpr__test10 = testCase "renameVarsAExpr EFun no match" $
  renameVarsAExpr [("a","y")] (EFun "f" ResValue TInt [Var TInt "x"])
  @?=
  (EFun "f" ResValue TInt [Var TInt "x"])

renameVarsAExpr__test11 = testCase "renameVarsAExpr ArrayElem" $
  renameVarsAExpr [("x","y")] (ArrayElem TInt (Just $ ArraySizeInt 3) "x" [(Var TInt "x")])
  @?=
  ArrayElem TInt (Just $ ArraySizeInt 3) "y" [(Var TInt "y")]

renameVarsAExpr__test12 = testCase "renameVarsAExpr ArrayElem no match" $
  renameVarsAExpr [("a","y")] (ArrayElem TInt (Just $ ArraySizeInt 3) "x" [(Var TInt "x")])
  @?=
  (ArrayElem TInt (Just $ ArraySizeInt 3) "x" [(Var TInt "x")])

renameVarsAExpr__test13 = testCase "renameVarsAExpr RListIte" $
  renameVarsAExpr [("x","y")] (RListIte [(Rel Eq (Var TInt "x") (Int 4),Var TInt "b")] (Var TInt "x"))
  @?=
  (RListIte [(Rel Eq (Var TInt "y") (Int 4),Var TInt "b")] (Var TInt "y"))

renameVarsAExpr__test14 = testCase "renameVarsAExpr RListIte no match" $
  renameVarsAExpr [("a","y")] (RListIte [(Rel Eq (Var TInt "x") (Int 4),Var TInt "b")] (Var TInt "x"))
  @?=
  (RListIte [(Rel Eq (Var TInt "x") (Int 4),Var TInt "b")] (Var TInt "x"))

renameVarsAExpr__test15 = testCase "renameVarsAExpr Min" $
  renameVarsAExpr [("x","y")] (Min [(Var TInt "x")])
  @?=
  (Min [(Var TInt "y")])

renameVarsAExpr__test16 = testCase "renameVarsAExpr Max" $
  renameVarsAExpr [("x","y")] (Max [(Var TInt "x")])
  @?=
  (Max [(Var TInt "y")])

renameVarsAExpr__test17 = testCase "renameVarsAExpr MaxErr" $
  renameVarsAExpr [("x","y")] (MaxErr [(Var TInt "x")])
  @?=
  (MaxErr [(Var TInt "y")])

renameVarsAExpr__test18 = testCase "renameVarsAExpr ErrBinOp" $
  renameVarsAExpr [("x","y")] (ErrBinOp MulOp FPDouble (Var TInt "x") (Var TInt "x") (Var TInt "z") (Var TInt "z"))
  @?=
  (ErrBinOp MulOp FPDouble (Var TInt "y") (Var TInt "y") (Var TInt "z") (Var TInt "z"))

renameVarsAExpr__test19 = testCase "renameVarsAExpr MaxErr" $
  renameVarsAExpr [("x","y")] (MaxErr [(Var TInt "Err_x")])
  @?=
  (MaxErr [(Var TInt "Err_y")])

renameVarsAExpr__test20 = testCase "renameVarsAExpr RLet" $
  renameVarsAExpr [("x","y")] (RLet [LetElem {letVar = "Err_x",
                                              letType = TInt,
                                              letExpr = Int 3}] (Var TInt "x"))
  @?=
  RLet [LetElem {letVar = "Err_y",
                 letType = TInt,
                 letExpr = Int 3}] (Var TInt "y")

renameVarsFAExpr__tests = testGroup "renameVarsFAExpr__tests" $ [
  renameVarsFAExpr__test1
 ,renameVarsFAExpr__test2
 ,renameVarsFAExpr__test3
 ,renameVarsFAExpr__test4
 ,renameVarsFAExpr__test5
 ,renameVarsFAExpr__test6
 ,renameVarsFAExpr__test7
 ,renameVarsFAExpr__test8
 ,renameVarsFAExpr__test9
 ,renameVarsFAExpr__test10
 ,renameVarsFAExpr__test11
 ,renameVarsFAExpr__test12
 ,renameVarsFAExpr__test12
 ,renameVarsFAExpr__test13
 ,renameVarsFAExpr__test14
 ,renameVarsFAExpr__test15
 ,renameVarsFAExpr__test16
 ]

renameVarsFAExpr__test1 = testCase "renameVarsFAExpr Let" $
  renameVarsFAExpr [("x","y")] (Let [("x",TInt,FInt 3)] (FVar TInt "x"))
  @?=
  Let [("y",TInt,FInt 3)] (FVar TInt "y")

renameVarsFAExpr__test2 = testCase "renameVarsFAExpr Let no match" $
  renameVarsFAExpr [("a","y")] (Let [("x",TInt,FInt 3)] (FVar TInt "x"))
  @?=
  Let [("x",TInt,FInt 3)] (FVar TInt "x")

renameVarsFAExpr__test3 = testCase "renameVarsFAExpr Ite" $
  renameVarsFAExpr [("x","y")] (Ite (FRel Eq (FVar TInt "x") (FInt 4)) (FVar TInt "x") (FVar TInt "z"))
  @?=
  Ite (FRel Eq (FVar TInt "y") (FInt 4)) (FVar TInt "y") (FVar TInt "z")

renameVarsFAExpr__test4 = testCase "renameVarsFAExpr Ite no match" $
  renameVarsFAExpr [("a","y")] (Ite (FRel Eq (FVar TInt "x") (FInt 4)) (FVar TInt "x") (FVar TInt "z"))
  @?=
  Ite (FRel Eq (FVar TInt "x") (FInt 4)) (FVar TInt "x") (FVar TInt "z")

renameVarsFAExpr__test5 = testCase "renameVarsFAExpr Binary Op" $
  renameVarsFAExpr [("x","y")] (BinaryFPOp MulOp FPDouble (FVar TInt "x") (FVar TInt "z"))
  @?=
  BinaryFPOp MulOp FPDouble (FVar TInt "y") (FVar TInt "z")

renameVarsFAExpr__test6 = testCase "renameVar Binary Op no match" $
  renameVarsFAExpr [("a","y")] (BinaryFPOp MulOp FPDouble (FVar TInt "x") (FVar TInt "z"))
  @?=
  (BinaryFPOp MulOp FPDouble (FVar TInt "x") (FVar TInt "z"))

renameVarsFAExpr__test7 = testCase "renameVar Unary Op" $
  renameVarsFAExpr [("x","y")] (UnaryFPOp NegOp FPDouble (FVar TInt "x"))
  @?=
  (UnaryFPOp NegOp FPDouble (FVar TInt "y"))

renameVarsFAExpr__test8 = testCase "renameVar Unary Op no match" $
  renameVarsFAExpr [("a","y")] (UnaryFPOp NegOp FPDouble (FVar TInt "x"))
  @?=
  (UnaryFPOp NegOp FPDouble (FVar TInt "x"))

renameVarsFAExpr__test9 = testCase "renameVar EFun" $
  renameVarsFAExpr [("x","y")] (FEFun True "f" ResValue TInt [FVar TInt "x"])
  @?=
  FEFun True "f" ResValue TInt [FVar TInt "y"]

renameVarsFAExpr__test10 = testCase "renameVar EFun no match" $
  renameVarsFAExpr [("a","y")] (FEFun True "f" ResValue TInt [FVar TInt "x"])
  @?=
  (FEFun True "f" ResValue TInt [FVar TInt "x"])

renameVarsFAExpr__test11 = testCase "renameVar ArrayElem" $
  renameVarsFAExpr [("x","y")] (FArrayElem TInt (Just $ ArraySizeInt 3) "x" [(FVar TInt "x")])
  @?=
  FArrayElem TInt (Just $ ArraySizeInt 3) "y" [(FVar TInt "y")]

renameVarsFAExpr__test12 = testCase "renameVar ArrayElem no match" $
  renameVarsFAExpr [("a","y")] (FArrayElem TInt (Just $ ArraySizeInt 3) "x" [(FVar TInt "x")])
  @?=
  (FArrayElem TInt (Just $ ArraySizeInt 3) "x" [(FVar TInt "x")])

renameVarsFAExpr__test13 = testCase "renameVar ListIte" $
  renameVarsFAExpr [("x","y")] (ListIte [(FRel Eq (FVar TInt "x") (FInt 4),FVar TInt "b")] (FVar TInt "x"))
  @?=
  (ListIte [(FRel Eq (FVar TInt "y") (FInt 4),FVar TInt "b")] (FVar TInt "y"))

renameVarsFAExpr__test14 = testCase "renameVar ListIte no match" $
  renameVarsFAExpr [("a","y")] (ListIte [(FRel Eq (FVar TInt "x") (FInt 4),FVar TInt "b")] (FVar TInt "x"))
  @?=
  (ListIte [(FRel Eq (FVar TInt "x") (FInt 4),FVar TInt "b")] (FVar TInt "x"))

renameVarsFAExpr__test15 = testCase "renameVar FMin" $
  renameVarsFAExpr [("x","y")] (FMin [(FVar TInt "x")])
  @?=
  (FMin [(FVar TInt "y")])

renameVarsFAExpr__test16 = testCase "renameVar FMax" $
  renameVarsFAExpr [("x","y")] (FMax [(FVar TInt "x")])
  @?=
  (FMax [(FVar TInt "y")])

renameVarsFBExprStm__tests = testGroup "renameVarsFBExpr__tests" $ [
  renameVarsFBExprStm__test1
 ,renameVarsFBExprStm__test2
 ,renameVarsFBExprStm__test3
 ,renameVarsFBExprStm__test4
 ,renameVarsFBExprStm__test5
 ,renameVarsFBExprStm__test6
 ,renameVarsFBExprStm__test7
 ,renameVarsFBExprStm__test8
 ,renameVarsFBExprStm__test9
 ]

renameVarsFBExprStm__test1 = testCase "renameVarsFBExprStm Let" $
  renameVarsFBExprStm [("x","y")] (BLet [("x",TInt,FInt 3)] (BExpr (FRel Eq (FVar TInt "x") (FInt 4))))
  @?=
  BLet [("y",TInt,FInt 3)] (BExpr (FRel Eq (FVar TInt "y") (FInt 4)))

renameVarsFBExprStm__test2 = testCase "renameVarsFBExprStm Let no match" $
  renameVarsFBExprStm [("a","y")] (BLet [("x",TInt,FInt 3)] (BExpr (FRel Eq (FVar TInt "x") (FInt 4))))
  @?=
  BLet [("x",TInt,FInt 3)] (BExpr (FRel Eq (FVar TInt "x") (FInt 4)))

renameVarsFBExprStm__test3 = testCase "renameVarsFBExprStm BIte" $
  renameVarsFBExprStm [("x","y")] (BIte (FRel Eq (FVar TInt "x") (FInt 4))
                                        (BExpr (FRel Neq (FVar TInt "x") (FInt 4)))
                                        (BExpr (FRel Eq (FVar TInt "x") (FInt 4))))
  @?=
  BIte (FRel Eq  (FVar TInt "y") (FInt 4))
       (BExpr (FRel Neq (FVar TInt "y") (FInt 4)))
       (BExpr (FRel Eq  (FVar TInt "y") (FInt 4)))

renameVarsFBExprStm__test4 = testCase "renameVarsFBExprStm BIte no match" $
  renameVarsFBExprStm [("a","y")] (BIte (FRel Eq (FVar TInt "x") (FInt 4))
                                        (BExpr (FRel Neq (FVar TInt "x") (FInt 4)))
                                        (BExpr (FRel Eq (FVar TInt "x") (FInt 4))))
  @?=
  BIte (FRel Eq (FVar TInt "x") (FInt 4))
       (BExpr (FRel Neq (FVar TInt "x") (FInt 4)))
       (BExpr (FRel Eq (FVar TInt "x") (FInt 4)))

renameVarsFBExprStm__test5 = testCase "renameVar BListIte" $
  renameVarsFBExprStm [("x","y")] (BListIte [(FRel Eq (FVar TInt "x") (FInt 4)
                                            ,BExpr (FRel Eq (FVar TInt "b") (FInt 4)))]
                                             (BExpr (FRel Eq (FVar TInt "x") (FInt 4))))
  @?=
  BListIte [(FRel Eq (FVar TInt "y") (FInt 4), BExpr (FRel Eq (FVar TInt "b") (FInt 4)))]
                                         (BExpr (FRel Eq (FVar TInt "y") (FInt 4)))

renameVarsFBExprStm__test6 = testCase "renameVar BListIte no match" $
  renameVarsFBExprStm [("a","y")] (BListIte [(FRel Eq (FVar TInt "x") (FInt 4)
                                             ,BExpr (FRel Eq (FVar TInt "b") (FInt 4)))]
                                             (BExpr (FRel Eq (FVar TInt "x") (FInt 4))))
  @?=
  BListIte [(FRel Eq (FVar TInt "x") (FInt 4), BExpr (FRel Eq (FVar TInt "b") (FInt 4)))]
                                             (BExpr (FRel Eq (FVar TInt "x") (FInt 4)))

renameVarsFBExprStm__test7 = testCase "renameVar FOr" $
  renameVarsFBExprStm [("x","y")] (BExpr (FOr (FRel Eq (FVar TInt "x") (FInt 4)) (FRel Eq (FVar TInt "z") (FInt 4))))
  @?=
  BExpr (FOr (FRel Eq (FVar TInt "y") (FInt 4)) (FRel Eq (FVar TInt "z") (FInt 4)))

renameVarsFBExprStm__test8 = testCase "renameVar FAnd" $
  renameVarsFBExprStm [("x","y")] (BExpr (FAnd (FRel Eq (FVar TInt "x") (FInt 4)) (FRel Eq (FVar TInt "z") (FInt 4))))
  @?=
  BExpr (FAnd (FRel Eq (FVar TInt "y") (FInt 4)) (FRel Eq (FVar TInt "z") (FInt 4)))

renameVarsFBExprStm__test9 = testCase "renameVar FNot" $
  renameVarsFBExprStm [("x","y")] (BExpr (FNot (FRel Eq (FVar TInt "x") (FInt 4))))
  @?=
  BExpr (FNot (FRel Eq (FVar TInt "y") (FInt 4)))

renameVarsBExprStm__tests = testGroup "renameVarsFBExpr__tests" $ [
  renameVarsBExprStm__test1
 ,renameVarsBExprStm__test2
 ,renameVarsBExprStm__test3
 ,renameVarsBExprStm__test4
 ,renameVarsBExprStm__test5
 ,renameVarsBExprStm__test6
 ,renameVarsBExprStm__test7
 ,renameVarsBExprStm__test8
 ,renameVarsBExprStm__test9
 ]

renameVarsBExprStm__test1 = testCase "renameVarsBExprStm Let" $
  renameVarsBExprStm [("x","y")] (RBLet [LetElem {letVar = "x",
                                              letType = TInt,
                                              letExpr = Int 3}] (RBExpr (Rel Eq (Var TInt "x") (Int 4))))
  @?=
  RBLet [LetElem {letVar = "y",
                  letType = TInt,
                  letExpr = Int 3}] (RBExpr (Rel Eq (Var TInt "y") (Int 4)))

renameVarsBExprStm__test2 = testCase "renameVarsBExprStm Let no match" $
  renameVarsBExprStm [("a","y")] (RBLet [LetElem {letVar = "x",
                                              letType = TInt,
                                              letExpr = Int 3}] (RBExpr (Rel Eq (Var TInt "x") (Int 4))))
  @?=
  RBLet [LetElem {letVar = "x",
                 letType = TInt,
                 letExpr = Int 3}] (RBExpr (Rel Eq (Var TInt "x") (Int 4)))

renameVarsBExprStm__test3 = testCase "renameVarsBExprStm RBIte" $
  renameVarsBExprStm [("x","y")] (RBIte (Rel Eq (Var TInt "x") (Int 4))
                                        (RBExpr (Rel Neq (Var TInt "x") (Int 4)))
                                        (RBExpr (Rel Eq (Var TInt "x") (Int 4))))
  @?=
  RBIte (Rel Eq  (Var TInt "y") (Int 4))
       (RBExpr (Rel Neq (Var TInt "y") (Int 4)))
       (RBExpr (Rel Eq  (Var TInt "y") (Int 4)))

renameVarsBExprStm__test4 = testCase "renameVarsBExprStm RBIte no match" $
  renameVarsBExprStm [("a","y")] (RBIte (Rel Eq (Var TInt "x") (Int 4))
                                        (RBExpr (Rel Neq (Var TInt "x") (Int 4)))
                                        (RBExpr (Rel Eq (Var TInt "x") (Int 4))))
  @?=
  RBIte (Rel Eq (Var TInt "x") (Int 4))
       (RBExpr (Rel Neq (Var TInt "x") (Int 4)))
       (RBExpr (Rel Eq (Var TInt "x") (Int 4)))

renameVarsBExprStm__test5 = testCase "renameVar RBListIte" $
  renameVarsBExprStm [("x","y")] (RBListIte [(Rel Eq (Var TInt "x") (Int 4)
                                            ,RBExpr (Rel Eq (Var TInt "b") (Int 4)))]
                                             (RBExpr (Rel Eq (Var TInt "x") (Int 4))))
  @?=
  RBListIte [(Rel Eq (Var TInt "y") (Int 4), RBExpr (Rel Eq (Var TInt "b") (Int 4)))]
                                         (RBExpr (Rel Eq (Var TInt "y") (Int 4)))

renameVarsBExprStm__test6 = testCase "renameVar RBListIte no match" $
  renameVarsBExprStm [("a","y")] (RBListIte [(Rel Eq (Var TInt "x") (Int 4)
                                             ,RBExpr (Rel Eq (Var TInt "b") (Int 4)))]
                                             (RBExpr (Rel Eq (Var TInt "x") (Int 4))))
  @?=
  RBListIte [(Rel Eq (Var TInt "x") (Int 4), RBExpr (Rel Eq (Var TInt "b") (Int 4)))]
                                             (RBExpr (Rel Eq (Var TInt "x") (Int 4)))

renameVarsBExprStm__test7 = testCase "renameVar FOr" $
  renameVarsBExprStm [("x","y")] (RBExpr (Or (Rel Eq (Var TInt "x") (Int 4)) (Rel Eq (Var TInt "z") (Int 4))))
  @?=
  RBExpr (Or (Rel Eq (Var TInt "y") (Int 4)) (Rel Eq (Var TInt "z") (Int 4)))

renameVarsBExprStm__test8 = testCase "renameVar And" $
  renameVarsBExprStm [("x","y")] (RBExpr (And (Rel Eq (Var TInt "x") (Int 4)) (Rel Eq (Var TInt "z") (Int 4))))
  @?=
  RBExpr (And (Rel Eq (Var TInt "y") (Int 4)) (Rel Eq (Var TInt "z") (Int 4)))

renameVarsBExprStm__test9 = testCase "renameVar Not" $
  renameVarsBExprStm [("x","y")] (RBExpr (Not (Rel Eq (Var TInt "x") (Int 4))))
  @?=
  RBExpr (Not (Rel Eq (Var TInt "y") (Int 4)))

simplAExpr__tests = testGroup "simplAExpr" $ [
   testCase "int 1" $ simplAExpr (Int 1) @?= Int 1
  ,testCase "maxerr(0,0)" $ simplAExpr (MaxErr [Int 0, Int 0]) @?= Int 0
  ,testCase "maxerr(errrat 0, errrat 0)" $ simplAExpr (MaxErr [ErrRat (toRational (0::Double)),ErrRat (toRational (0::Double))]) @?= Int 0
  ,testCase "1 + maxerr(0,0)"
    $ simplAExpr (BinaryOp AddOp (Int 1) (MaxErr [Int 0, Int 0])) @?= Int 1
  ]

hasConditionals__tests = testGroup "hasConditionals" $ [
    testGroup "function calls" $ [
      testCase "with if" $
                let program = [RDecl Real "f" [] (RIte BTrue (Int 1) (Int 2))] in
                    hasConditionals True program (EFun "f" ResValue Real []) @?= True,
      testCase "without if" $
                let program = [RDecl Real "f" [] (Int 2)] in
                    hasConditionals True program (EFun "f" ResValue Real []) @?= False
    ],
      testCase "variable" $
            hasConditionals True [] (Var TInt "x") @?= False
   ] ++ map (\(i,o) -> testCase (show i) $ hasConditionals True [] i @?= o) testcasesHasConditionals

testcasesHasConditionals
    =[
    (Int 4, False),
    (Rat (toRational (0.1::Double)), False),
    (Var  TInt "x",False),
    (ArrayElem TInt (Just $ ArraySizeInt 3) "a" [(Int 2)], False),
    (BinaryOp AddOp (Int 1) (Int 2),False),
    (UnaryOp NegOp (Int 2), False),
    (Min [Int 3], False),
    (Max [Int 3], False),
    (RLet [(LetElem "x" Real (Rat $ toRational (0.2::Double)))] (Var Real "x"), False),
    (RLet [(LetElem "x" Real (RIte BTrue (Rat $ toRational (0.2::Double)) (Rat $ toRational (0.1::Double))))] (Var Real "x"), True),
    (RLet [(LetElem "x" Real (Rat $ toRational (0.2::Double)))] (RIte BTrue (Rat $ toRational (0.2::Double)) (Var Real "x")), True),
    (RIte BTrue (Int 1) (Int 2), True),
    (RListIte [(BTrue, Int 1)] (Int 2), True),
    (RForLoop Real (Int 1) (Int 3) (Int 0) "x" "acc" (Int 4), False)
    ]

hasConditionalsBExpr__tests = testGroup "hasConditionalsBExpr" [
    testCase "function call with if" $
      let program = [RDecl Real "f" [] (RIte BTrue (Int 1) (Int 2))] in
        hasConditionalsBExpr True program (RBExpr (Rel Lt (Int 2) (EFun "f" ResValue Real []))) @?= True,
    testCase "function call with no if" $
      let program = [RDecl Real "f" [] (Int 2)] in
        hasConditionalsBExpr True program (RBExpr (Rel Lt (Int 2) (EFun "f" ResValue Real []))) @?= False,
    testCase "predicate call with no if" $
      let program = [RPred "f" [] (RBExpr BTrue)] in
        hasConditionalsBExpr True program (RBExpr $ EPred "f" []) @?= False,
    testCase "predicate call with if" $
      let program = [RPred "f" [] (RBIte (Rel Lt (Int 2) (Var TInt "x")) (RBExpr BTrue) (RBExpr BFalse))] in
        hasConditionalsBExpr True program (RBExpr $ EPred "f" []) @?= True,
    testCase "conditional" $
        hasConditionalsBExpr True [] (RBIte BTrue (RBExpr BTrue) (RBExpr BTrue)) @?= True,
    testCase "conditional list" $
        hasConditionalsBExpr True [] (RBListIte [(BTrue, RBExpr BTrue)] (RBExpr BTrue)) @?= True,
    testCase "simple boolean expression" $
        hasConditionalsBExpr True  [] (RBExpr BTrue) @?= False,
    testCase "let in with conditional body" $
        hasConditionalsBExpr True [] (RBLet [LetElem "x" TInt (Int 2)] (RBListIte [(Rel Lt (Int 2) (Var TInt "x"), RBExpr BTrue)] (RBExpr BTrue))) @?= True,
    testCase "let in with conditional let element" $
        hasConditionalsBExpr True [] (RBLet [LetElem "x" TInt (RIte (Rel Lt (Int 2) (Int 6)) (Int 3) (Int 5))] (RBExpr $ Rel Eq (Int 4) (Var TInt "x"))) @?= True,
    testCase "let in with non conditional body" $
        hasConditionalsBExpr True [] (RBLet [LetElem "x" TInt (Int 2)] (RBExpr BTrue)) @?= False
   ]


localVars__tests   = testGroup "localVars" [
    testCase "int" $
      localVars (FInt 1) @?= [],
    testCase "var" $
      localVars (FVar TInt "x") @?= [],
    testCase "let 1" $
      localVars (Let [("y", TInt , FInt 3)] (FVar TInt "x")) @?= [("y", FInt 3)],
    testCase "let 2" $
      localVars (Let [("y", TInt , FInt 3),("z", TInt , FInt 1)] (FVar TInt "x")) @?= [("y", FInt 3),("z", FInt 1)],
    testCase "nested let" $
      localVars (Let [("y", TInt , FInt 3)] (Let [("z", TInt , FInt 1)] (FVar TInt "x"))) @?= [("y", FInt 3),("z", FInt 1)]
  ]

localVarsBExpr__tests   = testGroup "localVarsBExpr" [
    testCase "true" $
      localVarsBExpr FBTrue @?= [],
    testCase "false" $
      localVarsBExpr FBFalse @?= [],
    testCase "rel" $
      localVarsBExpr (FRel LtE (FVar TInt "x") (FVar TInt "y")) @?= [],
    testCase "let rel" $
      localVarsBExpr (FRel LtE (Let [("y", TInt , FInt 3)] (FVar TInt "x")) (FVar TInt "z")) @?= [("y",FInt 3)],
    testCase "let or" $
      localVarsBExpr (FOr FBTrue (FRel LtE (Let [("y", TInt , FInt 3)] (FVar TInt "x")) (FVar TInt "z"))) @?= [("y",FInt 3)],
    testCase "let and" $
      localVarsBExpr (FAnd (FRel LtE (Let [("a", TInt , FInt 1)] (FVar TInt "x")) (FVar TInt "z")) (FRel LtE (Let [("y", TInt , FInt 3)] (FVar TInt "x")) (FVar TInt "z"))) @?= [("a", FInt 1),("y",FInt 3)]
  ]



localVarsBExprStm__tests   = testGroup "localVarsBExprStm" [
    testCase "true" $
      localVarsBExprStm (BExpr FBTrue) @?= [],
    testCase "rel" $
      localVarsBExprStm (BExpr (FRel LtE (Let [("y", TInt , FInt 3)] (FVar TInt "x")) (FVar TInt "z"))) @?= [("y",FInt 3)],
    testCase "let 2" $
      localVarsBExprStm (BLet [("y", TInt , FInt 3),("z", TInt , FInt 1)] (BExpr FBTrue)) @?= [("y", FInt 3),("z", FInt 1)],
    testCase "nested let" $
      localVarsBExprStm (BLet [("y", TInt , FInt 3)] (BLet [("z", TInt , FInt 1)] (BExpr FBTrue))) @?= [("y", FInt 3),("z", FInt 1)],
    testCase "nested let 2" $
      localVarsBExprStm (BLet [("y", TInt , Let [("a", TInt , FInt 3)] (FVar TInt "x"))] (BLet [("z", TInt , FInt 1)] (BExpr FBTrue))) @?= [("a", FInt 3),("y", Let [("a", TInt , FInt 3)] (FVar TInt "x")),("z", FInt 1)],
    testCase "nested let 3" $
      localVarsBExprStm (BLet [("y", TInt , Let [("a", TInt , FInt 3)] (FVar TInt "x")),("c", TInt, FInt 5) ] (BLet [("z", TInt , FInt 1)] (BExpr FBTrue))) @?= [("a", FInt 3),("y", Let [("a", TInt , FInt 3)] (FVar TInt "x")),("c", FInt 5),("z", FInt 1)]
  ]

forIndexes__tests  = testGroup "forIndexes" []
funCallList__tests = testGroup "funCallList" []

makeFPDeclRecursive__tests = testGroup "makeFPDeclRecursive"
  [makeFPDeclRecursive__test1
  ,makeFPDeclRecursive__test2]

makeFPDeclRecursive__test1 = testCase "makeFPDeclRecursive1" $
  makeFPDeclRecursive (Decl False TInt "f" [] (FInt 2))
  @?=
  (Decl False TInt "f" [] (FInt 2), [])

makeFPDeclRecursive__test2 = testCase "makeFPDeclRecursive2" $
  makeFPDeclRecursive (Decl False TInt "f" [] (ForLoop TInt (FInt 0) (FInt 10) (FInt 0) "I" "X"
                                        (BinaryFPOp AddOp TInt (FVar TInt "I") (FVar TInt "X"))))
  @?=
  (Decl False TInt "f" [] (FEFun False "for_f1" ResValue TInt [FInt 0,FInt 0])
  ,
  [Decl False TInt "for_f1" [Arg "I" TInt,Arg "X" TInt] (Ite (FRel Eq (FVar TInt "I") (FInt 10)) (FVar TInt "X")
          (FEFun False "for_f1" ResValue TInt [BinaryFPOp AddOp TInt (FVar TInt "I") (FInt 1)
                               ,BinaryFPOp AddOp TInt (FVar TInt "I") (FVar TInt "X")]))])


isFBExprEquivFalse__tests = testGroup "isFBExprEquivFalse"
  [testCase "FBFalse is quivalent to False" $
    isFBExprEquivFalse FBFalse @?= True
  ,testCase "FBTrue is not quivalent to False" $
    isFBExprEquivFalse FBTrue @?= False
  ,testCase "FBFalse and FBTrue is not quivalent to False" $
    isFBExprEquivFalse (FAnd FBFalse FBTrue) @?= True
  ,testCase "FBFalse or FBFalse is quivalent to False" $
    isFBExprEquivFalse (FOr FBFalse FBFalse) @?= True
  ,testCase "FBFalse and FBTrue is not quivalent to False" $
    isFBExprEquivFalse (FOr FBFalse FBTrue) @?= False
  ]

isBExprEquivFalse__tests = testGroup "isBExprEquivFalse"
  [testCase "BFalse is quivalent to False" $
    isBExprEquivFalse BFalse @?= True
  ,testCase "BTrue is not quivalent to False" $
    isBExprEquivFalse BTrue @?= False
  ,testCase "BTrue is not quivalent to False" $
    isBExprEquivFalse (And BFalse BTrue) @?= True
  ,testCase "BFalse or BFalse is quivalent to False" $
    isBExprEquivFalse (Or BFalse BFalse) @?= True
  ,testCase "BFalse and BTrue is not quivalent to False" $
    isBExprEquivFalse (Or BFalse BTrue) @?= False
  ]

rewriteEquivEExpr__tests = testGroup "rewriteEquivEExpr tests" $  map (\(msg, i, o) -> testCase msg (rewriteEquivEExpr i @?= o)) rewriteEquivEExpr_testsIOs

simplFAExpr__tests = testGroup "simplFAExpr tests" $
  [simplFAExpr__test1
  ,simplFAExpr__test2
  ,simplFAExpr__test3
  ,simplFAExpr__test4
  ,simplFAExpr__test5
  ]

simplFAExpr__test1 = testCase "" $
    simplFAExpr (BinaryFPOp AddOp  FPSingle (FInt 4) (FInt 0)) @?= FInt 4

simplFAExpr__test2 = testCase "" $
    simplFAExpr (BinaryFPOp SubOp  FPSingle (FInt 0) (FInt 4)) @?= FInt (-4)

simplFAExpr__test3 = testCase "" $
    simplFAExpr (BinaryFPOp MulOp  FPSingle (FInt 4) (FInt 0)) @?= FInt 0

simplFAExpr__test4 = testCase "" $
    simplFAExpr (BinaryFPOp AddOp FPSingle (FInt 4) (BinaryFPOp MulOp  FPSingle (FInt 3) (FInt 0))) @?= FInt 4

simplFAExpr__test5 = testCase "" $
    simplFAExpr (BinaryFPOp MulOp  FPSingle (FInt 5) (FInt 1)) @?= FInt 5

rewriteEquivEExpr_testsIOs =
           [("ErrMulPow2L"
                ,ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble))
                ,ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)
            )
            ,("ErrMulPow2R"
                ,ErrMulPow2R FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue  FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble))
                ,ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)
            )
            ,("ErrAbs"
                ,ErrUnOp AbsOp FPDouble (Int 3) (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X"  ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble))
                ,ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)
            )
            ,("ErrAbs"
                ,ErrUnOp NegOp FPDouble (Int 3) (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble))
                ,ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)
            )
            ,("MaxErr"
                ,MaxErr [ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble),
                         ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)]
                ,ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)
            )
            ,("MaxErr_ErrNeg"
                ,MaxErr [ErrUnOp NegOp FPDouble (Int 3) (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)),
                         ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)]
                ,ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)
            )
            ,("ErrAdd_ErrMulPow2L"
                ,ErrBinOp AddOp FPDouble (RealMark "Z" ResValue) (ErrMulPow2L FPDouble 2 (ErrBinOp SubOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y"  ResValue FPDouble)))
                                 (RealMark "K" ResValue) (ErrorMark "K" ResValue FPDouble)
                ,ErrBinOp AddOp FPDouble (RealMark "Z" ResValue) (ErrBinOp SubOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)) (RealMark "K" ResValue) (ErrorMark "K" ResValue FPDouble)
            )
            ,("ErrAdd_ErrMulPow2L"
                ,ErrBinOp AddOp FPDouble  (RealMark "Z" ResValue) (ErrMulPow2L FPDouble 2 (ErrBinOp SubOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble))) (RealMark "K" ResValue) (ErrorMark "K" ResValue FPDouble)
                ,ErrBinOp AddOp FPDouble (RealMark "Z" ResValue) (ErrBinOp SubOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)) (RealMark "K" ResValue) (ErrorMark "K" ResValue FPDouble)
            )
            ,("ErrSub_ErrMulPow2L"
                ,ErrBinOp SubOp FPDouble (RealMark "Z" ResValue) (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)))
                                 (RealMark "K" ResValue) (ErrorMark "K" ResValue FPDouble)
                ,ErrBinOp SubOp FPDouble (RealMark "Z" ResValue) (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)) (RealMark "K" ResValue) (ErrorMark "K" ResValue FPDouble)
            )
            ,("ErrMul_ErrMulPow2L"
                ,ErrBinOp MulOp FPDouble (RealMark "Z" ResValue) (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)))
                                 (RealMark "K" ResValue) (ErrorMark "K" ResValue FPDouble)
                ,ErrBinOp MulOp FPDouble (RealMark "Z" ResValue) (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)) (RealMark "K" ResValue) (ErrorMark "K" ResValue FPDouble)
            )
            ,("ErrDiv_ErrMulPow2L"
                ,ErrBinOp DivOp FPDouble  (RealMark "Z" ResValue) (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)))
                                                 (RealMark "K" ResValue) (ErrorMark "K" ResValue FPDouble)
                ,ErrBinOp DivOp FPDouble  (RealMark "Z" ResValue) (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble))  (RealMark "K" ResValue) (ErrorMark "K" ResValue FPDouble)
            )
            ,("ErrFloor_ErrMulPow2L"
                ,ErrUnOp FloorOp FPDouble  (RealMark "Z" ResValue) (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)))
                ,ErrUnOp FloorOp FPDouble  (RealMark "Z" ResValue) (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble))
            )
            ,("ErrFloor0_ErrMulPow2L"
                ,ErrFloorNoRound FPDouble  (RealMark "Z" ResValue) (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)))
                ,ErrFloorNoRound FPDouble  (RealMark "Z" ResValue) (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble))
            )
            ,("ErrSqrt_ErrMulPow2L"
                ,ErrUnOp SqrtOp FPDouble  (RealMark "Z" ResValue) (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)))
                ,ErrUnOp SqrtOp FPDouble  (RealMark "Z" ResValue) (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble))
            )
            ,("ErrSin_ErrMulPow2L"
                ,ErrUnOp SinOp FPDouble  (RealMark "Z" ResValue) (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)))
                ,ErrUnOp SinOp FPDouble  (RealMark "Z" ResValue) (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble))
            )
            ,("ErrCos_ErrMulPow2L"
                ,ErrUnOp CosOp FPDouble  (RealMark "Z" ResValue) (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)))
                ,ErrUnOp CosOp FPDouble  (RealMark "Z" ResValue) (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble))
            )
            ,("ErrTan_ErrMulPow2L"
                ,ErrUnOp TanOp FPDouble  (RealMark "Z" ResValue) (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)))
                ,ErrUnOp TanOp FPDouble  (RealMark "Z" ResValue) (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble))
            )
            ,("ErrAsin_ErrMulPow2L"
                ,ErrUnOp AsinOp FPDouble  (RealMark "Z" ResValue) (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)))
                ,ErrUnOp AsinOp FPDouble  (RealMark "Z" ResValue) (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble))
            )
            ,("ErrAcos_ErrMulPow2L"
                ,ErrUnOp AcosOp FPDouble  (RealMark "Z" ResValue) (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)))
                ,ErrUnOp AcosOp FPDouble  (RealMark "Z" ResValue) (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble))
            )
            ,("ErrAtan_ErrMulPow2L"
                ,ErrUnOp AtanOp FPDouble  (RealMark "Z" ResValue) (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)))
                ,ErrUnOp AtanOp FPDouble  (RealMark "Z" ResValue) (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)))
            ,("ErrAtanT_ErrMulPow2L"
                ,ErrUnOp AtanOp FPDouble  (RealMark "Z" ResValue) (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)))
                ,ErrUnOp AtanOp FPDouble  (RealMark "Z" ResValue) (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble))
            )
            ,("ErrLn_ErrMulPow2L"
                ,ErrUnOp LnOp FPDouble (RealMark "Z" ResValue) (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)))
                ,ErrUnOp LnOp FPDouble (RealMark "Z" ResValue) (ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble))
            )
            ,("ErrExpo_ErrMulPow2L"
                ,ErrUnOp ExpoOp FPSingle  (RealMark "Z" ResValue) (ErrMulPow2L FPSingle 2 (ErrBinOp AddOp FPSingle (RealMark "X" ResValue) (ErrorMark "X" ResValue FPSingle) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPSingle)))
                ,ErrUnOp ExpoOp FPSingle (RealMark "Z" ResValue) (ErrBinOp AddOp FPSingle (RealMark "X" ResValue) (ErrorMark "X"  ResValue FPSingle) (RealMark "Y"ResValue ) (ErrorMark "Y" ResValue FPSingle))
            )
            ]


equivEExpr__tests = testGroup "equivEExpr tests" $  map (\(msg, ee1, ee2, o) -> testCase msg (equivEExpr ee1 ee2 @?= o)) equivEExpr_testsIOs

equivEExpr_testsIOs =
            [("ErrMulPow2L_Add"
                ,ErrMulPow2L FPSingle 2 (ErrBinOp AddOp FPSingle (RealMark "X" ResValue) (ErrorMark "X" ResValue FPSingle)  (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPSingle))
                ,ErrBinOp AddOp FPSingle (RealMark "X" ResValue) (ErrorMark "X" ResValue FPSingle) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPSingle)
                ,True
            )
            ,("ErrMul_Add"
                ,ErrBinOp MulOp FPSingle (RealMark "X" ResValue) (ErrorMark "X" ResValue FPSingle) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPSingle)
                ,ErrBinOp AddOp FPSingle (RealMark "X" ResValue) (ErrorMark "X" ResValue FPSingle) (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPSingle)
                ,False
            )
            ]

replaceVarWithAExpr__tests = testGroup "replaceVarWithAExpr tests"
  [replaceVarWithAExpr__test1
  ,replaceVarWithAExpr__test2
  ,replaceVarWithAExpr__test3
  ,replaceVarWithAExpr__test4
  ]

replaceVarWithAExpr__test1 = testCase "replaceVar of x with 4 in x is Just 4" $
    replaceVarWithAExpr [("x",Int 4)] (Var Real "x") @?= Just (Int 4)

replaceVarWithAExpr__test2 = testCase "replaceVar of x with 4 in y+x is Nothing" $
    replaceVarWithAExpr [("x",Int 4)] (BinaryOp AddOp (Var Real "y") (Var Real "x")) @?= Nothing

replaceVarWithAExpr__test3 = testCase "replace nothing in y + x = Nothing" $
    replaceVarWithAExpr [] (BinaryOp AddOp (Var Real "y") (Var Real "x")) @?= Nothing

replaceVarWithAExpr__test4 = testCase "replaceVar of x with 4 and y with 6 in x = Just 4" $
    replaceVarWithAExpr [("x",Int 4),("y",Int 6)] (Var Real "x") @?= Just (Int 4)


substituteInAExpr__tests = testGroup "replaceVarWithAExpr tests"
  [substituteInAExpr__test1
  ,substituteInAExpr__test2
  ,substituteInAExpr__test3
  ,substituteInBExpr__test4
  ,substituteInAExpr__test5
  ,substituteInAExpr__test6
  ,substituteInAExpr__test7
  ,substituteInAExpr__test8
  ,substituteInAExpr__test9
  ]

substituteInAExpr__test1 = testCase "replaceVar of x with 4 in x is Just 4" $
    substituteInAExpr [("x",Int 4)] (Var Real "x") @?= Int 4

substituteInAExpr__test2 = testCase "replaceVar of x with 4 in y+x is Nothing" $
    substituteInAExpr [("x",Int 4)] (BinaryOp AddOp (Var Real "y") (Var Real "x")) @?= BinaryOp AddOp (Var Real "y") (Int 4)

substituteInAExpr__test3 = testCase "replace nothing in y + x = Nothing" $
    substituteInAExpr [] (BinaryOp AddOp (Var Real "y") (Var Real "x")) @?= BinaryOp AddOp (Var Real "y") (Var Real "x")

substituteInAExpr__test4 = testCase "replaceVar of x with 4 and y with 6 in x = Just 4" $
    substituteInAExpr [("x",Int 4),("y",Int 6)] (Var Real "x") @?= Int 4

substituteInAExpr__test5 = testCase "replace nothing in y + x = Nothing" $
    substituteInAExpr [("x",Int 4),("y",Int 6)] (BinaryOp AddOp (Var Real "y") (Var Real "x")) @?= BinaryOp AddOp (Int 6) (Int 4)

substituteInAExpr__test6 = testCase "replaceVar of x with 4 and y with 6 in Abs(x) = Just 4" $
    substituteInAExpr [("x",Int 4),("y",Int 6)] (UnaryOp AbsOp (Var Real "x")) @?= UnaryOp AbsOp (Int 4)

substituteInAExpr__test7 = testCase "replaceVar of x with 4 and y with 6 in (let z = x in x) is (let z=4 in 4)" $
    substituteInAExpr [("x",Int 4),("y",Int 6)]
    (RLet [LetElem{letVar = "z",letType = TInt, letExpr = Var Real "x"}] (Var Real "x"))
    @?=
    RLet [LetElem{letVar = "z", letType = TInt,letExpr = Int 4}] (Int 4)

substituteInAExpr__test8 = testCase "replaceVar of x with 4 and y with 6 in (if true then x else y) is (if true then 4 else 6)" $
    substituteInAExpr [("x",Int 4),("y",Int 6)] (RIte BTrue (Var Real "x") (Var Real "y")) @?= RIte BTrue (Int 4) (Int 6)

substituteInAExpr__test9 = testCase "replaceVar of x with 4 and y with 6 in let z = x in x" $
    substituteInAExpr [("x",Int 4),("y",Int 6)] (RListIte [(BTrue, Var Real "x")] (Var Real "y")) @?= RListIte [(BTrue, Int 4)] (Int 6)


substituteInBExpr__tests = testGroup "replaceVarWithAExpr tests"
  [substituteInBExpr__test1
  ,substituteInBExpr__test2
  ,substituteInBExpr__test3
  ,substituteInBExpr__test4
  ,substituteInBExpr__test5
  ]

substituteInBExpr__test1 = testCase "replaceVar of x with 4 in x is Just 4" $
    substituteInBExpr [("x",Int 4)] (Rel Lt (Var Real "x") (Var Real "y"))
    @?=
    Rel Lt (Int 4) (Var Real "y")

substituteInBExpr__test2 = testCase "replaceVar of x with 4 in y+x is Nothing" $
    substituteInBExpr [("x",Int 4)] (Rel Gt (BinaryOp AddOp (Var Real "y") (Var Real "x")) (Int 6))
    @?=
    Rel Gt (BinaryOp AddOp (Var Real "y") (Int 4)) (Int 6)

substituteInBExpr__test3 = testCase "replace nothing in y + x = Nothing" $
    substituteInBExpr [] (Rel Eq (BinaryOp AddOp (Var Real "y") (Var Real "x")) (Int 6))
    @?=
    Rel Eq (BinaryOp AddOp (Var Real "y") (Var Real "x")) (Int 6)

substituteInBExpr__test4 = testCase "replaceVar of x with 4 and y with 6 in x = Just 4" $
    substituteInBExpr [("x",Int 4),("y",Int 6)] (Rel LtE (Var Real "x") (Int 0))
    @?=
    Rel LtE (Int 4) (Int 0)

substituteInBExpr__test5 = testCase "replace nothing in y + x = Nothing" $
    substituteInBExpr [("x",Int 4),("y",Int 6)] (Rel GtE (Var Real "y") (BinaryOp AddOp (Var Real "y") (Var Real "x")))
    @?=
    Rel GtE (Int 6) (BinaryOp AddOp (Int 6) (Int 4))


initErrorMark__tests = testGroup "initErrorMark tests"
  [initErrorMark__test1
  ,initErrorMark__test2
  ,initErrorMark__test3
  ,initErrorMark__test4
  ]

initErrorMark__test1 = testCase "initErrorMark of E(x_int) is 0" $
    initErrorMark (ErrorMark "x" ResValue TInt) @?= Just (Int 0)

initErrorMark__test2 = testCase "initErrorMark of E(x_double) is half ulp R(x)" $
    initErrorMark (ErrorMark "x" ResValue FPDouble)  @?= Just (HalfUlp (RealMark "x" ResValue) FPDouble)

initErrorMark__test3 = testCase "initErrorMark of y+x is Nothing" $
    initErrorMark (BinaryOp AddOp (Var Real "y") (Var Real "x")) @?= Nothing

initErrorMark__test4 = testCase "initErrorMark of x is Nothing" $
    initErrorMark (Var Real "x") @?= Nothing


initAExpr__tests = testGroup "initAExpr tests"
  [initAExpr__test1
  ,initAExpr__test2
  ,initAExpr__test3
  ,initAExpr__test4
  ,initAExpr__test5
  ]

initAExpr__test1 = testCase "initAExpr of E(x_int) is 0" $
    initAExpr (ErrorMark "x" ResValue TInt) @?= (Int 0)

initAExpr__test2 = testCase "initAExpr of E(x_double) is half_ulp(R(x))" $
    initAExpr (ErrorMark "x" ResValue FPDouble)  @?= (HalfUlp (RealMark "x" ResValue) FPDouble)

initAExpr__test3 = testCase "initAExpr of y+x is y+x" $
    initAExpr (BinaryOp AddOp (Var Real "y") (Var Real "x")) @?= (BinaryOp AddOp (Var Real "y") (Var Real "x"))

initAExpr__test4 = testCase "initAExpr of E(x_int)+x is half_ulp(R(x))+x" $
    initAExpr (BinaryOp AddOp (ErrorMark "x" ResValue FPDouble) (Var Real "x")) @?= (BinaryOp AddOp (HalfUlp (RealMark "x" ResValue) FPDouble) (Var Real "x"))

initAExpr__test5 = testCase "initAExpr of StoR(RtoS(E(x)) is StoR(RtoS(half_ulp(R(x)))" $
    initAExpr (FromFloat FPSingle (ToFloat FPSingle(ErrorMark "x" ResValue FPDouble))) @?= FromFloat FPSingle (ToFloat FPSingle(HalfUlp (RealMark "x" ResValue) FPDouble))


initBExpr__tests = testGroup "initBExpr tests"
  [initBExpr__test1
  ,initBExpr__test2
  ,initBExpr__test3
  ,initBExpr__test4
  ]

initBExpr__test1 = testCase "initBExpr of E(x_int) < 6 is 0 < 6" $
    initBExpr (Rel Lt (ErrorMark "x" ResValue TInt) (Int 6)) @?= Rel Lt (Int 0) (Int 6)

initBExpr__test2 = testCase "initBExpr of E(x_double) < 5 is half ulp R(x) < 5" $
    initBExpr (Rel Gt (ErrorMark "x" ResValue FPDouble) (Int 5))  @?= Rel Gt (HalfUlp (RealMark "x" ResValue) FPDouble) (Int 5)

initBExpr__test3 = testCase "initBExpr of y+x < 9 and E(x_double) < 5 is y+x and half ulp R(x) < 5" $
    initBExpr (And (Rel Lt (BinaryOp AddOp (Var Real "y") (Var Real "x")) (Int 9)) (Rel Gt (ErrorMark "x" ResValue FPDouble) (Int 5)))
          @?= (And (Rel Lt (BinaryOp AddOp (Var Real "y") (Var Real "x")) (Int 9)) (Rel Gt (HalfUlp (RealMark "x" ResValue) FPDouble) (Int 5)))

initBExpr__test4 = testCase "initBExpr of E(x_int) is 0" $
    initBExpr BTrue @?= BTrue

funCallListFAExpr__tests = testGroup "funCallListFAExpr tests"
  [funCallListFAExpr__test1
  ,funCallListFAExpr__test2
  ,funCallListFAExpr__test3
  ,funCallListFAExpr__test4
  ,funCallListFAExpr__test5
  ,funCallListFAExpr__test6
  ,funCallListFAExpr__test7
  ]

funCallListFAExpr__test1 = testCase "funCallList of 8 is []" $
  funCallListFAExpr (FInt 8) @?= []

funCallListFAExpr__test2 = testCase "funCallList of f() is [f()]" $
  funCallListFAExpr (FEFun False "f" ResValue FPDouble []) @?= [FEFun False "f" ResValue FPDouble []]

funCallListFAExpr__test3 = testCase "funCallList of f()+g(5) is [f(),g(5)]" $
  funCallListFAExpr (BinaryFPOp AddOp FPDouble (FEFun False "f" ResValue FPDouble []) (FEFun False "g" ResValue FPDouble [FInt 5]))
  @?=
  [FEFun False "f" ResValue FPDouble [],FEFun False "g" ResValue FPDouble [FInt 5]]

funCallListFAExpr__test4 = testCase "funCallList of f()+g(5) is [f(),g(5)]" $
  funCallListFAExpr (BinaryFPOp AddOp FPDouble (FEFun False "f" ResValue FPDouble []) (FEFun False "g" ResValue FPDouble [FInt 5]))
  @?=
  [FEFun False "f" ResValue FPDouble [],FEFun False "g" ResValue FPDouble [FInt 5]]

funCallListFAExpr__test5 = testCase "funCallList of f(g(5)) is [f(g(5)), g(5)]" $
  funCallListFAExpr (FEFun False "f" ResValue FPDouble [FEFun False "g" ResValue FPDouble [FInt 5]])
  @?=
  [FEFun False "f" ResValue FPDouble [FEFun False "g" ResValue FPDouble [FInt 5]]
  ,FEFun False "g" ResValue FPDouble [FInt 5]]

funCallListFAExpr__test6 = testCase "funCallList of 0 is []" $
  funCallListFAExpr (ToFloat FPDouble (Int 9)) @?= []

funCallListFAExpr__test7 = testCase "funCallList of if(f(X)<0) then 1 else 2 is [f(X)]" $
  funCallListFAExpr (Ite (FRel Lt (FEFun False "f" ResValue FPDouble []) (FInt 0)) (FInt 1) (FInt 2))
  @?=
  [FEFun False "f" ResValue FPDouble []]

funCallListFBExpr__tests = testGroup "funCallListFBExpr tests"
  [funCallListFBExpr__test1
  ,funCallListFBExpr__test2
  ,funCallListFBExpr__test3
  ]

funCallListFBExpr__test1 = testCase "funCallList of f()<8 is [f()]" $
    funCallListFBExpr (FRel Lt (FEFun False "f" ResValue FPDouble []) (FInt 8))
    @?=
    [FEFun False "f" ResValue FPDouble []]

funCallListFBExpr__test2 = testCase "funCallList of f()>g(5) is [f(),g(5)]" $
    funCallListFBExpr (FRel Gt (FEFun False "f" ResValue FPDouble [])
                               (FEFun False "g" ResValue FPDouble [FInt 5]))
    @?=
    [FEFun False "f" ResValue FPDouble [],FEFun False "g" ResValue FPDouble [FInt 5]]

funCallListFBExpr__test3 = testCase "funCallList of f()<8 and f()>g(5) is [f(),g(5)]" $
    funCallListFBExpr (FAnd (FRel Lt (FEFun False "f" ResValue FPDouble []) (FInt 8))
                            (FRel Gt (FEFun False "f" ResValue FPDouble []) (FEFun False "g" ResValue FPDouble [FInt 5])))
    @?=
    [FEFun False "f" ResValue FPDouble [],FEFun False "g" ResValue FPDouble [FInt 5]]


funCallListAExpr__tests = testGroup "funCallListAExpr tests"
  [funCallListAExpr__test1
  ,funCallListAExpr__test2
  ,funCallListAExpr__test3
  ,funCallListAExpr__test4
  ,funCallListAExpr__test5
  ,funCallListAExpr__test6
  ]

funCallListAExpr__test1 = testCase "funCallList of 8 is []" $
    funCallListAExpr (Int 8) @?= []

funCallListAExpr__test2 = testCase "funCallList of f() is [f()]" $
    funCallListAExpr (EFun "f" ResValue Real []) @?= [EFun "f" ResValue Real []]

funCallListAExpr__test3 = testCase "funCallList of f()+g(5) is [f(),g(5)]" $
    funCallListAExpr (BinaryOp AddOp (EFun "f" ResValue Real []) (EFun "g" ResValue Real [Int 5]))
    @?=
    [EFun "f" ResValue Real [],EFun "g" ResValue Real [Int 5]]

funCallListAExpr__test4 = testCase "funCallList of f()+g(5) is [f(),g(5)]" $
    funCallListAExpr (BinaryOp AddOp (EFun "f" ResValue Real []) (EFun "g" ResValue Real [Int 5]))
    @?=
    [EFun "f" ResValue Real [],EFun "g" ResValue Real [Int 5]]

funCallListAExpr__test5 = testCase "funCallList of f(g(5)) is [f(g(5)), g(5)]" $
    funCallListAExpr (EFun "f" ResValue Real [EFun "g" ResValue Real [Int 5]])
    @?=
    [EFun "f" ResValue Real [EFun "g" ResValue Real [Int 5]],EFun "g" ResValue Real [Int 5]]

funCallListAExpr__test6 = testCase "funCallList of f(g(5)) is [f(g(5)), g(5)]" $
    funCallListAExpr (EFun "f" ResValue Real [EFun "g" ResValue Real [EFun "h" ResValue Real [Int 7]]])
    @?=
    [EFun "f" ResValue Real [EFun "g" ResValue Real [EFun "h" ResValue Real [Int 7]]]
    ,EFun "g" ResValue Real [EFun "h" ResValue Real [Int 7]]
    ,EFun "h" ResValue Real [Int 7]]

funCallListBExpr__tests = testGroup "funCallListBExpr tests"
  [funCallListBExpr__test1
  ,funCallListBExpr__test2
  ,funCallListBExpr__test3
  ]

funCallListBExpr__test1 = testCase "funCallList of f()<8 is [f()]" $
    funCallListBExpr (Rel Lt (EFun "f" ResValue Real []) (Int 8)) @?= [EFun "f" ResValue Real []]

funCallListBExpr__test2 = testCase "funCallList of f()>g(5) is [f(),g(5)]" $
    funCallListBExpr (Rel Gt (EFun "f" ResValue Real []) (EFun "g" ResValue Real [Int 5]))
    @?=
    [EFun "f" ResValue Real [],EFun "g" ResValue Real [Int 5]]

funCallListBExpr__test3 = testCase "funCallList of f()<8 and f()>g(5) is [f(),g(5)]" $
    funCallListBExpr (And (Rel Lt (EFun "f" ResValue Real []) (Int 8)) (Rel Gt (EFun "f" ResValue Real []) (EFun "g" ResValue Real [Int 5])))
    @?=
    [EFun "f" ResValue Real [],EFun "g" ResValue Real [Int 5]]



noRoundOffErrorInAExpr__tests = testGroup "noRoundOffErrorInAExpr tests"
  [noRoundOffErrorInAExpr__test1
  ,noRoundOffErrorInAExpr__test2
  ,noRoundOffErrorInAExpr__test3
  ,noRoundOffErrorInAExpr__test4
  ,noRoundOffErrorInAExpr__test5
  ,noRoundOffErrorInAExpr__test6
  ,noRoundOffErrorInAExpr__test7
  ,noRoundOffErrorInAExpr__test8
  ,noRoundOffErrorInAExpr__test9
  ,noRoundOffErrorInAExpr__test10
  ,noRoundOffErrorInAExpr__test11
  ,noRoundOffErrorInAExpr__test12
  ,noRoundOffErrorInAExpr__test13
  ]

noRoundOffErrorInAExpr__test1 = testCase "9 has no round-off error" $
    noRoundOffErrorInAExpr (FInt 8) @?= True

noRoundOffErrorInAExpr__test2 = testCase "0.1 has round-off error" $
    noRoundOffErrorInAExpr (FCnst FPDouble (fromDouble2Rat 0.1)) @?= False

noRoundOffErrorInAExpr__test3 = testCase "4+0.1 has round-off error" $
    noRoundOffErrorInAExpr (BinaryFPOp AddOp FPDouble (FInt 4) (FCnst FPDouble (fromDouble2Rat 0.1))) @?= False

noRoundOffErrorInAExpr__test4 = testCase "4+6 has no round-off error" $
    noRoundOffErrorInAExpr (BinaryFPOp AddOp FPDouble (FInt 4) (FInt 6)) @?= True

noRoundOffErrorInAExpr__test5 = testCase "4+int_x has no round-off error" $
    noRoundOffErrorInAExpr (BinaryFPOp SubOp FPDouble (FInt 4) (FVar TInt "x")) @?= True

noRoundOffErrorInAExpr__test6 = testCase "4+x has round-off error" $
    noRoundOffErrorInAExpr (BinaryFPOp MulOp FPDouble (FInt 4) (FVar FPDouble "x")) @?= False

noRoundOffErrorInAExpr__test7 = testCase "RtoD(4)-int_x has no round-off error" $
    noRoundOffErrorInAExpr (BinaryFPOp SubOp FPDouble (ToFloat FPDouble (Int 4)) (FVar TInt "x")) @?= True

noRoundOffErrorInAExpr__test8 = testCase "RtoS(4)-int_x has no round-off error" $
    noRoundOffErrorInAExpr (BinaryFPOp SubOp FPDouble (ToFloat FPSingle (Int 4)) (FVar TInt "x")) @?= True

noRoundOffErrorInAExpr__test9 = testCase "StoD(4) has no round-off error" $
    noRoundOffErrorInAExpr (TypeCast FPSingle FPDouble (FInt 4)) @?= True

noRoundOffErrorInAExpr__test10 = testCase "StoD(1) has no round-off error" $
    noRoundOffErrorInAExpr (TypeCast FPSingle FPDouble (FCnst FPDouble (fromDouble2Rat 1))) @?= True

noRoundOffErrorInAExpr__test11 = testCase "StoD(0.1) has round-off error" $
    noRoundOffErrorInAExpr (TypeCast FPSingle FPDouble (FCnst FPDouble (fromDouble2Rat 0.1))) @?= False

noRoundOffErrorInAExpr__test12 = testCase "int fun with no args has no round-off error" $
    noRoundOffErrorInAExpr (FEFun False "f" ResValue TInt []) @?= True

noRoundOffErrorInAExpr__test13 = testCase "int fun has no round-off error" $
    noRoundOffErrorInAExpr (FEFun False "f" ResValue TInt [FCnst FPDouble (fromDouble2Rat 0.1)]) @?= True

varList__tests = testGroup "varList tests"
  [varList__test1
  ,varList__test2
  ,varList__test3
  ,varList__test4
  ,varList__test5
  ,varList__test6
  ,varList__test7
  ,varList__test8
  ,varList__test9
  ,varList__test10
  ,varList__test11
  ,varList__test12
  ]

varList__test1 = testCase "varList of constant 0.1 is []" $
    varList (FCnst FPDouble (fromDouble2Rat 0.1)) @?= []

varList__test2 = testCase "varList of variable x is [Var x]" $
    varList (FVar FPDouble "x") @?= [FVar FPDouble "x"]

varList__test3 = testCase "varList of (0.1 + x) is [Var x]" $
    varList (BinaryFPOp AddOp FPDouble (FCnst FPDouble (fromDouble2Rat 0.1)) (FVar FPDouble "x"))
    @?=
    [FVar FPDouble "x"]

varList__test4 = testCase "varList of (y * x) is [Var x, Var y]" $
    varList (BinaryFPOp MulOp FPDouble (FVar FPDouble "y") (FVar FPDouble "x"))
    @?=
    [FVar FPDouble "y", FVar FPDouble "x"]

varList__test5 = testCase "varList of (0.1 + x)*x is [Var x]" $
    varList (BinaryFPOp MulOp FPDouble (BinaryFPOp AddOp FPDouble (FCnst FPDouble (fromDouble2Rat 0.1)) (FVar FPDouble "x")) (FVar FPDouble "x"))
    @?= [FVar FPDouble "x"]

varList__test6 = testCase "varList of min[(0.1 + x)*x, y, z] is [Var x, Var y, Var z]" $
    varList (FMin [(BinaryFPOp MulOp FPDouble (BinaryFPOp AddOp FPDouble (FCnst FPDouble (fromDouble2Rat 0.1))
                   (FVar FPDouble "x")) (FVar FPDouble "x"))
                   ,FVar FPDouble "y"
                   ,FVar FPDouble "z"])
    @?= [FVar FPDouble "x", FVar FPDouble "y", FVar FPDouble "z"]

varList__test7 = testCase "varList of  floor(x)/f(y,z) is [Var x, Var y, Var z]" $
    varList (BinaryFPOp DivOp FPDouble (UnaryFPOp FloorOp FPDouble (FVar FPDouble "x"))
                                       (FEFun False "f" ResValue FPDouble [(FVar FPDouble "x"),(FVar FPDouble "z")]))
    @?=
    [FVar FPDouble "x", FVar FPDouble "z"]

varList__test8 = testCase "varList of a RtoD(6) is []" $
    varList (ToFloat FPDouble (Int 6)) @?= []

varList__test9 = testCase "varList of a RtoD(6) is []" $
    varList (ToFloat FPDouble (FromFloat FPDouble(ToFloat FPDouble (Int 6)))) @?= []

varList__test10 = testCase "varList of a RtoD(DtoR(x)) is [x]" $
    varList (ToFloat FPDouble(Var Real "x")) @?= []

varList__test11 = testCase "varList of a RtoD(DtoR(x)) is [x]" $
    varList (ToFloat FPDouble(FromFloat FPDouble(FVar FPDouble "x"))) @?= [FVar FPDouble "x"]

varList__test12 = testCase "varList of a RtoD(DtoR(x + y)) is [x,y]" $
    varList (ToFloat FPDouble(FromFloat FPDouble(BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FVar FPDouble "y")))) @?= [FVar FPDouble "x",FVar FPDouble "y"]


equivModuloIndex__tests = testGroup "equivModuloIndex tests"
  [equivModuloIndex__test1
  ,equivModuloIndex__test2
  ,equivModuloIndex__test3
  ,equivModuloIndex__test4
  ,equivModuloIndex__test5
  ,equivModuloIndex__test6
  ,equivModuloIndex__test7
  ,equivModuloIndex__test8
  ,equivModuloIndex__test9
  ]

equivModuloIndex__test1 = testCase "v(3) size 8 double not equiv v(4) size 5 double" $
   equivModuloIndex (FArrayElem FPDouble (Just $ ArraySizeInt 8) "v" [(FInt 3)])
                    (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" [(FInt 4)])
                    @?=
                    False

equivModuloIndex__test2 = testCase "" $
   equivModuloIndex (FArrayElem FPSingle (Just $ ArraySizeInt 8) "v" [(FInt 3)])
                    (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" [(FInt 4)])
                    @?=
                    False

equivModuloIndex__test3 = testCase "v(3) equiv v(4)" $
   equivModuloIndex (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" [(FInt 3)])
                    (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" [(FInt 4)])
                    @?=
                    True

equivModuloIndex__test4 = testCase "sin(v(3)) equiv sin(v(4))" $
   equivModuloIndex (UnaryFPOp SinOp FPDouble (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" [(FInt 3)]))
                    (UnaryFPOp SinOp FPDouble (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" [(FInt 4)]))
                    @?=
                    True

equivModuloIndex__test5 = testCase "v(3) not equiv v(4)" $
   equivModuloIndex (UnaryFPOp SinOp FPDouble (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" [(FInt 3)]))
                    (UnaryFPOp SinOp FPDouble (FArrayElem FPDouble (Just $ ArraySizeInt 8) "v" [(FInt 4)]))
                    @?=
                    False

equivModuloIndex__test6 = testCase "w(3)+v(3) equiv w(3)+v(4)" $
   equivModuloIndex (BinaryFPOp AddOp FPDouble (FArrayElem FPDouble (Just $ ArraySizeInt 5) "w" [(FInt 3)])
                                               (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" [(FInt 3)]))
                    (BinaryFPOp AddOp FPDouble (FArrayElem FPDouble (Just $ ArraySizeInt 5) "w" [(FInt 3)])
                                               (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" [(FInt 4)]))
                    @?=
                    True

equivModuloIndex__test7 = testCase "w(3)+v(3) not equiv t(3)+v(4)" $
   equivModuloIndex (BinaryFPOp AddOp FPDouble (FArrayElem FPDouble (Just $ ArraySizeInt 5) "w" [(FInt 3)])
                                               (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" [(FInt 3)]))
                    (BinaryFPOp AddOp FPDouble (FArrayElem FPDouble (Just $ ArraySizeInt 5) "t" [(FInt 3)])
                                               (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" [(FInt 4)]))
                    @?=
                    False

equivModuloIndex__test8 = testCase "w(3)+v(3) equiv w(3)+v(4)" $
   equivModuloIndex (TypeCast FPDouble FPSingle (FArrayElem FPDouble (Just $ ArraySizeInt 5) "w" [(FInt 3)]))
                    (TypeCast FPDouble FPSingle (FArrayElem FPDouble (Just $ ArraySizeInt 5) "w" [(FInt 6)]))
                    @?=
                    True

equivModuloIndex__test9 = testCase "w(3)+v(3) not equiv t(3)+v(4)" $
   equivModuloIndex (TypeCast FPDouble FPSingle (FArrayElem FPDouble (Just $ ArraySizeInt 5) "w" [(FInt 3)]))
                    (TypeCast FPDouble FPSingle (FArrayElem FPDouble (Just $ ArraySizeInt 5) "t" [(FInt 3)]))
                    @?=
                    False

unfoldLetIn__tests = testGroup "unfoldLetIn tests"
  [unfoldLetIn__test1
  ,unfoldLetIn__test2
  ,unfoldLetIn__test3
  ,unfoldLetIn__test4
  ,unfoldLetIn__test5
  ,unfoldLetIn__test6
  ,unfoldLetIn__test7
  ,unfoldLetIn__test8
  ]

unfoldLetIn__test1 = testCase "unfoldLetIn__test1" $
  unfoldLetIn (FInt 0) @?= FInt 0

unfoldLetIn__test2 = testCase "unfoldLetIn__test2" $
  unfoldLetIn (FVar FPDouble "x") @?= FVar FPDouble "x"

unfoldLetIn__test3 = testCase "unfoldLetIn__test3" $
  unfoldLetIn (Let [("x", FPDouble, FInt 3)] (FVar FPDouble "x")) @?= FInt 3

unfoldLetIn__test4 = testCase "unfoldLetIn__test4" $
  unfoldLetIn (Let [("y", FPDouble, FInt 3),("x", FPDouble, FInt 3)] (FVar FPDouble "x")) @?= FInt 3

unfoldLetIn__test5 = testCase "unfoldLetIn__test5" $
  unfoldLetIn (Let [("y", FPDouble, FInt 4),("x", FPDouble, FInt 3)]
    (BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FVar FPDouble "y")))
    @?= BinaryFPOp AddOp FPDouble (FInt 3) (FInt 4)

unfoldLetIn__test6 = testCase "unfoldLetIn__test6" $
  unfoldLetIn (Let [("y", FPDouble, FInt 4)]
    (Let [("x", FPDouble, FInt 3)]
    (BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FVar FPDouble "y"))))
    @?= BinaryFPOp AddOp FPDouble (FInt 3) (FInt 4)

unfoldLetIn__test7 = testCase "unfoldLetIn__test7" $
  unfoldLetIn (Let [("z", FPDouble, FInt 6)]
    (BinaryFPOp AddOp FPDouble (FVar FPDouble "z") (Let [("x", FPDouble, FInt 3),("y", FPDouble, FInt 4)]
    (BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FVar FPDouble "y")))))
    @?= BinaryFPOp AddOp FPDouble (FInt 6) (BinaryFPOp AddOp FPDouble (FInt 3) (FInt 4))

unfoldLetIn__test8 = testCase "unfoldLetIn__test8" $
  unfoldLetIn (Let [("z", FPDouble, FInt 6)]
    (BinaryFPOp AddOp FPDouble (FVar FPDouble "z") (Let [("x", FPDouble, FInt 3),("y", FPDouble, FInt 4)]
    (Ite (FRel Lt (FEFun False "f" ResValue FPDouble []) (FVar FPDouble "z")) (FVar FPDouble "x") (FVar FPDouble "y")))))
    @?= BinaryFPOp AddOp FPDouble (FInt 6)
        (Ite (FRel Lt (FEFun False "f" ResValue FPDouble []) (FInt 6)) (FInt 3) (FInt 4))

unfoldForLoop__tests = testGroup "unfoldForLoop tests"
  [unfoldForLoop__test1
  ,unfoldForLoop__test2
  ,unfoldForLoop__test3
  ,unfoldForLoop__test4
  ,unfoldForLoop__test5
  ,unfoldForLoop__test6
  ]

unfoldForLoop__test1 = testCase "test1: for i=1 to 4 {i} = 4" $
   unfoldForLoop FPDouble 1 4 (FInt 0) "I" "Acc" (FVar FPDouble "I")
                 @?=
                 FInt 4

unfoldForLoop__test2 = testCase "test2: for i=1 to 4 {acc} = 0" $
   unfoldForLoop FPDouble 1 4 (FInt 0) "I" "Acc" (FVar FPDouble "Acc")
                 @?=
                 FInt 0

unfoldForLoop__test3 = testCase "test3: for i = 0 to 1 {i + acc} = 1 + (0 + 0)" $
   unfoldForLoop FPDouble 0 1 (FInt 0) "I" "Acc" (BinaryFPOp AddOp FPDouble (FVar FPDouble "I") (FVar FPDouble "Acc"))
                 @?=
                 BinaryFPOp AddOp FPDouble (FInt 1) (BinaryFPOp AddOp FPDouble (FInt 0) (FInt 0))

unfoldForLoop__test4 = testCase "test4" $
   unfoldForLoop FPDouble 0 2 (FInt 0) "I" "Acc" (BinaryFPOp AddOp FPDouble (FVar FPDouble "I") (FVar FPDouble "Acc"))
                 @?=
                 BinaryFPOp AddOp FPDouble (FInt 2) (BinaryFPOp AddOp FPDouble (FInt 1) (BinaryFPOp AddOp FPDouble (FInt 0) (FInt 0)))

unfoldForLoop__test5 = testCase "test5" $
   unfoldForLoop FPDouble 2 2 (FInt 10) "I" "Acc" (BinaryFPOp AddOp FPDouble (FVar FPDouble "I") (FVar FPDouble "Acc"))
                 @?=
                 (BinaryFPOp AddOp FPDouble (FInt 2) (FInt 10))

unfoldForLoop__test6 = testCase "test6" $
   unfoldForLoop FPDouble 0 1 (FInt 10) "I" "Acc" (Ite (FRel Lt (FVar FPDouble "X") (FInt 0)) (FInt 4) (BinaryFPOp AddOp FPDouble (FVar FPDouble "I") (FVar FPDouble "Acc")))
                 @?=
                 (Ite (FRel Lt (FVar FPDouble "X") (FInt 0))
                   (FInt 4)
                   (BinaryFPOp AddOp FPDouble (FInt 1)
                                              (Ite (FRel Lt (FVar FPDouble "X") (FInt 0))
                                                   (FInt 4)
                                                   (BinaryFPOp AddOp FPDouble (FInt 0) (FInt 10)))))

isArithExpr__tests = testGroup "isArithExpr tests"
  [isArithExpr__test1
  ,isArithExpr__test2
  ,isArithExpr__test3
  ,isArithExpr__test4
  ,isArithExpr__test5
  ]

isArithExpr__test1 = testCase "2 + (-1) is an arithmetic expression" $
    isArithExpr (BinaryFPOp AddOp FPDouble (FInt 2) (UnaryFPOp NegOp FPDouble (FInt 1)))
    @?= True

isArithExpr__test2 = testCase "if false then 2 else 1 is not an arithmetic expression" $
    isArithExpr (Ite FBFalse (FInt 2) (FInt 1))
    @?= False

isArithExpr__test3 = testCase "2 + (if false then 2 else 1) is not an arithmetic expression" $
    isArithExpr (BinaryFPOp AddOp FPDouble (Ite FBFalse (FInt 2) (FInt 1)) (UnaryFPOp NegOp FPDouble (FInt 1)))
    @?= False

isArithExpr__test4 = testCase "f(2) + (-1) is an arithmetic expression" $
    isArithExpr (BinaryFPOp AddOp FPDouble (FEFun False "f" ResValue FPDouble [FInt 2]) (UnaryFPOp NegOp FPDouble (FInt 1)))
    @?= True

isArithExpr__test5 = testCase "x + (-1) is an arithmetic expression" $
    isArithExpr (BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (UnaryFPOp NegOp FPDouble (FInt 1)))
    @?= True

isListArithExprs__tests = testGroup "isListArithExprs tests"
  [isListArithExprs__test1
  ,isListArithExprs__test2
  ,isListArithExprs__test3
  ,isListArithExprs__test4
  ,isListArithExprs__test5
  ]

isListArithExprs__test1 = testCase "isListArithExprs1" $
    isListArithExprs [BinaryFPOp AddOp FPDouble (FInt 2) (UnaryFPOp NegOp FPDouble (FInt 1))]
    @?= True

isListArithExprs__test2 = testCase "isListArithExprs2" $
    isListArithExprs [BinaryFPOp AddOp FPDouble (FInt 2) (UnaryFPOp NegOp FPDouble (FInt 1)), FInt 3]
    @?= True

isListArithExprs__test3 = testCase "isListArithExprs3" $
    isListArithExprs [Ite FBFalse (FInt 2) (FInt 1)]
    @?= False

isListArithExprs__test4 = testCase "isListArithExprs4" $
    isListArithExprs [Ite FBFalse (FInt 2) (FInt 1), FInt 3]
    @?= False

isListArithExprs__test5 = testCase "isListArithExprs5" $
    isListArithExprs [Ite FBFalse (FInt 2) (FInt 1)
                     ,Let [("x", FPDouble, FInt 3)] (FVar FPDouble "x")]
    @?= False

listLetElems__tests = testGroup "listLetElems tests"
  [listLetElems__test1
  ,listLetElems__test2
  ,listLetElems__test3
  ,listLetElems__test4
  ,listLetElems__test5
  ,listLetElems__test6
  ,listLetElems__test7
  ]

listLetElems__test1 = testCase "listLetElems1" $
  listLetElems (FInt 2)
  @?=
  []

listLetElems__test2 = testCase "listLetElems2" $
  listLetElems (Let [("x",FPDouble,FInt 1)] (FInt 2))
  @?=
  [("x",FPDouble,FInt 1)]

listLetElems__test3 = testCase "listLetElems3" $
  listLetElems (BinaryFPOp AddOp FPDouble (FInt 3) (Let [("x",FPDouble,FInt 1)] (FInt 2)))
  @?=
  [("x",FPDouble,FInt 1)]

listLetElems__test4 = testCase "listLetElems4" $
  listLetElems (BinaryFPOp AddOp FPDouble (Let [("y",FPDouble,FInt 5)] (FInt 2)) (Let [("x",FPDouble,FInt 1)] (FInt 2)))
  @?=
  [("y",FPDouble,FInt 5),("x",FPDouble,FInt 1)]

listLetElems__test5 = testCase "listLetElems5" $
  listLetElems (Ite FBTrue (Let [("y",FPDouble,FInt 5)] (FInt 2)) (Let [("x",FPDouble,FInt 1)] (FInt 2)))
  @?=
  [("y",FPDouble,FInt 5),("x",FPDouble,FInt 1)]

listLetElems__test6 = testCase "listLetElems6" $
  listLetElems (Ite FBTrue (Let [("y",FPDouble,FInt 5)] (FInt 2))
                           (Let [("x",FPDouble,FInt 1)] (BinaryFPOp AddOp FPDouble (FInt 3)
                                                        (Let [("z",FPDouble,FInt 7)] (FInt 6)))))
  @?=
  [("y",FPDouble,FInt 5),("x",FPDouble,FInt 1),("z",FPDouble,FInt 7)]

listLetElems__test7 = testCase "listLetElems7" $
  listLetElems (Let [("y",FPDouble,FInt 5)] (Let [("x",FPDouble,FInt 1)] (FInt 6)))
  @?=
  [("y",FPDouble,FInt 5),("x",FPDouble,FInt 1)]

removeLetInFAExpr__tests = testGroup "removeLetInFAExpr tests"
  [removeLetInFAExpr__test1
  ,removeLetInFAExpr__test2
  ,removeLetInFAExpr__test3
  ]

removeLetInFAExpr__test1  = testCase "removeLetInFAExpr1" $
  removeLetInFAExpr (Let [("x",FPDouble,FInt 1)] (FInt 6))
  @?=
  FInt 6

removeLetInFAExpr__test2  = testCase "removeLetInFAExpr2" $
  removeLetInFAExpr (Let [("y",FPDouble,FInt 5)] (Let [("x",FPDouble,FInt 1)] (FInt 6)))
  @?=
  FInt 6

removeLetInFAExpr__test3  = testCase "removeLetInFAExpr2" $
  removeLetInFAExpr (Ite FBTrue (Let [("x",FPDouble,FInt 1)] (FInt 1))
                                (Let [("y",FPDouble,FInt 5)] (Let [("x",FPDouble,FInt 1)] (FInt 6))))
  @?=
  Ite FBTrue (FInt 1) (FInt 6)


predCallListFBExprStmWithCond__tests = testGroup "predCallListFBExprStmWithCond tests"
  [predCallListFBExprStmWithCond__test1
  ]

predCallListFBExprStmWithCond__test1  = testCase "" $
  predCallListFBExprStmWithCond [RDecl Real "f" [Arg "X" Real] (BinaryOp AddOp (Var Real "X") (BinaryOp MulOp (Var Real "X") (Int 2)))
  ,RPred "g" [Arg "X" Real] (RBLet [LetElem {letVar = "y", letType = Real, letExpr = EFun "f" ResValue Real [Var Real "X"]}] (RBIte (Rel Gt (Var Real "y") (Int 0)) (RBExpr BTrue) (RBExpr BFalse)))] (BExpr $ FEPred False Original "g" [])
  @?=
  [(FEPred False Original "g" [])]



simplFRel__tests = testGroup "simplFRel tests"
  [simplFRel__test1
  ,simplFRel__test2
  ]

simplFRel__test1 = testCase "simplFRel 2 <= 0.1 is false" $
  simplRel (Rel LtE (Int 2) (Rat 0.1))
  @?=
  BFalse

simplFRel__test2 = testCase "simplFRel 2 <= 0.1 is false" $
  simplRel (Rel LtE (Int 2) (Rat 4.1))
  @?=
  BTrue

simplBExprFix__tests = testGroup "simplBExprFix tests"
  [simplBExprFix__test1
  ,simplBExprFix__test2
  ,simplBExprFix__test3
  ,simplBExprFix__test4
  ,simplBExprFix__test5
  ,simplBExprFix__test6
  ]

simplBExprFix__test1 = testCase "simplBExprFix 1" $
  simplBExprFix (And (Rel LtE (BinaryOp DivOp (Rat 2) (Int 2)) (Rat 0.1)) (Rel LtE (Rat 0.1) (BinaryOp MulOp (Rat 2) (Int 2))))
  @?=
  BFalse

simplBExprFix__test2 = testCase "simplBExprFix 2" $
  simplBExprFix (And (And (Rel LtE (BinaryOp DivOp (Rat 2) (Int 2)) (Rat 0.1)) (Rel LtE (Rat 0.1) (BinaryOp MulOp (Rat 2) (Int 2)))) (And BTrue BTrue))
  @?=
  BFalse


simplBExprFix__test3 = testCase "true AND false = false" $
  simplBExprFix (And BTrue BFalse)
  @?=
  BFalse

simplBExprFix__test4 = testCase "true AND true = true" $
  simplBExprFix (And BTrue BTrue)
  @?=
  BTrue

simplBExprFix__test5 = testCase "2/2 <= 0.1" $
  simplBExprFix (Rel LtE (BinaryOp DivOp (Rat 2) (Int 2)) (Rat 0.1))
  @?=
  BFalse

simplBExprFix__test6 = testCase "simplBExprFix 1" $
  simplBExprFix (Rel LtE (Rat 0.1) (BinaryOp MulOp (Rat 2) (Int 2)))
  @?=
  BTrue

isIntAExpr__tests = testGroup "isIntAExpr tests"
  [isIntAExpr__test1
  ,isIntAExpr__test2
  ,isIntAExpr__test3
  ,isIntAExpr__test4
  ,isIntAExpr__test5
  ,isIntAExpr__test6
  ,isIntAExpr__test7
  ,isIntAExpr__test8
  ,isIntAExpr__test9
  ,isIntAExpr__test10
  ,isIntAExpr__test11
  ,isIntAExpr__test12
  ]

isIntAExpr__test1 = testCase "isIntAExpr int" $
  isIntAExpr (Int 2)
  @?=
  True

isIntAExpr__test2 = testCase "isIntAExpr rat omt" $
  isIntAExpr (Rat 2)
  @?=
  True

isIntAExpr__test3 = testCase "isIntAExpr div" $
  isIntAExpr (BinaryOp DivOp (Int 1) (Int 2))
  @?=
  False

isIntAExpr__test4 = testCase "isIntAExpr mul int" $
  isIntAExpr (BinaryOp MulOp (Rat 2) (Int 2))
  @?=
  True

isIntAExpr__test5 = testCase "isIntAExpr add int" $
  isIntAExpr (BinaryOp AddOp (Rat 2) (Int 2))
  @?=
  True

isIntAExpr__test6 = testCase "isIntAExpr add" $
  isIntAExpr (BinaryOp AddOp (Rat 2.1) (Int 2))
  @?=
  False

isIntAExpr__test7 = testCase "isIntAExpr rat" $
  isIntAExpr (Rat 2.9)
  @?=
  False

isIntAExpr__test8 = testCase "isIntAExpr floor" $
  isIntAExpr (UnaryOp FloorOp (Rat 2.9))
  @?=
  True

isIntAExpr__test9 = testCase "isIntAExpr neg" $
  isIntAExpr (UnaryOp NegOp (Rat 2.9))
  @?=
  False

isIntAExpr__test10 = testCase "isIntAExpr abs" $
  isIntAExpr (UnaryOp AbsOp (Rat 2.9))
  @?=
  False

isIntAExpr__test11 = testCase "isIntAExpr neg int" $
  isIntAExpr (UnaryOp NegOp (Rat 2))
  @?=
  True

isIntAExpr__test12 = testCase "isIntAExpr abs int" $
  isIntAExpr (UnaryOp AbsOp (Rat 2))
  @?=
  True