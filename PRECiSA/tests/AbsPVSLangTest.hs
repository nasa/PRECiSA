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
   ]

hasConditionals__tests = testGroup "hasConditionals" $ [
    testGroup "function calls" $ [
      testCase "with if" $
                let program = [RDecl Real "f" [] (RIte BTrue (Int 1) (Int 2))] in
                    hasConditionals program (EFun "f" Real []) @?= True,
      testCase "without if" $
                let program = [RDecl Real "f" [] (Int 2)] in
                    hasConditionals program (EFun "f" Real []) @?= False
    ],
      testCase "variable" $
            hasConditionals [] (Var TInt "x") @?= False
   ] ++ map (\(i,o) -> testCase (show i) $ hasConditionals [] i @?= o) testcasesHasConditionals

testcasesHasConditionals
    =[
    (Int 4, False),
    (Rat (toRational 0.1), False),
    (Var  TInt "x",False),
    (ArrayElem TInt (Just $ ArraySizeInt 3) "a" (Int 2), False),
    (BinaryOp AddOp (Int 1) (Int 2),False),
    (UnaryOp NegOp (Int 2), False),
    (Min [Int 3], False),
    (Max [Int 3], False),
    (RLet [(LetElem "x" Real (Rat $ toRational 0.2))] (Var Real "x"), False),
    (RLet [(LetElem "x" Real (RIte BTrue (Rat $ toRational 0.2) (Rat $ toRational 0.1)))] (Var Real "x"), True),
    (RLet [(LetElem "x" Real (Rat $ toRational 0.2))] (RIte BTrue (Rat $ toRational 0.2) (Var Real "x")), True),
    (RIte BTrue (Int 1) (Int 2), True),
    (RListIte [(BTrue, Int 1)] (Int 2), True),
    (RForLoop Real (Int 1) (Int 3) (Int 0) "x" "acc" (Int 4), False)
    ]

hasConditionalsBExpr__tests = testGroup "hasConditionalsBExpr" $ [
    testCase "function call with if" $
      let program = [RDecl Real "f" [] (RIte BTrue (Int 1) (Int 2))] in
        hasConditionalsBExpr program (RBExpr (Rel Lt (Int 2) (EFun "f" Real []))) @?= True,
    testCase "function call with no if" $
      let program = [RDecl Real "f" [] (Int 2)] in
        hasConditionalsBExpr program (RBExpr (Rel Lt (Int 2) (EFun "f" Real []))) @?= False,
    testCase "predicate call with no if" $
      let program = [RPred "f" [] (RBExpr BTrue)] in
        hasConditionalsBExpr program (RBExpr $ EPred "f" []) @?= False,
    testCase "predicate call with if" $
      let program = [RPred "f" [] (RBIte (Rel Lt (Int 2) (Var TInt "x")) (RBExpr BTrue) (RBExpr BFalse))] in
        hasConditionalsBExpr program (RBExpr $ EPred "f" []) @?= True,
    testCase "conditional" $
        hasConditionalsBExpr [] (RBIte BTrue (RBExpr BTrue) (RBExpr BTrue)) @?= True,
    testCase "conditional list" $
        hasConditionalsBExpr [] (RBListIte [(BTrue, RBExpr BTrue)] (RBExpr BTrue)) @?= True,
    testCase "simple boolean expression" $
        hasConditionalsBExpr [] (RBExpr BTrue) @?= False,
    testCase "let in with conditional body" $
        hasConditionalsBExpr [] (RBLet [LetElem "x" TInt (Int 2)] (RBListIte [(Rel Lt (Int 2) (Var TInt "x"), RBExpr BTrue)] (RBExpr BTrue))) @?= True,
    testCase "let in with conditional let element" $
        hasConditionalsBExpr [] (RBLet [LetElem "x" TInt (RIte (Rel Lt (Int 2) (Int 6)) (Int 3) (Int 5))] (RBExpr $ Rel Eq (Int 4) (Var TInt "x"))) @?= True,
    testCase "let in with non conditional body" $
        hasConditionalsBExpr [] (RBLet [LetElem "x" TInt (Int 2)] (RBExpr BTrue)) @?= False
   ]


localVars__tests   = testGroup "localVars" []
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
  (Decl False TInt "f" [] (FEFun False "for_f1" TInt [FInt 0,FInt 0])
  ,
  [Decl False TInt "for_f1" [Arg "I" TInt,Arg "X" TInt] (Ite (FRel Eq (FVar TInt "I") (FInt 10)) (FVar TInt "X")
          (FEFun False "for_f1" TInt [BinaryFPOp AddOp TInt (FVar TInt "I") (FInt 1)
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
                ,ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
                ,ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)
            )
            ,("ErrMulPow2R"
                ,ErrMulPow2R FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
                ,ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)
            )
            ,("ErrAbs"
                ,ErrUnOp AbsOp False FPDouble (Int 3) (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
                ,ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)
            )
            ,("ErrAbs"
                ,ErrUnOp NegOp False FPDouble (Int 3) (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
                ,ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)
            )
            ,("MaxErr"
                ,MaxErr [ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble),
                         ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)]
                ,ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)
            )
            ,("MaxErr_ErrNeg"
                ,MaxErr [ErrUnOp NegOp False FPDouble (Int 3) (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)),
                         ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)]
                ,ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)
            )
            ,("ErrAdd_ErrMulPow2L"
                ,ErrBinOp AddOp FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrBinOp SubOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                                 (RealMark "K") (ErrorMark "K" FPDouble)
                ,ErrBinOp AddOp FPDouble (RealMark "Z") (ErrBinOp SubOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)) (RealMark "K") (ErrorMark "K" FPDouble)
            )
            ,("ErrAdd_ErrMulPow2L"
                ,ErrBinOp AddOp FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrBinOp SubOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))) (RealMark "K") (ErrorMark "K" FPDouble)
                ,ErrBinOp AddOp FPDouble (RealMark "Z") (ErrBinOp SubOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)) (RealMark "K") (ErrorMark "K" FPDouble)
            )
            ,("ErrSub_ErrMulPow2L"
                ,ErrBinOp SubOp FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                                 (RealMark "K") (ErrorMark "K" FPDouble)
                ,ErrBinOp SubOp FPDouble (RealMark "Z") (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)) (RealMark "K") (ErrorMark "K" FPDouble)
            )
            ,("ErrMul_ErrMulPow2L"
                ,ErrBinOp MulOp FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                                 (RealMark "K") (ErrorMark "K" FPDouble)
                ,ErrBinOp MulOp FPDouble (RealMark "Z") (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)) (RealMark "K") (ErrorMark "K" FPDouble)
            )
            ,("ErrDiv_ErrMulPow2L"
                ,ErrBinOp DivOp FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                                                (RealMark "K") (ErrorMark "K" FPDouble)
                ,ErrBinOp DivOp FPDouble (RealMark "Z") (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)) (RealMark "K") (ErrorMark "K" FPDouble)
            )
            ,("ErrFloor_ErrMulPow2L"
                ,ErrUnOp FloorOp False FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrUnOp FloorOp False FPDouble (RealMark "Z") (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
            )
            ,("ErrFloor0_ErrMulPow2L"
                ,ErrUnOp FloorOp True FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrUnOp FloorOp True FPDouble (RealMark "Z") (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
            )
            ,("ErrSqrt_ErrMulPow2L"
                ,ErrUnOp SqrtOp False FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrUnOp SqrtOp False FPDouble (RealMark "Z") (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
            )
            ,("ErrSin_ErrMulPow2L"
                ,ErrUnOp SinOp False FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrUnOp SinOp False FPDouble (RealMark "Z") (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
            )
            ,("ErrCos_ErrMulPow2L"
                ,ErrUnOp CosOp False FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrUnOp CosOp False FPDouble (RealMark "Z") (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
            )
            ,("ErrTan_ErrMulPow2L"
                ,ErrUnOp TanOp False FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrUnOp TanOp False FPDouble (RealMark "Z") (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
            )
            ,("ErrAsin_ErrMulPow2L"
                ,ErrUnOp AsinOp False FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrUnOp AsinOp False FPDouble (RealMark "Z") (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
            )
            ,("ErrAcos_ErrMulPow2L"
                ,ErrUnOp AcosOp False FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrUnOp AcosOp False FPDouble (RealMark "Z") (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
            )
            ,("ErrAtan_ErrMulPow2L"
                ,ErrUnOp AtanOp False FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrUnOp AtanOp False FPDouble (RealMark "Z") (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
            ,("ErrAtanT_ErrMulPow2L"
                ,ErrUnOp AtanOp True FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrUnOp AtanOp True FPDouble (RealMark "Z") (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
            )
            ,("ErrLn_ErrMulPow2L"
                ,ErrUnOp LnOp False FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrUnOp LnOp False FPDouble (RealMark "Z") (ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
            )
            ,("ErrExpo_ErrMulPow2L"
                ,ErrUnOp ExpoOp False FPSingle (RealMark "Z") (ErrMulPow2L FPSingle 2 (ErrBinOp AddOp FPSingle (RealMark "X") (ErrorMark "X" FPSingle) (RealMark "Y") (ErrorMark "Y" FPSingle)))
                ,ErrUnOp ExpoOp False FPSingle (RealMark "Z") (ErrBinOp AddOp FPSingle (RealMark "X") (ErrorMark "X" FPSingle) (RealMark "Y") (ErrorMark "Y" FPSingle))
            )
            ]


equivEExpr__tests = testGroup "equivEExpr tests" $  map (\(msg, ee1, ee2, o) -> testCase msg (equivEExpr ee1 ee2 @?= o)) equivEExpr_testsIOs

equivEExpr_testsIOs =
            [("ErrMulPow2L_Add"
                ,ErrMulPow2L FPSingle 2 (ErrBinOp AddOp FPSingle (RealMark "X") (ErrorMark "X" FPSingle) (RealMark "Y") (ErrorMark "Y" FPSingle))
                ,ErrBinOp AddOp FPSingle (RealMark "X") (ErrorMark "X" FPSingle) (RealMark "Y") (ErrorMark "Y" FPSingle)
                ,True
            )
            ,("ErrMul_Add"
                ,ErrBinOp MulOp FPSingle (RealMark "X") (ErrorMark "X" FPSingle) (RealMark "Y") (ErrorMark "Y" FPSingle)
                ,ErrBinOp AddOp FPSingle (RealMark "X") (ErrorMark "X" FPSingle) (RealMark "Y") (ErrorMark "Y" FPSingle)
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
    initErrorMark (ErrorMark "x" TInt) @?= Just (Int 0)

initErrorMark__test2 = testCase "initErrorMark of E(x_double) is half ulp R(x)" $
    initErrorMark (ErrorMark "x" FPDouble)  @?= Just (HalfUlp (RealMark "x") FPDouble)

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
    initAExpr (ErrorMark "x" TInt) @?= (Int 0)

initAExpr__test2 = testCase "initAExpr of E(x_double) is half_ulp(R(x))" $
    initAExpr (ErrorMark "x" FPDouble)  @?= (HalfUlp (RealMark "x") FPDouble)

initAExpr__test3 = testCase "initAExpr of y+x is y+x" $
    initAExpr (BinaryOp AddOp (Var Real "y") (Var Real "x")) @?= (BinaryOp AddOp (Var Real "y") (Var Real "x"))

initAExpr__test4 = testCase "initAExpr of E(x_int)+x is half_ulp(R(x))+x" $
    initAExpr (BinaryOp AddOp (ErrorMark "x" FPDouble) (Var Real "x")) @?= (BinaryOp AddOp (HalfUlp (RealMark "x") FPDouble) (Var Real "x"))

initAExpr__test5 = testCase "initAExpr of StoR(RtoS(E(x)) is StoR(RtoS(half_ulp(R(x)))" $
    initAExpr (FromFloat FPSingle (ToFloat FPSingle(ErrorMark "x" FPDouble))) @?= FromFloat FPSingle (ToFloat FPSingle(HalfUlp (RealMark "x") FPDouble))


initBExpr__tests = testGroup "initBExpr tests"
  [initBExpr__test1
  ,initBExpr__test2
  ,initBExpr__test3
  ,initBExpr__test4
  ]

initBExpr__test1 = testCase "initBExpr of E(x_int) < 6 is 0 < 6" $
    initBExpr (Rel Lt (ErrorMark "x" TInt) (Int 6)) @?= Rel Lt (Int 0) (Int 6)

initBExpr__test2 = testCase "initBExpr of E(x_double) < 5 is half ulp R(x) < 5" $
    initBExpr (Rel Gt (ErrorMark "x" FPDouble) (Int 5))  @?= Rel Gt (HalfUlp (RealMark "x") FPDouble) (Int 5)

initBExpr__test3 = testCase "initBExpr of y+x < 9 and E(x_double) < 5 is y+x and half ulp R(x) < 5" $
    initBExpr (And (Rel Lt (BinaryOp AddOp (Var Real "y") (Var Real "x")) (Int 9)) (Rel Gt (ErrorMark "x" FPDouble) (Int 5)))
          @?= (And (Rel Lt (BinaryOp AddOp (Var Real "y") (Var Real "x")) (Int 9)) (Rel Gt (HalfUlp (RealMark "x") FPDouble) (Int 5)))

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
  funCallListFAExpr (FEFun False "f" FPDouble []) @?= [FEFun False "f" FPDouble []]

funCallListFAExpr__test3 = testCase "funCallList of f()+g(5) is [f(),g(5)]" $
  funCallListFAExpr (BinaryFPOp AddOp FPDouble (FEFun False "f" FPDouble []) (FEFun False "g" FPDouble [FInt 5]))
  @?=
  [FEFun False "f" FPDouble [],FEFun False "g" FPDouble [FInt 5]]

funCallListFAExpr__test4 = testCase "funCallList of f()+g(5) is [f(),g(5)]" $
  funCallListFAExpr (BinaryFPOp AddOp FPDouble (FEFun False "f" FPDouble []) (FEFun False "g" FPDouble [FInt 5]))
  @?=
  [FEFun False "f" FPDouble [],FEFun False "g" FPDouble [FInt 5]]

funCallListFAExpr__test5 = testCase "funCallList of f(g(5)) is [f(g(5)), g(5)]" $
  funCallListFAExpr (FEFun False "f" FPDouble [FEFun False "g" FPDouble [FInt 5]])
  @?=
  [FEFun False "f" FPDouble [FEFun False "g" FPDouble [FInt 5]]
  ,FEFun False "g" FPDouble [FInt 5]]

funCallListFAExpr__test6 = testCase "funCallList of 0 is []" $
  funCallListFAExpr (ToFloat FPDouble (Int 9)) @?= []

funCallListFAExpr__test7 = testCase "funCallList of if(f(X)<0) then 1 else 2 is [f(X)]" $
  funCallListFAExpr (Ite (FRel Lt (FEFun False "f" FPDouble []) (FInt 0)) (FInt 1) (FInt 2))
  @?=
  [FEFun False "f" FPDouble []]

funCallListFBExpr__tests = testGroup "funCallListFBExpr tests"
  [funCallListFBExpr__test1
  ,funCallListFBExpr__test2
  ,funCallListFBExpr__test3
  ]

funCallListFBExpr__test1 = testCase "funCallList of f()<8 is [f()]" $
    funCallListFBExpr (FRel Lt (FEFun False "f" FPDouble []) (FInt 8))
    @?=
    [FEFun False "f" FPDouble []]

funCallListFBExpr__test2 = testCase "funCallList of f()>g(5) is [f(),g(5)]" $
    funCallListFBExpr (FRel Gt (FEFun False "f" FPDouble [])
                               (FEFun False "g" FPDouble [FInt 5]))
    @?=
    [FEFun False "f" FPDouble [],FEFun False "g" FPDouble [FInt 5]]

funCallListFBExpr__test3 = testCase "funCallList of f()<8 and f()>g(5) is [f(),g(5)]" $
    funCallListFBExpr (FAnd (FRel Lt (FEFun False "f" FPDouble []) (FInt 8))
                            (FRel Gt (FEFun False "f" FPDouble []) (FEFun False "g" FPDouble [FInt 5])))
    @?=
    [FEFun False "f" FPDouble [],FEFun False "g" FPDouble [FInt 5]]


funCallListAExpr__tests = testGroup "funCallListAExpr tests"
  [funCallListAExpr__test1
  ,funCallListAExpr__test2
  ,funCallListAExpr__test3
  ,funCallListAExpr__test4
  ,funCallListAExpr__test5
  ]

funCallListAExpr__test1 = testCase "funCallList of 8 is []" $
    funCallListAExpr (Int 8) @?= []

funCallListAExpr__test2 = testCase "funCallList of f() is [f()]" $
    funCallListAExpr (EFun "f" Real []) @?= [EFun "f" Real []]

funCallListAExpr__test3 = testCase "funCallList of f()+g(5) is [f(),g(5)]" $
    funCallListAExpr (BinaryOp AddOp (EFun "f" Real []) (EFun "g" Real [Int 5]))
    @?=
    [EFun "f" Real [],EFun "g" Real [Int 5]]

funCallListAExpr__test4 = testCase "funCallList of f()+g(5) is [f(),g(5)]" $
    funCallListAExpr (BinaryOp AddOp (EFun "f" Real []) (EFun "g" Real [Int 5]))
    @?=
    [EFun "f" Real [],EFun "g" Real [Int 5]]

funCallListAExpr__test5 = testCase "funCallList of f(g(5)) is [f(g(5)), g(5)]" $
    funCallListAExpr (EFun "f" Real [EFun "g" Real [Int 5]])
    @?=
    [EFun "f" Real [EFun "g" Real [Int 5]],EFun "g" Real [Int 5]]

-- funCallListFAExpr__test6 = testCase "funCallList of f(g(5)) is [f(g(5)), g(5)]" $
--     funCallListFAExpr (ToFloat FPDouble (Int 9)) @?= []

funCallListBExpr__tests = testGroup "funCallListBExpr tests"
  [funCallListBExpr__test1
  ,funCallListBExpr__test2
  ,funCallListBExpr__test3
  ]

funCallListBExpr__test1 = testCase "funCallList of f()<8 is [f()]" $
    funCallListBExpr (Rel Lt (EFun "f" Real []) (Int 8)) @?= [EFun "f" Real []]

funCallListBExpr__test2 = testCase "funCallList of f()>g(5) is [f(),g(5)]" $
    funCallListBExpr (Rel Gt (EFun "f" Real []) (EFun "g" Real [Int 5]))
    @?=
    [EFun "f" Real [],EFun "g" Real [Int 5]]

funCallListBExpr__test3 = testCase "funCallList of f()<8 and f()>g(5) is [f(),g(5)]" $
    funCallListBExpr (And (Rel Lt (EFun "f" Real []) (Int 8)) (Rel Gt (EFun "f" Real []) (EFun "g" Real [Int 5])))
    @?=
    [EFun "f" Real [],EFun "g" Real [Int 5]]



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
    noRoundOffErrorInAExpr (FEFun False "f" TInt []) @?= True

noRoundOffErrorInAExpr__test13 = testCase "int fun has no round-off error" $
    noRoundOffErrorInAExpr (FEFun False "f" TInt [FCnst FPDouble (fromDouble2Rat 0.1)]) @?= True

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
                                       (FEFun False "f" FPDouble [(FVar FPDouble "x"),(FVar FPDouble "z")]))
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
   equivModuloIndex (FArrayElem FPDouble (Just $ ArraySizeInt 8) "v" (FInt 3))
                    (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" (FInt 4))
                    @?=
                    False

equivModuloIndex__test2 = testCase "" $
   equivModuloIndex (FArrayElem FPSingle (Just $ ArraySizeInt 8) "v" (FInt 3))
                    (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" (FInt 4))
                    @?=
                    False

equivModuloIndex__test3 = testCase "v(3) equiv v(4)" $
   equivModuloIndex (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" (FInt 3))
                    (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" (FInt 4))
                    @?=
                    True

equivModuloIndex__test4 = testCase "sin(v(3)) equiv sin(v(4))" $
   equivModuloIndex (UnaryFPOp SinOp FPDouble (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" (FInt 3)))
                    (UnaryFPOp SinOp FPDouble (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" (FInt 4)))
                    @?=
                    True

equivModuloIndex__test5 = testCase "v(3) not equiv v(4)" $
   equivModuloIndex (UnaryFPOp SinOp FPDouble (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" (FInt 3)))
                    (UnaryFPOp SinOp FPDouble (FArrayElem FPDouble (Just $ ArraySizeInt 8) "v" (FInt 4)))
                    @?=
                    False

equivModuloIndex__test6 = testCase "w(3)+v(3) equiv w(3)+v(4)" $
   equivModuloIndex (BinaryFPOp AddOp FPDouble (FArrayElem FPDouble (Just $ ArraySizeInt 5) "w" (FInt 3))
                                               (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" (FInt 3)))
                    (BinaryFPOp AddOp FPDouble (FArrayElem FPDouble (Just $ ArraySizeInt 5) "w" (FInt 3))
                                               (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" (FInt 4)))
                    @?=
                    True

equivModuloIndex__test7 = testCase "w(3)+v(3) not equiv t(3)+v(4)" $
   equivModuloIndex (BinaryFPOp AddOp FPDouble (FArrayElem FPDouble (Just $ ArraySizeInt 5) "w" (FInt 3))
                                               (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" (FInt 3)))
                    (BinaryFPOp AddOp FPDouble (FArrayElem FPDouble (Just $ ArraySizeInt 5) "t" (FInt 3))
                                               (FArrayElem FPDouble (Just $ ArraySizeInt 5) "v" (FInt 4)))
                    @?=
                    False

equivModuloIndex__test8 = testCase "w(3)+v(3) equiv w(3)+v(4)" $
   equivModuloIndex (TypeCast FPDouble FPSingle (FArrayElem FPDouble (Just $ ArraySizeInt 5) "w" (FInt 3)))
                    (TypeCast FPDouble FPSingle (FArrayElem FPDouble (Just $ ArraySizeInt 5) "w" (FInt 6)))
                    @?=
                    True

equivModuloIndex__test9 = testCase "w(3)+v(3) not equiv t(3)+v(4)" $
   equivModuloIndex (TypeCast FPDouble FPSingle (FArrayElem FPDouble (Just $ ArraySizeInt 5) "w" (FInt 3)))
                    (TypeCast FPDouble FPSingle (FArrayElem FPDouble (Just $ ArraySizeInt 5) "t" (FInt 3)))
                    @?=
                    False


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
    isArithExpr (BinaryFPOp AddOp FPDouble (FEFun False "f" FPDouble [FInt 2]) (UnaryFPOp NegOp FPDouble (FInt 1)))
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
  ,RPred "g" [Arg "X" Real] (RBLet [LetElem {letVar = "y", letType = Real, letExpr = EFun "f" Real [Var Real "X"]}] (RBIte (Rel Gt (Var Real "y") (Int 0)) (RBExpr BTrue) (RBExpr BFalse)))] (BExpr $ FEPred False Original "g" [])
  @?=
  [(FEPred False Original "g" [])]