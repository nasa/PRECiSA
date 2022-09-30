-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module TransformationTest where

import Test.Tasty
import Test.Tasty.HUnit
import Transformation
import AbsPVSLang
import PVSTypes
import Control.Monad.State
import Operators
import Common.TypesUtils

returnsValueEqualTo :: (Eq a, Show a) => IO a -> a -> IO ()
returnsValueEqualTo lhs rhs = lhs >>= (@?= rhs)

returnsValueEqualTo':: State TranStateInterp FAExpr -> FAExpr -> IO ()
returnsValueEqualTo' lhs rhs = return (fst $ runState lhs initState) `returnsValueEqualTo` rhs
  where
    initState = [("f", TransState { freshErrVar = FreshErrVar { env = [], count = 0, localEnv = [] },
                                                     forExprMap = [] })]

checkOutputIs :: (Eq a, Show a) => IO a -> a -> IO ()
checkOutputIs actual expected =
  actual >>=
      \out ->
          (out == expected)
              @? ("Output: " ++ show out ++ " is different than expected: " ++ show expected)

initState :: [Decl] -> TranStateInterp
initState = concatMap initStateDecl
initStateDecl decl =
  [(declName decl,
    TransState { freshErrVar = FreshErrVar { env = [], count = 0, localEnv = [] }, forExprMap = [] })]

testTransformation = testGroup "Transformation"
  [transformStmSymb__tests
  ,origDeclName__tests
  ,localVarsInExpr__tests
  ,generateVarName__tests
  ,testGroup "betaBExprStm" $
    [ testCase "basic" $
      let
        rdecls = [
          RPred "f"
            [Arg "X" Real,Arg "Y" Real]
            (RBIte
              (Rel Gt
                (BinaryOp MulOp (Var Real "X") (Var Real "Y"))
                (Int 0))
              (RBExpr BTrue)
              (RBExpr BFalse))]
        decls = [
          Pred True Original "f"
            [Arg "X" FPDouble,Arg "Y" FPDouble]
            (BIte
              (FRel Gt
                (BinaryFPOp MulOp FPDouble (FVar FPDouble "X") (FVar FPDouble "Y"))
                (TypeCast TInt FPDouble (FInt 0)))
              (BExpr FBTrue)
              (BExpr FBFalse))]
        stm = BIte (FRel Gt (BinaryFPOp MulOp FPDouble (FVar FPDouble "X") (FVar FPDouble "Y")) (TypeCast TInt FPDouble (FInt 0))) (BExpr FBTrue) (BExpr FBFalse)
        ret = BIte (FRel Gt (BinaryFPOp MulOp FPDouble (FVar FPDouble "X") (FVar FPDouble "Y")) (FVar FPDouble "E_0")) (BExpr FBTrue) (BIte (FRel LtE (BinaryFPOp MulOp FPDouble (FVar FPDouble "X") (FVar FPDouble "Y")) (UnaryFPOp NegOp FPDouble (FVar FPDouble "E_0"))) (BExpr FBFalse) BUnstWarning)
      in (fst $ runState (betaBExprStm rdecls True decls "f" FBTrue True stm) (initState decls)) @?= ret
        ]
  ]

localVarsInExpr__tests = testGroup "localVarsInExpr tests"
  [localVarsInExpr1__test
  ,localVarsInExpr2__test
  ,localVarsInExpr3__test
  ,localVarsInExpr4__test
  ,localVarsInExpr5__test
  ,localVarsInExpr6__test
  ,localVarsInExpr7__test
  ,localVarsInExpr8__test
  ,localVarsInExpr9__test
  ,localVarsInExpr10__test
  ,localVarsInExpr11__test
  ,localVarsInExpr12__test
  ]

localVarsInExpr1__test = testCase "localVarsInExpr1" $
  localVarsInExpr [("X",FPDouble,FVar FPDouble "Y")] (FInt 1) @?= []

localVarsInExpr2__test = testCase "localVarsInExpr2" $
  localVarsInExpr [("F", FPDouble, FInt 5)] (FVar FPDouble "X") @?= []

localVarsInExpr3__test = testCase "localVarsInExpr3" $
  localVarsInExpr [("X",FPDouble,FVar FPDouble "Y")] (FVar FPDouble "X")
  @?=
  [("X", FPDouble, FVar FPDouble "Y")]

localVarsInExpr4__test = testCase "localVarsInExpr4" $
  localVarsInExpr [("X",FPDouble,FVar FPDouble "Y"),("Y",FPDouble,FVar FPDouble "Z")]
                  (FVar FPDouble "X")
  @?=
  [("X",FPDouble,FVar FPDouble "Y"),("Y",FPDouble,FVar FPDouble "Z")]

localVarsInExpr5__test = testCase "localVarsInExpr5" $
  localVarsInExpr [("X",FPDouble, BinaryFPOp AddOp FPDouble (FInt 5) (FVar FPDouble "Y"))
                  ,("Y",FPDouble,FVar FPDouble "Z")]
                  (FVar FPDouble "X")
  @?=
  [("X",FPDouble,BinaryFPOp AddOp FPDouble (FInt 5) (FVar FPDouble "Y"))
  ,("Y",FPDouble,FVar FPDouble "Z")]

localVarsInExpr6__test = testCase "localVarsInExpr6" $
  localVarsInExpr [("X",FPDouble, BinaryFPOp AddOp FPDouble (FInt 5) (FVar FPDouble "Y"))
                  ,("Y",FPDouble, FEFun False "f" FPDouble [FVar FPDouble "Z"])]
                  (FVar FPDouble "X")
  @?=
  [("X",FPDouble,BinaryFPOp AddOp FPDouble (FInt 5) (FVar FPDouble "Y"))
  ,("Y",FPDouble,FEFun False "f" FPDouble [FVar FPDouble "Z"])]

localVarsInExpr7__test = testCase "localVarsInExpr7" $
  localVarsInExpr [("X", FPDouble, BinaryFPOp AddOp FPDouble (FInt 5) (FVar FPDouble "Y"))
                  ,("Y", FPDouble, BinaryFPOp AddOp FPDouble (FInt 5) (FVar FPDouble "Z"))
                  ,("Z", FPDouble, FInt 2)]
                  (FVar FPDouble "X")
  @?=
  [("X",FPDouble,BinaryFPOp AddOp FPDouble (FInt 5) (FVar FPDouble "Y"))
  ,("Y",FPDouble,BinaryFPOp AddOp FPDouble (FInt 5) (FVar FPDouble "Z"))
  ,("Z", FPDouble, FInt 2)]

localVarsInExpr8__test = testCase "localVarsInExpr8" $
  localVarsInExpr [("X", FPDouble, BinaryFPOp AddOp FPDouble (FVar FPDouble "Z") (FVar FPDouble "Y"))
                  ,("Y", FPDouble, FEFun False "f" FPDouble [FVar FPDouble "Z"])
                  ,("Z", FPDouble, FInt 2)
                  ,("F", FPDouble, FInt 5)]
                  (FVar FPDouble "X")
  @?=
  [("X",FPDouble,BinaryFPOp AddOp FPDouble (FVar FPDouble "Z") (FVar FPDouble "Y"))
  ,("Y",FPDouble,FEFun False "f" FPDouble [FVar FPDouble "Z"])
  ,("Z", FPDouble, FInt 2)]

localVarsInExpr9__test = testCase "localVarsInExpr9" $
  localVarsInExpr [("Y", FPDouble, FEFun False "f" FPDouble [FVar FPDouble "X"])
                  ,("X", FPDouble, FInt 2)
                  ,("F", FPDouble, FInt 5)]
                  (FVar FPDouble "Y")
  @?=
  [("Y",FPDouble,FEFun False "f" FPDouble [FVar FPDouble "X"])
  ,("X",FPDouble,FInt 2)]

localVarsInExpr10__test = testCase "localVarsInExpr10" $
  localVarsInExpr [("Z", FPDouble, FInt 2)
                  ,("Y", FPDouble, FEFun False "f" FPDouble [FVar FPDouble "X"])
                  ,("X", FPDouble, FVar FPDouble "Z")
                  ,("F", FPDouble, FInt 5)]
                  (BinaryFPOp AddOp FPDouble (FVar FPDouble "F") (FVar FPDouble "Y"))
  @?=
  [("Z", FPDouble, FInt 2)
  ,("Y", FPDouble, FEFun False "f" FPDouble [FVar FPDouble "X"])
  ,("X", FPDouble, FVar FPDouble "Z")
  ,("F", FPDouble, FInt 5)]

localVarsInExpr11__test = testCase "localVarsInExpr11" $
  localVarsInExpr [("Y", FPDouble, FEFun False "f" FPDouble [FVar FPDouble "X"])
                  ,("X", FPDouble, FVar FPDouble "Z")
                  ,("F", FPDouble, FInt 5)]
                  (BinaryFPOp AddOp FPDouble (FVar FPDouble "F") (FVar FPDouble "Y"))
  @?=
  [("Y", FPDouble, FEFun False "f" FPDouble [FVar FPDouble "X"])
  ,("X", FPDouble, FVar FPDouble "Z")
  ,("F", FPDouble, FInt 5)]

localVarsInExpr12__test = testCase "localVarsInExpr12" $
  localVarsInExpr [("THIS_x",FPDouble,BinaryFPOp SubOp FPDouble (FVar FPDouble "P_V1_x") (FVar FPDouble "S_x"))
                  ,("THIS_y",FPDouble,BinaryFPOp SubOp FPDouble (FVar FPDouble "P_V1_y") (FVar FPDouble "S_y"))
                  ,("NEXT_x",FPDouble,BinaryFPOp SubOp FPDouble (FVar FPDouble "P_V2_x") (FVar FPDouble "S_x"))
                  ,("NEXT_y",FPDouble,BinaryFPOp SubOp FPDouble (FVar FPDouble "P_V2_y") (FVar FPDouble "S_y"))
                  ,("DISTANCE_x",FPDouble,BinaryFPOp SubOp FPDouble (FVar FPDouble "NEXT_x") (FVar FPDouble "THIS_x"))
                  ,("DISTANCE_y",FPDouble,BinaryFPOp SubOp FPDouble (FVar FPDouble "NEXT_y") (FVar FPDouble "THIS_y"))
                  ,("DET",FPDouble,BinaryFPOp SubOp FPDouble (BinaryFPOp MulOp FPDouble (FVar FPDouble "DISTANCE_x") (FVar FPDouble "THIS_y")) (BinaryFPOp MulOp FPDouble (FVar FPDouble "DISTANCE_y") (FVar FPDouble "THIS_x")))
                  ,("K",TInt,TypeCast TInt FPDouble (FEFun True "quadrant" TInt [FVar FPDouble "THIS_x",FVar FPDouble "THIS_y"]))
                  ,("P",TInt,TypeCast TInt FPDouble (FEFun True "quadrant" TInt [FVar FPDouble "NEXT_x",FVar FPDouble "NEXT_y"]))]
                  (FVar FPDouble "DET")
  @?=
  [("THIS_x",FPDouble,BinaryFPOp SubOp FPDouble (FVar FPDouble "P_V1_x") (FVar FPDouble "S_x"))
   ,("THIS_y",FPDouble,BinaryFPOp SubOp FPDouble (FVar FPDouble "P_V1_y") (FVar FPDouble "S_y"))
   ,("NEXT_x",FPDouble,BinaryFPOp SubOp FPDouble (FVar FPDouble "P_V2_x") (FVar FPDouble "S_x"))
   ,("NEXT_y",FPDouble,BinaryFPOp SubOp FPDouble (FVar FPDouble "P_V2_y") (FVar FPDouble "S_y"))
   ,("DISTANCE_x",FPDouble,BinaryFPOp SubOp FPDouble (FVar FPDouble "NEXT_x") (FVar FPDouble "THIS_x"))
   ,("DISTANCE_y",FPDouble,BinaryFPOp SubOp FPDouble (FVar FPDouble "NEXT_y") (FVar FPDouble "THIS_y"))
   ,("DET",FPDouble,BinaryFPOp SubOp FPDouble (BinaryFPOp MulOp FPDouble (FVar FPDouble "DISTANCE_x") (FVar FPDouble "THIS_y")) (BinaryFPOp MulOp FPDouble (FVar FPDouble "DISTANCE_y") (FVar FPDouble "THIS_x")))
   ]

origDeclName__tests = testGroup "origDeclName tests"
  [origDeclName1__test
  ,origDeclName2__test
  ,origDeclName3__test
  ]

origDeclName1__test = testCase "The original declaration name of f_plus is f" $
  origDeclName "f_tauplus" @?= "f"

origDeclName2__test = testCase "The original declaration name of f_plus is f" $
  origDeclName "f_tauminus" @?= "f"

origDeclName3__test = testCase "The original declaration name of f_plus is f" $
  origDeclName "f" @?= "f"

transformStmSymb__tests = testGroup "transformStm tests"
  [transformStmSymbUnstWarning__tests
  ,transformStmSymbAexpr__tests
  ,transformStmSymbLet1__tests
  ,transformStmSymbIte__tests
  ,transformStmSymbIte2__tests
  ,transformStmSymbNestedIte__tests
  ,transformStmSymbNestedIte2__tests
  ,transformStmSymbListIte1__tests
  ,transformStmSymbListIte2__tests
  ,transformStmSymbListIte3__tests
  ]

transformStmSymbUnstWarning__tests = testCase "the symbolic transformation is correct for UnstWarning" $
  transformStmSymb [] [] "f" FBTrue True UnstWarning
  `returnsValueEqualTo'`
  UnstWarning

transformStmSymbAexpr__tests = testCase "the transformation is correct for AExpr" $
  transformStmSymb [] [] "f" FBTrue True (FVar FPDouble "x")
  `returnsValueEqualTo'`
   FVar FPDouble "x"

transformStmSymbLet1__tests = testCase "the transformation is correct for Let 1" $
  transformStmSymb [] [] "f" FBTrue True (Let [("x", FPDouble, (FInt 4))] (BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FVar FPDouble "y")))
  `returnsValueEqualTo'`
  Let [("x", FPDouble, (FInt 4))] (BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FVar FPDouble "y"))

transformStmSymbIte__tests = testCase "the transformation is correct for Ite with no rounding error" $
  transformStmSymb [] [] "f" FBTrue True
    (Ite (FRel LtE (FVar TInt "x") (FInt 0)) (FVar TInt "x") (FInt 4))
    `returnsValueEqualTo'`
     Ite (FRel LtE (FVar TInt "x") (FInt 0)) (FVar TInt "x") (FInt 4)

transformStmSymbIte2__tests = testCase "the transformation is correct for Ite with no rounding error" $
  transformStmSymb [] [] "f" FBTrue True
    (Ite (FRel LtE (BinaryFPOp AddOp FPDouble (FVar TInt "x") (FVar FPDouble "y")) (FInt 0)) (FVar TInt "x") (FInt 4))
    `returnsValueEqualTo'`
    Ite (FRel LtE (BinaryFPOp AddOp FPDouble (FVar TInt "x") (FVar FPDouble "y")) (UnaryFPOp NegOp FPDouble (FVar FPDouble "E_0"))) (FVar TInt "x")
    (Ite (FRel Gt (BinaryFPOp AddOp FPDouble (FVar TInt "x") (FVar FPDouble "y")) (FVar FPDouble "E_0")) (FInt 4) UnstWarning)

transformStmSymbNestedIte__tests = testCase "the transformation is correct for Ite 1" $
  transformStmSymb [] [] "f" FBTrue True
    (Ite (FRel LtE (FVar TInt "x") (FInt 0)) (FInt 1)
    (Ite (FRel LtE (FVar TInt "y") (FInt 0)) (FInt 2) (FInt 3)))
    `returnsValueEqualTo'`
    (Ite (FRel LtE (FVar TInt "x") (FInt 0)) (FInt 1)
    (Ite (FRel LtE (FVar TInt "y") (FInt 0)) (FInt 2) (FInt 3)))

transformStmSymbNestedIte2__tests = testCase "the transformation is correct for Ite 2" $
  transformStmSymb [] [] "f" FBTrue True
    (Ite (FRel LtE (FVar FPDouble "x") (FInt 0)) (FInt 1)
    (Ite (FRel LtE (FVar FPDouble "y") (FInt 0)) (FInt 2) (FInt 3)))
    `returnsValueEqualTo'`
     Ite (FRel LtE (FVar FPDouble "x") (UnaryFPOp NegOp FPDouble (FVar FPDouble "E_1"))) (FInt 1)
    (Ite (FRel Gt  (FVar FPDouble "x") (FVar FPDouble "E_1"))
    (Ite (FRel LtE (FVar FPDouble "y") (UnaryFPOp NegOp FPDouble (FVar FPDouble "E_0"))) (FInt 2)
    (Ite (FRel Gt  (FVar FPDouble "y") (FVar FPDouble "E_0")) (FInt 3) UnstWarning)) UnstWarning)

transformStmSymbListIte1__tests = testCase "the transformation is correct for ListIte 1" $
  transformStmSymb [] [] "f" FBTrue True
  (ListIte [(FRel LtE (FVar TInt "x") (FInt 0), (FInt 4)),(FRel LtE (FVar TInt "y") (FInt 0), (FInt 1))] (FInt 8))
  `returnsValueEqualTo'`
  (ListIte [(FRel LtE (FVar TInt "x") (FInt 0), (FInt 4))
           ,(FRel LtE (FVar TInt "y") (FInt 0), (FInt 1))]
           (FInt 8))

transformStmSymbListIte2__tests = testCase "the transformation is correct for ListIte 2" $
  transformStmSymb [] [] "f" FBTrue True
  (ListIte [(FRel LtE (FVar FPDouble "x") (FInt 0), (FInt 4)),
                             (FRel LtE (FVar FPDouble "y") (FInt 0), (FInt 1))] (FInt 8))
  `returnsValueEqualTo'`
  (ListIte [(FRel LtE (FVar FPDouble "x") (UnaryFPOp NegOp FPDouble (FVar FPDouble "E_0")), (FInt 4))
           ,(FAnd (FRel LtE (FVar FPDouble "y") (UnaryFPOp NegOp FPDouble (FVar FPDouble "E_1")))
                  (FRel Gt  (FVar FPDouble "x") (FVar FPDouble "E_0")), (FInt 1))
           ,(FAnd (FRel Gt  (FVar FPDouble "y") (FVar FPDouble "E_1"))
                  (FRel Gt  (FVar FPDouble "x") (FVar FPDouble "E_0")), (FInt 8))] UnstWarning)

transformStmSymbListIte3__tests = testCase "the transformation is correct for ListIte 2" $
  transformStmSymb [] [] "f" FBTrue True
  (ListIte [(FRel LtE (FVar FPDouble "x") (FInt 0), (FInt 4)),
                             (FRel LtE (FVar TInt "y") (FInt 0), (FInt 1)),
                             (FRel LtE (FVar FPDouble "z") (FInt 0), (FInt 2))] (FInt 8))
  `returnsValueEqualTo'`
  (ListIte [(FRel LtE (FVar FPDouble "x") (UnaryFPOp NegOp FPDouble (FVar FPDouble "E_0")), (FInt 4))
           ,(FAnd (FRel LtE (FVar TInt "y") (FInt 0))
                  (FRel Gt  (FVar FPDouble "x") (FVar FPDouble "E_0")), (FInt 1))
           ,(FAnd (FRel LtE (FVar FPDouble "z") (UnaryFPOp NegOp FPDouble (FVar FPDouble "E_1")))
                  (FRel Gt  (FVar FPDouble "x") (FVar FPDouble "E_0")), (FInt 2))
           ,(FAnd (FRel Gt  (FVar FPDouble "z") (FVar FPDouble "E_1"))
                  (FRel Gt  (FVar FPDouble "x") (FVar FPDouble "E_0")), (FInt 8))] UnstWarning)

generateVarName__tests = testGroup "generateVarName tests"
  [generateVarName1__test
  ,generateVarName2__test
  ,generateVarName3__test
  ]

generateVarName1__test = testCase "Empty error vars environment" $
  generateVarName (BinaryFPOp MulOp FPDouble (FVar FPDouble "X") (FVar FPDouble "X")) [] 0 FBTrue
  @?=
   ("E_0",[("E_0",BinaryFPOp MulOp FPDouble (FVar FPDouble "X") (FVar FPDouble "X"),FBTrue)],1)

generateVarName2__test = testCase "Expression in environment" $
  generateVarName (BinaryFPOp MulOp FPDouble (FVar FPDouble "X") (FVar FPDouble "X")) [("E_0",BinaryFPOp MulOp FPDouble (FVar FPDouble "X") (FVar FPDouble "X"),FBTrue)] 0 FBTrue
  @?=
   ("E_0",[("E_0",BinaryFPOp MulOp FPDouble (FVar FPDouble "X") (FVar FPDouble "X"),FBTrue)],0)

generateVarName3__test = testCase "Expression in environment" $
  generateVarName (FVar FPDouble "X") [("E_0",BinaryFPOp MulOp FPDouble (FVar FPDouble "X") (FVar FPDouble "X"),FBTrue)] 1 FBTrue
  @?=
   ("E_1",[("E_0",BinaryFPOp MulOp FPDouble (FVar FPDouble "X") (FVar FPDouble "X"),FBTrue), ("E_1", FVar FPDouble "X",FBTrue)],2)