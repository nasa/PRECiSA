module Kodiak.ErrorComputationTest where

import AbsPVSLang
import AbsSpecLang
import Control.Exception (SomeException, evaluate, try)
import Kodiak.ErrorComputation (expandArrayArguments, expandArrays)
import Test.Tasty
import Test.Tasty.HUnit

kodiakErrorComputationTests :: TestTree
kodiakErrorComputationTests =
  testGroup
    "ErrorComputation"
    [ testExpandArrays,
      testExpandArrayArguments
    ]

_x :: AExpr
_x =
  ErrBinOp
    (ArrayDotOp 3)
    FPDouble
    (RealMark "x" ResValue)
    (HalfUlp (RealMark "x" ResValue) (ArrayOf 3 FPDouble))
    (RealMark "y" ResValue)
    (HalfUlp (RealMark "y" ResValue) (ArrayOf 3 FPDouble))

_xTransformed :: AExpr
_xTransformed =
  ErrBinOp
    AddOp
    FPDouble
    ( BinaryOp
        MulOp
        (RealMark "x" ResValue)
        (RealMark "y" ResValue)
    )
    ( ErrBinOp
        MulOp
        FPDouble
        (RealMark "x" ResValue)
        (HalfUlp (RealMark "x" ResValue) (ArrayOf 3 FPDouble))
        (RealMark "y" ResValue)
        (HalfUlp (RealMark "y" ResValue) (ArrayOf 3 FPDouble))
    )
    ( BinaryOp
        AddOp
        (BinaryOp MulOp (RealMark "x" ResValue) (RealMark "y" ResValue))
        (BinaryOp MulOp (RealMark "x" ResValue) (RealMark "y" ResValue))
    )
    ( ErrBinOp
        AddOp
        FPDouble
        (BinaryOp MulOp (RealMark "x" ResValue) (RealMark "y" ResValue))
        ( ErrBinOp
            MulOp
            FPDouble
            (RealMark "x" ResValue)
            (HalfUlp (RealMark "x" ResValue) (ArrayOf 3 FPDouble))
            (RealMark "y" ResValue)
            (HalfUlp (RealMark "y" ResValue) (ArrayOf 3 FPDouble))
        )
        ( BinaryOp
            MulOp
            (RealMark "x" ResValue)
            (RealMark "y" ResValue)
        )
        ( ErrBinOp
            MulOp
            FPDouble
            (RealMark "x" ResValue)
            (HalfUlp (RealMark "x" ResValue) (ArrayOf 3 FPDouble))
            (RealMark "y" ResValue)
            (HalfUlp (RealMark "y" ResValue) (ArrayOf 3 FPDouble))
        )
    )

testExpandArrays :: TestTree
testExpandArrays =
  testGroup
    "expandArrays tests"
    [ testCase "ArrayElem with Var of type Array of Doubles" $ do
        let input = ArrayElem FPDouble "x" [Int 2]
        result <- expandArrays input
        result
          @?= Var FPDouble "x_array_idx_2",
      testCase "with ArrayAddOp of length 1 (I)" $ do
        let input =
              ErrBinOp
                (ArrayAddOp 1)
                FPDouble
                (RealMark "x" ResValue)
                (HalfUlp (RealMark "x" ResValue) (ArrayOf 1 FPDouble))
                (RealMark "y" ResValue)
                (HalfUlp (RealMark "y" ResValue) (ArrayOf 1 FPDouble))
        result <- expandArrays input
        result
          @?=
            ErrBinOp
              AddOp
              FPDouble
              (RealMark "x_array_idx_0" ResValue)
              (HalfUlp (RealMark "x_array_idx_0" ResValue) FPDouble)
              (RealMark "y_array_idx_0" ResValue)
              (HalfUlp (RealMark "y_array_idx_0" ResValue) FPDouble),
      testCase "with ArrayAddOp of length 2 (I)" $ do
        let input =
              ErrBinOp
                (ArrayAddOp 2)
                FPDouble
                (RealMark "r1" ResValue)
                (HalfUlp (RealMark "r1" ResValue) (ArrayOf 2 FPDouble))
                (RealMark "r2" ResValue)
                (HalfUlp (RealMark "r2" ResValue) (ArrayOf 2 FPDouble))
        result <- expandArrays input
        result
          @?= ErrBinOp
            AddOp
            FPDouble
            (RealMark "r1_array_idx_0" ResValue)
            (HalfUlp (RealMark "r1_array_idx_0" ResValue) FPDouble)
            (RealMark "r2_array_idx_0" ResValue)
            (HalfUlp (RealMark "r2_array_idx_0" ResValue) FPDouble),
      testCase "with ArrayDotOp of length 1 (I)" $ do
        let input =
              ErrBinOp
                (ArrayDotOp 1)
                FPDouble
                (RealMark "x" ResValue)
                (HalfUlp (RealMark "x" ResValue) (ArrayOf 1 FPDouble))
                (RealMark "y" ResValue)
                (HalfUlp (RealMark "y" ResValue) (ArrayOf 1 FPDouble))
        result <- expandArrays input
        result
          @?=
            ErrBinOp
              MulOp
              FPDouble
              (RealMark "x_array_idx_0" ResValue)
              (HalfUlp (RealMark "x_array_idx_0" ResValue) FPDouble)
              (RealMark "y_array_idx_0" ResValue)
              (HalfUlp (RealMark "y_array_idx_0" ResValue) FPDouble),
      testCase "with ArrayDotOp of length 2 (I)" $ do
        let input =
              ErrBinOp
                (ArrayDotOp 2)
                FPDouble
                (RealMark "r1" ResValue)
                (HalfUlp (RealMark "r1" ResValue) (ArrayOf 2 FPDouble))
                (RealMark "r2" ResValue)
                (HalfUlp (RealMark "r2" ResValue) (ArrayOf 2 FPDouble))
        result <- expandArrays input
        result
          @?= ErrBinOp
            AddOp
            FPDouble
            ( BinaryOp
                MulOp
                (RealMark "r1_array_idx_0" ResValue)
                (RealMark "r2_array_idx_0" ResValue)
            )
            ( ErrBinOp
                MulOp
                FPDouble
                (RealMark "r1_array_idx_0" ResValue)
                (HalfUlp (RealMark "r1_array_idx_0" ResValue) FPDouble)
                (RealMark "r2_array_idx_0" ResValue)
                (HalfUlp (RealMark "r2_array_idx_0" ResValue) FPDouble)
            )
            ( BinaryOp
                MulOp
                (RealMark "r1_array_idx_1" ResValue)
                (RealMark "r2_array_idx_1" ResValue)
            )
            ( ErrBinOp
                MulOp
                FPDouble
                (RealMark "r1_array_idx_1" ResValue)
                (HalfUlp (RealMark "r1_array_idx_0" ResValue) FPDouble)
                (RealMark "r2_array_idx_1" ResValue)
                (HalfUlp (RealMark "r2_array_idx_0" ResValue) FPDouble)
            ),
      testCase "with ArrayDotOp of length 3 (I)" $ do
        let input =
              ErrBinOp
                (ArrayDotOp 3)
                FPDouble
                (Var (ArrayOf 3 FPDouble) "r1")
                (Var FPDouble "e1")
                (Var (ArrayOf 3 FPDouble) "r2")
                (Var FPDouble "e2")
        result <- expandArrays input
        result
          @?= ErrBinOp
            AddOp
            FPDouble
            (BinaryOp MulOp (Var FPDouble "r1_array_idx_0") (Var FPDouble "r2_array_idx_0"))
            ( ErrBinOp
                MulOp
                FPDouble
                (Var FPDouble "r1_array_idx_0")
                (Var FPDouble "e1")
                (Var FPDouble "r2_array_idx_0")
                (Var FPDouble "e2")
            )
            ( BinaryOp
                AddOp
                (BinaryOp MulOp (Var FPDouble "r1_array_idx_1") (Var FPDouble "r2_array_idx_1"))
                (BinaryOp MulOp (Var FPDouble "r1_array_idx_2") (Var FPDouble "r2_array_idx_2"))
            )
            ( ErrBinOp
                AddOp
                FPDouble
                (BinaryOp MulOp (Var FPDouble "r1_array_idx_1") (Var FPDouble "r2_array_idx_1"))
                ( ErrBinOp
                    MulOp
                    FPDouble
                    (Var FPDouble "r1_array_idx_1")
                    (Var FPDouble "e1")
                    (Var FPDouble "r2_array_idx_1")
                    (Var FPDouble "e2")
                )
                (BinaryOp MulOp (Var FPDouble "r1_array_idx_2") (Var FPDouble "r2_array_idx_2"))
                ( ErrBinOp
                    MulOp
                    FPDouble
                    (Var FPDouble "r1_array_idx_2")
                    (Var FPDouble "e1")
                    (Var FPDouble "r2_array_idx_2")
                    (Var FPDouble "e2")
                )
            ),
      testCase "with ArrayDotOp of length 3 (II)" $ do
        let input =
              ErrBinOp
                (ArrayDotOp 3)
                FPDouble
                (RealMark "r1" ResValue)
                (HalfUlp (RealMark "r1" ResValue) (ArrayOf 3 FPDouble))
                (RealMark "r2" ResValue)
                (HalfUlp (RealMark "r2" ResValue) (ArrayOf 3 FPDouble))
        result <- expandArrays input
        result
          @?= ErrBinOp
            AddOp
            FPDouble
            ( BinaryOp
                MulOp
                (RealMark "r1_array_idx_0" ResValue)
                (RealMark "r2_array_idx_0" ResValue)
            )
            ( ErrBinOp
                MulOp
                FPDouble
                (RealMark "r1_array_idx_0" ResValue)
                (HalfUlp (RealMark "r1_array_idx_0" ResValue) FPDouble)
                (RealMark "r2_array_idx_0" ResValue)
                (HalfUlp (RealMark "r2_array_idx_0" ResValue) FPDouble)
            )
            ( BinaryOp
                AddOp
                (BinaryOp MulOp (RealMark "r1_array_idx_1" ResValue) (RealMark "r2_array_idx_1" ResValue))
                (BinaryOp MulOp (RealMark "r1_array_idx_2" ResValue) (RealMark "r2_array_idx_2" ResValue))
            )
            ( ErrBinOp
                AddOp
                FPDouble
                (BinaryOp MulOp (RealMark "r1_array_idx_1" ResValue) (RealMark "r2_array_idx_1" ResValue))
                ( ErrBinOp
                    MulOp
                    FPDouble
                    (RealMark "r1_array_idx_1" ResValue)
                    (HalfUlp (RealMark "r1_array_idx_0" ResValue) FPDouble)
                    (RealMark "r2_array_idx_1" ResValue)
                    (HalfUlp (RealMark "r2_array_idx_0" ResValue) FPDouble)
                )
                ( BinaryOp
                    MulOp
                    (RealMark "r1_array_idx_2" ResValue)
                    (RealMark "r2_array_idx_2" ResValue)
                )
                ( ErrBinOp
                    MulOp
                    FPDouble
                    (RealMark "r1_array_idx_2" ResValue)
                    (HalfUlp (RealMark "r1_array_idx_0" ResValue) FPDouble)
                    (RealMark "r2_array_idx_2" ResValue)
                    (HalfUlp (RealMark "r2_array_idx_0" ResValue) FPDouble)
                )
            )
    ]

testExpandArrayArguments :: TestTree
testExpandArrayArguments =
  testGroup
    "expandArrayArguments tests"
    [ testCase "empty spec binds" $ do
        let input = []
        let result = expandArrayArguments input
        result @?= [],
      testCase "spec binds with arrays" $ do
        let input = [SpecBind "f" [VarBind "r" ResValue (ArrayOf 3 FPDouble) (LBInt 1) (UBInt 10)]]
        let result = expandArrayArguments input
        result
          @?= [ SpecBind
                  "f"
                  [ VarBind "r_array_idx_0" ResValue FPDouble (LBInt 1) (UBInt 10),
                    VarBind "r_array_idx_1" ResValue FPDouble (LBInt 1) (UBInt 10),
                    VarBind "r_array_idx_2" ResValue FPDouble (LBInt 1) (UBInt 10)
                  ]
              ],
      testCase "multiple spec binds" $ do
        let input = [SpecBind "g" [VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 5)],SpecBind "f" [VarBind "r" ResValue (ArrayOf 1 FPDouble) (LBInt 1) (UBInt 10)]]
        let result = expandArrayArguments input
        result
          @?= [ SpecBind
                  "g"
                  [ VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 5) ]
              , SpecBind
                  "f"
                  [ VarBind "r_array_idx_0" ResValue FPDouble (LBInt 1) (UBInt 10) ]
              ]
    ]
