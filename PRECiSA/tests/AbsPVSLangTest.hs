module AbsPVSLangTest where

import Test.Tasty
import Test.Tasty.HUnit
import AbsPVSLang
import FPrec

testAbsPVSLang = testGroup "AbsPVSLang"
   [
    rewriteEquivEExpr__tests
   ,equivEExpr__tests
   ,replaceVarWithAExpr__tests
   ,substituteInAExpr__tests
   ,initErrorMark__tests
   ,initAExpr__tests
   ,initBExpr__tests
   ,funCallListFAExpr__tests
   ,funCallListFBExpr__tests
   ,noRoundOffErrorInAExpr__tests
   ,varList__tests
   ]


rewriteEquivEExpr__tests = testGroup "rewriteEquivEExpr tests" $  map (\(msg, i, o) -> testCase msg (rewriteEquivEExpr i @?= o)) rewriteEquivEExpr_testsIOs


rewriteEquivEExpr_testsIOs =
           [("ErrMulPow2L"
                ,ErrMulPow2L FPDouble 2 (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
                ,ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)
            )
            ,("ErrMulPow2R"
                ,ErrMulPow2R FPDouble 2 (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
                ,ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)
            )
            ,("ErrAbs"
                ,ErrAbs FPDouble (Int 3) (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
                ,ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)
            )
            ,("ErrAbs"
                ,ErrNeg FPDouble (Int 3) (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
                ,ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)
            )
            ,("MaxErr"
                ,MaxErr [ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble),
                         ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)]
                ,ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)
            )
            ,("MaxErr_ErrNeg"
                ,MaxErr [ErrNeg FPDouble (Int 3) (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)),
                         ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)]
                ,ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)
            )
            ,("ErrAdd_ErrMulPow2L"
                ,ErrAdd FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrSub FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                                 (RealMark "K") (ErrorMark "K" FPDouble)
                ,ErrAdd FPDouble (RealMark "Z") (ErrSub FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)) (RealMark "K") (ErrorMark "K" FPDouble)
            )
            ,("ErrAdd_ErrMulPow2L"
                ,ErrAdd FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrSub FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))) (RealMark "K") (ErrorMark "K" FPDouble)
                ,ErrAdd FPDouble (RealMark "Z") (ErrSub FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)) (RealMark "K") (ErrorMark "K" FPDouble)
            )
            ,("ErrSub_ErrMulPow2L"
                ,ErrSub FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                                 (RealMark "K") (ErrorMark "K" FPDouble)
                ,ErrSub FPDouble (RealMark "Z") (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)) (RealMark "K") (ErrorMark "K" FPDouble)
            )
            ,("ErrMul_ErrMulPow2L"
                ,ErrMul FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                                 (RealMark "K") (ErrorMark "K" FPDouble)
                ,ErrMul FPDouble (RealMark "Z") (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)) (RealMark "K") (ErrorMark "K" FPDouble)
            )
            ,("ErrDiv_ErrMulPow2L"
                ,ErrDiv FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                                                (RealMark "K") (ErrorMark "K" FPDouble)
                ,ErrDiv FPDouble (RealMark "Z") (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)) (RealMark "K") (ErrorMark "K" FPDouble)
            )
            ,("ErrFloor_ErrMulPow2L"
                ,ErrFloor FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrFloor FPDouble (RealMark "Z") (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)) 
            )
            ,("ErrFloor0_ErrMulPow2L"
                ,ErrFloor0 FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrFloor0 FPDouble (RealMark "Z") (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)) 
            )
            ,("ErrSqrt_ErrMulPow2L"
                ,ErrSqrt FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrSqrt FPDouble (RealMark "Z") (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)) 
            )
            ,("ErrSin_ErrMulPow2L"
                ,ErrSin FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrSin FPDouble (RealMark "Z") (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)) 
            )
            ,("ErrCos_ErrMulPow2L"
                ,ErrCos FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrCos FPDouble (RealMark "Z") (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
            )
            ,("ErrTan_ErrMulPow2L"
                ,ErrTan FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrTan FPDouble (RealMark "Z") (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)) 
            )
            ,("ErrAsin_ErrMulPow2L"
                ,ErrAsin FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrAsin FPDouble (RealMark "Z") (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
            )
            ,("ErrAcos_ErrMulPow2L"
                ,ErrAcos FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrAcos FPDouble (RealMark "Z") (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
            )
            ,("ErrAtan_ErrMulPow2L"
                ,ErrAtan FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrAtan FPDouble (RealMark "Z") (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
            ,("ErrAtanT_ErrMulPow2L"
                ,ErrAtanT FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrAtanT FPDouble (RealMark "Z") (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble))
            )
            ,("ErrLn_ErrMulPow2L"
                ,ErrLn FPDouble (RealMark "Z") (ErrMulPow2L FPDouble 2 (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)))
                ,ErrLn FPDouble (RealMark "Z") (ErrAdd FPDouble (RealMark "X") (ErrorMark "X" FPDouble) (RealMark "Y") (ErrorMark "Y" FPDouble)) 
            )
            ,("ErrExpo_ErrMulPow2L"
                ,ErrExpo FPSingle (RealMark "Z") (ErrMulPow2L FPSingle 2 (ErrAdd FPSingle (RealMark "X") (ErrorMark "X" FPSingle) (RealMark "Y") (ErrorMark "Y" FPSingle)))
                ,ErrExpo FPSingle (RealMark "Z") (ErrAdd FPSingle (RealMark "X") (ErrorMark "X" FPSingle) (RealMark "Y") (ErrorMark "Y" FPSingle)) 
            )
            ]


equivEExpr__tests = testGroup "equivEExpr tests" $  map (\(msg, ee1, ee2, o) -> testCase msg (equivEExpr ee1 ee2 @?= o)) equivEExpr_testsIOs

equivEExpr_testsIOs =
            [("ErrMulPow2L_Add"
                ,ErrMulPow2L FPSingle 2 (ErrAdd FPSingle (RealMark "X") (ErrorMark "X" FPSingle) (RealMark "Y") (ErrorMark "Y" FPSingle))
                ,ErrAdd FPSingle (RealMark "X") (ErrorMark "X" FPSingle) (RealMark "Y") (ErrorMark "Y" FPSingle)
                ,True
            )
            ,("ErrMul_Add"
                ,ErrMul FPSingle (RealMark "X") (ErrorMark "X" FPSingle) (RealMark "Y") (ErrorMark "Y" FPSingle)
                ,ErrAdd FPSingle (RealMark "X") (ErrorMark "X" FPSingle) (RealMark "Y") (ErrorMark "Y" FPSingle)
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
    replaceVarWithAExpr [("x",Int 4)] (Add (Var Real "y") (Var Real "x")) @?= Nothing

replaceVarWithAExpr__test3 = testCase "replace nothing in y + x = Nothing" $
    replaceVarWithAExpr [] (Add (Var Real "y") (Var Real "x")) @?= Nothing

replaceVarWithAExpr__test4 = testCase "replaceVar of x with 4 and y with 6 in x = Just 4" $
    replaceVarWithAExpr [("x",Int 4),("y",Int 6)] (Var Real "x") @?= Just (Int 4)


substituteInAExpr__tests = testGroup "replaceVarWithAExpr tests"
  [substituteInAExpr__test1
  ,substituteInAExpr__test2
  ,substituteInAExpr__test3
  ,substituteInBExpr__test4
  ,substituteInAExpr__test5
  ]

substituteInAExpr__test1 = testCase "replaceVar of x with 4 in x is Just 4" $
    substituteInAExpr [("x",Int 4)] (Var Real "x") @?= Int 4

substituteInAExpr__test2 = testCase "replaceVar of x with 4 in y+x is Nothing" $
    substituteInAExpr [("x",Int 4)] (Add (Var Real "y") (Var Real "x")) @?= Add (Var Real "y") (Int 4)

substituteInAExpr__test3 = testCase "replace nothing in y + x = Nothing" $
    substituteInAExpr [] (Add (Var Real "y") (Var Real "x")) @?= Add (Var Real "y") (Var Real "x")

substituteInAExpr__test4 = testCase "replaceVar of x with 4 and y with 6 in x = Just 4" $
    substituteInAExpr [("x",Int 4),("y",Int 6)] (Var Real "x") @?= Int 4

substituteInAExpr__test5 = testCase "replace nothing in y + x = Nothing" $
    substituteInAExpr [("x",Int 4),("y",Int 6)] (Add (Var Real "y") (Var Real "x")) @?= Add (Int 6) (Int 4)


substituteInBExpr__tests = testGroup "replaceVarWithAExpr tests"
  [substituteInBExpr__test1
  ,substituteInBExpr__test2
  ,substituteInBExpr__test3
  ,substituteInBExpr__test4
  ,substituteInBExpr__test5
  ]

substituteInBExpr__test1 = testCase "replaceVar of x with 4 in x is Just 4" $
    substituteInBExpr [("x",Int 4)] (Lt (Var Real "x") (Var Real "y")) @?= Lt (Int 4) (Var Real "y")

substituteInBExpr__test2 = testCase "replaceVar of x with 4 in y+x is Nothing" $
    substituteInBExpr [("x",Int 4)] (Gt (Add (Var Real "y") (Var Real "x")) (Int 6)) @?= Gt (Add (Var Real "y") (Int 4)) (Int 6)

substituteInBExpr__test3 = testCase "replace nothing in y + x = Nothing" $
    substituteInBExpr [] (Eq (Add (Var Real "y") (Var Real "x")) (Int 6)) @?= Eq (Add (Var Real "y") (Var Real "x")) (Int 6)

substituteInBExpr__test4 = testCase "replaceVar of x with 4 and y with 6 in x = Just 4" $
    substituteInBExpr [("x",Int 4),("y",Int 6)] (LtE (Var Real "x") (Int 0)) @?= LtE (Int 4) (Int 0)

substituteInBExpr__test5 = testCase "replace nothing in y + x = Nothing" $
    substituteInBExpr [("x",Int 4),("y",Int 6)] (GtE (Var Real "y") (Add (Var Real "y") (Var Real "x"))) @?= GtE (Int 6) (Add (Int 6) (Int 4))


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
    initErrorMark (Add (Var Real "y") (Var Real "x")) @?= Nothing

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
    initAExpr (Add (Var Real "y") (Var Real "x")) @?= (Add (Var Real "y") (Var Real "x"))

initAExpr__test4 = testCase "initAExpr of E(x_int)+x is half_ulp(R(x))+x" $
    initAExpr (Add (ErrorMark "x" FPDouble) (Var Real "x")) @?= (Add (HalfUlp (RealMark "x") FPDouble) (Var Real "x"))

initAExpr__test5 = testCase "initAExpr of StoR(RtoS(E(x)) is StoR(RtoS(half_ulp(R(x)))" $
    initAExpr (StoR(RtoS(ErrorMark "x" FPDouble))) @?= StoR(RtoS(HalfUlp (RealMark "x") FPDouble))


initBExpr__tests = testGroup "initBExpr tests"
  [initBExpr__test1
  ,initBExpr__test2
  ,initBExpr__test3
  ,initBExpr__test4
  ]

initBExpr__test1 = testCase "initBExpr of E(x_int) < 6 is 0 < 6" $
    initBExpr (Lt (ErrorMark "x" TInt) (Int 6)) @?= Lt (Int 0) (Int 6)

initBExpr__test2 = testCase "initBExpr of E(x_double) < 5 is half ulp R(x) < 5" $
    initBExpr (Gt (ErrorMark "x" FPDouble) (Int 5))  @?= Gt (HalfUlp (RealMark "x") FPDouble) (Int 5)

initBExpr__test3 = testCase "initBExpr of y+x < 9 and E(x_double) < 5 is y+x and half ulp R(x) < 5" $
    initBExpr (And (Lt (Add (Var Real "y") (Var Real "x")) (Int 9)) (Gt (ErrorMark "x" FPDouble) (Int 5)))
          @?= (And (Lt (Add (Var Real "y") (Var Real "x")) (Int 9)) (Gt (HalfUlp (RealMark "x") FPDouble) (Int 5)))

initBExpr__test4 = testCase "initBExpr of E(x_int) is 0" $
    initBExpr BTrue @?= BTrue

funCallListFAExpr__tests = testGroup "funCallListFAExpr tests"
  [funCallListFAExpr__test1
  ,funCallListFAExpr__test2
  ,funCallListFAExpr__test3
  ,funCallListFAExpr__test4
  ]

funCallListFAExpr__test1 = testCase "funCallList of 8 is []" $
    funCallListFAExpr (FInt 8) @?= []

funCallListFAExpr__test2 = testCase "funCallList of f() is [f()]" $
    funCallListFAExpr (FEFun "f" FPDouble []) @?= [FEFun "f" FPDouble []]

funCallListFAExpr__test3 = testCase "funCallList of f()+g(5) is [f(),g(5)]" $
    funCallListFAExpr (FAdd FPDouble (FEFun "f" FPDouble []) (FEFun "g" FPDouble [FInt 5])) @?= [FEFun "f" FPDouble [],FEFun "g" FPDouble [FInt 5]]

funCallListFAExpr__test4 = testCase "funCallList of f(g(5)) is [f(g(5)), g(5)]" $
    funCallListFAExpr (FEFun "f" FPDouble [FEFun "g" FPDouble [FInt 5]]) @?= [FEFun "f" FPDouble [FEFun "g" FPDouble [FInt 5]],FEFun "g" FPDouble [FInt 5]]



funCallListFBExpr__tests = testGroup "funCallListFBExpr tests"
  [funCallListFBExpr__test1
  ,funCallListFBExpr__test2
  ,funCallListFBExpr__test3
  ]

funCallListFBExpr__test1 = testCase "funCallList of f()<8 is [f()]" $
    funCallListFBExpr (FLt (FEFun "f" FPDouble []) (FInt 8)) @?= [FEFun "f" FPDouble []]

funCallListFBExpr__test2 = testCase "funCallList of f()>g(5) is [f(),g(5)]" $
    funCallListFBExpr (FGt (FEFun "f" FPDouble []) (FEFun "g" FPDouble [FInt 5])) @?= [FEFun "f" FPDouble [],FEFun "g" FPDouble [FInt 5]]

funCallListFBExpr__test3 = testCase "funCallList of f()<8 and f()>g(5) is [f(),g(5)]" $
    funCallListFBExpr (FAnd (FLt (FEFun "f" FPDouble []) (FInt 8)) (FGt (FEFun "f" FPDouble []) (FEFun "g" FPDouble [FInt 5]))) @?= [FEFun "f" FPDouble [],FEFun "g" FPDouble [FInt 5]]



noRoundOffErrorInAExpr__tests = testGroup "noRoundOffErrorInAExpr tests"
  [noRoundOffErrorInAExpr__test1
  ,noRoundOffErrorInAExpr__test2
  ,noRoundOffErrorInAExpr__test3
  ,noRoundOffErrorInAExpr__test4
  ,noRoundOffErrorInAExpr__test5
  ,noRoundOffErrorInAExpr__test6
  ,noRoundOffErrorInAExpr__test7
  ,noRoundOffErrorInAExpr__test8
  ]

noRoundOffErrorInAExpr__test1 = testCase "9 has no round-off error" $
    noRoundOffErrorInAExpr (FInt 8) @?= True

noRoundOffErrorInAExpr__test2 = testCase "0.1 has round-off error" $
    noRoundOffErrorInAExpr (FCnst FPDouble (toRational 0.1)) @?= False

noRoundOffErrorInAExpr__test3 = testCase "4+0.1 has round-off error" $
    noRoundOffErrorInAExpr (FAdd FPDouble (FInt 4) (FCnst FPDouble (toRational 0.1))) @?= False

noRoundOffErrorInAExpr__test4 = testCase "4+6 has no round-off error" $
    noRoundOffErrorInAExpr (FAdd FPDouble (FInt 4) (FInt 6)) @?= True

noRoundOffErrorInAExpr__test5 = testCase "4+int_x has no round-off error" $
    noRoundOffErrorInAExpr (FSub FPDouble (FInt 4) (FVar TInt "x")) @?= True

noRoundOffErrorInAExpr__test6 = testCase "4+x has round-off error" $
    noRoundOffErrorInAExpr (FMul FPDouble (FInt 4) (FVar FPDouble "x")) @?= False

noRoundOffErrorInAExpr__test7 = testCase "RtoD(4)+int_x has no round-off error" $
    noRoundOffErrorInAExpr (FSub FPDouble (RtoD (Int 4)) (FVar TInt "x")) @?= True    

noRoundOffErrorInAExpr__test8 = testCase "RtoS(4)+int_x has no round-off error" $
    noRoundOffErrorInAExpr (FSub FPDouble (RtoS (Int 4)) (FVar TInt "x")) @?= True    

varList__tests = testGroup "varList tests"
  [varList__test1
  ,varList__test2
  ,varList__test3
  ,varList__test4
  ,varList__test5
  ,varList__test6
  ,varList__test7
  ]

varList__test1 = testCase "varList of constant 0.1 is []" $
    varList (FCnst FPDouble (toRational 0.1)) @?= []

varList__test2 = testCase "varList of variable x is [Var x]" $
    varList (FVar FPDouble "x") @?= [FVar FPDouble "x"]

varList__test3 = testCase "varList of a (0.1 + x) is [Var x]" $
    varList (FAdd FPDouble (FCnst FPDouble (toRational 0.1)) (FVar FPDouble "x")) @?= [FVar FPDouble "x"]

varList__test4 = testCase "varList of a (y * x) is [Var x, Var y]" $
    varList (FMul FPDouble (FVar FPDouble "y") (FVar FPDouble "x")) @?= [FVar FPDouble "x", FVar FPDouble "y"]

varList__test5 = testCase "varList of a (0.1 + x)*x is [Var x]" $
    varList (FMul FPDouble (FAdd FPDouble (FCnst FPDouble (toRational 0.1)) (FVar FPDouble "x")) (FVar FPDouble "x"))
    @?= [FVar FPDouble "x"]

varList__test6 = testCase "varList of a min[(0.1 + x)*x, y, z] is [Var x, Var y, Var z]" $
    varList (FMin [(FMul FPDouble (FAdd FPDouble (FCnst FPDouble (toRational 0.1)) (FVar FPDouble "x"))
                                  (FVar FPDouble "x"))
                   ,FVar FPDouble "y"
                   ,FVar FPDouble "z"])
    @?= [FVar FPDouble "x", FVar FPDouble "y", FVar FPDouble "z"]

varList__test7 = testCase "varList of  floor(x)/f(y,z) is [Var x, Var y, Var z]" $
    varList (FDiv FPDouble (FFloor FPDouble (FVar FPDouble "x")) (FEFun "f" FPDouble [(FVar FPDouble "x"),(FVar FPDouble "z")]))
    @?= [FVar FPDouble "x", FVar FPDouble "z"]



                                           
