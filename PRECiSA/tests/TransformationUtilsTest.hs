module TransformationUtilsTest where

import Test.Tasty
import Test.Tasty.HUnit
import Transformation
import TransformationUtils
import AbsPVSLang
import FPrec
import AbsSpecLang
import Control.Monad.State

returnsValueEqualTo :: (Eq a, Show a) => IO a -> a -> IO ()
returnsValueEqualTo lhs rhs = lhs >>= (@?= rhs)

returnsValueEqualTo':: State ErrVarInterp Stm -> Stm -> IO ()
returnsValueEqualTo' lhs rhs = return (fst $ runState lhs initState) `returnsValueEqualTo` rhs
  where
    initState = [("f",FreshErrVar { env = [], count = 0, localEnv = [] })]

checkOutputIs :: (Eq a, Show a) => IO a -> a -> IO ()
checkOutputIs actual expected =
  actual >>=
      \out ->
          (out == expected)
              @? ("Output: " ++ show out ++ " is different than expected: " ++ show expected)

testTransformationUtils = testGroup "TransformationUtils"
  [computeErrorGuards__tests
  ,computeErrorVarValue_tests
  ]

computeErrorGuards__tests = testGroup "computeErrorGuards tests"
  [computeErrorGuards__test1
  ,computeErrorGuards__test2
  ]

computeErrorGuards__test1 =
  testCase "" $
    computeErrorGuards spec (decl, errVarEnv,localEnv) 
    `checkOutputIs`
    (decl
    ,[("E_x"
      ,FVar FPDouble "x"
      ,RealMark "x"
      ,FBTrue
      ,ErrorMark "y" FPDouble
      ,4.440892098500626e-16
      ,[FVar FPDouble "x"]
      ,[RealMark "x"]
      ,[ErrorMark "x" FPDouble]
      ,[VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)])])
    where
      decl = Decl FPDouble "f" [] (StmExpr (FInt 5))
      errVarEnv = [("E_x", FVar FPDouble "x", FBTrue)]
      spec = Spec [SpecBind "f" [VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)]]
      localEnv = [("x",FVar FPDouble "y")]


computeErrorGuards__test2 =
  testCase "" $
    computeErrorGuards spec (decl, errVarEnv,localEnv) 
    `checkOutputIs`
    (decl
    ,[("E_x"
      ,FVar FPDouble "x"
      ,RealMark "x"
      ,FBTrue
      ,ErrorMark "y" FPDouble
      ,4.440892098500626e-16
      ,[FVar FPDouble "x"]
      ,[RealMark "x"]
      ,[ErrorMark "x" FPDouble]
      ,[VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)]),
      ("E_y"
      ,FAdd FPDouble (FVar FPDouble "x") (FVar FPDouble "y")
      ,Add (RealMark "x") (RealMark "y")
      ,FBTrue
      ,ErrAdd FPDouble (RealMark "y") (ErrorMark "y" FPDouble) (RealMark "y") (ErrorMark "y" FPDouble)
      ,1.7763568394002505e-15
      ,[FVar FPDouble "x",FVar FPDouble "y"]
      ,[RealMark "x",RealMark "y"]
      ,[ErrorMark "x" FPDouble,ErrorMark "y" FPDouble]
      ,[VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)])
      ])
 where
      decl = Decl FPDouble "f" [] (StmExpr (FInt 5))
      errVarEnv = [("E_x", FVar FPDouble "x", FBTrue)
                   ,("E_y", FAdd FPDouble (FVar FPDouble "x") (FVar FPDouble "y"), FBTrue)]
      spec = Spec [SpecBind "g" [VarBind "z" FPDouble (LBDouble 7) (UBDouble 12)],
                   SpecBind "f" [VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)]]
      localEnv = [("x",FVar FPDouble "y")]

computeErrorVarValue_tests = testGroup "computeErrorVarValue tests"
  [computeErrorVarValue__test1
  ,computeErrorVarValue__test2
  ,computeErrorVarValue__test3
  ,computeErrorVarValue__test4
  ]
-- 
computeErrorVarValue__test1 =
  testCase "" $
    computeErrorVarValue [VarBind "x" FPDouble (LBDouble 2) (UBDouble 5)] [] ("E_x", FVar FPDouble "x", FBTrue)
    `checkOutputIs`
    ("E_x"
    ,FVar FPDouble "x"
    ,RealMark "x"
    ,FBTrue
    ,ErrorMark "x" FPDouble
    ,4.440892098500626e-16
    ,[FVar FPDouble "x"]
    ,[RealMark "x"]
    ,[ErrorMark "x" FPDouble]
    ,[VarBind "x" FPDouble (LBDouble 2) (UBDouble 5)])

computeErrorVarValue__test2 =
  testCase "" $
    computeErrorVarValue [VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)] [("x",FVar FPDouble "y")] ("E_x", FVar FPDouble "x", FBTrue)
    `checkOutputIs`
    ("E_x"
    ,FVar FPDouble "x"
    ,RealMark "x"
    ,FBTrue
    ,ErrorMark "y" FPDouble
    ,4.440892098500626e-16
    ,[FVar FPDouble "x"]
    ,[RealMark "x"]
    ,[ErrorMark "x" FPDouble]
    ,[VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)])

computeErrorVarValue__test3 =
  testCase "" $
    computeErrorVarValue [VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)]
                         [("x",FVar FPDouble "y")]
                         ("E_x", FAdd FPDouble (FVar FPDouble "x") (FVar FPDouble "y"), FBTrue)
    `checkOutputIs`
    ("E_x"
    ,FAdd FPDouble (FVar FPDouble "x") (FVar FPDouble "y")
    ,Add (RealMark "x") (RealMark "y")
    ,FBTrue
    ,ErrAdd FPDouble (RealMark "y") (ErrorMark "y" FPDouble) (RealMark "y") (ErrorMark "y" FPDouble)
    ,1.7763568394002505e-15
    ,[FVar FPDouble "x",FVar FPDouble "y"]
    ,[RealMark "x",RealMark "y"]
    ,[ErrorMark "x" FPDouble,ErrorMark "y" FPDouble]
    ,[VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)])

computeErrorVarValue__test4 =
  testCase "" $
    computeErrorVarValue [VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)]
                         [("x",FMul FPDouble (FVar FPDouble "y") (FVar FPDouble "y"))]
                         ("E_x", FAdd FPDouble (FVar FPDouble "x") (FVar FPDouble "y"), FBTrue)
    `checkOutputIs`
    ("E_x"
    ,FAdd FPDouble (FVar FPDouble "x") (FVar FPDouble "y")
    ,Add (RealMark "x") (RealMark "y")
    ,FBTrue
    ,ErrAdd FPDouble (Mul (RealMark "y") (RealMark "y"))
                     (ErrMul FPDouble (RealMark "y") (ErrorMark "y" FPDouble) (RealMark "y") (ErrorMark "y" FPDouble))
                     (RealMark "y") (ErrorMark "y" FPDouble)
    ,8.437694987151191e-15
    ,[FVar FPDouble "x",FVar FPDouble "y"]
    ,[RealMark "x",RealMark "y"]
    ,[ErrorMark "x" FPDouble,ErrorMark "y" FPDouble]
    ,[VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)])

