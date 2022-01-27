-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module TransformationUtilsTest where

import Test.Tasty
import Test.Tasty.HUnit
import Transformation
import TransformationUtils
import AbsPVSLang
import PVSTypes
import AbsSpecLang
import Control.Monad.State
import Operators

returnsValueEqualTo :: (Eq a, Show a) => IO a -> a -> IO ()
returnsValueEqualTo lhs rhs = lhs >>= (@?= rhs)

returnsValueEqualTo':: State TranStateInterp FAExpr -> FAExpr -> IO ()
returnsValueEqualTo' lhs rhs = return (fst $ runState lhs initState) `returnsValueEqualTo` rhs
  where
    initState = [("f",TransState { freshErrVar = FreshErrVar { env = [], count = 0, localEnv = [] },
                                                     forExprMap = [] })]

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
    computeErrorGuards spec [] (decl, errVarEnv,localEnv',forMap)
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
      decl = Decl False FPDouble "f" [] (FInt 5)
      errVarEnv = [("E_x", FVar FPDouble "x", FBTrue)]
      spec = Spec [SpecBind "f" [VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)]]
      localEnv' = [("x",FPDouble,FVar FPDouble "y")]
      forMap = []


computeErrorGuards__test2 =
  testCase "" $
    computeErrorGuards spec [] (decl, errVarEnv,localEnv',forMap)
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
      ,BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FVar FPDouble "y")
      ,BinaryOp AddOp (RealMark "x") (RealMark "y")
      ,FBTrue
      ,ErrBinOp AddOp FPDouble (RealMark "y") (ErrorMark "y" FPDouble) (RealMark "y") (ErrorMark "y" FPDouble)
      ,1.7763568394002505e-15
      ,[FVar FPDouble "x",FVar FPDouble "y"]
      ,[RealMark "x",RealMark "y"]
      ,[ErrorMark "x" FPDouble,ErrorMark "y" FPDouble]
      ,[VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)])
      ])
 where
      decl = Decl False FPDouble "f" [] (FInt 5)
      errVarEnv = [("E_x", FVar FPDouble "x", FBTrue)
                  ,("E_y", BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FVar FPDouble "y"), FBTrue)]
      spec = Spec [SpecBind "g" [VarBind "z" FPDouble (LBDouble 7) (UBDouble 12)],
                   SpecBind "f" [VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)]]
      localEnv' = [("x",FPDouble,FVar FPDouble "y")]
      forMap = []

computeErrorVarValue_tests = testGroup "computeErrorVarValue tests"
  [computeErrorVarValue__test1
  ,computeErrorVarValue__test2
  ,computeErrorVarValue__test3
  ,computeErrorVarValue__test4
  ]
--
computeErrorVarValue__test1 =
  testCase "test1" $
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
  testCase "test2" $
    computeErrorVarValue [VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)] [] ("E_x", FVar FPDouble "x", FBTrue)
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
  testCase "test3" $
    computeErrorVarValue [VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)] []
                         ("E_x", BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FVar FPDouble "y"), FBTrue)
    `checkOutputIs`
    ("E_x"
    ,BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FVar FPDouble "y")
    ,BinaryOp AddOp (RealMark "x") (RealMark "y")
    ,FBTrue
    ,ErrBinOp AddOp FPDouble (RealMark "y") (ErrorMark "y" FPDouble) (RealMark "y") (ErrorMark "y" FPDouble)
    ,1.7763568394002505e-15
    ,[FVar FPDouble "x",FVar FPDouble "y"]
    ,[RealMark "x",RealMark "y"]
    ,[ErrorMark "x" FPDouble,ErrorMark "y" FPDouble]
    ,[VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)])

computeErrorVarValue__test4 =
  testCase "test4" $
    computeErrorVarValue [VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)] []
                         ("E_x", BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FVar FPDouble "y"), FBTrue)
    `checkOutputIs`
    ("E_x"
    ,BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FVar FPDouble "y")
    ,BinaryOp AddOp (RealMark "x") (RealMark "y")
    ,FBTrue
    ,ErrBinOp AddOp FPDouble (BinaryOp MulOp (RealMark "y") (RealMark "y"))
                     (ErrBinOp MulOp FPDouble (RealMark "y") (ErrorMark "y" FPDouble) (RealMark "y") (ErrorMark "y" FPDouble))
                     (RealMark "y") (ErrorMark "y" FPDouble)
    ,8.437694987151191e-15
    ,[FVar FPDouble "x",FVar FPDouble "y"]
    ,[RealMark "x",RealMark "y"]
    ,[ErrorMark "x" FPDouble,ErrorMark "y" FPDouble]
    ,[VarBind "y" FPDouble (LBDouble 2) (UBDouble 5)])

