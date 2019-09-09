module TransformationTest where

import Test.Tasty
import Test.Tasty.HUnit
import Transformation
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

testTransformation = testGroup "Transformation"
  [transformProg__tests
  ,transformStmSymb__tests
--  ,betaPlus_tests
--  ,betaMinus_tests0
  ]



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
  transformStmSymb [] "f" FBTrue UnstWarning
  `returnsValueEqualTo'`
  UnstWarning

transformStmSymbAexpr__tests = testCase "the transformation is correct for AExpr" $
  transformStmSymb [] "f" FBTrue (StmExpr (FVar FPDouble "x"))
  `returnsValueEqualTo'`
  StmExpr (FVar FPDouble "x")

transformStmSymbLet1__tests = testCase "the transformation is correct for Let 1" $
  transformStmSymb [] "f" FBTrue (Let "x" FPDouble (FInt 4) (StmExpr $ FAdd FPDouble (FVar FPDouble "x") (FVar FPDouble "y")))
  `returnsValueEqualTo'`
  Let "x" FPDouble (FInt 4) (StmExpr $ FAdd FPDouble (FVar FPDouble "x") (FVar FPDouble "y"))

transformStmSymbIte__tests = testCase "the transformation is correct for Ite with no rounding error" $
  transformStmSymb [] "f" FBTrue
    (Ite (FLtE (FVar TInt "x") (FInt 0)) (StmExpr $ FVar TInt "x") (StmExpr $ FInt 4))
    `returnsValueEqualTo'`
    Ite (FLtE (FVar TInt "x") (FInt 0)) (StmExpr $ FVar TInt "x") (StmExpr $ FInt 4)

transformStmSymbIte2__tests = testCase "the transformation is correct for Ite with no rounding error" $
  transformStmSymb [] "f" FBTrue
    (Ite (FLtE (FAdd FPDouble (FVar TInt "x") (FVar FPDouble "y")) (FInt 0)) (StmExpr $ FVar TInt "x") (StmExpr $ FInt 4))
    `returnsValueEqualTo'`
    Ite (FLtE (FAdd FPDouble (FVar TInt "x") (FVar FPDouble "y")) (FNeg FPDouble (FVar FPDouble "E_0"))) (StmExpr $ FVar TInt "x")
    (Ite (FGt (FAdd FPDouble (FVar TInt "x") (FVar FPDouble "y")) (FVar FPDouble "E_0")) (StmExpr $ FInt 4) UnstWarning)

transformStmSymbNestedIte__tests = testCase "the transformation is correct for Ite 1" $
  transformStmSymb [] "f" FBTrue
    (Ite (FLtE (FVar TInt "x") (FInt 0)) (StmExpr $ FInt 1)
    (Ite (FLtE (FVar TInt "y") (FInt 0)) (StmExpr $ FInt 2) (StmExpr $ FInt 3)))
    `returnsValueEqualTo'`
    (Ite (FLtE (FVar TInt "x") (FInt 0)) (StmExpr $ FInt 1)
    (Ite (FLtE (FVar TInt "y") (FInt 0)) (StmExpr $ FInt 2) (StmExpr $ FInt 3)))

transformStmSymbNestedIte2__tests = testCase "the transformation is correct for Ite 2" $
  transformStmSymb [] "f" FBTrue
    (Ite (FLtE (FVar FPDouble "x") (FInt 0)) (StmExpr $ FInt 1)
    (Ite (FLtE (FVar FPDouble "y") (FInt 0)) (StmExpr $ FInt 2) (StmExpr $ FInt 3)))
    `returnsValueEqualTo'`
     Ite (FLtE (FVar FPDouble "x") (FNeg FPDouble (FVar FPDouble "E_1"))) (StmExpr (FInt 1))
    (Ite (FGt  (FVar FPDouble "x") (FVar FPDouble "E_1"))
    (Ite (FLtE (FVar FPDouble "y") (FNeg FPDouble (FVar FPDouble "E_0"))) (StmExpr (FInt 2))
    (Ite (FGt  (FVar FPDouble "y") (FVar FPDouble "E_0")) (StmExpr (FInt 3)) UnstWarning)) UnstWarning)

transformStmSymbListIte1__tests = testCase "the transformation is correct for ListIte 1" $
  transformStmSymb [] "f" FBTrue
  (ListIte [(FLtE (FVar TInt "x") (FInt 0), (StmExpr $ FInt 4)),(FLtE (FVar TInt "y") (FInt 0), (StmExpr $ FInt 1))] (StmExpr $ FInt 8))
  `returnsValueEqualTo'`
  (ListIte [(FLtE (FVar TInt "x") (FInt 0), (StmExpr $ FInt 4))
           ,(FLtE (FVar TInt "y") (FInt 0), (StmExpr $ FInt 1))]
           (StmExpr $ FInt 8))

transformStmSymbListIte2__tests = testCase "the transformation is correct for ListIte 2" $
  transformStmSymb [] "f" FBTrue
  (ListIte [(FLtE (FVar FPDouble "x") (FInt 0), (StmExpr $ FInt 4)),
                             (FLtE (FVar FPDouble "y") (FInt 0), (StmExpr $ FInt 1))] (StmExpr $ FInt 8))
  `returnsValueEqualTo'`
  (ListIte [(FLtE (FVar FPDouble "x") (FNeg FPDouble (FVar FPDouble "E_0")), (StmExpr $ FInt 4))
           ,(FAnd (FLtE (FVar FPDouble "y") (FNeg FPDouble (FVar FPDouble "E_1")))
                  (FGt  (FVar FPDouble "x") (FVar FPDouble "E_0")), (StmExpr $ FInt 1))
           ,(FAnd (FGt  (FVar FPDouble "y") (FVar FPDouble "E_1"))
                  (FGt  (FVar FPDouble "x") (FVar FPDouble "E_0")), (StmExpr $ FInt 8))] UnstWarning)

transformStmSymbListIte3__tests = testCase "the transformation is correct for ListIte 2" $
  transformStmSymb [] "f" FBTrue
  (ListIte [(FLtE (FVar FPDouble "x") (FInt 0), (StmExpr $ FInt 4)),
                             (FLtE (FVar TInt "y") (FInt 0), (StmExpr $ FInt 1)),
                             (FLtE (FVar FPDouble "z") (FInt 0), (StmExpr $ FInt 2))] (StmExpr $ FInt 8))
  `returnsValueEqualTo'`
  (ListIte [(FLtE (FVar FPDouble "x") (FNeg FPDouble (FVar FPDouble "E_0")), (StmExpr $ FInt 4))
           ,(FAnd (FLtE (FVar TInt "y") (FInt 0))
                  (FGt  (FVar FPDouble "x") (FVar FPDouble "E_0")), (StmExpr $ FInt 1))
           ,(FAnd (FLtE (FVar FPDouble "z") (FNeg FPDouble (FVar FPDouble "E_1")))
                  (FGt  (FVar FPDouble "x") (FVar FPDouble "E_0")), (StmExpr $ FInt 2))
           ,(FAnd (FGt  (FVar FPDouble "z") (FVar FPDouble "E_1"))
                  (FGt  (FVar FPDouble "x") (FVar FPDouble "E_0")), (StmExpr $ FInt 8))] UnstWarning)

--transformStm__tests = testGroup "transformStm tests"
--  [transformStmUnstWarning__tests
--  ,transformStmAexpr__tests
--  ,transformStmLet1__tests
--  ,transformStmLet2__tests
--  ,transformStmIte__tests
--  ,transformStmNestedIte__tests
--  ,transformStmListIte1__tests
--  ,transformStmListIte2__tests
--  ]

--transformStmUnstWarning__tests = testCase "the transformation is correct for UnstWarning" $
--  transformStm [] [] UnstWarning `returnsValueEqualTo` UnstWarning
  
--transformStmAexpr__tests = testCase "the transformation is correct for AExpr" $
--  transformStm [] [] (StmExpr (FVar FPDouble "x")) `returnsValueEqualTo` StmExpr (FVar FPDouble "x")

--transformStmLet1__tests = testCase "the transformation is correct for Let 1" $
--  transformStm [VarBind "x" FPDouble (LBInt 0) (UBInt 3)] [] (Let "x" FPDouble (FInt 4) (StmExpr $ FAdd FPDouble (FVar FPDouble "x") (FVar FPDouble "y")))
--  `returnsValueEqualTo`
--  Let "x" FPDouble (FInt 4) (StmExpr $ FAdd FPDouble (FVar FPDouble "x") (FVar FPDouble "y"))

--transformStmLet2__tests = testCase "the transformation is correct for Let 2" $
--  transformStm [VarBind "x" TInt (LBInt 0) (UBInt 3)] []
--    (Let "x" FPDouble (FInt 4)
--      (Ite (FLtE (FVar FPDouble "x") (FInt 0)) (StmExpr $ FVar FPDouble "x") (StmExpr $ FInt 4)))
--  `returnsValueEqualTo`
--    Let "x" FPDouble (FInt 4)
--      (Ite (FLtE (FVar FPDouble "x") (FCnst FPDouble (toRational 0))) (StmExpr $ FVar FPDouble "x")
--      (Ite (FGt  (FVar FPDouble "x") (FCnst FPDouble (toRational 0))) (StmExpr $ FInt 4) UnstWarning))

--transformStmIte__tests = testCase "the transformation is correct for Ite" $
--  transformStm [VarBind "x" TInt (LBInt 0) (UBInt 3)] []
--    (Ite (FLtE (FVar TInt "x") (FInt 0)) (StmExpr $ FVar TInt "x") (StmExpr $ FInt 4))
--    `returnsValueEqualTo`
--    Ite (FLtE (FVar TInt "x") (FCnst FPDouble (toRational 0))) (StmExpr $ FVar TInt "x") (Ite (FGt (FVar TInt "x") (FCnst FPDouble (toRational 0))) (StmExpr $ FInt 4) UnstWarning)

--transformStmNestedIte__tests = testCase "the transformation is correct for Ite" $
--  transformStm [VarBind "x" TInt (LBInt 0) (UBInt 3),VarBind "y" TInt (LBInt 0) (UBInt 8)] []
--    (Ite (FLtE (FVar TInt "x") (FInt 0)) (StmExpr $ FInt 1)
--    (Ite (FLtE (FVar TInt "y") (FInt 0)) (StmExpr $ FInt 2) (StmExpr $ FInt 3)))
--    `returnsValueEqualTo`
--     Ite (FLtE (FVar TInt "x") (FCnst FPDouble (toRational 0))) (StmExpr (FInt 1))
--    (Ite (FGt  (FVar TInt "x") (FCnst FPDouble (toRational 0)))
--    (Ite (FLtE (FVar TInt "y") (FCnst FPDouble (toRational 0))) (StmExpr (FInt 2))
--    (Ite (FGt  (FVar TInt "y") (FCnst FPDouble (toRational 0))) (StmExpr (FInt 3)) UnstWarning)) UnstWarning)

--transformStmListIte1__tests = testCase "the transformation is correct for ListIte 1" $
--  transformStm [VarBind "x" TInt (LBInt 0) (UBInt 3),VarBind "y" TInt (LBInt 0) (UBInt 8)] []
--  (ListIte [(FLtE (FVar TInt "x") (FInt 0), (StmExpr $ FInt 4))
--           ,(FLtE (FVar TInt "y") (FInt 0), (StmExpr $ FInt 1))]
--           (StmExpr $ FInt 8))
--  `returnsValueEqualTo`
--  (ListIte [(FLtE (FVar TInt "x") (FCnst FPDouble (toRational 0)), (StmExpr $ FInt 4))
--           ,(FAnd (FLtE (FVar TInt "y") (FCnst FPDouble (toRational 0)))
--                  (FGt  (FVar TInt "x") (FCnst FPDouble (toRational 0))), (StmExpr $ FInt 1))
--           ,(FAnd (FGt  (FVar TInt "y") (FCnst FPDouble (toRational 0)))
--                  (FGt  (FVar TInt "x") (FCnst FPDouble (toRational 0))), (StmExpr $ FInt 8))] UnstWarning)

--transformStmListIte2__tests = testCase "the transformation is correct for ListIte 2" $
--  transformStm [VarBind "x" TInt (LBInt 0) (UBInt 3)] []
--    (ListIte [(FLtE (FVar TInt "x") (FInt 0), (StmExpr $ FInt 4))] (StmExpr $ FInt 8))
--    `returnsValueEqualTo`
--    (ListIte [(FLtE (FVar TInt "x") (FCnst FPDouble (toRational 0)), (StmExpr $ FInt 4)),(FGt (FVar TInt "x") (FCnst FPDouble (toRational 0)),(StmExpr $ FInt 8))] UnstWarning)  


transformProg__tests = testGroup "transformProg tests"
  [
    -- transformProg1__tests
  ]

-- transformProg1__tests = testCase "the transformation is correct for a Program" $
--   transformProgramSymb [decl1,decl2,decl3]
--   @?=
--   [(expectedDecl1,[])
--   ,(expectedDecl2,[("E_0",FVar FPDouble "x",FBTrue),("E_1",FVar FPDouble "y",FBTrue)])
--   ,(expectedDecl3,[("E_0",FVar FPDouble "x",FBTrue)])]
--   where
--     decl1 = Decl FPDouble "f" [Arg "x" FPDouble]
--             (Ite (FLtE (FVar TInt "x") (FInt 0)) (StmExpr $ FVar TInt "x") (StmExpr $ FInt 4))
--     expectedDecl1 = Decl FPDouble "f" [Arg "x" FPDouble]
--                     (Ite (FLtE (FVar TInt "x") (FInt 0)) (StmExpr (FVar TInt "x")) (StmExpr (FInt 4)))
--     --
--     decl2 = Decl FPDouble "g" [Arg "x" FPDouble,Arg "y" FPDouble]
--              (ListIte [(FLtE (FVar FPDouble "x") (FCnst FPDouble (toRational 0)), (StmExpr $ FInt 4))
--                       ,(FLtE (FVar FPDouble "y") (FCnst FPDouble (toRational 0)), (StmExpr $ FInt 1))]
--                       (StmExpr $ FInt 8))
--     expectedDecl2 = Decl FPDouble "g" [Arg "x" FPDouble,Arg "y" FPDouble,Arg "E_0" FPDouble,Arg "E_1" FPDouble]
--                     (ListIte [(FLtE (FVar FPDouble "x") (FNeg FPDouble (FVar FPDouble "E_0")),StmExpr (FInt 4))
--                     ,(FAnd (FLtE (FVar FPDouble "y") (FNeg FPDouble (FVar FPDouble "E_1"))) (FGt (FVar FPDouble "x") (FVar FPDouble "E_0")),StmExpr (FInt 1))
--                     ,(FAnd (FGt  (FVar FPDouble "y") (FVar FPDouble "E_1")) (FGt (FVar FPDouble "x") (FVar FPDouble "E_0")),StmExpr (FInt 8))] UnstWarning)
--     --
--     decl3 = Decl FPDouble "h" [Arg "x" FPDouble]
--             (Ite (FLtE (FVar FPDouble "x") (FCnst FPDouble (toRational 0))) (StmExpr $ FVar TInt "x") (StmExpr $ FInt 4))
--     expectedDecl3 = Decl FPDouble "h" [Arg "x" FPDouble,Arg "E_0" FPDouble]
--                         (Ite (FLtE (FVar FPDouble "x") (FNeg FPDouble (FVar FPDouble "E_0"))) (StmExpr $ FVar TInt "x")
--                         (Ite (FGt  (FVar FPDouble "x") (FVar FPDouble "E_0")) (StmExpr $ FInt 4) UnstWarning))


