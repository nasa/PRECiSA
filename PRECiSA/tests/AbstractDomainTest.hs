module AbstractDomainTest where

import Common.ControlFlow
import Common.DecisionPath
import Data.Ratio
import Test.Tasty
import Test.Tasty.HUnit

import AbsPVSLang
import AbstractDomain
import FPrec


testAbstractDomain = testGroup "AbstractDomain"
  [mergeACeb__tests
  ,mergeACebFold__tests
  ,unionACebS__tests
  ,initErrAceb__tests
  ]


mergeACeb__tests = testGroup "mergeACeb tests"
  [mergeACeb__test1
  ,mergeACeb__test2
  ,mergeACeb__test3
  ]

mergeACeb__test1 = testCase "the semantics fields are correctly merged 1" $
    mergeACeb aceb1 aceb2 @?= expected
    where
      aceb1 = ACeb {
        conds  = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrRat (toRational 1),
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }
      aceb2 = ACeb {
        conds  = Cond [(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))],
        rExprs = [Int 4],
        fpExprs = [FInt 4],
        eExpr  = ErrRat (toRational 2),
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      expected = ACeb {
        conds  = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1)),(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))],
        rExprs = [Int 2,Int 4],
        fpExprs = [FInt 2,FInt 4],
        eExpr  = MaxErr [ErrRat (toRational 1), ErrRat (toRational 2)],
        decisionPath = root ~> 1,
        cFlow  = Stable
      }


mergeACeb__test2 = testCase "the semantics fields are correctly merged 2" $
    mergeACeb aceb1 aceb2 @?= expected
    where
      aceb1 = ACeb {
        conds  = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrRat (toRational 1),
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }
      aceb2 = ACeb {
        conds  = Cond [(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))],
        rExprs = [Int 4],
        fpExprs = [FInt 4],
        eExpr  = ErrRat (toRational 2),
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Unstable
        }
      expected = ACeb {
        conds  = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1)),(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))],
        rExprs = [Int 2,Int 4],
        fpExprs = [FInt 2,FInt 4],
        eExpr  = MaxErr [ErrRat (toRational 1), ErrRat (toRational 2)],
        decisionPath = root ~> 1,
        cFlow  = Unstable
      }

mergeACeb__test3 = testCase "the semantics fields are correctly merged 3" $
    mergeACeb aceb1 aceb2 @?= expected
    where
      aceb1 = ACeb {
        conds  = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrRat (toRational 1),
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Unstable
        }
      aceb2 = ACeb {
        conds  = Cond [(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))],
        rExprs = [Int 4],
        fpExprs = [FInt 2],
        eExpr  = ErrRat (toRational 2),
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Unstable
        }
      expected = ACeb {
        conds  = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1)),(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))],
        rExprs = [Int 2,Int 4],
        fpExprs = [FInt 2],
        eExpr  = MaxErr [ErrRat (toRational 1), ErrRat (toRational 2)],
        decisionPath = root ~> 1,
        cFlow  = Unstable
      }

mergeACebFold__tests = testGroup "mergeACebFold tests"
  [mergeACebFold__test1
  ,mergeACebFold__test2
  ]

mergeACebFold__test1 = testCase "the semantics fields of a list are correctly merged 1" $
    mergeACebFold [aceb1,aceb2,aceb3] @?= expected
    where
      aceb1 = ACeb {
        conds  = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrRat (toRational 1),
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }
      aceb2 = ACeb {
        conds  = Cond [(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))],
        rExprs = [Int 4],
        fpExprs = [FInt 4],
        eExpr  = ErrRat (toRational 2),
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      aceb3 = ACeb {
        conds  = Cond [(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))],
        rExprs = [Var Real "X"],
        fpExprs = [FVar FPDouble "X"],
        eExpr  = ErrRat (toRational 2),
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      expected  = ACeb {
        conds   = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1)),(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))],
        rExprs  = [Int 2, Int 4, Var Real "X"],
        fpExprs = [FInt 2,FInt 4,FVar FPDouble "X"],
        eExpr   = MaxErr [MaxErr [ErrRat (toRational 1),ErrRat (toRational 2)],ErrRat (toRational 2)],
        decisionPath = root ~> 1,
        cFlow  = Stable
      }

mergeACebFold__test2 = testCase "the semantics fields of a list are correctly merged 2" $
    mergeACebFold [aceb1,aceb2,aceb3] @?= expected
    where
      aceb1 = ACeb {
        conds  = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrRat (toRational 1),
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Unstable
        }
      aceb2 = ACeb {
        conds  = Cond [(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))],
        rExprs = [Int 4],
        fpExprs = [FInt 2],
        eExpr  = ErrRat (toRational 2),
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      aceb3 = ACeb {
        conds  = Cond [(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))],
        rExprs = [Var Real "X"],
        fpExprs = [FVar FPDouble "X"],
        eExpr  = ErrRat (toRational 2),
        decisionPath = root ~> 0 ~> 1,
        cFlow  = Stable
        }
      expected  = ACeb {
        conds   = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1)),(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))],
        rExprs  = [Int 2, Int 4, Var Real "X"],
        fpExprs = [FInt 2,FVar FPDouble "X"],
        eExpr   = MaxErr [MaxErr [ErrRat (toRational 1),ErrRat (toRational 2)],ErrRat (toRational 2)],
        decisionPath = root,
        cFlow  = Unstable
      }

unionACebS__tests = testGroup "unionACebS tests"
  [unionACebS__test1
  ,unionACebS__test2
  ]

unionACebS__test1 = testCase "the semantics fields of a list are correctly merged 1" $
    unionACebS [aceb1,aceb2] [aceb3] @?= expected
    where
      aceb1 = ACeb {
        conds  = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrRat (toRational 1),
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }
      aceb2 = ACeb {
        conds  = Cond [(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))],
        rExprs = [Int 4],
        fpExprs = [FInt 4],
        eExpr  = ErrRat (toRational 2),
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      aceb3 = ACeb {
        conds  = Cond [(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))],
        rExprs = [Var Real "X"],
        fpExprs = [FVar FPDouble "X"],
        eExpr  = ErrRat (toRational 2),
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      expected  = [aceb1,aceb2,aceb3]
      
unionACebS__test2 = testCase "the semantics fields of a list are correctly merged 2" $
    unionACebS [aceb1,aceb2,aceb3] [aceb3] @?= expected
    where
      aceb1 = ACeb {
        conds  = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrRat (toRational 1),
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }
      aceb2 = ACeb {
        conds  = Cond [(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))],
        rExprs = [Int 4],
        fpExprs = [FInt 4],
        eExpr  = ErrRat (toRational 2),
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      aceb3 = ACeb {
        conds  = Cond [(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))],
        rExprs = [Var Real "X"],
        fpExprs = [FVar FPDouble "X"],
        eExpr  = ErrRat (toRational 2),
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      expected  = [aceb1,aceb2,aceb3]


initErrAceb__tests = testGroup "initErrAceb tests"
  [initErrAceb__test1
  ,initErrAceb__test2
  ,initErrAceb__test3
  ]

initErrAceb__test1 = testCase "init a constant error 1 is 1" $
    initErrAceb aceb @?= expected
      where
        aceb = ACeb {
        conds  = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrRat (toRational 1),
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }

        expected = aceb

initErrAceb__test2 = testCase "init an integer error mark is (Int 0)" $
    initErrAceb aceb @?= expected
      where
        aceb = ACeb {
        conds  = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrorMark "x" TInt,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }

        expected = aceb {eExpr = Int 0}

initErrAceb__test3 = testCase "init an integer error mark is (Int 0)" $
    initErrAceb aceb @?= expected
      where
        aceb = ACeb {
        conds  = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrorMark "x" TInt,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }

        expected = aceb {eExpr = Int 0}
