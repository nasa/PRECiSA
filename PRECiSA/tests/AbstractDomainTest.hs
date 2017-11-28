module AbstractDomainTest where

import Common.ControlFlow
import Common.DecisionPath
import Common.PVSProof
import Data.Ratio
import Test.Tasty
import Test.Tasty.HUnit

import AbsPVSLang
import AbstractDomain
import FPrec


testAbstractDomain = testGroup "AbstractDomain" [mergeACeb__tests]


mergeACeb__tests = testGroup "semEFun tests"
  [mergeACeb__test1
  ]

mergeACeb__test1 = testCase "the semantics fields are correctly merged 1" $
    mergeACeb aceb1 aceb2 @?= expected
    where
      aceb1 = dummyACeb {
        conds  = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        eExpr  = ErrRat (toRational 1),
        decisionPath = root ~> True ~> False ~> False,
        cFlow  = Stable
        }
      aceb2 = dummyACeb {
        conds  = Cond [(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))],
        rExprs = [Int 4],
        eExpr  = ErrRat (toRational 2),
        decisionPath = root ~> True ~> True,
        cFlow  = Stable
        }
      expected = ACeb {
            conds  = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1)),(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))],
            rExprs = [Int 2,Int 4],
            eExpr  = MaxErr [ErrRat (toRational 1), ErrRat (toRational 2)],
            decisionPath = root ~> True,
            cFlow  = Stable,
            proof = proof aceb1
      }
