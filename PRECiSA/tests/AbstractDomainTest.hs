-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module AbstractDomainTest where

import Common.ControlFlow
import Common.DecisionPath
import Test.Tasty
import Test.Tasty.HUnit
import AbsPVSLang
import AbstractDomain
import PVSTypes
import Operators

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
        conds  = Cond [(Rel Lt (Int 0) (Int 1),FRel Lt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrRat 1,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }
      aceb2 = ACeb {
        conds  = Cond [(Rel Lt (Int 1) (Int 2),FRel Lt (FInt 1) (FInt 2))],
        rExprs = [Int 4],
        fpExprs = [FInt 4],
        eExpr  = ErrRat 2,
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      expected = ACeb {
        conds  = Cond [(Rel Lt (Int 0) (Int 1),FRel Lt (FInt 0) (FInt 1)),(Rel Lt (Int 1) (Int 2),FRel Lt (FInt 1) (FInt 2))],
        rExprs = [Int 2,Int 4],
        fpExprs = [FInt 2,FInt 4],
        eExpr  = MaxErr [ErrRat 1, ErrRat 2],
        decisionPath = root ~> 1,
        cFlow  = Stable
      }


mergeACeb__test2 = testCase "the semantics fields are correctly merged 2" $
    mergeACeb aceb1 aceb2 @?= expected
    where
      aceb1 = ACeb {
        conds  = Cond [(Rel Lt (Int 0) (Int 1),FRel Lt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrRat 1,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }
      aceb2 = ACeb {
        conds  = Cond [(Rel Lt (Int 1) (Int 2),FRel Lt (FInt 1) (FInt 2))],
        rExprs = [Int 4],
        fpExprs = [FInt 4],
        eExpr  = ErrRat (2 :: Rational),
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Unstable
        }
      expected = ACeb {
        conds  = Cond [(Rel Lt (Int 0) (Int 1),FRel Lt (FInt 0) (FInt 1)),(Rel Lt (Int 1) (Int 2),FRel Lt (FInt 1) (FInt 2))],
        rExprs = [Int 2,Int 4],
        fpExprs = [FInt 2,FInt 4],
        eExpr  = MaxErr [ErrRat (1 :: Rational), ErrRat (2 :: Rational)],
        decisionPath = root ~> 1,
        cFlow  = Unstable
      }

mergeACeb__test3 = testCase "the semantics fields are correctly merged 3" $
    mergeACeb aceb1 aceb2 @?= expected
    where
      aceb1 = ACeb {
        conds  = Cond [(Rel Lt (Int 0) (Int 1),FRel Lt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrRat (1 :: Rational),
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Unstable
        }
      aceb2 = ACeb {
        conds  = Cond [(Rel Lt (Int 1) (Int 2),FRel Lt (FInt 1) (FInt 2))],
        rExprs = [Int 4],
        fpExprs = [FInt 2],
        eExpr  = ErrRat 2,
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Unstable
        }
      expected = ACeb {
        conds  = Cond [(Rel Lt (Int 0) (Int 1),FRel Lt (FInt 0) (FInt 1)),(Rel Lt (Int 1) (Int 2),FRel Lt (FInt 1) (FInt 2))],
        rExprs = [Int 2,Int 4],
        fpExprs = [FInt 2],
        eExpr  = MaxErr [ErrRat 1, ErrRat 2],
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
        conds  = Cond [(Rel Lt (Int 0) (Int 1),FRel Lt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrRat 1,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }
      aceb2 = ACeb {
        conds  = Cond [(Rel Lt (Int 1) (Int 2),FRel Lt (FInt 1) (FInt 2))],
        rExprs = [Int 4],
        fpExprs = [FInt 4],
        eExpr  = ErrRat 2,
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      aceb3 = ACeb {
        conds  = Cond [(Rel Lt (Int 1) (Int 2),FRel Lt (FInt 1) (FInt 2))],
        rExprs = [Var Real "X"],
        fpExprs = [FVar FPDouble "X"],
        eExpr  = ErrRat 2,
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      expected  = ACeb {
        conds   = Cond [(Rel Lt (Int 0) (Int 1),FRel Lt (FInt 0) (FInt 1)),(Rel Lt (Int 1) (Int 2),FRel Lt (FInt 1) (FInt 2))],
        rExprs  = [Int 2, Int 4, Var Real "X"],
        fpExprs = [FInt 2,FInt 4,FVar FPDouble "X"],
        eExpr   = MaxErr [MaxErr [ErrRat 1,ErrRat 2],ErrRat 2],
        decisionPath = root ~> 1,
        cFlow  = Stable
      }

mergeACebFold__test2 = testCase "the semantics fields of a list are correctly merged 2" $
    mergeACebFold [aceb1,aceb2,aceb3] @?= expected
    where
      aceb1 = ACeb {
        conds  = Cond [(Rel Lt (Int 0) (Int 1),FRel Lt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrRat 1,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Unstable
        }
      aceb2 = ACeb {
        conds  = Cond [(Rel Lt (Int 1) (Int 2),FRel Lt (FInt 1) (FInt 2))],
        rExprs = [Int 4],
        fpExprs = [FInt 2],
        eExpr  = ErrRat 2,
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      aceb3 = ACeb {
        conds  = Cond [(Rel Lt (Int 1) (Int 2),FRel Lt (FInt 1) (FInt 2))],
        rExprs = [Var Real "X"],
        fpExprs = [FVar FPDouble "X"],
        eExpr  = ErrRat 2,
        decisionPath = root ~> 0 ~> 1,
        cFlow  = Stable
        }
      expected  = ACeb {
        conds   = Cond [(Rel Lt (Int 0) (Int 1),FRel Lt (FInt 0) (FInt 1)),(Rel Lt (Int 1) (Int 2),FRel Lt (FInt 1) (FInt 2))],
        rExprs  = [Int 2, Int 4, Var Real "X"],
        fpExprs = [FInt 2,FVar FPDouble "X"],
        eExpr   = MaxErr [MaxErr [ErrRat 1,ErrRat 2],ErrRat 2],
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
        conds  = Cond [(Rel Lt (Int 0) (Int 1),FRel Lt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrRat 1,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }
      aceb2 = ACeb {
        conds  = Cond [(Rel Lt (Int 1) (Int 2),FRel Lt (FInt 1) (FInt 2))],
        rExprs = [Int 4],
        fpExprs = [FInt 4],
        eExpr  = ErrRat 2,
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      aceb3 = ACeb {
        conds  = Cond [(Rel Lt (Int 1) (Int 2),FRel Lt (FInt 1) (FInt 2))],
        rExprs = [Var Real "X"],
        fpExprs = [FVar FPDouble "X"],
        eExpr  = ErrRat 2,
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      expected  = [aceb1,aceb2,aceb3]

unionACebS__test2 = testCase "the semantics fields of a list are correctly merged 2" $
    unionACebS [aceb1,aceb2,aceb3] [aceb3] @?= expected
    where
      aceb1 = ACeb {
        conds  = Cond [(Rel Lt (Int 0) (Int 1),FRel Lt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrRat 1,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }
      aceb2 = ACeb {
        conds  = Cond [(Rel Lt (Int 1) (Int 2),FRel Lt (FInt 1) (FInt 2))],
        rExprs = [Int 4],
        fpExprs = [FInt 4],
        eExpr  = ErrRat 2,
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      aceb3 = ACeb {
        conds  = Cond [(Rel Lt (Int 1) (Int 2),FRel Lt (FInt 1) (FInt 2))],
        rExprs = [Var Real "X"],
        fpExprs = [FVar FPDouble "X"],
        eExpr  = ErrRat 2,
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
        conds  = Cond [(Rel Lt (Int 0) (Int 1),FRel Lt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrRat 1,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }

        expected = aceb

initErrAceb__test2 = testCase "init an integer error mark is (Int 0)" $
    initErrAceb aceb @?= expected
      where
        aceb = ACeb {
        conds  = Cond [(Rel Lt (Int 0) (Int 1),FRel Lt (FInt 0) (FInt 1))],
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
        conds  = Cond [(Rel Lt (Int 0) (Int 1),FRel Lt (FInt 0) (FInt 1))],
        rExprs = [Int 2],
        fpExprs = [FInt 2],
        eExpr  = ErrorMark "x" TInt,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }

        expected = aceb {eExpr = Int 0}

