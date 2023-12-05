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
  [mergeConds__tests
  ,mergeACeb__tests
  ,mergeACebFold__tests
  ,unionACebS__tests
  ,initErrAceb__tests
  ,isTrueCondition__tests
  ]

mergeConds__tests = testGroup "mergeConds tests"
  [mergeConds__test1
  ,mergeConds__test2
  ,mergeConds__test3
  ,mergeConds__test4
  ]

mergeConds__test1 = testCase "the conditions are correctly merged 1" $
    mergeConds cond1 cond2 @?= expected
    where
      cond1 = trueConds
      cond2 = trueConds
      expected = trueConds

mergeConds__test2 = testCase "the conditions are correctly merged 1" $
    mergeConds cond1 cond2 @?= expected
    where
      cond1 = Conds [Cond {realPathCond = BTrue
                          ,fpPathCond = FBTrue
                          ,realCond = Rel Lt (Int 0) (Int 1)
                          ,fpCond = FRel Lt (FInt 0) (FInt 1)}]
      cond2 = trueConds
      expected = Conds [Cond {realPathCond = BTrue
                          ,fpPathCond = FBTrue
                          ,realCond = Rel Lt (Int 0) (Int 1)
                          ,fpCond = FRel Lt (FInt 0) (FInt 1)}]

mergeConds__test3 = testCase "the conditions are correctly merged 1" $
    mergeConds cond1 cond2 @?= expected
    where
      cond1 = Conds [Cond {realPathCond = BTrue
                          ,fpPathCond = FBTrue
                          ,realCond = Rel Lt (Int 0) (Int 1)
                          ,fpCond = FRel Lt (FInt 0) (FInt 1)}]
      cond2 = Conds [Cond {realPathCond = BTrue
                          ,fpPathCond = FBTrue
                          ,realCond = Rel Lt (Int 0) (Int 1)
                          ,fpCond = FRel Lt (FInt 0) (FInt 1)}]
      expected = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 0) (Int 1)
                             ,fpCond = FRel Lt (FInt 0) (FInt 1)}]

mergeConds__test4 = testCase "the conditions are correctly merged 1" $
    mergeConds cond1 cond2 @?= expected
    where
      cond1 = Conds [Cond {realPathCond = BTrue
                          ,fpPathCond = FBTrue
                          ,realCond = Rel Lt (Int 0) (Int 1)
                          ,fpCond = FRel Lt (FInt 0) (FInt 1)}]
      cond2 = Conds [Cond {realPathCond = BTrue
                          ,fpPathCond = FBTrue
                          ,realCond = Rel Lt (Int 1) (Int 2)
                          ,fpCond = FRel Lt (FInt 1) (FInt 2)}]
      expected = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 0) (Int 1)
                             ,fpCond = FRel Lt (FInt 0) (FInt 1)}
                       ,Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 1) (Int 2)
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}]

mergeACeb__tests = testGroup "mergeACeb tests"
  [mergeACeb__test1
  ,mergeACeb__test2
  ,mergeACeb__test3
  ,mergeACeb__test4
  ,mergeACeb__test5
  ]

mergeACeb__test1 = testCase "the semantics fields are correctly merged 1" $
    mergeACeb aceb1 aceb2 @?= expected
    where
      aceb1 = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 0) (Int 1)
                             ,fpCond = FRel Lt (FInt 0) (FInt 1)}],
        rExprs = RDeclRes [Int 2],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $ ErrRat 1,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }
      aceb2 = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 1) (Int 2)
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}],
        rExprs = RDeclRes [Int 4],
        fpExprs = FDeclRes [FInt 4],
        eExpr  = Just $ ErrRat 2,
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      expected = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 0) (Int 1)
                             ,fpCond = FRel Lt (FInt 0) (FInt 1)}
                       ,Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 1) (Int 2)
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}],
        rExprs = RDeclRes [Int 2,Int 4],
        fpExprs = FDeclRes [FInt 2,FInt 4],
        eExpr  = Just $ MaxErr [ErrRat 1, ErrRat 2],
        decisionPath = root ~> 1,
        cFlow  = Stable
      }


mergeACeb__test2 = testCase "the semantics fields are correctly merged 2" $
    mergeACeb aceb1 aceb2 @?= expected
    where
      aceb1 = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 0) (Int 1)
                             ,fpCond = FRel Lt (FInt 0) (FInt 1)}],
        rExprs = RDeclRes [Int 2],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $ ErrRat 1,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }
      aceb2 = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 1) (Int 2)
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}],
        rExprs = RDeclRes [Int 4],
        fpExprs = FDeclRes [FInt 4],
        eExpr  = Just $ ErrRat (2 :: Rational),
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Unstable
        }
      expected = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 0) (Int 1)
                             ,fpCond = FRel Lt (FInt 0) (FInt 1)}
                       ,Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 1) (Int 2)
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}],
        rExprs = RDeclRes [Int 2,Int 4],
        fpExprs = FDeclRes [FInt 2,FInt 4],
        eExpr  = Just $ MaxErr [ErrRat (1 :: Rational), ErrRat (2 :: Rational)],
        decisionPath = root ~> 1,
        cFlow  = Unstable
      }

mergeACeb__test3 = testCase "the semantics fields are correctly merged 3" $
    mergeACeb aceb1 aceb2 @?= expected
    where
      aceb1 = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 0) (Int 1)
                             ,fpCond = FRel Lt (FInt 0) (FInt 1)}],
        rExprs = RDeclRes [Int 2],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $ ErrRat (1 :: Rational),
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Unstable
        }
      aceb2 = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 1) (Int 2)
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}],
        rExprs = RDeclRes [Int 4],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $ ErrRat 2,
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Unstable
        }
      expected = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 0) (Int 1)
                             ,fpCond = FRel Lt (FInt 0) (FInt 1)}
                       ,Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 1) (Int 2)
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}],
        rExprs = RDeclRes [Int 2,Int 4],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $ MaxErr [ErrRat 1, ErrRat 2],
        decisionPath = root ~> 1,
        cFlow  = Unstable
      }

mergeACeb__test4 = testCase "the semantics fields are correctly merged 4" $
    mergeACeb aceb1 aceb2 @?= expected
    where
      aceb1 = ACeb {
        conds  = trueConds,
        rExprs = RDeclRes [Int 2],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $ ErrRat (1 :: Rational),
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Unstable
        }
      aceb2 = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = BTrue
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}],
        rExprs = RDeclRes [Int 4],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $ ErrRat 2,
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Unstable
        }
      expected = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = BTrue
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}],
        rExprs = RDeclRes [Int 2,Int 4],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $ MaxErr [ErrRat 1, ErrRat 2],
        decisionPath = root ~> 1,
        cFlow  = Unstable
      }

mergeACeb__test5 = testCase "the semantics fields are correctly merged 4" $
    mergeACeb aceb1 aceb2 @?= expected
    where
      aceb1 = ACeb {
        conds  = trueConds,
        rExprs = RDeclRes [Int 2],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $ ErrRat (1 :: Rational),
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Unstable
        }
      aceb2 = ACeb {
        conds  = trueConds,
        rExprs = RDeclRes [Int 4],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $ ErrRat 2,
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Unstable
        }
      expected = ACeb {
        conds  = trueConds,
        rExprs = RDeclRes [Int 2,Int 4],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $ MaxErr [ErrRat 1, ErrRat 2],
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
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 0) (Int 1)
                             ,fpCond = FRel Lt (FInt 0) (FInt 1)}],
        rExprs = RDeclRes [Int 2],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $ ErrRat 1,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }
      aceb2 = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 1) (Int 2)
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}],
        rExprs = RDeclRes [Int 4],
        fpExprs = FDeclRes [FInt 4],
        eExpr  = Just $ ErrRat 2,
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      aceb3 = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 1) (Int 2)
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}],
        rExprs = RDeclRes [Var Real "X"],
        fpExprs = FDeclRes [FVar FPDouble "X"],
        eExpr  = Just $ ErrRat 2,
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      expected  = ACeb {
        conds   = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 0) (Int 1)
                             ,fpCond = FRel Lt (FInt 0) (FInt 1)}
                        ,Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 1) (Int 2)
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}],
        rExprs  = RDeclRes [Int 2, Int 4, Var Real "X"],
        fpExprs = FDeclRes [FInt 2,FInt 4,FVar FPDouble "X"],
        eExpr   = Just $ MaxErr [MaxErr [ErrRat 1,ErrRat 2],ErrRat 2],
        decisionPath = root ~> 1,
        cFlow  = Stable
      }

mergeACebFold__test2 = testCase "the semantics fields of a list are correctly merged 2" $
    mergeACebFold [aceb1,aceb2,aceb3] @?= expected
    where
      aceb1 = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 0) (Int 1)
                             ,fpCond = FRel Lt (FInt 0) (FInt 1)}],
        rExprs = RDeclRes [Int 2],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $ ErrRat 1,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Unstable
        }
      aceb2 = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 1) (Int 2)
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}],
        rExprs = RDeclRes [Int 4],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $ ErrRat 2,
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      aceb3 = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 1) (Int 2)
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}],
        rExprs = RDeclRes [Var Real "X"],
        fpExprs = FDeclRes [FVar FPDouble "X"],
        eExpr  = Just $ ErrRat 2,
        decisionPath = root ~> 0 ~> 1,
        cFlow  = Stable
        }
      expected  = ACeb {
        conds   = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 0) (Int 1)
                             ,fpCond = FRel Lt (FInt 0) (FInt 1)}
                        ,Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 1) (Int 2)
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}],
        rExprs  = RDeclRes [Int 2, Int 4, Var Real "X"],
        fpExprs = FDeclRes [FInt 2,FVar FPDouble "X"],
        eExpr   = Just $ MaxErr [MaxErr [ErrRat 1,ErrRat 2],ErrRat 2],
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
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 0) (Int 1)
                             ,fpCond = FRel Lt (FInt 0) (FInt 1)}],
        rExprs = RDeclRes [Int 2],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $ ErrRat 1,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }
      aceb2 = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 1) (Int 2)
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}],
        rExprs = RDeclRes [Int 4],
        fpExprs = FDeclRes [FInt 4],
        eExpr  = Just $ ErrRat 2,
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      aceb3 = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 1) (Int 2)
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}],
        rExprs = RDeclRes [Var Real "X"],
        fpExprs = FDeclRes [FVar FPDouble "X"],
        eExpr  = Just $ ErrRat 2,
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      expected  = [aceb1,aceb2,aceb3]

unionACebS__test2 = testCase "the semantics fields of a list are correctly merged 2" $
    unionACebS [aceb1,aceb2,aceb3] [aceb3] @?= expected
    where
      aceb1 = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 0) (Int 1)
                             ,fpCond = FRel Lt (FInt 0) (FInt 1)}],
        rExprs = RDeclRes [Int 2],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $ ErrRat 1,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }
      aceb2 = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 1) (Int 2)
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}],
        rExprs = RDeclRes [Int 4],
        fpExprs = FDeclRes [FInt 4],
        eExpr  = Just $ ErrRat 2,
        decisionPath = root ~> 1 ~> 1,
        cFlow  = Stable
        }
      aceb3 = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 1) (Int 2)
                             ,fpCond = FRel Lt (FInt 1) (FInt 2)}],
        rExprs = RDeclRes [Var Real "X"],
        fpExprs = FDeclRes [FVar FPDouble "X"],
        eExpr  = Just $ ErrRat 2,
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
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 0) (Int 1)
                             ,fpCond = FRel Lt (FInt 0) (FInt 1)}],
        rExprs = RDeclRes [Int 2],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $ ErrRat 1,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }

        expected = aceb

initErrAceb__test2 = testCase "init an integer error mark is (Int 0)" $
    initErrAceb aceb @?= expected
      where
        aceb = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 0) (Int 1)
                             ,fpCond = FRel Lt (FInt 0) (FInt 1)}],
        rExprs = RDeclRes [Int 2],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $  ErrorMark "x" TInt,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }

        expected = aceb {eExpr = Just $ Int 0}

initErrAceb__test3 = testCase "init an integer error mark is (Int 0)" $
    initErrAceb aceb @?= expected
      where
        aceb = ACeb {
        conds  = Conds [Cond {realPathCond = BTrue
                             ,fpPathCond = FBTrue
                             ,realCond = Rel Lt (Int 0) (Int 1)
                             ,fpCond = FRel Lt (FInt 0) (FInt 1)}],
        rExprs = RDeclRes [Int 2],
        fpExprs = FDeclRes [FInt 2],
        eExpr  = Just $ ErrorMark "x" TInt,
        decisionPath = root ~> 1 ~> 0 ~> 0,
        cFlow  = Stable
        }

        expected = aceb {eExpr = Just $ Int 0}

isTrueCondition__tests = testGroup "isTrueCondition tests"
  [isTrueCondition__test1
  ,isTrueCondition__test2
  ,isTrueCondition__test3
  ,isTrueCondition__test4
  ,isTrueCondition__test5
  ]

isTrueCondition__test1 = testCase "(true,true) is a true condition" $
    isTrueCondition (trueConds) @?= True

isTrueCondition__test2 = testCase "(true,false) is not a true condition" $
    isTrueCondition (Conds [Cond {realPathCond = BTrue
                                 ,fpPathCond = FBTrue
                                 ,realCond = BFalse
                                ,fpCond = FRel Lt (FInt 1) (FInt 2)}]) @?= False

isTrueCondition__test3 = testCase "(false,true) is not a true condition" $
    isTrueCondition (Conds [Cond {realPathCond = BFalse
                                 ,fpPathCond = FBTrue
                                 ,realCond = BTrue
                                ,fpCond = FBTrue}]) @?= False

isTrueCondition__test4 = testCase "(true,true) is a true condition" $
    isTrueCondition (Conds [Cond {realPathCond = Or BTrue BTrue
                                 ,fpPathCond = FBTrue
                                 ,realCond = Or BTrue BTrue
                                ,fpCond = FBTrue}]) @?= True

isTrueCondition__test5 = testCase "(true,true) is a true condition" $
    isTrueCondition (Conds [Cond {realPathCond = And BTrue BTrue
                                 ,fpPathCond = FBTrue
                                 ,realCond = Or BTrue BTrue
                                ,fpCond = FAnd FBTrue FBTrue}]) @?= True