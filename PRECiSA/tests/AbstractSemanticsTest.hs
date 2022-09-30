-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module AbstractSemanticsTest where

import Common.ControlFlow
import Common.DecisionPath
import Data.Ratio
import Test.Tasty
import Test.Tasty.HUnit

import AbsPVSLang
import AbstractDomain
import AbstractSemantics hiding (functionSemantics)
import PVSTypes
import Operators
import Data.Set (fromList)

semEquiv :: ACebS -> ACebS -> IO()
semEquiv sem1 sem2 = fromList sem1 @?= fromList sem2

testAbstractSemantics = testGroup "AbstractSemantics"
    [semEFun__tests
    ,equivInterp__tests
    ,unfoldLocalVars__tests
    ,stmSem__tests
    -- ,unfoldFunCallInFBExpr__tests
    -- ,unfoldFunCallInBExpr__tests
    ,unfoldFunCallsInCond__tests
    ]

unfoldFunCallsInCond__tests = testGroup "unfoldFunCallsInCond"
  [ testCase "true condition returns true" $
      unfoldFunCallsInCond undefined (BTrue, FBTrue)
      @?=
      [(BTrue, FBTrue)]
  , testCase "condition with no function calls returns itself" $
      unfoldFunCallsInCond undefined (Rel Lt (Int 3) (Var Real "x"), FRel Lt (FInt 3) (FVar FPDouble "x"))
      @?=
      [(Rel Lt (Int 3) (Var Real "x"), FRel Lt (FInt 3) (FVar FPDouble "x"))]
  , unfoldFunCallsInCond__test1
  , unfoldFunCallsInCond__test2
  , unfoldFunCallsInCond__test3
  , unfoldFunCallsInCond__test4
  , unfoldFunCallsInCond__test5
  , unfoldFunCallsInCond__test6
  , unfoldFunCallsInCond__test7
  , unfoldFunCallsInCond__test8
  , unfoldFunCallsInCond__test9
  , unfoldFunCallsInCond__test10
  , unfoldFunCallsInCond__test11
  , unfoldFunCallsInCond__test12
  , unfoldFunCallsInCond__test13
  , unfoldFunCallsInCond__test14
  , unfoldFunCallsInCond__test15
  , unfoldFunCallsInCond__test16
  -- , unfoldFunCallsInCond__test17
  , unfoldFunCallsInCond__test18
  ]

unfoldFunCallsInCond__test1 = testCase "unfoldFunCallsInCond__test1" $
  unfoldFunCallsInCond interp (Rel Lt (Var Real "y") (Int 3),
                               FRel Lt (FEFun False "f" FPDouble [FVar FPDouble "z"]) (FInt 3))
  @?=
  [(Rel Lt (Var Real "y") (Int 3),
    FRel Lt (BinaryFPOp AddOp TInt (FVar FPDouble "z") (FInt 2)) (FInt 3))]
  where
    interp = [("f",(False, FPDouble, [Arg "x" FPDouble], [aceb]))]
    aceb = ACeb { conds   = Cond [(BTrue, FBTrue)],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = [BinaryFPOp AddOp TInt (FVar FPDouble "x") (FInt 2)],
          eExpr   =  ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test2 = testCase "unfoldFunCallsInCond__test2" $
  unfoldFunCallsInCond interp (Rel  Lt (Var Real "y") (Int 3),
                               FRel Lt (FEFun False "f" FPDouble [FVar FPDouble "z"]) (FInt 3))
  @?=
  [(Rel Lt (Var Real "y") (Int 3),
    FAnd (FRel Gt (FVar FPDouble "z") (FInt 0))
         (FRel Lt (BinaryFPOp AddOp TInt (FVar FPDouble "z") (FInt 2)) (FInt 3)))]
  where
    interp = [("f",(False, FPDouble, [Arg "x" FPDouble], [aceb]))]
    aceb = ACeb { conds   = Cond [(BTrue, FRel Gt (FVar FPDouble "x") (FInt 0))],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = [BinaryFPOp AddOp TInt (FVar FPDouble "x") (FInt 2)],
          eExpr   =  ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test3 = testCase "unfoldFunCallsInCond__test3" $
  unfoldFunCallsInCond interp (Rel Lt (Var Real "y") (Int 3),
                               FRel Lt (FEFun False "f" FPDouble [FVar FPDouble "z"]) (FInt 3))
  @?=
  [(Rel Lt (Var Real "y") (Int 3),
    FAnd (FRel Gt (FVar FPDouble "z") (FInt 0))
         (FRel Lt (BinaryFPOp AddOp TInt (FVar FPDouble "z") (FInt 2)) (FInt 3)))
  , (Rel Lt (Var Real "y") (Int 3),
        (FRel Lt (BinaryFPOp MulOp TInt (FVar FPDouble "z") (FInt 2)) (FInt 3)))
  ]
  where
    interp = [("f",(False, FPDouble, [Arg "x" FPDouble], [aceb1,aceb2]))]
    aceb1 = ACeb { conds   = Cond [(BTrue, FRel Gt (FVar FPDouble "x") (FInt 0))],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = [BinaryFPOp AddOp TInt (FVar FPDouble "x") (FInt 2)],
          eExpr   =  ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }
    aceb2 = ACeb { conds   = Cond [(BTrue, FBTrue)],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = [BinaryFPOp MulOp TInt (FVar FPDouble "x") (FInt 2)],
          eExpr   =  ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test4 = testCase "unfoldFunCallsInCond__test4" $
  unfoldFunCallsInCond interp (Rel Lt (Var Real "y") (Int 3),
                               FRel Lt (FEFun False "f" FPDouble [FVar FPDouble "z"])
                                   (FEFun False "g" FPDouble [FVar FPDouble "h"]))
  @?=
  [(Rel Lt (Var Real "y") (Int 3),
    FAnd (FRel Gt (FVar FPDouble "z") (FInt 0))
         (FRel Lt (BinaryFPOp AddOp TInt (FVar FPDouble "z") (FInt 2))
              (BinaryFPOp MulOp TInt (FVar FPDouble "h") (FInt 2))))]
  where
    interp = [("f",(False, FPDouble, [Arg "x" FPDouble], [acebf])),
              ("g",(False, FPDouble, [Arg "x" FPDouble], [acebg]))]
    acebf = ACeb { conds = Cond [(BTrue, FRel Gt (FVar FPDouble "x") (FInt 0))],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = [BinaryFPOp AddOp TInt (FVar FPDouble "x") (FInt 2)],
          eExpr   =  ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }
    acebg = ACeb { conds = Cond [(BTrue, FBTrue)],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = [BinaryFPOp MulOp TInt (FVar FPDouble "x") (FInt 2)],
          eExpr   =  ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test5 = testCase "unfoldFunCallsInCond__test5" $
  unfoldFunCallsInCond interp (Rel  Lt (Var Real "y") (Int 3),
                               FRel Lt (FEFun False "f" FPDouble [FVar FPDouble "z"])
                                       (FEFun False "g" FPDouble [FVar FPDouble "h"]))
  @?=
   [(Rel Lt (Var Real "y") (Int 3)
   ,FAnd (FRel Gt (FVar FPDouble "z") (FInt 0))
         (FAnd (FRel Lt (FVar FPDouble "h") (FInt 10))
               (FRel Lt (BinaryFPOp AddOp TInt (FVar FPDouble "z") (FInt 2))
                        (BinaryFPOp MulOp TInt (FVar FPDouble "h") (FInt 2)))))]
  where
    interp = [("f",(False, FPDouble, [Arg "x" FPDouble], [acebf])),
              ("g",(False, FPDouble, [Arg "x" FPDouble], [acebg]))]
    acebf = ACeb { conds = Cond [(BTrue, FRel Gt (FVar FPDouble "x") (FInt 0))],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = [BinaryFPOp AddOp TInt (FVar FPDouble "x") (FInt 2)],
          eExpr   =  ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }
    acebg = ACeb { conds = Cond [(BTrue, FRel Lt (FVar FPDouble "x") (FInt 10))],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = [BinaryFPOp MulOp TInt (FVar FPDouble "x") (FInt 2)],
          eExpr   =  ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test6 = testCase "unfoldFunCallsInCond__test6" $
  unfoldFunCallsInCond interp (Rel  Lt (EFun "f" Real [Var Real "z"]) (Int 3),
                               FRel Lt (FEFun False "f" FPDouble [FVar FPDouble "z"]) (FInt 1))
  @?=
  [(Rel  Lt (BinaryOp AddOp (Var Real "z") (Int 2)) (Int 3)
   ,FRel Lt (BinaryFPOp AddOp FPDouble (FVar FPDouble "z") (FInt 2)) (FInt 1))]
  where
    interp = [("f",(False, FPDouble, [Arg "x" FPDouble], [aceb]))]
    aceb = ACeb { conds   = Cond [(BTrue, FBTrue)],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   =  ErrBinOp AddOp FPDouble (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test7 = testCase "unfoldFunCallsInCond__test7" $
  unfoldFunCallsInCond interp (Rel  Lt (EFun "f" Real [Var Real "z"]) (Int 3),
                               FRel Lt (FEFun False "f" FPDouble [FVar FPDouble "z"]) (FInt 1))
  @?=
  [(And (Rel Lt (Var Real "z") (Int 5))
        (Rel Lt (BinaryOp AddOp (Var Real "z") (Int 2)) (Int 3))
  , FAnd (FRel Gt (FVar FPDouble "z") (FInt 0))
         (FRel Lt (BinaryFPOp AddOp FPDouble (FVar FPDouble "z") (FInt 2)) (FInt 1)))]
  where
    interp = [("f",(False, FPDouble, [Arg "x" FPDouble], [aceb]))]
    aceb = ACeb { conds   = Cond [(Rel Lt (Var Real "x") (Int 5), FRel Gt (FVar FPDouble "x") (FInt 0))],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   =  ErrBinOp AddOp FPDouble (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test8 = testCase "unfoldFunCallsInCond__test8" $
  unfoldFunCallsInCond interp (Rel Lt (EFun "f" Real [Var Real "z"])
                                  (EFun "g" Real [Var Real "h"])
                              ,FRel Lt (FEFun False "f" FPDouble [FVar FPDouble "z"])
                                   (FEFun False "g" FPDouble [FVar FPDouble "h"]))
  @?=
   [(And (Rel Lt (Var Real "z") (Int 5))
         (And (Rel Gt (Var Real "h") (Int 0))
              (Rel Lt (BinaryOp AddOp (Var Real "z") (Int 2))
                      (BinaryOp MulOp (Var Real "h") (Int 2))))
   ,FAnd (FRel Gt (FVar FPDouble "z") (FInt 0))
         (FAnd (FRel Gt (FVar FPDouble "h") (FInt 0))
               (FRel Lt (BinaryFPOp AddOp FPDouble (FVar FPDouble "z") (FInt 2))
                        (BinaryFPOp MulOp FPDouble (FVar FPDouble "h") (FInt 2)))))]
  where
    interp = [("f",(False, FPDouble, [Arg "x" FPDouble], [acebf]))
             ,("g",(False, FPDouble, [Arg "x" FPDouble], [acebg]))]
    acebf = ACeb { conds   = Cond [(Rel Lt (Var Real "x") (Int 5), FRel Gt (FVar FPDouble "x") (FInt 0))],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   =  ErrBinOp AddOp FPDouble (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }
    acebg = ACeb { conds = Cond [(Rel Gt (Var Real "x") (Int 0), FRel Gt (FVar FPDouble "x") (FInt 0))],
          rExprs  = [BinaryOp MulOp (Var Real "x") (Int 2)],
          fpExprs = [BinaryFPOp MulOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   =  ErrBinOp MulOp FPDouble (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test9 = testCase "unfoldFunCallsInCond__test9" $
  unfoldFunCallsInCond interp (Rel Lt (EFun "f" Real [Var Real "z"])
                                  (EFun "g" Real [Var Real "h"])
                              ,FRel Lt (FEFun False "f" FPDouble [FVar FPDouble "z"])
                                   (FEFun False "g" FPDouble [FVar FPDouble "h"]))
  @?=
  [cond1,cond2]
  where
    interp = [("f",(False, FPDouble, [Arg "x" FPDouble], [acebf1,acebf2]))
             ,("g",(False, FPDouble, [Arg "x" FPDouble], [acebg]))]
    acebf1 = ACeb { conds   = Cond [(BTrue, FRel Gt (FVar FPDouble "x") (FInt 0))],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   =  ErrBinOp AddOp FPDouble (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }
    acebf2 = ACeb { conds   = Cond [(BTrue, FBTrue)],
          rExprs  = [BinaryOp SubOp (Var Real "x") (Int 3)],
          fpExprs = [BinaryFPOp SubOp FPDouble (FVar FPDouble "x") (FInt 3)],
          eExpr   =  ErrBinOp SubOp FPDouble (Var Real "x") (ErrorMark "x" FPDouble) (Int 3) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }
    acebg = ACeb { conds = Cond [(Rel Gt (Var Real "x") (Int 0), FRel Gt (FVar FPDouble "x") (FInt 0))],
          rExprs  = [BinaryOp MulOp (Var Real "x") (Int 4)],
          fpExprs = [BinaryFPOp MulOp FPDouble (FVar FPDouble "x") (FInt 4)],
          eExpr   =  ErrBinOp MulOp FPDouble (Var Real "x") (ErrorMark "x" FPDouble) (Int 4) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }
    cond1 = (And (Rel Gt (Var Real "h") (Int 0))
                 (Rel Lt (BinaryOp AddOp (Var Real "z") (Int 2)) (BinaryOp MulOp (Var Real "h") (Int 4)))
            ,FAnd (FRel Gt (FVar FPDouble "z") (FInt 0))
                  (FAnd (FRel Gt (FVar FPDouble "h") (FInt 0))
                        (FRel Lt (BinaryFPOp AddOp FPDouble (FVar FPDouble "z") (FInt 2))
                                 (BinaryFPOp MulOp FPDouble (FVar FPDouble "h") (FInt 4)))))
    cond2 = (And (Rel Gt (Var Real "h") (Int 0))
                 (Rel Lt (BinaryOp SubOp (Var Real "z") (Int 3))
                         (BinaryOp MulOp (Var Real "h") (Int 4)))
            ,FAnd (FRel Gt (FVar FPDouble "h") (FInt 0))
                  (FRel Lt (BinaryFPOp SubOp FPDouble (FVar FPDouble "z") (FInt 3))
                           (BinaryFPOp MulOp FPDouble (FVar FPDouble "h") (FInt 4))))

unfoldFunCallsInCond__test10 = testCase "unfoldFunCallsInCond__test10" $
  unfoldFunCallsInCond interp (Rel Lt (Var Real "y") (Int 3),
                               FRel Lt (FEFun False "f" FPDouble [FVar FPDouble "z"]) (FInt 3))
  @?=
  [(Rel Lt (Var Real "y") (Int 3),
    FOr (FRel Lt (FVar FPDouble "z") (FInt 3)) (FRel Lt (FVar FPDouble "y") (FInt 3)))]
  where
    interp = [("f",(False, FPDouble, [Arg "x" FPDouble], [aceb]))]
    aceb = ACeb { conds   = Cond [(BTrue, FBTrue)],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = [FVar FPDouble "x", FVar FPDouble "y"],
          eExpr   =  ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test11 = testCase "unfoldFunCallsInCond__test11" $
  unfoldFunCallsInCond interp (Rel Lt (Var Real "y") (Int 3),
                               FRel Lt (FEFun False "f" FPDouble [FVar FPDouble "z"]) (FInt 3))
  @?=
  [(Rel Lt (Var Real "y") (Int 3),
    FOr (FRel Lt (FVar FPDouble "z") (FInt 3)) (FRel Lt (FVar FPDouble "i") (FInt 3)))]
  where
    interp = [("f",(False, FPDouble, [Arg "x" FPDouble], [aceb]))]
    aceb = ACeb { conds   = Cond [(BTrue, FBTrue)],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2), (Var Real "x")],
          fpExprs = [FVar FPDouble "x", FVar FPDouble "i"],
          eExpr   =  ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test12 = testCase "unfoldFunCallsInCond__test12" $
  unfoldFunCallsInCond interp (Rel  Lt (EFun "f" Real [Var Real "z"]) (Int 3)
                              ,FRel Lt (FVar FPDouble "y") (FInt 3))
  @?=
  [(Rel  Lt (BinaryOp AddOp (Var Real "z") (Int 2)) (Int 3)
   ,FRel Lt (FVar FPDouble "y") (FInt 3))]
  where
    interp = [("f",(False, FPDouble, [Arg "x" FPDouble], [aceb]))]
    aceb = ACeb { conds   = Cond [(BTrue, FBTrue)],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   =  ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test13 = testCase "unfoldFunCallsInCond__test13" $
  unfoldFunCallsInCond interp (Rel  Lt (EFun "f" Real [Var Real "z"]) (Int 3)
                              ,FRel Lt (FVar FPDouble "y") (FInt 3))
  @?=
  [(Or (Rel Lt (BinaryOp AddOp (Var Real "z") (Int 2)) (Int 3)) (Rel Lt (Var Real "i") (Int 3))
  ,FRel Lt (FVar FPDouble "y") (FInt 3))]
  where
    interp = [("f",(False, FPDouble, [Arg "x" FPDouble], [aceb]))]
    aceb = ACeb { conds   = Cond [(BTrue, FBTrue)],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2), (Var Real "i")],
          fpExprs = [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   =  ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test14 = testCase "unfoldFunCallsInCond__test14" $
  unfoldFunCallsInCond interp (Rel Lt (EFun "f" Real [Var Real "z"]) (Int 3)
                              ,FRel Lt (FVar FPDouble "y") (FInt 3))
  @?=
  [(And (Rel Lt (Var Real "z") (Int 9))
        (Or (Rel Lt (BinaryOp AddOp (Var Real "z") (Int 2)) (Int 3)) (Rel Lt (Var Real "i") (Int 3)))
  ,FRel Lt (FVar FPDouble "y") (FInt 3))]
  where
    interp = [("f",(False, FPDouble, [Arg "x" FPDouble], [aceb]))]
    aceb = ACeb { conds   = Cond [(Rel Lt (Var Real "x") (Int 9), FBTrue)],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2), (Var Real "i")],
          fpExprs = [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   =  ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test15 = testCase "unfoldFunCallsInCond__test15" $
  unfoldFunCallsInCond interp (Rel Lt  (EFun  "f" Real     [Var  Real     "z"]) (Int 3)
                              ,FRel Lt (FEFun False "g" FPDouble [FVar FPDouble "h"]) (FInt 5))
  @?=
  [(And (Rel Lt (Var Real "z") (Int 9))
        (Or (Rel Lt (BinaryOp AddOp (Var Real "z") (Int 2)) (Int 3)) (Rel Lt (Var Real "i") (Int 3)))
  ,FAnd (FRel Gt (FVar FPDouble "h") (FInt 0))
        (FRel Lt (BinaryFPOp MulOp FPDouble (FVar FPDouble "h") (FInt 4)) (FInt 5)))]
  where
    interp = [("f",(False, FPDouble, [Arg "x" FPDouble], [acebf]))
             ,("g",(False, FPDouble, [Arg "x" FPDouble], [acebg]))]
    acebf = ACeb { conds   = Cond [(Rel Lt (Var Real "x") (Int 9), FBTrue)],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2), (Var Real "i")],
          fpExprs = [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   =  ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }
    acebg = ACeb { conds = Cond [(Rel Gt (Var Real "x") (Int 0), FRel Gt (FVar FPDouble "x") (FInt 0))],
          rExprs  = [BinaryOp MulOp (Var Real "x") (Int 4)],
          fpExprs = [BinaryFPOp MulOp FPDouble (FVar FPDouble "x") (FInt 4)],
          eExpr   =  ErrBinOp MulOp FPDouble (Var Real "x") (ErrorMark "x" FPDouble) (Int 4) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test16 = testCase "unfoldFunCallsInCond__test16" $
  unfoldFunCallsInCond interp (Rel Lt  (EFun "quadrant" Real [Var  Real "px",Var  Real "py"]) (Int 2)
                              ,FNot $ FRel Lt (FEFun False "quadrant" FPDouble [FVar FPDouble "px",FVar FPDouble "py"]) (FInt 2))
  @?=
  [(And (Rel GtE (Var Real "px") (Int 0))
        (Rel GtE (Var Real "py") (Int 0))
   ,FAnd (FRel GtE (FVar FPDouble "px") (FInt 0))
         (FRel GtE (FVar FPDouble "py") (FInt 0)))
  ,(Not $ And (Rel GtE (Var Real "px") (Int 0))
              (Rel GtE (Var Real "py") (Int 0))
   ,FNot $ FAnd (FRel GtE (FVar FPDouble "px") (FInt 0))
                (FRel GtE (FVar FPDouble "py") (FInt 0)))
  ,(And (Rel GtE (Var Real "px") (Int 0))
        (Rel GtE (Var Real "py") (Int 0))
   ,FNot $ FAnd (FRel GtE (FVar FPDouble "px") (FInt 0))
         (FRel GtE (FVar FPDouble "py") (FInt 0)))
  ,(Not $ And (Rel GtE (Var Real "px") (Int 0))
              (Rel GtE (Var Real "py") (Int 0))
   ,FAnd (FRel GtE (FVar FPDouble "px") (FInt 0))
                (FRel GtE (FVar FPDouble "py") (FInt 0)))
  ]
  where
    interp = [("quadrant",(False, FPDouble, [Arg "x" FPDouble, Arg "y" FPDouble], [aceb1,aceb2]))]
    aceb1 = ACeb { conds   = Cond [(And (Rel GtE (Var Real "x") (Int 0))
                                        (Rel GtE (Var Real "y") (Int 0))
                                   ,FAnd (FRel GtE (FVar FPDouble "x") (FInt 0))
                                         (FRel GtE (FVar FPDouble "y") (FInt 0)))
                                  ,(Not $ And (Rel GtE (Var Real "x") (Int 0))
                                              (Rel GtE (Var Real "y") (Int 0))
                                   ,FNot $ FAnd (FRel GtE (FVar FPDouble "x") (FInt 0))
                                                (FRel GtE (FVar FPDouble "y") (FInt 0)))],
          rExprs  = [Int  1, Int  4],
          fpExprs = [FInt 1, FInt 4],
          eExpr   = ErrRat 0,
          decisionPath = root,
          cFlow  = Stable
      }
    aceb2 = ACeb { conds   = Cond [(And (Rel GtE (Var Real "x") (Int 0))
                                        (Rel GtE (Var Real "y") (Int 0))
                                   ,FNot $ FAnd (FRel GtE (FVar FPDouble "x") (FInt 0))
                                                (FRel GtE (FVar FPDouble "y") (FInt 0)))
                                  ,(Not $ And (Rel GtE (Var Real "x") (Int 0))
                                              (Rel GtE (Var Real "y") (Int 0))
                                   ,FAnd (FRel GtE (FVar FPDouble "x") (FInt 0))
                                                (FRel GtE (FVar FPDouble "y") (FInt 0)))],
          rExprs  = [Int  1, Int  4],
          fpExprs = [FInt 1, FInt 4],
          eExpr   = ErrRat 0,
          decisionPath = root,
          cFlow  = Unstable
      }

unfoldFunCallsInCond__test17 = testCase "unfoldFunCallsInCond__test17" $
  unfoldFunCallsInCond interp (Rel Lt (Var Real "y") (Int 3),
                               FRel Lt (FEFun False "f" FPDouble [FVar FPDouble "z"]) (FInt 3))
  @?=
  [(Rel Lt (Var Real "y") (Int 3),
    FAnd (FOr (FRel Gt (FVar FPDouble "z") (FInt 0))
              (FRel Lt (FVar FPDouble "z") (FInt 10)))
         (FRel Lt (BinaryFPOp AddOp TInt (FVar FPDouble "z") (FInt 2)) (FInt 3)))]
  where
    interp = [("f",(False, FPDouble, [Arg "x" FPDouble], [aceb]))]
    aceb = ACeb { conds   = Cond [(BTrue, FRel Gt (FVar FPDouble "x") (FInt 0))
                                 ,(BTrue, FRel Lt (FVar FPDouble "x") (FInt 10))],
          rExprs  = [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = [BinaryFPOp AddOp TInt (FVar FPDouble "x") (FInt 2)],
          eExpr   =  ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test18 = testCase "unfoldFunCallsInCond__test18" $
  unfoldFunCallsInCond interp (Rel  Gt  (EFun "f" Real [Var Real "z"]) (Int 0),
                               FRel LtE (FEFun False "f" FPDouble [FVar FPDouble "z"]) (FInt 0))
  @?=
  [(And  (Rel GtE (Var  Real     "z") (Int  1)) (Rel  Gt   (Var  Real     "z") (Int  0))
   ,FAnd (FRel Lt (FVar FPDouble "z") (FInt 1)) (FRel LtE (FVar FPDouble "z") (FInt 0)))
  ,(And  (Rel GtE (Var  Real     "z") (Int  3)) (Rel  Gt   (Var  Real     "z") (Int  0))
   ,FAnd (FRel Lt (FVar FPDouble "z") (FInt 3)) (FRel LtE (FVar FPDouble "z") (FInt 0)))]
  where
    interp = [("f",(False, FPDouble, [Arg "x" FPDouble], [aceb]))]
    aceb = ACeb { conds   = Cond [(Rel GtE (Var Real "x") (Int 1), FRel Lt (FVar FPDouble "x") (FInt 1))
                                 ,(Rel GtE (Var Real "x") (Int 3), FRel Lt (FVar FPDouble "x") (FInt 3))],
          rExprs  = [Var Real "x"],
          fpExprs = [FVar FPDouble "x"],
          eExpr   =  ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

stmSem__tests = testGroup "equivInterp tests"
    [stmSem__IntAdd
    ,stmSem__Add
    ,stmSem__IntSub
    ,stmSem__Sub
    ,stmSem__IntMul
    ,stmSem__Mul
    ,stmSem__MulPow2
    ,stmSem__Div1
    ,stmSem__Div2
    ,stmSem__IDiv
    ,stmSem__ItDiv
    ,stmSem__IMod
    ,stmSem__ItMod
    ,stmSem__Neg
    ,stmSem__Abs
    ,stmSem__Ln
    ,stmSem__Expo
    ,stmSem__Floor
    ,stmSem__Sqrt
    ,stmSem__Sin
    ,stmSem__Cos
    ,stmSem__Atan
    ,stmSem__StoD
    ,stmSem__DtoS
    ,stmSem__ItoS
    ,stmSem__ItoD
    ,stmSem__Ite
    ,stmSem__ListIte
    ,stmSem__LetIn
    ,stmSem__LetIn2
    ,stmSem__LetIn3
    ,stmSem__LetIn4
    ,stmSem__LetIn5
    ,stmSem__LetIn6
    ,stmSem__LetIn7
    ,stmSem__UnstWarning
    ,stmSem__IteIntAdd
    -- ,stmSem__LetInMul
    ]

semConf = SemConf { assumeTestStability = False, mergeUnstables = True}

stmSem__IntAdd = testCase "IntAdd" $
    stmSem (BinaryFPOp AddOp TInt (FInt 1) (FInt 2)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue, FBTrue)],
            rExprs  = [BinaryOp AddOp(Int 1) (Int 2)],
            fpExprs = [BinaryFPOp AddOp TInt (FInt 1) (FInt 2)],
            eExpr   =  ErrBinOp AddOp TInt (Int 1) (ErrRat 0) (Int 2) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Add = testCase "Add" $
    stmSem (BinaryFPOp AddOp FPDouble (FCnst FPDouble 0.1) (FCnst FPDouble 2)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue, FBTrue)],
            rExprs  = [BinaryOp AddOp(Rat 0.1) (Rat 2)],
            fpExprs = [BinaryFPOp AddOp FPDouble (FCnst FPDouble 0.1) (FCnst FPDouble 2)],
            eExpr   =  ErrBinOp AddOp FPDouble (Rat 0.1) (ErrRat (1 % 180143985094819840))
                                       (Rat 2) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__IntSub = testCase "IntSub" $
    stmSem (BinaryFPOp SubOp TInt (FInt 1) (FInt 2)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue, FBTrue)],
            rExprs  = [BinaryOp SubOp(Int 1) (Int 2)],
            fpExprs = [BinaryFPOp SubOp TInt (FInt 1) (FInt 2)],
            eExpr   =  ErrBinOp SubOp TInt (Int 1) (ErrRat 0) (Int 2) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Sub = testCase "Sub" $
    stmSem (BinaryFPOp SubOp FPDouble (FCnst FPDouble 0.1) (FCnst FPDouble 2)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue, FBTrue)],
            rExprs  = [BinaryOp SubOp(Rat 0.1) (Rat 2)],
            fpExprs = [BinaryFPOp SubOp FPDouble (FCnst FPDouble 0.1) (FCnst FPDouble 2)],
            eExpr   =  ErrBinOp SubOp FPDouble (Rat 0.1) (ErrRat (1 % 180143985094819840))
                                       (Rat 2) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__IntMul = testCase "IntMul" $
    stmSem (BinaryFPOp MulOp TInt (FInt 1) (FInt 3)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue, FBTrue)],
            rExprs  = [BinaryOp MulOp (Int 1) (Int 3)],
            fpExprs = [BinaryFPOp MulOp TInt (FInt 1) (FInt 3)],
            eExpr   =  ErrBinOp MulOp TInt (Int 1) (ErrRat 0) (Int 3) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Mul = testCase "Mul" $
    stmSem (BinaryFPOp MulOp FPDouble (FCnst FPDouble 0.1) (FCnst FPDouble 3)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue, FBTrue)],
            rExprs  = [BinaryOp MulOp (Rat 0.1) (Rat 3)],
            fpExprs = [BinaryFPOp MulOp FPDouble (FCnst FPDouble 0.1) (FCnst FPDouble 3)],
            eExpr   =  ErrBinOp MulOp FPDouble (Rat 0.1) (ErrRat (1 % 180143985094819840))
                                       (Rat 3) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__MulPow2 = testCase "Mul power of 2" $
    stmSem (BinaryFPOp MulOp FPDouble (FCnst FPDouble 0.1) (FCnst FPDouble 2)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds  = Cond [(Rel Lt (Int 1) (BinaryOp SubOp (Prec FPDouble) (FExp (FCnst FPDouble (1 % 10)))), FBTrue)],
            rExprs  = [BinaryOp MulOp (Rat 0.1) (Rat 2)],
            fpExprs = [BinaryFPOp MulOp FPDouble (FCnst FPDouble 0.1) (FCnst FPDouble 2)],
            eExpr   = ErrMulPow2R FPDouble 1 (ErrRat (1 % 180143985094819840)),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Div1 = testCase "Div" $
    stmSem (BinaryFPOp DivOp FPDouble (FCnst FPDouble 6) (FCnst FPDouble 3)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue, FBTrue)],
            rExprs  = [BinaryOp DivOp (Rat 6) (Rat 3)],
            fpExprs = [BinaryFPOp DivOp FPDouble (FCnst FPDouble 6) (FCnst FPDouble 3)],
            eExpr   =  ErrBinOp DivOp FPDouble (Rat 6) (ErrRat 0)
                                       (Rat 3) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Div2 = testCase "Div" $
    stmSem (BinaryFPOp DivOp FPDouble (FCnst FPDouble 6) (FVar FPDouble "X")) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(Rel Neq (RealMark "X") (Int 0)
                           ,FRel Neq (FVar FPDouble "X") (TypeCast TInt FPDouble (FInt 0)))],
            rExprs  = [BinaryOp DivOp (Rat 6) (RealMark "X")],
            fpExprs = [BinaryFPOp DivOp FPDouble (FCnst FPDouble 6) (FVar FPDouble "X")],
            eExpr   =  ErrBinOp DivOp FPDouble (Rat 6) (ErrRat 0)
                                       (RealMark "X") (ErrorMark "X" FPDouble),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__IDiv = testCase "IDiv" $
    stmSem (BinaryFPOp IDivOp TInt (FInt 6) (FInt 3)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue,FBTrue)],
            rExprs  = [BinaryOp IDivOp (Int 6) (Int 3)],
            fpExprs = [BinaryFPOp IDivOp TInt (FInt 6) (FInt 3)],
            eExpr   =  ErrBinOp IDivOp TInt (Int 6) (ErrRat 0)
                                       (Int 3) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__ItDiv = testCase "ItDiv" $
    stmSem (BinaryFPOp ItDivOp TInt (FInt 6) (FInt 3)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue,FBTrue)],
            rExprs  = [BinaryOp ItDivOp (Int 6) (Int 3)],
            fpExprs = [BinaryFPOp ItDivOp TInt (FInt 6) (FInt 3)],
            eExpr   =  ErrBinOp ItDivOp TInt (Int 6) (ErrRat 0)
                                       (Int 3) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__IMod = testCase "IMod" $
    stmSem (BinaryFPOp ModOp TInt (FInt 6) (FInt 3)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue,FBTrue)],
            rExprs  = [BinaryOp ModOp (Int 6) (Int 3)],
            fpExprs = [BinaryFPOp ModOp TInt (FInt 6) (FInt 3)],
            eExpr   = ErrBinOp ModOp TInt (Int 6) (ErrRat 0)
                                       (Int 3) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__ItMod = testCase "ItMod" $
    stmSem (BinaryFPOp ItModOp TInt (FInt 6) (FInt 3)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond  [(BTrue,FBTrue)],
            rExprs  = [BinaryOp ItModOp (Int 6) (Int 3)],
            fpExprs = [BinaryFPOp ItModOp TInt (FInt 6) (FInt 3)],
            eExpr   =  ErrBinOp ItModOp TInt (Int 6) (ErrRat 0)
                                       (Int 3) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Neg = testCase "Neg" $
    stmSem (UnaryFPOp NegOp FPDouble (FCnst FPDouble 0.1)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue, FBTrue)],
            rExprs  = [UnaryOp NegOp (Rat 0.1)],
            fpExprs = [UnaryFPOp NegOp FPDouble (FCnst FPDouble 0.1)],
            eExpr   =  ErrUnOp NegOp False FPDouble (Rat 0.1) (ErrRat $ 1 % 180143985094819840),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Abs = testCase "Abs" $
    stmSem (UnaryFPOp AbsOp FPDouble (FCnst FPDouble 0.1)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue, FBTrue)],
            rExprs  = [UnaryOp   AbsOp (Rat 0.1)],
            fpExprs = [UnaryFPOp AbsOp FPDouble (FCnst FPDouble 0.1)],
            eExpr   =  ErrUnOp AbsOp False FPDouble (Rat 0.1) (ErrRat $ 1 % 180143985094819840),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Ln = testCase "Ln" $
    stmSem (UnaryFPOp LnOp FPDouble (FCnst FPDouble 0.1)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(And (Rel Lt (Int 0) (BinaryOp SubOp(Rat (1 % 10)) (ErrRat (1 % 180143985094819840))))
                                 (Rel Gt (FromFloat FPDouble (FCnst FPDouble (1 % 10))) (Int 0)),FBTrue)],
            rExprs  = [UnaryOp   LnOp (Rat 0.1)],
            fpExprs = [UnaryFPOp LnOp FPDouble (FCnst FPDouble 0.1)],
            eExpr   =  ErrUnOp LnOp False FPDouble (Rat 0.1) (ErrRat $ 1 % 180143985094819840),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Expo = testCase "Expo" $
    stmSem (UnaryFPOp ExpoOp FPDouble (FCnst FPDouble 0.1)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue,FBTrue)],
            rExprs  = [UnaryOp   ExpoOp (Rat 0.1)],
            fpExprs = [UnaryFPOp ExpoOp FPDouble (FCnst FPDouble 0.1)],
            eExpr   =  ErrUnOp ExpoOp False FPDouble (Rat 0.1) (ErrRat $ 1 % 180143985094819840),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Floor = testCase "Floor" $
    stmSem (UnaryFPOp FloorOp FPDouble (FCnst FPDouble 0.1)) [] (Env []) semConf (LDP []) [] @?=
     [ACeb {conds = Cond [((Or (Rel Neq (UnaryOp FloorOp (Rat (1 % 10)))
                                              (UnaryOp FloorOp (BinaryOp SubOp (Rat (1 % 10)) (ErrRat (1 % 180143985094819840)))))
                               (Rel Neq (UnaryOp FloorOp (Rat (1 % 10)))
                                        (UnaryOp FloorOp (BinaryOp AddOp (Rat (1 % 10)) (ErrRat (1 % 180143985094819840))))))
                         ,FBTrue)],
            rExprs = [UnaryOp FloorOp (Rat (1 % 10))],
            fpExprs = [UnaryFPOp FloorOp FPDouble (FCnst FPDouble (1 % 10))],
            eExpr = ErrUnOp FloorOp False FPDouble (Rat (1 % 10)) (ErrRat (1 % 180143985094819840)),
            decisionPath = root,
            cFlow = Stable},
      ACeb {conds = Cond [(And (Rel Eq (UnaryOp FloorOp (Rat (1 % 10)))
                                   (UnaryOp FloorOp (BinaryOp SubOp(Rat (1 % 10)) (ErrRat (1 % 180143985094819840)))))
                               (Rel Eq (UnaryOp FloorOp (Rat (1 % 10)))
                                   (UnaryOp FloorOp (BinaryOp AddOp(Rat (1 % 10)) (ErrRat (1 % 180143985094819840)))))
                         ,FBTrue)],
            rExprs = [UnaryOp FloorOp (Rat (1 % 10))],
            fpExprs = [UnaryFPOp FloorOp FPDouble (FCnst FPDouble (1 % 10))],
            eExpr = ErrUnOp FloorOp True FPDouble (Rat (1 % 10)) (ErrRat (1 % 180143985094819840)),
            decisionPath = root,
            cFlow = Stable}]

stmSem__Sqrt = testCase "Sqrt" $
    stmSem (UnaryFPOp SqrtOp FPDouble (FCnst FPDouble 0.1)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [((Rel GtE (BinaryOp SubOp(Rat 0.1) (ErrRat $ 1 % 180143985094819840)) (Int 0)),FBTrue)],
            rExprs  = [UnaryOp   SqrtOp (Rat 0.1)],
            fpExprs = [UnaryFPOp SqrtOp FPDouble (FCnst FPDouble 0.1)],
            eExpr   =  ErrUnOp SqrtOp False FPDouble (Rat 0.1) (ErrRat $ 1 % 180143985094819840),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Sin = testCase "Sin" $
    stmSem (UnaryFPOp SinOp FPDouble (FCnst FPDouble 0.1)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue,FBTrue)],
            rExprs  = [UnaryOp   SinOp (Rat 0.1)],
            fpExprs = [UnaryFPOp SinOp FPDouble (FCnst FPDouble 0.1)],
            eExpr   =  ErrUnOp SinOp False FPDouble (Rat 0.1) (ErrRat $ 1 % 180143985094819840),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Cos = testCase "Cos" $
    stmSem (UnaryFPOp CosOp FPDouble (FCnst FPDouble 0.1)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue,FBTrue)],
            rExprs  = [UnaryOp   CosOp (Rat 0.1)],
            fpExprs = [UnaryFPOp CosOp FPDouble (FCnst FPDouble 0.1)],
            eExpr   =  ErrUnOp CosOp False FPDouble (Rat 0.1) (ErrRat $ 1 % 180143985094819840),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Atan = testCase "ATan" $
    stmSem (UnaryFPOp AtanOp FPDouble (FCnst FPDouble 0.1)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue,FBTrue)],
            rExprs  = [UnaryOp   AtanOp (Rat 0.1)],
            fpExprs = [UnaryFPOp AtanOp FPDouble (FCnst FPDouble 0.1)],
            eExpr   =  ErrUnOp AtanOp False FPDouble (Rat 0.1) (ErrRat $ 1 % 180143985094819840),
            decisionPath = root,
            cFlow  = Stable
        }
    ]

stmSem__StoD = testCase "StoD" $
    stmSem (TypeCast FPSingle FPDouble (FCnst FPSingle 0.1)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue,FBTrue)],
            rExprs  = [Rat 0.1],
            fpExprs = [TypeCast FPSingle FPDouble (FCnst FPSingle 0.1)],
            eExpr   = ErrRat (1 % 671088640),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__DtoS = testCase "DtoS" $
    stmSem (TypeCast FPDouble FPSingle (FCnst FPDouble 0.1)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue,FBTrue)],
            rExprs  = [Rat 0.1],
            fpExprs = [TypeCast FPDouble FPSingle (FCnst FPDouble 0.1)],
            eExpr   = ErrRat (1 % 671088640),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__ItoS = testCase "ItoS" $
    stmSem (TypeCast TInt FPSingle (FInt 1)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue,FBTrue)],
            rExprs  = [Int 1],
            fpExprs = [FInt 1],
            eExpr   = ErrRat 0,
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__ItoD = testCase "ItoD" $
    stmSem (TypeCast TInt FPDouble (FInt 1)) [] (Env []) semConf (LDP []) [] @?=
    [ACeb { conds   = Cond [(BTrue,FBTrue)],
            rExprs  = [Int 1],
            fpExprs = [FInt 1],
            eExpr   = ErrRat 0,
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Ite = testCase "Ite" $
    stmSem (Ite (FRel Lt (FVar FPDouble "X") (FInt 3)) (FInt 0) (FInt 1)) [] (Env []) semConf (LDP []) [LDP []]
    `semEquiv`
    [ACeb {conds = Cond [(Rel Lt (RealMark "X") (Int 3),FRel Lt (FVar FPDouble "X") (FInt 3))]
          ,rExprs = [Int 0]
          ,fpExprs = [FInt 0]
          ,eExpr = ErrRat 0
          ,decisionPath = LDP [0]
          ,cFlow = Stable}
    ,ACeb {conds = Cond [(Not (Rel Lt (RealMark "X") (Int 3)),FNot (FRel Lt (FVar FPDouble "X") (FInt 3)))]
          ,rExprs = [Int 1]
          ,fpExprs = [FInt 1]
          ,eExpr = ErrRat 0
          ,decisionPath = LDP [1]
          ,cFlow = Stable}
    ,ACeb {conds = Cond [(Not (Rel Lt (RealMark "X") (Int 3)),FRel Lt (FVar FPDouble "X") (FInt 3)),(Rel Lt (RealMark "X") (Int 3),FNot (FRel Lt (FVar FPDouble "X") (FInt 3)))]
          ,rExprs = [Int 0,Int 1]
          ,fpExprs = [FInt 0,FInt 1]
          ,eExpr = MaxErr [BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 1) (Int 0)))
                          ,BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 0) (Int 1)))]
          ,decisionPath = LDP []
          ,cFlow = Unstable}
    ]

stmSem__ListIte = testCase "ListIte" $
    stmSem (ListIte [(FRel Gt (FVar FPDouble "X") (FInt 3), (FInt 0)), (FRel Lt (FVar FPDouble "X") (FInt (-2)), (FInt 2))] (FInt 1)) [] (Env []) semConf (LDP []) [LDP []]
    `semEquiv`
    [ACeb {conds = Cond [(Rel Gt (RealMark "X") (Int 3),FRel Gt (FVar FPDouble "X") (FInt 3))]
          ,rExprs  = [Int 0]
          ,fpExprs = [FInt 0]
          ,eExpr   = ErrRat 0
          ,decisionPath = LDP [0]
          ,cFlow = Stable}
    ,ACeb {conds = Cond [(And  (Rel Lt  (RealMark "X")      (Int  (-2))) (Not  (Rel Gt  (RealMark "X")      (Int 3)))
                         ,FAnd (FRel Lt (FVar FPDouble "X") (FInt (-2))) (FNot (FRel Gt (FVar FPDouble "X") (FInt 3))))]
          ,rExprs  = [Int 2]
          ,fpExprs = [FInt 2]
          ,eExpr = ErrRat 0
          ,decisionPath = LDP [1]
          ,cFlow = Stable}
    ,ACeb {conds = Cond [(And  (Not  (Rel Lt  (RealMark "X")       (Int (-2)))) (Not  (Rel Gt  (RealMark "X")      (Int 3)))
                         ,FAnd (FNot (FRel Lt (FVar FPDouble "X") (FInt (-2)))) (FNot (FRel Gt (FVar FPDouble "X") (FInt 3))))]
          ,rExprs = [Int 1]
          ,fpExprs = [FInt 1]
          ,eExpr = ErrRat 0
          ,decisionPath = LDP [2]
          ,cFlow = Stable}
    ,ACeb {conds = Cond [(And (Not (Rel Gt (RealMark "X") (Int 3))) (Not (Rel Lt (RealMark "X") (Int (-2))))
                         ,FAnd (FRel Lt (FVar FPDouble "X") (FInt (-2))) (FNot (FRel Gt (FVar FPDouble "X") (FInt 3))))
                        ,(And (Not (Rel Gt (RealMark "X") (Int 3))) (Not (Rel Lt (RealMark "X") (Int (-2))))
                         ,FRel Gt (FVar FPDouble "X") (FInt 3))
                        ,(And (Rel Lt (RealMark "X") (Int (-2))) (Not (Rel Gt (RealMark "X") (Int 3)))
                         ,FAnd (FNot (FRel Gt (FVar FPDouble "X") (FInt 3))) (FNot (FRel Lt (FVar FPDouble "X") (FInt (-2)))))
                        ,(And (Rel Lt (RealMark "X") (Int (-2))) (Not (Rel Gt (RealMark "X") (Int 3))),FRel Gt (FVar FPDouble "X") (FInt 3)),(Rel Gt (RealMark "X") (Int 3)
                         ,FAnd (FNot (FRel Gt (FVar FPDouble "X") (FInt 3))) (FNot (FRel Lt (FVar FPDouble "X") (FInt (-2))))),(Rel Gt (RealMark "X") (Int 3)
                         ,FAnd (FRel Lt (FVar FPDouble "X") (FInt (-2))) (FNot (FRel Gt (FVar FPDouble "X") (FInt 3))))]
                         ,rExprs = [Int 0,Int 1,Int 2]
                         ,fpExprs = [FInt 0,FInt 1,FInt 2]
                         ,eExpr = MaxErr [MaxErr [MaxErr [MaxErr [MaxErr
                         [BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 2) (Int 0)))
                         ,BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 1) (Int 0)))]
                         ,BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 0) (Int 2)))]
                         ,BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 1) (Int 2)))]
                         ,BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 0) (Int 1)))]
                         ,BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 2) (Int 1)))]
                         ,decisionPath = LDP []
                         ,cFlow = Unstable}
    ]

stmSem__LetIn = testCase "LetIn" $
    stmSem (Let [("X", FPDouble, FInt 5)] (BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FInt 3)))  [] (Env [])  semConf (LDP []) [LDP []]
    `semEquiv`
    [ACeb {conds   = Cond [(BTrue,FBTrue)]
          ,rExprs  = [RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5}]
                     (BinaryOp AddOp (RealMark "X") (Int 3))]
          ,fpExprs = [Let [("X", FPDouble, FInt 5)] (BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FInt 3))]
          ,eExpr   = RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5},
                            LetElem{letVar = "Err_X", letType = Real, letExpr = ErrRat 0}]
                     $ ErrBinOp AddOp FPDouble (RealMark "X") (Var Real "Err_X") (Int 3) (ErrRat 0)
          ,decisionPath = LDP []
          ,cFlow = Stable}]

stmSem__LetIn2 = testCase "LetIn2" $
    stmSem (Let [("X", FPDouble, FInt 5),("Y", FPDouble, FInt 1)]
           (BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FVar FPDouble "Y")))
           [] (Env []) semConf (LDP []) [LDP []]
    `semEquiv`
    [ACeb {conds   = Cond [(BTrue,FBTrue)]
          ,rExprs  = [RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5}]
                      (RLet [LetElem{letVar = "Y", letType = Real, letExpr = Int 1}]
                     (BinaryOp AddOp (RealMark "X") (RealMark "Y")))]
          ,fpExprs = [Let [("X", FPDouble, FInt 5)] (
                      Let [("Y", FPDouble, FInt 1)]
                      (BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FVar FPDouble "Y")))]
          ,eExpr   = RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5}
                          ,LetElem{letVar = "Err_X", letType = Real, letExpr = ErrRat 0}] (
                     RLet [LetElem{letVar = "Y", letType = Real, letExpr = Int 1}
                          ,LetElem{letVar = "Err_Y", letType = Real, letExpr = ErrRat 0}]
                     $ ErrBinOp AddOp FPDouble (RealMark "X") (Var Real "Err_X")
                                               (RealMark "Y") (Var Real "Err_Y"))
          ,decisionPath = LDP []
          ,cFlow = Stable}]

stmSem__LetIn3 = testCase "LetIn3" $
    stmSem (Let [("X", FPDouble, Ite (FRel LtE (FVar FPDouble "Z") (FInt 0)) (FInt 1) (FInt 2))]
                (BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FInt 3)))  [] (Env [])  semConf (LDP []) [LDP []]
    `semEquiv`
     [ACeb {conds = Cond [(Not (Rel LtE (RealMark "Z") (Int 0))
                         ,FNot (FRel LtE (FVar FPDouble "Z") (FInt 0)))
                         ,(Rel LtE (RealMark "Z") (Int 0)
                         ,FRel LtE (FVar FPDouble "Z") (FInt 0))]
            ,rExprs = [BinaryOp AddOp (Int 1) (Int 3),BinaryOp AddOp (Int 2) (Int 3)]
            ,fpExprs = [BinaryFPOp AddOp FPDouble (FInt 1) (FInt 3),BinaryFPOp AddOp FPDouble (FInt 2) (FInt 3)]
            ,eExpr = MaxErr [ErrBinOp AddOp FPDouble (Int 1) (ErrRat 0) (Int 3) (ErrRat 0)
                            ,ErrBinOp AddOp FPDouble (Int 2) (ErrRat 0) (Int 3) (ErrRat 0)]
            ,decisionPath = LDP []
            ,cFlow = Stable}
     ,ACeb {conds = Cond [(Not (Rel LtE (RealMark "Z") (Int 0))
                          ,FRel LtE (FVar FPDouble "Z") (FInt 0))
                          ,(Rel LtE (RealMark "Z") (Int 0)
                          ,FNot (FRel LtE (FVar FPDouble "Z") (FInt 0)))]
           ,rExprs = [BinaryOp AddOp (Int 1) (Int 3),BinaryOp AddOp (Int 2) (Int 3)]
           ,fpExprs = [BinaryFPOp AddOp FPDouble (FInt 1) (FInt 3),BinaryFPOp AddOp FPDouble (FInt 2) (FInt 3)]
           ,eExpr = MaxErr [ErrBinOp AddOp FPDouble (Int 1)
                           (MaxErr [BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 2) (Int 1)))
                           ,BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 1) (Int 2)))]) (Int 3) (ErrRat 0)
                           ,ErrBinOp AddOp FPDouble (Int 2) (MaxErr [BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 2) (Int 1)))
                           ,BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 1) (Int 2)))]) (Int 3) (ErrRat 0)]
           ,decisionPath = LDP []
           ,cFlow = Unstable}]

stmSem__LetIn4 = testCase "LetIn4" $
    stmSem (Let [("X", FPDouble, FInt 5)
                ,("Y", FPDouble, BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FInt 2))]
    (BinaryFPOp AddOp FPDouble (FVar FPDouble "Y") (FInt 3)))  [] (Env [])  semConf (LDP []) [LDP []]
    `semEquiv`
    [ACeb {conds   = Cond [(BTrue,FBTrue)]
          ,rExprs  = [RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5}]
                     (RLet [LetElem{letVar = "Y", letType = Real, letExpr = BinaryOp AddOp (RealMark "X") (Int 2)}]
                     (BinaryOp AddOp (RealMark "Y") (Int 3)))]
          ,fpExprs = [Let [("X", FPDouble, FInt 5)]
                     (Let [("Y", FPDouble, BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FInt 2))]
                      (BinaryFPOp AddOp FPDouble (FVar FPDouble "Y") (FInt 3)))]
          ,eExpr   = RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5},
                            LetElem{letVar = "Err_X", letType = Real, letExpr = ErrRat 0}] (
                     RLet [LetElem{letVar = "Y", letType = Real, letExpr = BinaryOp AddOp (RealMark "X") (Int 2)},
                            LetElem{letVar = "Err_Y", letType = Real
                                   ,letExpr = ErrBinOp AddOp FPDouble (RealMark "X") (Var Real "Err_X") (Int 2) (ErrRat 0)}] (
                     ErrBinOp AddOp FPDouble (RealMark "Y") (Var Real "Err_Y") (Int 3) (ErrRat 0)))
          ,decisionPath = LDP []
          ,cFlow = Stable}]

stmSem__LetIn5 = testCase "LetIn5" $
    stmSem (Let [("X", FPDouble, FInt 5)
                ,("Y", FPDouble, BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FInt 2))
                ,("Z", FPDouble, BinaryFPOp MulOp FPDouble (FVar FPDouble "Y") (FInt 7))]
    (BinaryFPOp AddOp FPDouble (FVar FPDouble "Z") (FInt 3)))  [] (Env [])  semConf (LDP []) [LDP []]
    `semEquiv`
    [ACeb {conds   = Cond [(BTrue,FBTrue)]
          ,rExprs  = [RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5}]
                     (RLet [LetElem{letVar = "Y", letType = Real, letExpr = BinaryOp AddOp (RealMark "X") (Int 2)}]
                     (RLet [LetElem{letVar = "Z", letType = Real, letExpr = BinaryOp MulOp (RealMark "Y") (Int 7)}]
                     (BinaryOp AddOp (RealMark "Z") (Int 3))))]
          ,fpExprs = [Let [("X", FPDouble, FInt 5)]
                     (Let [("Y", FPDouble, BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FInt 2))]
                     (Let [("Z", FPDouble, BinaryFPOp MulOp FPDouble (FVar FPDouble "Y") (FInt 7))]
                      (BinaryFPOp AddOp FPDouble (FVar FPDouble "Z") (FInt 3))))]
          ,eExpr   = RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5},
                            LetElem{letVar = "Err_X", letType = Real, letExpr = ErrRat 0}] (
                     RLet [LetElem{letVar = "Y", letType = Real, letExpr = BinaryOp AddOp (RealMark "X") (Int 2)},
                            LetElem{letVar = "Err_Y", letType = Real
                                   ,letExpr = ErrBinOp AddOp FPDouble (RealMark "X") (Var Real "Err_X") (Int 2) (ErrRat 0)}] (
                     RLet [LetElem{letVar = "Z", letType = Real, letExpr = BinaryOp MulOp (RealMark "Y") (Int 7)},
                            LetElem{letVar = "Err_Z", letType = Real
                                   ,letExpr = ErrBinOp MulOp FPDouble (RealMark "Y") (Var Real "Err_Y") (Int 7) (ErrRat 0)}] (
                     ErrBinOp AddOp FPDouble (RealMark "Z") (Var Real "Err_Z") (Int 3) (ErrRat 0))))
          ,decisionPath = LDP []
          ,cFlow = Stable}]


stmSem__LetIn6 = testCase "LetIn6" $
    stmSem (Let [("X", FPDouble, FInt 5)
                ,("Y", FPDouble, FEFun False "f" FPDouble [(FVar FPDouble "X")])]
    (BinaryFPOp AddOp FPDouble (FVar FPDouble "Y") (FInt 3)))
    [("f", (False, FPSingle, [Arg "x" FPDouble],fSemantics))]
    (Env [])  semConf (LDP []) [LDP []]
    `semEquiv`
    [ACeb {conds   = Cond [(BTrue,FBTrue)]
          ,rExprs  = [RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5}]
                     (RLet [LetElem{letVar = "Y", letType = Real, letExpr = (RealMark "X")}]
                     (BinaryOp AddOp (RealMark "Y") (Int 3)))]
          ,fpExprs = [Let [("X", FPDouble, FInt 5)]
                     (Let [("Y", FPDouble, FEFun False "f" FPDouble [FVar FPDouble "X"])]
                      (BinaryFPOp AddOp FPDouble (FVar FPDouble "Y") (FInt 3)))]
          ,eExpr   = RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5},
                            LetElem{letVar = "Err_X", letType = Real, letExpr = ErrRat 0}] (
                     RLet [LetElem{letVar = "Y", letType = Real, letExpr = (RealMark "X") },
                            LetElem{letVar = "Err_Y", letType = Real
                                   ,letExpr = ErrRat 5}] (
                     ErrBinOp AddOp FPDouble (RealMark "Y") (Var Real "Err_Y") (Int 3) (ErrRat 0)))
          ,decisionPath = LDP []
          ,cFlow = Stable}]
      where
        fSemantics = [ACeb {
                conds  = Cond [(BTrue,FBTrue)],
                rExprs = [Var Real "x"],
                fpExprs = [FVar FPDouble "x"],
                eExpr  = ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }]

stmSem__LetIn7 = testCase "LetIn7" $
    stmSem (Let [("A", FPDouble, (BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FVar FPDouble "Y")))
                ,("B", FPDouble, FEFun False "f" FPDouble [(FVar FPDouble "A")])]
          (FVar FPDouble "B"))
    [("f", (False, FPDouble, [Arg "X" FPDouble],fSemantics))]
    (Env [])  semConf (LDP []) [LDP []]
    `semEquiv`
    [ACeb {conds   = Cond [(BTrue,FBTrue)]
          ,rExprs  = [RLet [LetElem{letVar = "A", letType = Real,
                            letExpr = BinaryOp AddOp (RealMark "X") (RealMark "Y")}]
                     (RLet [LetElem{letVar = "B", letType = Real,
                            letExpr = BinaryOp MulOp (RealMark "A") (Int 2)}]
                     (RealMark "B"))]
          ,fpExprs = [Let [("A", FPDouble, BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FVar FPDouble "Y"))]
                     (Let [("B", FPDouble, FEFun False "f" FPDouble [(FVar FPDouble "A")])]
                      (FVar FPDouble "B"))]
          ,eExpr   = RLet [LetElem{letVar = "A", letType = Real,
                           letExpr = BinaryOp AddOp (RealMark "X") (RealMark "Y")},
                           LetElem{letVar = "Err_A", letType = Real,
                           letExpr = ErrBinOp AddOp FPDouble (RealMark "X") (ErrorMark "X" FPDouble)
                                                            (RealMark "Y") (ErrorMark "Y" FPDouble)}] (
                     RLet [LetElem{letVar = "B", letType = Real,
                           letExpr =  BinaryOp MulOp (RealMark "A") (Int 2) },
                            LetElem{letVar = "Err_B", letType = Real
                                   ,letExpr = ErrBinOp MulOp FPDouble (RealMark "A") (Var Real "Err_X") (Int 2) (ErrRat (0 % 1))}]
                                   (Var Real "Err_B"))
          ,decisionPath = LDP []
          ,cFlow = Stable}]
      where
        fSemantics = [ACeb {
                conds  = Cond [(BTrue,FBTrue)],
                rExprs = [BinaryOp MulOp (RealMark "X") (Int 2)],
                fpExprs = [BinaryFPOp MulOp FPDouble (FVar FPDouble "X") (FInt 2)],
                eExpr  = ErrBinOp MulOp FPDouble (RealMark "X") (Var Real "Err_X") (Int 2) (ErrRat 0),
                decisionPath = root,
                cFlow = Stable
                }]

stmSem__UnstWarning = testCase "UnstWarning" $
    stmSem UnstWarning [] (Env [])  semConf (LDP []) [LDP []]
    `semEquiv`
    [ ACeb { conds  = Cond [(BTrue,FBTrue)],
             rExprs = [Int 0],
             fpExprs = [FInt 0],
             eExpr  = ErrRat 0,
             decisionPath = LDP [],
             cFlow  = Stable
             } ]

stmSem__IteIntAdd = testCase "IteIntAdd" $
    stmSem (BinaryFPOp AddOp FPDouble (Ite (FRel Gt (FVar FPDouble "X") (FInt 0)) (FInt 0) (FInt 1))
                                      (FInt 3))
      [] (Env []) semConf (LDP []) [LDP []]
    `semEquiv`
    [ACeb {conds = Cond [(Not (Rel Gt (RealMark "X") (Int 0)),FNot (FRel Gt (FVar FPDouble "X") (FInt 0))),(Rel Gt (RealMark "X") (Int 0),FRel Gt (FVar FPDouble "X") (FInt 0))]
          ,rExprs = [BinaryOp AddOp (Int 0) (Int 3),BinaryOp AddOp (Int 1) (Int 3)]
          ,fpExprs = [BinaryFPOp AddOp FPDouble (FInt 0) (FInt 3)
                     ,BinaryFPOp AddOp FPDouble (FInt 1) (FInt 3)]
          ,eExpr = MaxErr [ErrBinOp AddOp FPDouble (Int 0) (ErrRat 0) (Int 3) (ErrRat 0)
                          ,ErrBinOp AddOp FPDouble (Int 1) (ErrRat (0 % 1)) (Int 3) (ErrRat (0 % 1))]
          ,decisionPath = LDP []
          ,cFlow = Stable}
    ,ACeb {conds = Cond [(Not (Rel Gt (RealMark "X") (Int 0)),FRel Gt (FVar FPDouble "X") (FInt 0)),(Rel Gt (RealMark "X") (Int 0),FNot (FRel Gt (FVar FPDouble "X") (FInt 0)))]
          ,rExprs = [BinaryOp AddOp (Int 0) (Int 3),BinaryOp AddOp (Int 1) (Int 3)]
          ,fpExprs = [BinaryFPOp AddOp FPDouble (FInt 0) (FInt 3),BinaryFPOp AddOp FPDouble (FInt 1) (FInt 3)]
          ,eExpr = MaxErr [ErrBinOp AddOp FPDouble (Int 0)
                  (MaxErr [BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 1) (Int 0)))
                          ,BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 0) (Int 1)))]) (Int 3) (ErrRat 0)
                          ,ErrBinOp AddOp FPDouble (Int 1) (MaxErr [BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 1) (Int 0))),BinaryOp AddOp (ErrRat 0)
                          (UnaryOp AbsOp (BinaryOp SubOp (Int 0) (Int 1)))]) (Int 3) (ErrRat (0 % 1))]
          ,decisionPath = LDP []
          ,cFlow = Unstable}]


-- stmSem__LetInMul = testCase "LetInMul" $
--     stmSem (BinaryFPOp MulOp FPDouble (Let [("X", FPDouble, (FInt 5))] (BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FInt 3)))
--                                       (FInt 5))
--       [] (Env [])  semConf (LDP []) [LDP []]
--     `semEquiv`
--     [ACeb {conds = Cond [(BTrue,FBTrue)]
--           ,rExprs = [BinaryOp MulOp (BinaryOp AddOp (Int 5) (Int 3)) (Int 5)]
--           ,fpExprs = [BinaryFPOp MulOp FPDouble (BinaryFPOp AddOp FPDouble (FInt 5) (FInt 3)) (FInt 5)]
--           ,eExpr = ErrBinOp MulOp FPDouble (BinaryOp AddOp (Int 5) (Int 3)) (ErrBinOp AddOp FPDouble (Int 5) (ErrRat 0) (Int 3) (ErrRat 0)) (Int 5) (ErrRat 0)
--           ,decisionPath = LDP []
--           ,cFlow = Stable}]

equivInterp__tests = testGroup "equivInterp tests"
    [equivInterp__test1
    ,equivInterp__test2
    ,equivInterp__test3
    ,equivInterp__test4
    ]

equivInterp__test1 = testCase "Equivalent Interpretation 1" $
    equivInterp interp1 interp2 @?= False
    where
      interp1 = [("f", (False, FPDouble, [Arg "x" FPDouble],fSemantics))]
      interp2 = [("f", (False, FPSingle, [Arg "x" FPDouble],fSemantics))]
      fSemantics = [ACeb {
                conds  = Cond [(Rel Lt (Int 8) (Int 9), FRel Lt (FInt 8) (FInt 9))
                         ,(Rel Lt (Int 9) (Int 10),FRel Lt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }]

equivInterp__test2 = testCase "Equivalent Interpretation 2" $
    equivInterp interp1 interp2 @?= True
    where
      interp1 = [("f", (False, FPSingle, [Arg "x" FPDouble],fSemantics))
                ,("g", (False, FPDouble, [Arg "x" FPDouble],gSemantics))]
      interp2 = [("g", (False, FPDouble, [Arg "x" FPDouble],gSemantics))
                ,("f", (False, FPSingle, [Arg "x" FPDouble],fSemantics))]
      fSemantics = [ACeb {
                conds  = Cond [(Rel Lt (Int 8) (Int 9), FRel Lt (FInt 8) (FInt 9))
                         ,(Rel Lt (Int 9) (Int 10),FRel Lt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }]
      gSemantics = [ACeb {
                conds  = Cond [(Rel Lt (Int 6) (Int 9), FRel Lt (FInt 8) (FInt 9))
                         ,(Rel Lt (Int 9) (Int 10),FRel Lt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9],
                fpExprs = [FInt 9],
                eExpr  = ErrRat 5,
                decisionPath = root,
                cFlow = Unstable
                }]

equivInterp__test3 = testCase "Equivalent Interpretation 3" $
    equivInterp interp1 interp2 @?= False
    where
      interp1 = [("f", (False, FPSingle, [Arg "y" FPDouble],fSemantics))
                ,("g", (False, FPDouble, [Arg "x" FPDouble],gSemantics))]
      interp2 = [("g", (False, FPDouble, [Arg "x" FPDouble],gSemantics))
                ,("f", (False, FPSingle, [Arg "x" FPDouble],fSemantics))]
      fSemantics = [ACeb {
                conds  = Cond [(Rel Lt (Int 8) (Int 9), FRel Lt (FInt 8) (FInt 9))
                         ,(Rel Lt (Int 9) (Int 10),FRel Lt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }]
      gSemantics = [ACeb {
                conds  = Cond [(Rel Lt (Int 6) (Int 9), FRel Lt (FInt 8) (FInt 9))
                         ,(Rel Lt (Int 9) (Int 10),FRel Lt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9],
                fpExprs = [FInt 9],
                eExpr  = ErrRat 5,
                decisionPath = root,
                cFlow = Unstable
                }]

equivInterp__test4 = testCase "Equivalent Interpretation 3" $
    equivInterp interp1 interp2 @?= False
    where
      interp1 = [("f", (False, FPSingle, [Arg "x" FPDouble],fSemantics1))
                ,("g", (False, FPDouble, [Arg "x" FPDouble],gSemantics))]
      interp2 = [("g", (False, FPDouble, [Arg "x" FPDouble],gSemantics))
                ,("f", (False, FPSingle, [Arg "x" FPDouble],fSemantics2))]
      fSemantics1 = [ACeb {
                conds  = Cond [(Rel Lt (Int 8) (Int 9), FRel Lt (FInt 8) (FInt 9))
                         ,(Rel Lt (Int 9) (Int 10),FRel Lt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                },
                ACeb {
                conds  = Cond [(Rel Lt (Int 8) (Int 9), FRel Lt (FInt 8) (FInt 9))
                         ,(Rel Lt (Int 9) (Int 10),FRel Lt (FInt 9) (FInt 10))
                         ],
                rExprs = [Var Real "v"],
                fpExprs = [FVar FPSingle "v"],
                eExpr  = ErrRat 0,
                decisionPath = root,
                cFlow = Unstable
                }]
      fSemantics2 = [ACeb {
                conds  = Cond [(Rel Lt (Int 8) (Int 9), FRel Lt (FInt 8) (FInt 9))
                         ,(Rel Lt (Int 9) (Int 10),FRel Lt (FInt 9) (FInt 10))
                         ],
                rExprs = [Var Real "v"],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat 0,
                decisionPath = root,
                cFlow = Unstable
                },
                ACeb {
                conds  = Cond [(Rel Lt (Int 8) (Int 9), FRel Lt (FInt 8) (FInt 9))
                         ,(Rel Lt (Int 9) (Int 10),FRel Lt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }]
      gSemantics = [ACeb {
                conds  = Cond [(Rel Lt (Int 6) (Int 9), FRel Lt (FInt 8) (FInt 9))
                         ,(Rel Lt (Int 9) (Int 10),FRel Lt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9],
                fpExprs = [FInt 9],
                eExpr  = ErrRat 5,
                decisionPath = root,
                cFlow = Unstable
                }]


semEFun__tests = testGroup "semEFun tests"
    [semEFun__test1
    ,semEFun__test2
    ,semEFun__test3
    ,semEFun__test4
    ,semEFun__test5
    ,semEFun__test6
    ,semEFun__test7
    ]

semEFun__test1 = testCase "it correctly combines ACeBS" $
    actual @?= expected
    where
      actual = semEFun "f" formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
        where
          formalParams = [Arg "x" FPDouble
                         ,Arg "y" FPDouble
                         ]
          actualParams = [FVar FPDouble "a"
                         ,FVar FPDouble "b"
                         ]
          semanticArgumentCombinations =
            [
              [xSemantics1,ySemantics1],
              [xSemantics1,ySemantics2],
              [xSemantics2,ySemantics1],
              [xSemantics2,ySemantics2]
            ]
            where
              xSemantics1 = ACeb {
                conds  = Cond [(Rel Lt (Int 0) (Int 1),FRel Lt (FInt 0) (FInt 1))
                         ,(Rel Lt (Int 1) (Int 2),FRel Lt (FInt 1) (FInt 2))
                         ],
                rExprs = [Int 1,Int 2],
                fpExprs = [FInt 3, FInt 2],
                eExpr  = ErrRat 1,
                decisionPath = root,
                cFlow = Stable
                }
              xSemantics2 = ACeb {
                conds  = Cond [(Rel Lt (Int 2) (Int 3),FRel Lt (FInt 2) (FInt 3))
                         ,(Rel Lt (Int 3) (Int 4),FRel Lt (FInt 3) (FInt 4))
                         ],
                rExprs = [Int 3,Int 4],
                fpExprs = [FInt 3,FInt 4],
                eExpr  = ErrRat 2,
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics1 = ACeb {
                conds  = Cond [(Rel Lt (Int 4) (Int 5),FRel Lt (FInt 4) (FInt 5))
                         ,(Rel Lt (Int 5) (Int 6),FRel Lt (FInt 5) (FInt 6))
                         ],
                rExprs = [Int 5,Int 6],
                fpExprs = [FInt 5,FInt 6],
                eExpr  = ErrRat 3,
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics2 = ACeb {
                conds  = Cond [(Rel Lt (Int 6) (Int 7),FRel Lt (FInt 6) (FInt 7))
                         ,(Rel Lt (Int 7) (Int 8),FRel Lt (FInt 7) (FInt 8))
                         ],
                rExprs = [Int 7,Int 8],
                fpExprs = [FInt 7,FInt 8],
                eExpr  = ErrRat 4,
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1
                              ,fSemantics2
                              ]
            where
              fSemantics1 = ACeb {
                conds  = Cond [(Rel Lt (Int 8) (Int 9), FRel Lt (FInt 8) (FInt 9))
                         ,(Rel Lt (Int 9) (Int 10),FRel Lt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }
              fSemantics2 = ACeb {
                conds  = Cond [(Rel Lt (Int 10) (Int 11),FRel Lt (FInt 10) (FInt 11)),
                          (Rel Lt (Int 11) (Int 12),FRel Lt (FInt 11) (FInt 12))
                         ],
                rExprs = [Int 11,Int 12],
                fpExprs = [FInt 11,FInt 12],
                eExpr  = ErrRat 6,
                decisionPath = root,
                cFlow = Stable
                }
      expected = [
        ACeb {
            conds  = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 9,Int 10],
            fpExprs = [FInt 9,FInt 10],
            eExpr  = ErrRat (5 % 1),
            decisionPath = root,
            cFlow  = Stable
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 9,Int 10],
            fpExprs = [FInt 9,FInt 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 9,Int 10],
            fpExprs = [FInt 9,FInt 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 9,Int 10],
            fpExprs = [FInt 9,FInt 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 11,Int 12],
            fpExprs = [FInt 11,FInt 12],
            eExpr = ErrRat (6 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 11,Int 12],
            fpExprs = [FInt 11,FInt 12],
            eExpr = ErrRat (6 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 11,Int 12],
            fpExprs = [FInt 11,FInt 12],
            eExpr = ErrRat (6 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 11,Int 12],
            fpExprs = [FInt 11,FInt 12],
            eExpr = ErrRat (6 % 1),
            decisionPath = root,
            cFlow = Stable
            }
        ]

semEFun__test2 = testCase "it correctly combines arguments-combinations conditions II" $
    actual @?= expected
    where
      actual = semEFun "f" formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
        where
          formalParams = [Arg "x" FPDouble
                         ,Arg "y" FPDouble
                         ]
          actualParams = [FVar FPDouble "a"
                         ,FVar FPDouble "b"
                         ]
          semanticArgumentCombinations = [[xSemantics1,ySemantics1]]
            where
              xSemantics1 = ACeb {
                conds  = Cond [
                          (Rel Lt (Var Real "0") (Int 1),FRel Lt (FVar FPDouble "0") (FInt 1))
                         ,(Rel Lt (Var Real "1") (Int 2),FRel Lt (FVar FPDouble "1") (FInt 2))
                         ],
                rExprs = [Int 1,Int 2],
                fpExprs = [FInt 1,FInt 2],
                eExpr  = ErrRat 1,
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics1 = ACeb {
                conds  = Cond [(Rel Lt (Var Real "4") (Int 5),FRel Lt (FVar FPDouble "4") (FInt 5))],
                rExprs = [Int 5,Int 6],
                fpExprs = [FInt 5,FInt 6],
                eExpr  = ErrRat 3,
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1]
            where
              fSemantics1 = ACeb {
                conds  = Cond [(Rel Lt (Var Real "8") (Int 9), FRel Lt (FVar FPDouble "8") (FInt 9))],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }
      expected = -- []
        [
          ACeb {
            conds = Cond [
                (And (Rel Lt (Var Real "8") (Int 9)) (And (Rel Lt (Var Real "0") (Int 1)) (Rel Lt (Var Real "4") (Int 5)))
                ,FAnd (FRel Lt (FVar FPDouble "8") (FInt 9)) (FAnd (FRel Lt (FVar FPDouble "0") (FInt 1)) (FRel Lt (FVar FPDouble "4") (FInt 5)))),
                (And (Rel Lt (Var Real "8") (Int 9)) (And (Rel Lt (Var Real "1") (Int 2)) (Rel Lt (Var Real "4") (Int 5)))
                ,FAnd (FRel Lt (FVar FPDouble "8") (FInt 9)) (FAnd (FRel Lt (FVar FPDouble "1") (FInt 2)) (FRel Lt (FVar FPDouble "4") (FInt 5))))],
            rExprs = [Int 9,Int 10],
            fpExprs = [FInt 9,FInt 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            }
        ]

semEFun__test3 = testCase "it correctly combines arguments-combinations conditions I" $
    actual @?= expected
    where
      actual = semEFun "f" formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
        where
          formalParams = [Arg "x" FPDouble
                         ,Arg "y" FPDouble
                         ]
          actualParams = [FVar FPDouble "a"
                         ,FVar FPDouble "b"
                         ]
          semanticArgumentCombinations = [[xSemantics1,ySemantics1]]
            where
              xSemantics1 = ACeb {
                conds  = Cond [(Rel Lt (Var Real "0") (Int 1),FRel Lt (FVar FPDouble "0") (FInt 1))
                         ,(Rel Lt (Var Real "1") (Int 2),FRel Lt (FVar FPDouble "1") (FInt 2))
                         ],
                rExprs = [Int 1,Int 2],
                fpExprs = [FInt 1,FInt 2],
                eExpr  = ErrRat 1,
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics1 = ACeb {
                conds  = Cond [(Rel Lt (Var Real "4") (Int 5),FRel Lt (FVar FPDouble "4") (FInt 5))
                         ,(Rel Lt (Var Real "6") (Int 7),FRel Lt (FVar FPDouble "6") (FInt 7))],
                rExprs = [Int 5,Int 6],
                fpExprs = [FInt 5,FInt 6],
                eExpr  = ErrRat 3,
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1]
            where
              fSemantics1 = ACeb {
                conds  = Cond [(Rel Lt (Var Real "8") (Int 9), FRel Lt (FVar FPDouble "8") (FInt 9))],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }
      expected = [
        ACeb {
            conds = Cond [
                (And (Rel Lt (Var Real "8") (Int 9)) (And (Rel Lt (Var Real "0") (Int 1)) (Rel Lt (Var Real "4") (Int 5)))
                ,FAnd (FRel Lt (FVar FPDouble "8") (FInt 9)) (FAnd (FRel Lt (FVar FPDouble "0") (FInt 1)) (FRel Lt (FVar FPDouble "4") (FInt 5)))),
                (And (Rel Lt (Var Real "8") (Int 9)) (And (Rel Lt (Var Real "0") (Int 1)) (Rel Lt (Var Real "6") (Int 7)))
                ,FAnd (FRel Lt (FVar FPDouble "8") (FInt 9)) (FAnd (FRel Lt (FVar FPDouble "0") (FInt 1)) (FRel Lt (FVar FPDouble "6") (FInt 7)))),
                (And (Rel Lt (Var Real "8") (Int 9)) (And (Rel Lt (Var Real "1") (Int 2)) (Rel Lt (Var Real "4") (Int 5)))
                ,FAnd (FRel Lt (FVar FPDouble "8") (FInt 9)) (FAnd (FRel Lt (FVar FPDouble "1") (FInt 2)) (FRel Lt (FVar FPDouble "4") (FInt 5)))),
                (And (Rel Lt (Var Real "8") (Int 9)) (And (Rel Lt (Var Real "1") (Int 2)) (Rel Lt (Var Real "6") (Int 7)))
                ,FAnd (FRel Lt (FVar FPDouble "8") (FInt 9)) (FAnd (FRel Lt (FVar FPDouble "1") (FInt 2)) (FRel Lt (FVar FPDouble "6") (FInt 7))))
                ],
              rExprs = [Int 9,Int 10],
              fpExprs = [FInt 9,FInt 10],
              eExpr = ErrRat (5 % 1),
              decisionPath = root,
              cFlow = Stable
            }
        ]


semEFun__test4 = testCase "it correctly combines argument combinations" $
    actual @?= expected
    where
      actual = semEFun "f" formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
        where
          formalParams = [Arg "x" FPDouble
                         ,Arg "y" FPDouble
                         ]
          actualParams = [FVar FPDouble "a"
                         ,FVar FPDouble "b"
                         ]
          semanticArgumentCombinations =
            [
              [xSemantics1,ySemantics1],
              [xSemantics2,ySemantics1]
            ]
            where
              xSemantics1 = ACeb {
                conds  = Cond [
                    (Rel Lt (Var Real "0") (Int 1),FRel Lt (FVar FPDouble "0") (FInt 1)),
                    (Rel Lt (Var Real "1") (Int 2),FRel Lt (FVar FPDouble "1") (FInt 2))
                    ],
                rExprs = [Int 1,Int 2],
                fpExprs = [FInt 1,FInt 2],
                eExpr  = ErrRat 1,
                decisionPath = root,
                cFlow = Stable
                }
              xSemantics2 = ACeb {
                conds  = Cond [(Rel Lt (Var Real "2") (Int 3),FRel Lt (FVar FPDouble "2") (FInt 3))],
                rExprs = [Int 3,Int 4],
                fpExprs = [FInt 3,FInt 4],
                eExpr  = ErrRat 2,
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics1 = ACeb {
                conds  = Cond [(Rel Lt (Var Real "4") (Int 5),FRel Lt (FVar FPDouble "4") (FInt 5))],
                rExprs = [Int 5,Int 6],
                fpExprs = [FInt 5,FInt 6],
                eExpr  = ErrRat 3,
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1]
            where
              fSemantics1 = ACeb {
                conds  = Cond [(Rel Lt (Var Real "8") (Int 9), FRel Lt (FVar FPDouble "8") (FInt 9))],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }
      expected = [
        ACeb {
            conds = Cond [
                (And (Rel Lt (Var Real "8") (Int 9)) (And (Rel Lt (Var Real "0") (Int 1)) (Rel Lt (Var Real "4") (Int 5)))
                ,FAnd (FRel Lt (FVar FPDouble "8") (FInt 9)) (FAnd (FRel Lt (FVar FPDouble "0") (FInt 1)) (FRel Lt (FVar FPDouble "4") (FInt 5)))),
                (And (Rel Lt (Var Real "8") (Int 9)) (And (Rel Lt (Var Real "1") (Int 2)) (Rel Lt (Var Real "4") (Int 5)))
                ,FAnd (FRel Lt (FVar FPDouble "8") (FInt 9)) (FAnd (FRel Lt (FVar FPDouble "1") (FInt 2)) (FRel Lt (FVar FPDouble "4") (FInt 5))))
                ],
            rExprs = [Int 9,Int 10],
            fpExprs = [FInt 9,FInt 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Cond [
                  (And (Rel Lt (Var Real "8") (Int 9)) (And (Rel Lt (Var Real "2") (Int 3)) (Rel Lt (Var Real "4") (Int 5)))
                  ,FAnd (FRel Lt (FVar FPDouble "8") (FInt 9)) (FAnd (FRel Lt (FVar FPDouble "2") (FInt 3)) (FRel Lt (FVar FPDouble "4") (FInt 5))))],
            rExprs = [Int 9,Int 10],
            fpExprs = [FInt 9,FInt 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            }
        ]


semEFun__test5 = testCase "it correctly combines function semantics ACebS" $
    actual @?= expected
    where
      actual = semEFun "f" formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
        where
          formalParams = [Arg "x" FPDouble
                         ,Arg "y" FPDouble
                         ]
          actualParams = [FVar FPDouble "a"
                         ,FVar FPDouble "b"
                         ]
          semanticArgumentCombinations = [[xSemantics1,ySemantics1]]
            where
              xSemantics1 = ACeb {
                conds  = Cond [
                    (Rel Lt (Var Real "0") (Int 1),FRel Lt (FVar FPDouble "0") (FInt 1)),
                    (Rel Lt (Var Real "1") (Int 2),FRel Lt (FVar FPDouble "1") (FInt 2))
                    ],
                rExprs = [Int 1,Int 2],
                fpExprs = [FInt 1,FInt 2],
                eExpr  = ErrRat 1,
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics1 = ACeb {
                conds  = Cond [(Rel Lt (Var Real "4") (Int 5),FRel Lt (FVar FPDouble "4") (FInt 5))],
                rExprs = [Int 5,Int 6],
                fpExprs = [FInt 5,FInt 6],
                eExpr  = ErrRat 3,
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1,fSemantics2]
            where
              fSemantics1 = ACeb {
                conds  = Cond [(Rel Lt (Var Real "8") (Int 9), FRel Lt (FVar FPDouble "8") (FInt 9))],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }
              fSemantics2 = ACeb {
                conds  = Cond [(Rel Lt (Var Real "10") (Int 11), FRel Lt (FVar FPDouble "10") (FInt 11))],
                rExprs = [Int 11,Int 12],
                fpExprs = [FInt 11,FInt 12],
                eExpr  = ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }
      expected = [
        ACeb {
            conds =  Cond [
                (And (Rel Lt (Var Real "8") (Int 9)) (And (Rel Lt (Var Real "0") (Int 1)) (Rel Lt (Var Real "4") (Int 5)))
                ,FAnd (FRel Lt (FVar FPDouble "8") (FInt 9)) (FAnd (FRel Lt (FVar FPDouble "0") (FInt 1)) (FRel Lt (FVar FPDouble "4") (FInt 5)))),
                (And (Rel Lt (Var Real "8") (Int 9)) (And (Rel Lt (Var Real "1") (Int 2)) (Rel Lt (Var Real "4") (Int 5)))
                ,FAnd (FRel Lt (FVar FPDouble "8") (FInt 9)) (FAnd (FRel Lt (FVar FPDouble "1") (FInt 2)) (FRel Lt (FVar FPDouble "4") (FInt 5))))
                ],
            rExprs = [Int 9,Int 10],
            fpExprs = [FInt 9,FInt 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Cond [
                (And (Rel Lt (Var Real "10") (Int 11)) (And (Rel Lt (Var Real "0") (Int 1)) (Rel Lt (Var Real "4") (Int 5))),FAnd (FRel Lt (FVar FPDouble "10") (FInt 11)) (FAnd (FRel Lt (FVar FPDouble "0") (FInt 1)) (FRel Lt (FVar FPDouble "4") (FInt 5)))),
                (And (Rel Lt (Var Real "10") (Int 11)) (And (Rel Lt (Var Real "1") (Int 2)) (Rel Lt (Var Real "4") (Int 5))),FAnd (FRel Lt (FVar FPDouble "10") (FInt 11)) (FAnd (FRel Lt (FVar FPDouble "1") (FInt 2)) (FRel Lt (FVar FPDouble "4") (FInt 5))))
                ],
            rExprs = [Int 11,Int 12],
            fpExprs = [FInt 11,FInt 12],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            }
        ]

semEFun__test6 = testCase "it correctly combines function semantics ACebS" $
    actual @?= expected
    where
      actual = semEFun "f" formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
        where
          formalParams = [Arg "x" FPDouble
                         ,Arg "y" FPDouble
                         ]
          actualParams = [FVar FPDouble "a"
                         ,FVar FPDouble "b"
                         ]
          semanticArgumentCombinations = [[xSemantics1,ySemantics1]]
            where
              xSemantics1 = ACeb {
                conds  = Cond [
                    (Rel Lt (Var Real "0") (Int 1),FRel Lt (FVar FPDouble "0") (FInt 1)),
                    (Rel Lt (Var Real "1") (Int 2),FRel Lt (FVar FPDouble "1") (FInt 2))
                    ],
                rExprs = [Int 1,Int 2],
                fpExprs = [FInt 1,FInt 2],
                eExpr  = ErrRat 1,
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics1 = ACeb {
                conds  = Cond [(Rel Lt (Var Real "4") (Int 5),FRel Lt (FVar FPDouble "4") (FInt 5))],
                rExprs = [Int 5,Int 6],
                fpExprs = [FInt 5,FInt 6],
                eExpr  = ErrRat 3,
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1,fSemantics2]
            where
              fSemantics1 = ACeb {
                conds  = Cond [(Rel Lt (Var Real "8") (Int 9), FRel Lt (FVar FPDouble "8") (FInt 9))],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat 5,
                decisionPath = root,
                cFlow = Unstable
                }
              fSemantics2 = ACeb {
                conds  = Cond [(Rel Lt (Var Real "10") (Int 11), FRel Lt (FVar FPDouble "10") (FInt 11))],
                rExprs = [Int 11,Int 12],
                fpExprs = [FInt 11,FInt 12],
                eExpr  = ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }
      expected = [
        ACeb {
            conds =  Cond [
                (And (Rel Lt (Var Real "8") (Int 9)) (And (Rel Lt (Var Real "0") (Int 1)) (Rel Lt (Var Real "4") (Int 5)))
                ,FAnd (FRel Lt (FVar FPDouble "8") (FInt 9)) (FAnd (FRel Lt (FVar FPDouble "0") (FInt 1)) (FRel Lt (FVar FPDouble "4") (FInt 5)))),
                (And (Rel Lt (Var Real "8") (Int 9)) (And (Rel Lt (Var Real "1") (Int 2)) (Rel Lt (Var Real "4") (Int 5)))
                ,FAnd (FRel Lt (FVar FPDouble "8") (FInt 9)) (FAnd (FRel Lt (FVar FPDouble "1") (FInt 2)) (FRel Lt (FVar FPDouble "4") (FInt 5))))
                ],
            rExprs = [Int 9,Int 10],
            fpExprs = [FInt 9,FInt 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Unstable
            },
        ACeb {
            conds = Cond [
                (And (Rel Lt (Var Real "10") (Int 11)) (And (Rel Lt (Var Real "0") (Int 1)) (Rel Lt (Var Real "4") (Int 5))),FAnd (FRel Lt (FVar FPDouble "10") (FInt 11)) (FAnd (FRel Lt (FVar FPDouble "0") (FInt 1)) (FRel Lt (FVar FPDouble "4") (FInt 5)))),
                (And (Rel Lt (Var Real "10") (Int 11)) (And (Rel Lt (Var Real "1") (Int 2)) (Rel Lt (Var Real "4") (Int 5))),FAnd (FRel Lt (FVar FPDouble "10") (FInt 11)) (FAnd (FRel Lt (FVar FPDouble "1") (FInt 2)) (FRel Lt (FVar FPDouble "4") (FInt 5))))
                ],
            rExprs = [Int 11,Int 12],
            fpExprs = [FInt 11,FInt 12],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            }
        ]

semEFun__test7 = testCase "it correctly combines function semantics ACebS" $
    actual @?= expected
    where
      actual = semEFun "f" formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
        where
          formalParams = [Arg "x" FPDouble
                         ,Arg "y" FPDouble
                         ]
          actualParams = [FVar FPDouble "a"
                         ,FVar FPDouble "b"
                         ]
          semanticArgumentCombinations = [[xSemantics1,ySemantics1]]
            where
              xSemantics1 = ACeb {
                conds  = Cond [
                    (Rel Lt (Var Real "0") (Int 1),FRel Lt (FVar FPDouble "0") (FInt 1)),
                    (Rel Lt (Var Real "1") (Int 2),FRel Lt (FVar FPDouble "1") (FInt 2))
                    ],
                rExprs = [Int 1,Int 2],
                fpExprs = [FInt 1,FInt 2],
                eExpr  = ErrRat 1,
                decisionPath = root,
                cFlow = Unstable
                }
              ySemantics1 = ACeb {
                conds  = Cond [(Rel Lt (Var Real "4") (Int 5),FRel Lt (FVar FPDouble "4") (FInt 5))],
                rExprs = [Int 5,Int 6],
                fpExprs = [FInt 5,FInt 6],
                eExpr  = ErrRat 3,
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1]
            where
              fSemantics1 = ACeb {
                conds  = Cond [(Rel Lt (Var Real "8") (Int 9), FRel Lt (FVar FPDouble "8") (FInt 9))],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }
      expected = [
        ACeb {
            conds =  Cond [
                (And (Rel Lt (Var Real "8") (Int 9)) (And (Rel Lt (Var Real "0") (Int 1)) (Rel Lt (Var Real "4") (Int 5)))
                ,FAnd (FRel Lt (FVar FPDouble "8") (FInt 9)) (FAnd (FRel Lt (FVar FPDouble "0") (FInt 1)) (FRel Lt (FVar FPDouble "4") (FInt 5)))),
                (And (Rel Lt (Var Real "8") (Int 9)) (And (Rel Lt (Var Real "1") (Int 2)) (Rel Lt (Var Real "4") (Int 5)))
                ,FAnd (FRel Lt (FVar FPDouble "8") (FInt 9)) (FAnd (FRel Lt (FVar FPDouble "1") (FInt 2)) (FRel Lt (FVar FPDouble "4") (FInt 5))))
                ],
            rExprs = [Int 9,Int 10],
            fpExprs = [FInt 9,FInt 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Unstable
            }
        ]

unfoldLocalVars__tests = testGroup "unfoldLocalVars"
    [testCase "" $
        unfoldLocalVars [] (FCnst FPDouble 1) @?= FCnst FPDouble 1
    ,testCase "" $
        unfoldLocalVars [("x",FPDouble,FCnst FPDouble 1)] (FCnst FPDouble 1) @?= FCnst FPDouble 1
    ,testCase "" $
        unfoldLocalVars [("x",FPDouble,FCnst FPDouble 1)] (FVar FPDouble "x") @?= FCnst FPDouble 1
    ,testCase "" $
        unfoldLocalVars [("y",FPDouble,FCnst FPDouble 1)] (FVar FPDouble "x") @?= FVar FPDouble "x"
    ,testCase "" $
        unfoldLocalVars [("x",FPDouble,FCnst FPDouble 1)
                        ,("y",FPDouble,FCnst FPDouble 2)]
                        (BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FVar FPDouble "y"))
        @?= BinaryFPOp AddOp FPDouble (FCnst FPDouble 1) (FCnst FPDouble 2)
    ,testCase "" $
        unfoldLocalVars [("x",FPDouble,FVar FPDouble "y")
                        ,("y",FPDouble,FCnst FPDouble 2)] (FVar FPDouble "x")
        @?= (FCnst FPDouble 2)
    ,testCase "" $
        unfoldLocalVars [("x", FPDouble, BinaryFPOp AddOp FPDouble (FCnst FPDouble 2) (FVar FPDouble "y"))
                        ,("y", FPDouble, FCnst FPDouble 2)] (FVar FPDouble "x")
        @?= BinaryFPOp AddOp FPDouble (FCnst FPDouble 2) (FCnst FPDouble 2)
    ]

