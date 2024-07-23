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
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

import AbsPVSLang
import AbstractDomain
import AbstractSemantics hiding (functionSemantics)
import Operators
import Data.Set (fromList)

semEquiv :: ACebS -> ACebS -> IO()
semEquiv sem1 sem2 = fromList sem1 @?= fromList sem2

testAbstractSemantics = testGroup "AbstractSemantics"
    [semEFun__tests
    ,equivInterp__tests
    ,unfoldLocalVars__tests
    ,stmSem__tests
    ,unfoldFunCallsInCond__tests
--     ,stableCasesIteSem__tests
    ,bexprStmSem__tests
    ,replaceLetVarsFresh__tests
    ]

replaceLetVarsFresh__tests = testGroup "replaceLetVarsFresh"
  [
--       replaceLetVarsFresh__test1
--   ,replaceLetVarsFresh__test2
--   ,replaceLetVarsFresh__test3
  ]

replaceLetVarsFresh__test1 = testCase "stableCasesIteSem__test1" $
  replaceLetVarsFresh (emptyEnv) [] [ACeb {
          conds   = trueConds,
          rExprs  = RDeclRes [BinaryOp MulOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp MulOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp MulOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }]
  @?=
  [ACeb {
          conds   = trueConds,
          rExprs  = RDeclRes [BinaryOp MulOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp MulOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp MulOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }]

replaceLetVarsFresh__test2 = testCase "stableCasesIteSem__test2" $
  replaceLetVarsFresh (emptyEnv) [Arg "x" TInt] [ACeb {
          conds   = trueConds,
          rExprs  = RDeclRes [BinaryOp MulOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp MulOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp MulOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }]
  @?=
  [ACeb {
          conds   = trueConds,
          rExprs  = RDeclRes [BinaryOp MulOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp MulOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp MulOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }]

replaceLetVarsFresh__test3 = testCase "stableCasesIteSem__test2" $
  replaceLetVarsFresh (emptyEnv) [Arg "x" TInt] [ACeb {
          conds   = trueConds,
          rExprs  = RDeclRes [BinaryOp MulOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [Let [("x", TInt, FVar TInt "t")] (BinaryFPOp MulOp FPDouble (FVar FPDouble "x") (FInt 2))],
          eExpr   = Just $ ErrBinOp MulOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }]
  @?=
  [ACeb {
          conds   = trueConds,
          rExprs  = RDeclRes [BinaryOp MulOp (Var Real "x__0") (Int 2)],
          fpExprs = FDeclRes [Let [("x__0", TInt, FVar TInt "t")] (BinaryFPOp MulOp FPDouble (FVar FPDouble "x__0") (FInt 2))],
          eExpr   = Just $ ErrBinOp MulOp FPDouble (Var Real "x__0") (ErrorMark "x__0" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }]

stableCasesIteSem__tests = testGroup "stableCasesIteSem"
  [stableCasesIteSem__test1
  ,stableCasesIteSem__test2
  ,stableCasesIteSem__test3
  ]

stableCasesIteSem__test1 = testCase "stableCasesIteSem__test1" $
  stableCasesIteSem [] [acebThen] [acebElse] (FRel Lt (FInt 3) (FVar FPDouble "x"))
  `semEquiv`
  [ ACeb { conds  = Conds [Cond {realPathCond = Not (Rel Lt (Int 3) (RealMark "x" ResValue))
                              ,fpPathCond = FNot (FRel Lt (FInt 3) (FVar FPDouble "x"))
                              ,realCond = BTrue
                              ,fpCond = FBTrue}
                          ,Cond {realPathCond = (Rel Lt (Int 3) (RealMark "x" ResValue))
                                ,fpPathCond = FAnd (FRel Lt (FInt 3) (FVar FPDouble "x"))
                                                   (FRel Gt (FVar FPDouble "x") (FInt 0))
                                ,realCond = BTrue
                                ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)
                             ,BinaryOp MulOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)
                             ,BinaryFPOp MulOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ MaxErr [ErrBinOp AddOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0)
                                  ,ErrBinOp MulOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0)],
          decisionPath = root,
          cFlow  = Stable
      }]
  where
    acebThen = ACeb {
          conds = Conds [Cond {realPathCond = BTrue
                              ,fpPathCond = FRel Gt (FVar FPDouble "x") (FInt 0)
                              ,realCond = BTrue
                              ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }
    acebElse = ACeb {
          conds   = trueConds,
          rExprs  = RDeclRes [BinaryOp MulOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp MulOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp MulOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

stableCasesIteSem__test2 = testCase "stableCasesIteSem__test2" $
  stableCasesIteSem [LDP [1]] [acebThen] [acebElse] (FRel Lt (FInt 3) (FVar FPDouble "x"))
  `semEquiv`
  [ ACeb { conds = Conds [Cond {realPathCond = Rel Lt (Int 3) (RealMark "x" ResValue)
                               ,fpPathCond = FAnd (FRel Lt (FInt 3) (FVar FPDouble "x"))
                                                  (FRel Gt (FVar FPDouble "x") (FInt 0))
                              ,realCond = BTrue
                              ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = (LDP [1,0]),
          cFlow  = Stable
      }
    ,ACeb { conds   = Conds [Cond {realPathCond = Not $ Rel Lt (Int 3) (RealMark "x" ResValue)
                                  ,fpPathCond = FNot $ FRel Lt (FInt 3) (FVar FPDouble "x")
                                  ,realCond = BTrue
                                  ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp MulOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp MulOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp MulOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = (LDP [1,1]),
          cFlow  = Stable
      }]
  where
    acebThen = ACeb {
          conds   = Conds [Cond {realPathCond = BTrue
                               ,fpPathCond = FRel Gt (FVar FPDouble "x") (FInt 0)
                              ,realCond = BTrue
                              ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = (LDP [1,0]),
          cFlow  = Stable
      }
    acebElse = ACeb { conds   = trueConds,
          rExprs  = RDeclRes [BinaryOp MulOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp MulOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp MulOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = (LDP [1,1]),
          cFlow  = Stable
      }


stableCasesIteSem__test3 = testCase "stableCasesIteSem__test3" $
  stableCasesIteSem [LDP [0,1]] [acebThen] [acebElse] (FRel Lt (FInt 3) (FVar FPDouble "x"))
  `semEquiv`
  [ ACeb { conds   = Conds [Cond {realPathCond = Rel Lt (Int 3) (RealMark "x" ResValue)
                                ,fpPathCond = FAnd (FRel Lt (FInt 3) (FVar FPDouble "x"))
                                                   (FRel Gt (FVar FPDouble "x") (FInt 0))
                               ,realCond = BTrue
                               ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = (LDP [0,1]),
          cFlow  = Stable
      }
    ,ACeb { conds = Conds [Cond {realPathCond = Not $ Rel Lt (Int 3) (RealMark "x" ResValue)
                                ,fpPathCond = FNot $ FRel Lt (FInt 3) (FVar FPDouble "x")
                               ,realCond = BTrue
                               ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp MulOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp MulOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp MulOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = (LDP [1,1]),
          cFlow  = Stable
      }]
  where
    acebThen = ACeb {
          conds   = Conds [Cond {realPathCond = BTrue
                                ,fpPathCond = FRel Gt (FVar FPDouble "x") (FInt 0)
                               ,realCond = BTrue
                               ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = (LDP [0,1]),
          cFlow  = Stable
      }
    acebElse = ACeb {
          conds   = trueConds,
          rExprs  = RDeclRes [BinaryOp MulOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp MulOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp MulOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = (LDP [1,1]),
          cFlow  = Stable
      }

unfoldFunCallsInCond__tests = testGroup "unfoldFunCallsInCond"
  [ testCase "true condition returns true" $
      unfoldFunCallsInCond undefined trueCond
      @?=
      [trueCond]
  , testCase "condition with no function calls returns itself" $
      unfoldFunCallsInCond undefined Cond {realPathCond = Rel Lt (Int 3) (Var Real "x")
                                          ,fpPathCond = FRel Lt (FInt 3) (FVar FPDouble "x")
                                          ,realCond = BTrue
                                          ,fpCond = FBTrue}
      @?=
      [Cond {realPathCond = Rel Lt (Int 3) (Var Real "x")
           ,fpPathCond = FRel Lt (FInt 3) (FVar FPDouble "x")
           ,realCond = BTrue
           ,fpCond = FBTrue}]
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
  unfoldFunCallsInCond interp Cond {realPathCond = Rel Lt (Var Real "y") (Int 3)
                                   ,fpPathCond = FRel Lt (FEFun False "f" ResValue FPDouble [FVar FPDouble "z"]) (FInt 3)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
  @?=
  [Cond {realPathCond = Rel Lt (Var Real "y") (Int 3)
        ,fpPathCond = FRel Lt (BinaryFPOp AddOp TInt (FVar FPDouble "z") (FInt 2)) (FInt 3)
        ,realCond = BTrue
        ,fpCond = FBTrue}]
  where
    interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], sem))]
    sem = Map.fromList [(ResValue,[aceb])]
    aceb = ACeb { conds   = trueConds,
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp AddOp TInt (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test2 = testCase "unfoldFunCallsInCond__test2" $
  unfoldFunCallsInCond interp Cond {realPathCond = Rel Lt (Var Real "y") (Int 3)
                                   ,fpPathCond = FRel Lt (FEFun False "f" ResValue FPDouble [FVar FPDouble "z"]) (FInt 3)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
  @?=
  [Cond {realPathCond = Rel Lt (Var Real "y") (Int 3)
        ,fpPathCond = FAnd (FRel Gt (FVar FPDouble "z") (FInt 0))
                           (FRel Lt (BinaryFPOp AddOp TInt (FVar FPDouble "z") (FInt 2)) (FInt 3))
        ,realCond = BTrue
        ,fpCond = FBTrue}]
  where
    interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], sem))]
    sem = Map.fromList [(ResValue,[aceb])]
    aceb = ACeb {
          conds   = Conds [Cond {realPathCond = BTrue
                                ,fpPathCond = FRel Gt (FVar FPDouble "x") (FInt 0)
                                ,realCond = BTrue
                                ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp AddOp TInt (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test3 = testCase "unfoldFunCallsInCond__test3" $
  unfoldFunCallsInCond interp Cond {realPathCond = Rel Lt (Var Real "y") (Int 3)
                                   ,fpPathCond = FRel Lt (FEFun False "f" ResValue FPDouble [FVar FPDouble "z"]) (FInt 3)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
  @?=
  [Cond {realPathCond = Rel Lt (Var Real "y") (Int 3)
        ,fpPathCond = FAnd (FRel Gt (FVar FPDouble "z") (FInt 0))
         (FRel Lt (BinaryFPOp AddOp TInt (FVar FPDouble "z") (FInt 2)) (FInt 3))
        ,realCond = BTrue
        ,fpCond = FBTrue}
  ,Cond {realPathCond = Rel Lt (Var Real "y") (Int 3)
        ,fpPathCond = FRel Lt (BinaryFPOp MulOp TInt (FVar FPDouble "z") (FInt 2)) (FInt 3)
        ,realCond = BTrue
        ,fpCond = FBTrue}]
  where
    interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], sem))]
    sem = Map.fromList [(ResValue,[aceb1,aceb2])]
    aceb1 = ACeb {
          conds  = Conds [Cond {realPathCond = BTrue
                                ,fpPathCond = FRel Gt (FVar FPDouble "x") (FInt 0)
                                ,realCond = BTrue
                                ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp AddOp TInt (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }
    aceb2 = ACeb {
          conds   = trueConds,
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp MulOp TInt (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test4 = testCase "unfoldFunCallsInCond__test4" $
  unfoldFunCallsInCond interp Cond {realPathCond = Rel Lt (Var Real "y") (Int 3)
                                   ,fpPathCond = FRel Lt (FEFun False "f" ResValue FPDouble [FVar FPDouble "z"])
                                                         (FEFun False "g" ResValue FPDouble [FVar FPDouble "h"])
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
  @?=
  [Cond {realPathCond = Rel Lt (Var Real "y") (Int 3)
        ,fpPathCond = FAnd (FRel Gt (FVar FPDouble "z") (FInt 0))
                           (FRel Lt (BinaryFPOp AddOp TInt (FVar FPDouble "z") (FInt 2))
                                    (BinaryFPOp MulOp TInt (FVar FPDouble "h") (FInt 2)))
        ,realCond = BTrue
        ,fpCond = FBTrue}]
  where
    interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], semf)),
              ("g",(False, FPDouble, [Arg "x" FPDouble], semg))]
    semf = Map.fromList [(ResValue,[acebf])]
    semg = Map.fromList [(ResValue,[acebg])]
    acebf = ACeb {
          conds = Conds [Cond {realPathCond = BTrue
                                ,fpPathCond = FRel Gt (FVar FPDouble "x") (FInt 0)
                                ,realCond = BTrue
                                ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp AddOp TInt (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }
    acebg = ACeb { conds = trueConds,
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp MulOp TInt (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test5 = testCase "unfoldFunCallsInCond__test5" $
  unfoldFunCallsInCond interp Cond {realPathCond = Rel Lt (Var Real "y") (Int 3)
                                   ,fpPathCond = FRel Lt (FEFun False "f" ResValue FPDouble [FVar FPDouble "z"])
                                                         (FEFun False "g" ResValue FPDouble [FVar FPDouble "h"])
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
  @?=
   [Cond {realPathCond = Rel Lt (Var Real "y") (Int 3)
         ,fpPathCond = FAnd (FRel Gt (FVar FPDouble "z") (FInt 0))
         (FAnd (FRel Lt (FVar FPDouble "h") (FInt 10))
               (FRel Lt (BinaryFPOp AddOp TInt (FVar FPDouble "z") (FInt 2))
                        (BinaryFPOp MulOp TInt (FVar FPDouble "h") (FInt 2))))
         ,realCond = BTrue
         ,fpCond = FBTrue}]
  where
    interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], semf)),
              ("g",(False, FPDouble, [Arg "x" FPDouble], semg))]
    semf = Map.fromList [(ResValue,[acebf])]
    semg = Map.fromList [(ResValue,[acebg])]
    acebf = ACeb {
          conds =  Conds [Cond {realPathCond = BTrue
                                ,fpPathCond = FRel Gt (FVar FPDouble "x") (FInt 0)
                                ,realCond = BTrue
                                ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp AddOp TInt (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }
    acebg = ACeb { conds = Conds [Cond {realPathCond = BTrue
                                ,fpPathCond = FRel Lt (FVar FPDouble "x") (FInt 10)
                                ,realCond = BTrue
                                ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp MulOp TInt (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test6 = testCase "unfoldFunCallsInCond__test6" $
  unfoldFunCallsInCond interp Cond {realPathCond = Rel  Lt (EFun "f" ResValue Real [Var Real "z"]) (Int 3)
                                   ,fpPathCond = FRel Lt (FEFun False "f" ResValue FPDouble [FVar FPDouble "z"]) (FInt 1)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
  @?=
  [Cond {realPathCond = Rel  Lt (BinaryOp AddOp (Var Real "z") (Int 2)) (Int 3)
        ,fpPathCond = FRel Lt (BinaryFPOp AddOp FPDouble (FVar FPDouble "z") (FInt 2)) (FInt 1)
        ,realCond = BTrue
        ,fpCond = FBTrue}]
  where
    interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], sem))]
    sem = Map.fromList [(ResValue,[aceb])]
    aceb = ACeb {
          conds   = trueConds,
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test7 = testCase "unfoldFunCallsInCond__test7" $
  unfoldFunCallsInCond interp Cond {realPathCond = Rel  Lt (EFun "f" ResValue Real [Var Real "z"]) (Int 3)
                                   ,fpPathCond = FRel Lt (FEFun False "f" ResValue FPDouble [FVar FPDouble "z"]) (FInt 1)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
  @?=
  [Cond {realPathCond = And (Rel Lt (Var Real "z") (Int 5))
                            (Rel Lt (BinaryOp AddOp (Var Real "z") (Int 2)) (Int 3))
        ,fpPathCond = FAnd (FRel Gt (FVar FPDouble "z") (FInt 0))
                           (FRel Lt (BinaryFPOp AddOp FPDouble (FVar FPDouble "z") (FInt 2)) (FInt 1))
        ,realCond = BTrue
        ,fpCond = FBTrue}]
  where
    interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], sem))]
    sem = Map.fromList [(ResValue,[aceb])]
    aceb = ACeb {
          conds   = Conds [Cond {realPathCond = Rel Lt (Var Real "x") (Int 5)
                                ,fpPathCond = FRel Gt (FVar FPDouble "x") (FInt 0)
                                ,realCond = BTrue
                                ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test8 = testCase "unfoldFunCallsInCond__test8" $
  unfoldFunCallsInCond interp Cond {realPathCond = Rel Lt (EFun "f" ResValue Real [Var Real "z"])
                                                          (EFun "g" ResValue Real [Var Real "h"])
                                   ,fpPathCond = FRel Lt (FEFun False "f" ResValue FPDouble [FVar FPDouble "z"])
                                                         (FEFun False "g" ResValue FPDouble [FVar FPDouble "h"])
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
  @?=
  [Cond {realPathCond = And (Rel Lt (Var Real "z") (Int 5))
                            (And (Rel Gt (Var Real "h") (Int 0))
                                 (Rel Lt (BinaryOp AddOp (Var Real "z") (Int 2))
                                 (BinaryOp MulOp (Var Real "h") (Int 2))))
        ,fpPathCond = FAnd (FRel Gt (FVar FPDouble "z") (FInt 0))
                           (FAnd (FRel Gt (FVar FPDouble "h") (FInt 0))
                                 (FRel Lt (BinaryFPOp AddOp FPDouble (FVar FPDouble "z") (FInt 2))
                                 (BinaryFPOp MulOp FPDouble (FVar FPDouble "h") (FInt 2))))
        ,realCond = BTrue
        ,fpCond = FBTrue}]
  where
    interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], semf))
             ,("g",(False, FPDouble, [Arg "x" FPDouble], semg))]
    semf = Map.fromList [(ResValue,[acebf])]
    semg = Map.fromList [(ResValue,[acebg])]
    acebf = ACeb {
          conds   = Conds [Cond {realPathCond = Rel Lt (Var Real "x") (Int 5)
                                ,fpPathCond = FRel Gt (FVar FPDouble "x") (FInt 0)
                                ,realCond = BTrue
                                ,fpCond = FBTrue}],
          rExprs = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }
    acebg = ACeb {
          conds = Conds [Cond {realPathCond = Rel Gt (Var Real "x") (Int 0)
                                ,fpPathCond = FRel Gt (FVar FPDouble "x") (FInt 0)
                                ,realCond = BTrue
                                ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp MulOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp MulOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp MulOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test9 = testCase "unfoldFunCallsInCond__test9" $
  unfoldFunCallsInCond interp Cond {realPathCond = Rel Lt (EFun "f" ResValue Real [Var Real "z"])
                                                           (EFun "g" ResValue Real [Var Real "h"])
                                    ,fpPathCond = FRel Lt (FEFun False "f" ResValue FPDouble [FVar FPDouble "z"])
                                                          (FEFun False "g" ResValue FPDouble [FVar FPDouble "h"])
                                    ,realCond = BTrue
                                    ,fpCond = FBTrue}
  @?=
  [cond1,cond2]
  where
    interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], semf))
             ,("g",(False, FPDouble, [Arg "x" FPDouble], semg))]
    semf = Map.fromList [(ResValue,[acebf1,acebf2])]
    semg = Map.fromList [(ResValue,[acebg])]
    acebf1 = ACeb {
          conds   = Conds [Cond {realPathCond = BTrue
                                ,fpPathCond = FRel Gt (FVar FPDouble "x") (FInt 0)
                                ,realCond = BTrue
                                ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }
    acebf2 = ACeb { conds   = trueConds,
          rExprs  = RDeclRes [BinaryOp SubOp (Var Real "x") (Int 3)],
          fpExprs = FDeclRes [BinaryFPOp SubOp FPDouble (FVar FPDouble "x") (FInt 3)],
          eExpr   = Just $ ErrBinOp SubOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 3) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }
    acebg = ACeb { conds = Conds [Cond {realPathCond = Rel Gt (Var Real "x") (Int 0)
                                ,fpPathCond = FRel Gt (FVar FPDouble "x") (FInt 0)
                                ,realCond = BTrue
                                ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp MulOp (Var Real "x") (Int 4)],
          fpExprs = FDeclRes [BinaryFPOp MulOp FPDouble (FVar FPDouble "x") (FInt 4)],
          eExpr   = Just $ ErrBinOp MulOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 4) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }
    cond1 = Cond {realPathCond = And (Rel Gt (Var Real "h") (Int 0))
                                     (Rel Lt (BinaryOp AddOp (Var Real "z") (Int 2))
                                             (BinaryOp MulOp (Var Real "h") (Int 4)))
                 ,fpPathCond = FAnd (FRel Gt (FVar FPDouble "z") (FInt 0))
                                    (FAnd (FRel Gt (FVar FPDouble "h") (FInt 0))
                                    (FRel Lt (BinaryFPOp AddOp FPDouble (FVar FPDouble "z") (FInt 2))
                                             (BinaryFPOp MulOp FPDouble (FVar FPDouble "h") (FInt 4))))
                 ,realCond = BTrue
                 ,fpCond = FBTrue}
    cond2 = Cond {realPathCond = And (Rel Gt (Var Real "h") (Int 0))
                                     (Rel Lt (BinaryOp SubOp (Var Real "z") (Int 3))
                                     (BinaryOp MulOp (Var Real "h") (Int 4)))
                 ,fpPathCond = FAnd (FRel Gt (FVar FPDouble "h") (FInt 0))
                                    (FRel Lt (BinaryFPOp SubOp FPDouble (FVar FPDouble "z") (FInt 3))
                                             (BinaryFPOp MulOp FPDouble (FVar FPDouble "h") (FInt 4)))
                 ,realCond = BTrue
                 ,fpCond = FBTrue}

unfoldFunCallsInCond__test10 = testCase "unfoldFunCallsInCond__test10" $
  unfoldFunCallsInCond interp Cond {realPathCond = Rel Lt (Var Real "y") (Int 3)
                                   ,fpPathCond = FRel Lt (FEFun False "f" ResValue FPDouble [FVar FPDouble "z"]) (FInt 3)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
  @?=
  [Cond {realPathCond = Rel Lt (Var Real "y") (Int 3)
        ,fpPathCond = FOr (FRel Lt (FVar FPDouble "z") (FInt 3)) (FRel Lt (FVar FPDouble "y") (FInt 3))
        ,realCond = BTrue
        ,fpCond = FBTrue}]
  where
    interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], sem))]
    sem = Map.fromList [(ResValue,[aceb])]
    aceb = ACeb {
          conds   = trueConds,
          rExprs = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [FVar FPDouble "x", FVar FPDouble "y"],
          eExpr   = Just $ ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test11 = testCase "unfoldFunCallsInCond__test11" $
  unfoldFunCallsInCond interp Cond {realPathCond = Rel Lt (Var Real "y") (Int 3)
                                   ,fpPathCond = FRel Lt (FEFun False "f" ResValue FPDouble [FVar FPDouble "z"]) (FInt 3)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
  @?=
  [Cond {realPathCond = Rel Lt (Var Real "y") (Int 3)
        ,fpPathCond = FOr (FRel Lt (FVar FPDouble "z") (FInt 3)) (FRel Lt (FVar FPDouble "i") (FInt 3))
        ,realCond = BTrue
        ,fpCond = FBTrue}]
  where
    interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], sem))]
    sem = Map.fromList [(ResValue,[aceb])]
    aceb = ACeb { conds   = trueConds,
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2), (Var Real "x")],
          fpExprs = FDeclRes [FVar FPDouble "x", FVar FPDouble "i"],
          eExpr   = Just $ ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test12 = testCase "unfoldFunCallsInCond__test12" $
  unfoldFunCallsInCond interp Cond {realPathCond = Rel  Lt (EFun "f" ResValue Real [Var Real "z"]) (Int 3)
                                   ,fpPathCond = FRel Lt (FVar FPDouble "y") (FInt 3)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
  @?=
  [Cond {realPathCond = Rel  Lt (BinaryOp AddOp (Var Real "z") (Int 2)) (Int 3)
        ,fpPathCond = FRel Lt (FVar FPDouble "y") (FInt 3)
        ,realCond = BTrue
        ,fpCond = FBTrue}]
  where
    interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], sem))]
    sem = Map.fromList [(ResValue,[aceb])]
    aceb = ACeb { conds   = trueConds,
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test13 = testCase "unfoldFunCallsInCond__test13" $
  unfoldFunCallsInCond interp Cond {realPathCond = Rel  Lt (EFun "f" ResValue Real [Var Real "z"]) (Int 3)
                                   ,fpPathCond = FRel Lt (FVar FPDouble "y") (FInt 3)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
  @?=
  [Cond {realPathCond = Or (Rel Lt (BinaryOp AddOp (Var Real "z") (Int 2)) (Int 3)) (Rel Lt (Var Real "i") (Int 3))
        ,fpPathCond = FRel Lt (FVar FPDouble "y") (FInt 3)
        ,realCond = BTrue
        ,fpCond = FBTrue}]
  where
    interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], sem))]
    sem = Map.fromList [(ResValue,[aceb])]
    aceb = ACeb { conds   = trueConds,
          rExprs = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2), (Var Real "i")],
          fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test14 = testCase "unfoldFunCallsInCond__test14" $
  unfoldFunCallsInCond interp Cond {realPathCond = Rel  Lt (EFun "f" ResValue Real [Var Real "z"]) (Int 3)
                                   ,fpPathCond = FRel Lt (FVar FPDouble "y") (FInt 3)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
  @?=
  [Cond {realPathCond = And (Rel Lt (Var Real "z") (Int 9))
                            (Or (Rel Lt (BinaryOp AddOp (Var Real "z") (Int 2)) (Int 3))
                                (Rel Lt (Var Real "i") (Int 3)))
        ,fpPathCond = FRel Lt (FVar FPDouble "y") (FInt 3)
        ,realCond = BTrue
        ,fpCond = FBTrue}]
  where
    interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], sem))]
    sem = Map.fromList [(ResValue,[aceb])]
    aceb = ACeb {
          conds   = Conds [Cond {realPathCond = Rel Lt (Var Real "x") (Int 9)
                                ,fpPathCond = FBTrue
                                ,realCond = BTrue
                                ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2), (Var Real "i")],
          fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test15 = testCase "unfoldFunCallsInCond__test15" $
  unfoldFunCallsInCond interp Cond {realPathCond = Rel Lt  (EFun "f" ResValue Real [Var  Real     "z"]) (Int 3)
                                   ,fpPathCond = FRel Lt (FEFun False "g" ResValue FPDouble [FVar FPDouble "h"]) (FInt 5)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
  @?=
  [Cond {realPathCond = And (Rel Lt (Var Real "z") (Int 9))
                            (Or (Rel Lt (BinaryOp AddOp (Var Real "z") (Int 2)) (Int 3))
                                (Rel Lt (Var Real "i") (Int 3)))
        ,fpPathCond = FAnd (FRel Gt (FVar FPDouble "h") (FInt 0))
        (FRel Lt (BinaryFPOp MulOp FPDouble (FVar FPDouble "h") (FInt 4)) (FInt 5))
        ,realCond = BTrue
        ,fpCond = FBTrue}]
  where
    interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], semf))
             ,("g",(False, FPDouble, [Arg "x" FPDouble], semg))]
    semf = Map.fromList [(ResValue,[acebf])]
    semg = Map.fromList [(ResValue,[acebg])]
    acebf = ACeb {
          conds = Conds [Cond {realPathCond = Rel Lt (Var Real "x") (Int 9)
                              ,fpPathCond = FBTrue
                              ,realCond = BTrue
                              ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2), (Var Real "i")],
          fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }
    acebg = ACeb {
          conds = Conds [Cond {realPathCond = Rel Gt (Var Real "x") (Int 0)
                              ,fpPathCond = FRel Gt (FVar FPDouble "x") (FInt 0)
                              ,realCond = BTrue
                              ,fpCond = FBTrue}],
          rExprs  = RDeclRes [BinaryOp MulOp (Var Real "x") (Int 4)],
          fpExprs = FDeclRes [BinaryFPOp MulOp FPDouble (FVar FPDouble "x") (FInt 4)],
          eExpr   = Just $ ErrBinOp MulOp FPDouble (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 4) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test16 = testCase "unfoldFunCallsInCond__test16" $
  unfoldFunCallsInCond interp Cond {realPathCond = Rel Lt (EFun "quadrant" ResValue Real [Var  Real "px",Var  Real "py"]) (Int 2)
                                   ,fpPathCond = FNot $ FRel Lt (FEFun False "quadrant" ResValue FPDouble [FVar FPDouble "px",FVar FPDouble "py"]) (FInt 2)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
  @?=
  [Cond {realPathCond = And (Rel GtE (Var Real "px") (Int 0))
                            (Rel GtE (Var Real "py") (Int 0))
        ,fpPathCond = FAnd (FRel GtE (FVar FPDouble "px") (FInt 0))
         (FRel GtE (FVar FPDouble "py") (FInt 0))
        ,realCond = BTrue
        ,fpCond = FBTrue}
  ,Cond {realPathCond = Not $ And (Rel GtE (Var Real "px") (Int 0))
              (Rel GtE (Var Real "py") (Int 0))
        ,fpPathCond = FNot $ FAnd (FRel GtE (FVar FPDouble "px") (FInt 0))
                (FRel GtE (FVar FPDouble "py") (FInt 0))
        ,realCond = BTrue
        ,fpCond = FBTrue}
  ,Cond {realPathCond = And (Rel GtE (Var Real "px") (Int 0))
                            (Rel GtE (Var Real "py") (Int 0))
        ,fpPathCond = FNot $ FAnd (FRel GtE (FVar FPDouble "px") (FInt 0))
                                  (FRel GtE (FVar FPDouble "py") (FInt 0))
        ,realCond = BTrue
        ,fpCond = FBTrue}
  ,Cond {realPathCond = Not $ And (Rel GtE (Var Real "px") (Int 0))
                                  (Rel GtE (Var Real "py") (Int 0))
        ,fpPathCond = FAnd (FRel GtE (FVar FPDouble "px") (FInt 0))
                (FRel GtE (FVar FPDouble "py") (FInt 0))
        ,realCond = BTrue
        ,fpCond = FBTrue}]
  where
    interp = Map.fromList [("quadrant",(False, FPDouble, [Arg "x" FPDouble, Arg "y" FPDouble], sem))]
    sem = Map.fromList [(ResValue,[aceb1,aceb2])]
    aceb1 = ACeb { conds = Conds [Cond {realPathCond = And (Rel GtE (Var Real "x") (Int 0))
                                                           (Rel GtE (Var Real "y") (Int 0))
                                       ,fpPathCond = FAnd (FRel GtE (FVar FPDouble "x") (FInt 0))
                                                          (FRel GtE (FVar FPDouble "y") (FInt 0))
                                       ,realCond = BTrue
                                       ,fpCond = FBTrue}
                                 ,Cond {realPathCond = Not $ And (Rel GtE (Var Real "x") (Int 0))
                                                                 (Rel GtE (Var Real "y") (Int 0))
                                       ,fpPathCond = FNot $ FAnd (FRel GtE (FVar FPDouble "x") (FInt 0))
                                                                 (FRel GtE (FVar FPDouble "y") (FInt 0))
                                       ,realCond = BTrue
                                       ,fpCond = FBTrue}],
          rExprs  = RDeclRes [Int  1, Int  4],
          fpExprs = FDeclRes [FInt 1, FInt 4],
          eExpr   = Just $ ErrRat 0,
          decisionPath = root,
          cFlow  = Stable
      }
    aceb2 = ACeb { conds = Conds [Cond {realPathCond = And (Rel GtE (Var Real "x") (Int 0))
                                                           (Rel GtE (Var Real "y") (Int 0))
                                       ,fpPathCond = FNot $ FAnd (FRel GtE (FVar FPDouble "x") (FInt 0))
                                                    (FRel GtE (FVar FPDouble "y") (FInt 0))
                                       ,realCond = BTrue
                                       ,fpCond = FBTrue}
                                 ,Cond {realPathCond = Not $ And (Rel GtE (Var Real "x") (Int 0))
                                              (Rel GtE (Var Real "y") (Int 0))
                                       ,fpPathCond = FAnd (FRel GtE (FVar FPDouble "x") (FInt 0))
                                                (FRel GtE (FVar FPDouble "y") (FInt 0))
                                       ,realCond = BTrue
                                       ,fpCond = FBTrue}],
          rExprs  = RDeclRes [Int  1, Int  4],
          fpExprs = FDeclRes [FInt 1, FInt 4],
          eExpr   = Just $ ErrRat 0,
          decisionPath = root,
          cFlow  = Unstable
      }

unfoldFunCallsInCond__test17 = testCase "unfoldFunCallsInCond__test17" $
  unfoldFunCallsInCond interp Cond {realPathCond = Rel Lt (Var Real "y") (Int 3)
                                   ,fpPathCond = FRel Lt (FEFun False "f" ResValue FPDouble [FVar FPDouble "z"]) (FInt 3)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
  @?=
  [Cond {realPathCond = Rel Lt (Var Real "y") (Int 3)
        ,fpPathCond = FAnd (FOr (FRel Gt (FVar FPDouble "z") (FInt 0))
                           (FRel Lt (FVar FPDouble "z") (FInt 10)))
                           (FRel Lt (BinaryFPOp AddOp TInt (FVar FPDouble "z") (FInt 2)) (FInt 3))
        ,realCond = BTrue
        ,fpCond = FBTrue}]
  where
    interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], sem))]
    sem = Map.fromList [(ResValue,[aceb])]
    aceb = ACeb { conds = Conds [Cond {realPathCond = BTrue
                                   ,fpPathCond = FRel Gt (FVar FPDouble "x") (FInt 0)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
                                ,Cond {realPathCond = BTrue
                                   ,fpPathCond = FRel Lt (FVar FPDouble "x") (FInt 10)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}],
          rExprs = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 2)],
          fpExprs = FDeclRes [BinaryFPOp AddOp TInt (FVar FPDouble "x") (FInt 2)],
          eExpr   = Just $ ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
          decisionPath = root,
          cFlow  = Stable
      }

unfoldFunCallsInCond__test18 = testCase "unfoldFunCallsInCond__test18" $
  unfoldFunCallsInCond interp Cond {realPathCond = Rel Gt (EFun "f" ResValue Real [Var Real "z"]) (Int 0)
                                   ,fpPathCond = FRel LtE (FEFun False "f" ResValue FPDouble [FVar FPDouble "z"]) (FInt 0)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
  @?=
  [Cond {realPathCond = And (Rel GtE (Var Real "z") (Int  1)) (Rel Gt (Var Real "z") (Int 0))
        ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "z") (FInt 1)) (FRel LtE (FVar FPDouble "z") (FInt 0))
        ,realCond = BTrue
        ,fpCond = FBTrue}
  ,Cond {realPathCond = And  (Rel GtE (Var Real "z") (Int  3)) (Rel Gt (Var Real "z") (Int  0))
        ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "z") (FInt 3)) (FRel LtE (FVar FPDouble "z") (FInt 0))
        ,realCond = BTrue
        ,fpCond = FBTrue}]
  where
    interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], sem))]
    sem = Map.fromList [(ResValue,[aceb])]
    aceb = ACeb {
      conds = Conds [Cond {realPathCond = Rel GtE (Var Real "x") (Int 1)
                          ,fpPathCond = FRel Lt (FVar FPDouble "x") (FInt 1)
                          ,realCond = BTrue
                          ,fpCond = FBTrue}
                    ,Cond {realPathCond = Rel GtE (Var Real "x") (Int 3)
                          ,fpPathCond = FRel Lt (FVar FPDouble "x") (FInt 3)
                          ,realCond = BTrue
                          ,fpCond = FBTrue}],
      rExprs  = RDeclRes [Var Real "x"],
      fpExprs = FDeclRes [FVar FPDouble "x"],
      eExpr   = Just $ ErrBinOp AddOp TInt (Var Real "x") (ErrorMark "x" ResValue FPDouble) (Int 2) (ErrRat 0),
      decisionPath = root,
      cFlow  = Stable
      }

bexprStmSem__tests = testGroup "BExpr stmSem tests"
  [bexprStmSem__True
  ,bexprStmSem__False
--   ,bexprStmSem__Lt
--   ,bexprStmSem__Gt
--   ,bexprStmSem__And
--   ,bexprStmSem__And_div
--   ,bexprStmSem__Or
--   ,bexprStmSem__Not
--   ,bexprStmSem__Gt_div
--   ,bexprStmSem__Ite
  ]

bexprStmSem__True = testCase "True Semantics" $
  bexprStmSem (BExpr FBTrue) emptyInterp (emptyEnv) semConf (LDP []) [LDP []]
  `semEquiv`
  [ACeb {conds  = trueConds,
         rExprs = RPredRes [RBExpr BTrue] ,
         fpExprs = FPredRes [BExpr FBTrue],
         eExpr = Nothing,
         decisionPath = root,
         cFlow  = Stable
        }]

bexprStmSem__False = testCase "False Semantics" $
  bexprStmSem (BExpr FBFalse) emptyInterp (emptyEnv) semConf (LDP []) [LDP []]
  `semEquiv`
  [ACeb {conds  = trueConds,
         rExprs = RPredRes [RBExpr BFalse] ,
         fpExprs = FPredRes [BExpr FBFalse],
         eExpr = Nothing,
         decisionPath = root,
         cFlow  = Stable
        }]

bexprStmSem__Lt = testCase "Lt Semantics" $
  bexprStmSem (BExpr (FRel Lt (FVar FPDouble "X") (FInt 3))) emptyInterp (emptyEnv) semConf (LDP []) [LDP []]
  `semEquiv`
  [ACeb {conds = trueConds,
         rExprs = RPredRes [RBExpr $ Rel Lt (RealMark "X" ResValue) (Int 3)] ,
         fpExprs = FPredRes [BExpr $ FRel Lt (FVar FPDouble "X") (FInt 3)],
         eExpr = Nothing,
         decisionPath = root,
         cFlow  = Stable}]


bexprStmSem__Gt = testCase "Gt Semantics" $
  bexprStmSem (BExpr $ FRel Gt (FVar FPDouble "Y") (FInt 2)) emptyInterp (emptyEnv) semConf (LDP []) [LDP []]
  `semEquiv`
  [ACeb {conds  = trueConds,
         rExprs = RPredRes [RBExpr $ Rel Gt (RealMark "Y" ResValue) (Int 2)] ,
         fpExprs = FPredRes [BExpr $ FRel Gt (FVar FPDouble "Y") (FInt 2)],
         eExpr = Nothing,
         decisionPath = root,
         cFlow  = Stable
        }]

bexprStmSem__Gt_div = testCase "Gt Semantics with division" $
  bexprStmSem (BExpr $ FRel Gt (FVar FPDouble "Y")
                       (BinaryFPOp DivOp FPDouble (FInt 6) (FVar FPDouble "X")))
              emptyInterp (emptyEnv) semConf (LDP []) [LDP []]
  `semEquiv`
  [ACeb {conds  = Conds [Cond {realPathCond = BTrue
                              ,fpPathCond = FBTrue
                              ,realCond = Rel Neq (Var Real "X") (Int 0)
                              ,fpCond = FRel Neq (FVar FPDouble "X") (TypeCast TInt FPDouble (FInt 0))}],
         rExprs = RPredRes [RBExpr $ Rel Gt (Var Real "Y") (BinaryOp DivOp (Int 6) (Var Real "X"))] ,
         fpExprs = FPredRes [BExpr $ FRel Gt (FVar FPDouble "Y") (BinaryFPOp DivOp FPDouble (FInt 6) (FVar FPDouble "X"))],
         eExpr = Nothing,
         decisionPath = root,
         cFlow  = Stable
        }]

bexprStmSem__And = testCase "And Semantics" $
  bexprStmSem (BExpr $ FAnd (FRel Gt (FVar FPDouble "Y") (FInt 2))(FRel Lt (FVar FPDouble "X") (FInt 3))) emptyInterp (emptyEnv) semConf (LDP []) [LDP []]
  `semEquiv`
  [ACeb {conds  = trueConds,
         rExprs = RPredRes [RBExpr $ And (Rel Gt (RealMark "Y" ResValue) (Int 2))(Rel Lt (RealMark "X" ResValue) (Int 3))],
         fpExprs = FPredRes [BExpr $ FAnd (FRel Gt (FVar FPDouble "Y") (FInt 2))(FRel Lt (FVar FPDouble "X") (FInt 3))],
         eExpr = Nothing,
         decisionPath = root,
         cFlow  = Stable
        }]

bexprStmSem__And_div = testCase "And Semantics with division" $
  bexprStmSem (BExpr $ FAnd (FRel Gt (FVar FPDouble "Y")
                       (BinaryFPOp DivOp FPDouble (FInt 6) (FVar FPDouble "X")))
                       (FRel Lt (FVar FPDouble "X") (FInt 3))) emptyInterp (emptyEnv) semConf (LDP []) [LDP []]
  `semEquiv`
  [ACeb {conds  = Conds [Cond {realPathCond = BTrue
                              ,fpPathCond = FBTrue
                              ,realCond = Rel Neq (RealMark "X" ResValue) (Int 0)
                              ,fpCond = FRel Neq (FVar FPDouble "X") (TypeCast TInt FPDouble (FInt 0))}],
         rExprs = RPredRes [RBExpr $ And (Rel Gt (RealMark "Y" ResValue) (BinaryOp DivOp (Int 6) (RealMark "X" ResValue)))(Rel Lt (RealMark "X" ResValue) (Int 3))],
         fpExprs = FPredRes [BExpr $ FAnd (FRel Gt (FVar FPDouble "Y")
                       (BinaryFPOp DivOp FPDouble (FInt 6) (FVar FPDouble "X")))(FRel Lt (FVar FPDouble "X") (FInt 3))],
         eExpr = Nothing,
         decisionPath = root,
         cFlow  = Stable
        }]

bexprStmSem__Or = testCase "Or Semantics" $
  bexprStmSem (BExpr $ FOr (FRel Gt (FVar FPDouble "Y") (FInt 2))(FRel Lt (FVar FPDouble "X") (FInt 3))) emptyInterp (emptyEnv) semConf (LDP []) [LDP []]
  `semEquiv`
  [ACeb {conds  = trueConds,
         rExprs = RPredRes [RBExpr $ Or (Rel Gt (RealMark "Y" ResValue) (Int 2))(Rel Lt (RealMark "X" ResValue) (Int 3))],
         fpExprs = FPredRes [BExpr $ FOr (FRel Gt (FVar FPDouble "Y") (FInt 2))(FRel Lt (FVar FPDouble "X") (FInt 3))],
         eExpr = Nothing,
         decisionPath = root,
         cFlow  = Stable
        }]

bexprStmSem__Not = testCase "Not Semantics" $
  bexprStmSem (BExpr $ FNot (FRel Gt (FVar FPDouble "Y") (FInt 2))) emptyInterp (emptyEnv) semConf (LDP []) [LDP []]
  `semEquiv`
  [ACeb {conds  = trueConds,
         rExprs = RPredRes [RBExpr $ Not $ Rel Gt (RealMark "Y" ResValue) (Int 2)],
         fpExprs = FPredRes [BExpr $ FNot $ FRel Gt (FVar FPDouble "Y") (FInt 2)],
         eExpr = Nothing,
         decisionPath = root,
         cFlow  = Stable
        }]

bexprStmSem__Ite = testCase "BIte Semantics" $
    bexprStmSem (BIte (FRel Lt (FVar FPDouble "X") (FInt 3))
                      (BExpr $ FRel Gt (FVar FPDouble "Y") (FInt 2))
                      (BExpr $ FRel Gt (FVar FPDouble "Z") (FInt 1)))
                emptyInterp (emptyEnv) semConf root [LDP []]
    `semEquiv`
    [ACeb {conds = Conds [Cond {realPathCond = Rel Lt (Var Real "X") (Int 3)
                              ,fpPathCond = FRel Lt (FVar FPDouble "X") (FInt 3)
                              ,realCond = BTrue
                              ,fpCond = FBTrue}]
          ,rExprs  = RPredRes [RBExpr $ Rel Gt (RealMark "Y" ResValue) (Int 2)]
          ,fpExprs = FPredRes [BExpr $ FRel Gt (FVar FPDouble "Y") (FInt 2)]
          ,eExpr = Nothing
          ,decisionPath = LDP [0]
          ,cFlow = Stable}
    ,ACeb {conds = Conds [Cond {realPathCond = Not (Rel Lt (Var Real "X") (Int 3))
                              ,fpPathCond = FNot (FRel Lt (FVar FPDouble "X") (FInt 3))
                              ,realCond = BTrue
                              ,fpCond = FBTrue}]
          ,rExprs = RPredRes [RBExpr $ Rel Gt (RealMark "Z" ResValue) (Int 1)]
          ,fpExprs = FPredRes [BExpr $ FRel Gt (FVar FPDouble "Z") (FInt 1)]
          ,eExpr = Nothing
          ,decisionPath = LDP [1]
          ,cFlow = Stable}
    ,ACeb {conds = Conds [Cond {realPathCond = Not (Rel Lt (Var Real "X") (Int 3))
                              ,fpPathCond = FRel Lt (FVar FPDouble "X") (FInt 3)
                              ,realCond = BTrue
                              ,fpCond = FBTrue},
                          Cond {realPathCond = Rel Lt (Var Real "X") (Int 3)
                              ,fpPathCond = FNot (FRel Lt (FVar FPDouble "X") (FInt 3))
                              ,realCond = BTrue
                              ,fpCond = FBTrue}]
          ,rExprs = RPredRes [RBExpr $ Rel Gt (RealMark "Y" ResValue) (Int 2)
                             ,RBExpr $ Rel Gt (RealMark "Z" ResValue) (Int 1)]
          ,fpExprs = FPredRes [BExpr $ FRel Gt (FVar FPDouble "Y") (FInt 2)
                              ,BExpr $ FRel Gt (FVar FPDouble "Z") (FInt 1)]
          ,eExpr = Nothing
          ,decisionPath = LDP []
          ,cFlow = Unstable}
    ]

stmSem__tests = testGroup "stmSem tests"
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
    ,stmSem__Floor__Improved
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
--     ,stmSem__LetIn7
    ,stmSem__UnstWarning
--     ,stmSem__IteIntAdd
    ,stmSem__ErrFun1
    ,stmSem__ErrFun2
    -- ,stmSem__LetInMul
   ,stmSem__map
    ]

semConf = SemConf { improveError = False
                  , assumeTestStability = False
                  , mergeUnstables = True
                  , unfoldFunCalls = True}

semConfImproveError = SemConf { improveError = True
                              , assumeTestStability = False
                              , mergeUnstables = True
                              , unfoldFunCalls = True}

semConfAbs = SemConf { improveError = False
                  , assumeTestStability = False
                  , mergeUnstables = True
                  , unfoldFunCalls = False}

stmSem__IntAdd = testCase "IntAdd" $
    stmSem (BinaryFPOp AddOp TInt (FInt 1) (FInt 2)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [BinaryOp AddOp(Int 1) (Int 2)],
            fpExprs = FDeclRes [BinaryFPOp AddOp TInt (FInt 1) (FInt 2)],
            eExpr   = Just $ ErrBinOp AddOp TInt (Int 1) (ErrRat 0) (Int 2) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Add = testCase "Add" $
    stmSem (BinaryFPOp AddOp FPDouble (FCnst FPDouble 0.1) (FCnst FPDouble 2)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [BinaryOp AddOp(Rat 0.1) (Rat 2)],
            fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FCnst FPDouble 0.1) (FCnst FPDouble 2)],
            eExpr   = Just $ ErrBinOp AddOp FPDouble (Rat 0.1) (ErrRat (1 % 180143985094819840))
                                       (Rat 2) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__IntSub = testCase "IntSub" $
    stmSem (BinaryFPOp SubOp TInt (FInt 1) (FInt 2)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [BinaryOp SubOp(Int 1) (Int 2)],
            fpExprs = FDeclRes [BinaryFPOp SubOp TInt (FInt 1) (FInt 2)],
            eExpr   = Just $ ErrBinOp SubOp TInt (Int 1) (ErrRat 0) (Int 2) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Sub = testCase "Sub" $
    stmSem (BinaryFPOp SubOp FPDouble (FCnst FPDouble 0.1) (FCnst FPDouble 2)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [
     ACeb { conds   = trueConds,
            rExprs  = RDeclRes [BinaryOp SubOp(Rat 0.1) (Rat 2)],
            fpExprs = FDeclRes [BinaryFPOp SubOp FPDouble (FCnst FPDouble 0.1) (FCnst FPDouble 2)],
            eExpr   = Just $ ErrBinOp SubOp FPDouble (Rat 0.1) (ErrRat (1 % 180143985094819840))
                                       (Rat 2) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__IntMul = testCase "IntMul" $
    stmSem (BinaryFPOp MulOp TInt (FInt 1) (FInt 3)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [BinaryOp MulOp (Int 1) (Int 3)],
            fpExprs = FDeclRes [BinaryFPOp MulOp TInt (FInt 1) (FInt 3)],
            eExpr   = Just $ ErrBinOp MulOp TInt (Int 1) (ErrRat 0) (Int 3) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Mul = testCase "Mul" $
    stmSem (BinaryFPOp MulOp FPDouble (FCnst FPDouble 0.1) (FCnst FPDouble 3)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [BinaryOp MulOp (Rat 0.1) (Rat 3)],
            fpExprs = FDeclRes [BinaryFPOp MulOp FPDouble (FCnst FPDouble 0.1) (FCnst FPDouble 3)],
            eExpr   = Just $ ErrBinOp MulOp FPDouble (Rat 0.1) (ErrRat (1 % 180143985094819840))
                                       (Rat 3) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__MulPow2 = testCase "Mul power of 2" $
    stmSem (BinaryFPOp MulOp FPDouble (FCnst FPDouble 0.1) (FCnst FPDouble 2)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds  = Conds [Cond {realPathCond = BTrue
                                 ,fpPathCond = FBTrue
                                 ,realCond = Rel Lt (Int 1) (BinaryOp SubOp (Prec FPDouble) (FExp (FCnst FPDouble (1 % 10))))
                                 ,fpCond = FBTrue}],
            rExprs  = RDeclRes [BinaryOp MulOp (Rat 0.1) (Rat 2)],
            fpExprs = FDeclRes [BinaryFPOp MulOp FPDouble (FCnst FPDouble 0.1) (FCnst FPDouble 2)],
            eExpr   = Just $ ErrMulPow2R FPDouble 1 (ErrRat (1 % 180143985094819840)),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Div1 = testCase "Div" $
    stmSem (BinaryFPOp DivOp FPDouble (FCnst FPDouble 6) (FCnst FPDouble 3)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [BinaryOp DivOp (Rat 6) (Rat 3)],
            fpExprs = FDeclRes [BinaryFPOp DivOp FPDouble (FCnst FPDouble 6) (FCnst FPDouble 3)],
            eExpr   = Just $ ErrBinOp DivOp FPDouble (Rat 6) (ErrRat 0)
                                       (Rat 3) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Div2 = testCase "Div" $
    stmSem (BinaryFPOp DivOp FPDouble (FCnst FPDouble 6) (FVar FPDouble "X")) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = Conds [Cond {realPathCond = BTrue
                                 ,fpPathCond = FBTrue
                                 ,realCond = Rel Neq (RealMark "X" ResValue) (Int 0)
                                 ,fpCond = FRel Neq (FVar FPDouble "X") (TypeCast TInt FPDouble (FInt 0))}],
            rExprs  = RDeclRes [BinaryOp DivOp (Rat 6) (RealMark "X" ResValue)],
            fpExprs = FDeclRes [BinaryFPOp DivOp FPDouble (FCnst FPDouble 6) (FVar FPDouble "X")],
            eExpr   = Just $ ErrBinOp DivOp FPDouble (Rat 6) (ErrRat 0)
                                       (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__IDiv = testCase "IDiv" $
    stmSem (BinaryFPOp IDivOp TInt (FInt 6) (FInt 3)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [BinaryOp IDivOp (Int 6) (Int 3)],
            fpExprs = FDeclRes [BinaryFPOp IDivOp TInt (FInt 6) (FInt 3)],
            eExpr   = Just $ ErrBinOp IDivOp TInt (Int 6) (ErrRat 0)
                                       (Int 3) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__ItDiv = testCase "ItDiv" $
    stmSem (BinaryFPOp ItDivOp TInt (FInt 6) (FInt 3)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [BinaryOp ItDivOp (Int 6) (Int 3)],
            fpExprs = FDeclRes [BinaryFPOp ItDivOp TInt (FInt 6) (FInt 3)],
            eExpr   = Just $ ErrBinOp ItDivOp TInt (Int 6) (ErrRat 0)
                                       (Int 3) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__IMod = testCase "IMod" $
    stmSem (BinaryFPOp ModOp TInt (FInt 6) (FInt 3)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [BinaryOp ModOp (Int 6) (Int 3)],
            fpExprs = FDeclRes [BinaryFPOp ModOp TInt (FInt 6) (FInt 3)],
            eExpr   = Just $ ErrBinOp ModOp TInt (Int 6) (ErrRat 0)
                                       (Int 3) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__ItMod = testCase "ItMod" $
    stmSem (BinaryFPOp ItModOp TInt (FInt 6) (FInt 3)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [BinaryOp ItModOp (Int 6) (Int 3)],
            fpExprs = FDeclRes [BinaryFPOp ItModOp TInt (FInt 6) (FInt 3)],
            eExpr   = Just $ ErrBinOp ItModOp TInt (Int 6) (ErrRat 0)
                                       (Int 3) (ErrRat 0),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Neg = testCase "Neg" $
    stmSem (UnaryFPOp NegOp FPDouble (FCnst FPDouble 0.1)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [UnaryOp NegOp (Rat 0.1)],
            fpExprs = FDeclRes [UnaryFPOp NegOp FPDouble (FCnst FPDouble 0.1)],
            eExpr   = Just $ ErrUnOp NegOp FPDouble (Rat 0.1) (ErrRat $ 1 % 180143985094819840),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Abs = testCase "Abs" $
    stmSem (UnaryFPOp AbsOp FPDouble (FCnst FPDouble 0.1)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [UnaryOp   AbsOp (Rat 0.1)],
            fpExprs = FDeclRes [UnaryFPOp AbsOp FPDouble (FCnst FPDouble 0.1)],
            eExpr   = Just $ ErrUnOp AbsOp FPDouble (Rat 0.1) (ErrRat $ 1 % 180143985094819840),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Ln = testCase "Ln" $
    stmSem (UnaryFPOp LnOp FPDouble (FCnst FPDouble 0.1)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = Conds [Cond {realPathCond = BTrue
                              ,fpPathCond = FBTrue
                              ,realCond = And (Rel Lt (Int 0) (BinaryOp SubOp(Rat (1 % 10)) (ErrRat (1 % 180143985094819840))))
                                 (Rel Gt (FromFloat FPDouble (FCnst FPDouble (1 % 10))) (Int 0))
                              ,fpCond = FBTrue}],
            rExprs  = RDeclRes [UnaryOp   LnOp (Rat 0.1)],
            fpExprs = FDeclRes [UnaryFPOp LnOp FPDouble (FCnst FPDouble 0.1)],
            eExpr   = Just $ ErrUnOp LnOp FPDouble (Rat 0.1) (ErrRat $ 1 % 180143985094819840),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Expo = testCase "Expo" $
    stmSem (UnaryFPOp ExpoOp FPDouble (FCnst FPDouble 0.1)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [UnaryOp   ExpoOp (Rat 0.1)],
            fpExprs = FDeclRes [UnaryFPOp ExpoOp FPDouble (FCnst FPDouble 0.1)],
            eExpr   = Just $ ErrUnOp ExpoOp FPDouble (Rat 0.1) (ErrRat $ 1 % 180143985094819840),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Floor = testCase "Floor" $
    stmSem (UnaryFPOp FloorOp FPDouble (FCnst FPDouble 0.1)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
     [ACeb {conds = trueConds,
            rExprs = RDeclRes [UnaryOp FloorOp (Rat (1 % 10))],
            fpExprs = FDeclRes [UnaryFPOp FloorOp FPDouble (FCnst FPDouble (1 % 10))],
            eExpr = Just $ ErrUnOp FloorOp FPDouble (Rat (1 % 10)) (ErrRat (1 % 180143985094819840)),
            decisionPath = root,
            cFlow = Stable}]

stmSem__Floor__Improved = testCase "Floor_improved" $
    stmSem (UnaryFPOp FloorOp FPDouble (FCnst FPDouble 0.1)) emptyInterp (emptyEnv) semConfImproveError (LDP []) [] @?=
     [ACeb {conds = Conds [Cond {realPathCond = BTrue
                                ,fpPathCond = FBTrue
                                ,realCond = Or (Rel Neq (UnaryOp FloorOp (Rat (1 % 10))) (UnaryOp FloorOp (BinaryOp SubOp (Rat (1 % 10)) (ErrRat (1 % 180143985094819840))))) (Rel Neq (UnaryOp FloorOp (Rat (1 % 10))) (UnaryOp FloorOp (BinaryOp AddOp (Rat (1 % 10)) (ErrRat (1 % 180143985094819840)))))
                              ,fpCond = FBTrue}],
            rExprs = RDeclRes [UnaryOp FloorOp (Rat (1 % 10))],
            fpExprs = FDeclRes [UnaryFPOp FloorOp FPDouble (FCnst FPDouble (1 % 10))],
            eExpr = Just $ ErrUnOp FloorOp FPDouble (Rat (1 % 10)) (ErrRat (1 % 180143985094819840)),
            decisionPath = root,
            cFlow = Stable},
      ACeb {conds = Conds [Cond {realPathCond = BTrue
                                ,fpPathCond = FBTrue
                                ,realCond = And (Rel Eq (UnaryOp FloorOp (Rat (1 % 10)))
                                   (UnaryOp FloorOp (BinaryOp SubOp(Rat (1 % 10)) (ErrRat (1 % 180143985094819840)))))
                               (Rel Eq (UnaryOp FloorOp (Rat (1 % 10)))
                                   (UnaryOp FloorOp (BinaryOp AddOp(Rat (1 % 10)) (ErrRat (1 % 180143985094819840)))))
                                ,fpCond = FBTrue}],
            rExprs = RDeclRes [UnaryOp FloorOp (Rat (1 % 10))],
            fpExprs = FDeclRes [UnaryFPOp FloorOp FPDouble (FCnst FPDouble (1 % 10))],
            eExpr = Just $ ErrFloorNoRound FPDouble (Rat (1 % 10)) (ErrRat (1 % 180143985094819840)),
            decisionPath = root,
            cFlow = Stable}]

stmSem__Sqrt = testCase "Sqrt" $
    stmSem (UnaryFPOp SqrtOp FPDouble (FCnst FPDouble 0.1)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = Conds [Cond {realPathCond = BTrue
                                 ,fpPathCond = FBTrue
                                 ,realCond =Rel GtE (BinaryOp SubOp(Rat 0.1) (ErrRat $ 1 % 180143985094819840)) (Int 0)
                                 ,fpCond = FBTrue}],
            rExprs  = RDeclRes [UnaryOp   SqrtOp (Rat 0.1)],
            fpExprs = FDeclRes [UnaryFPOp SqrtOp FPDouble (FCnst FPDouble 0.1)],
            eExpr   = Just $ ErrUnOp SqrtOp FPDouble (Rat 0.1) (ErrRat $ 1 % 180143985094819840),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Sin = testCase "Sin" $
    stmSem (UnaryFPOp SinOp FPDouble (FCnst FPDouble 0.1)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [UnaryOp   SinOp (Rat 0.1)],
            fpExprs = FDeclRes [UnaryFPOp SinOp FPDouble (FCnst FPDouble 0.1)],
            eExpr   = Just $ ErrUnOp SinOp FPDouble (Rat 0.1) (ErrRat $ 1 % 180143985094819840),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Cos = testCase "Cos" $
    stmSem (UnaryFPOp CosOp FPDouble (FCnst FPDouble 0.1)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [UnaryOp   CosOp (Rat 0.1)],
            fpExprs = FDeclRes [UnaryFPOp CosOp FPDouble (FCnst FPDouble 0.1)],
            eExpr   = Just $ ErrUnOp CosOp FPDouble (Rat 0.1) (ErrRat $ 1 % 180143985094819840),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Atan = testCase "ATan" $
    stmSem (UnaryFPOp AtanOp FPDouble (FCnst FPDouble 0.1)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [UnaryOp   AtanOp (Rat 0.1)],
            fpExprs = FDeclRes [UnaryFPOp AtanOp FPDouble (FCnst FPDouble 0.1)],
            eExpr   = Just $ ErrUnOp AtanOp FPDouble (Rat 0.1) (ErrRat $ 1 % 180143985094819840),
            decisionPath = root,
            cFlow  = Stable
        }
    ]

stmSem__StoD = testCase "StoD" $
    stmSem (TypeCast FPSingle FPDouble (FCnst FPSingle 0.1)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [Rat 0.1],
            fpExprs = FDeclRes [TypeCast FPSingle FPDouble (FCnst FPSingle 0.1)],
            eExpr   = Just $ ErrRat (1 % 671088640),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__DtoS = testCase "DtoS" $
    stmSem (TypeCast FPDouble FPSingle (FCnst FPDouble 0.1)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [Rat 0.1],
            fpExprs = FDeclRes [TypeCast FPDouble FPSingle (FCnst FPDouble 0.1)],
            eExpr   = Just $ ErrRat (1 % 671088640),
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__ItoS = testCase "ItoS" $
    stmSem (TypeCast TInt FPSingle (FInt 1)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [Int 1],
            fpExprs = FDeclRes [FInt 1],
            eExpr   = Just $ ErrRat 0,
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__ItoD = testCase "ItoD" $
    stmSem (TypeCast TInt FPDouble (FInt 1)) emptyInterp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb { conds   = trueConds,
            rExprs  = RDeclRes [Int 1],
            fpExprs = FDeclRes [FInt 1],
            eExpr   = Just $ ErrRat 0,
            decisionPath = root,
            cFlow  = Stable
        }]

stmSem__Ite = testCase "Ite" $
    stmSem (Ite (FRel Lt (FVar FPDouble "X") (FInt 3)) (FInt 0) (FInt 1)) emptyInterp (emptyEnv) semConf (LDP []) [LDP []]
    `semEquiv`
    [ACeb {conds = Conds [Cond {realPathCond = Rel Lt (Var Real "X") (Int 3)
                                 ,fpPathCond = FRel Lt (FVar FPDouble "X") (FInt 3)
                                 ,realCond = BTrue
                                 ,fpCond = FBTrue}]
          ,rExprs = RDeclRes [Int 0]
          ,fpExprs = FDeclRes [FInt 0]
          ,eExpr = Just $ ErrRat 0
          ,decisionPath = LDP [0]
          ,cFlow = Stable}
    ,ACeb {conds = Conds [Cond {realPathCond = Not $ Rel Lt (Var Real "X") (Int 3)
                                 ,fpPathCond = FNot $ FRel Lt (FVar FPDouble "X") (FInt 3)
                                 ,realCond = BTrue
                                 ,fpCond = FBTrue}]
          ,rExprs = RDeclRes [Int 1]
          ,fpExprs = FDeclRes [FInt 1]
          ,eExpr = Just $ ErrRat 0
          ,decisionPath = LDP [1]
          ,cFlow = Stable}
    ,ACeb {conds = Conds [Cond {realPathCond = Not $ Rel Lt (Var Real "X") (Int 3)
                                 ,fpPathCond = FRel Lt (FVar FPDouble "X") (FInt 3)
                                 ,realCond = BTrue
                                 ,fpCond = FBTrue}
                         ,Cond {realPathCond = Rel Lt (Var Real "X") (Int 3)
                                 ,fpPathCond = FNot $ FRel Lt (FVar FPDouble "X") (FInt 3)
                                 ,realCond = BTrue
                                 ,fpCond = FBTrue}]
          ,rExprs = RDeclRes [Int 0,Int 1]
          ,fpExprs = FDeclRes [FInt 0,FInt 1]
          ,eExpr = Just $ MaxErr [BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 1) (Int 0)))
                          ,BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 0) (Int 1)))]
          ,decisionPath = LDP []
          ,cFlow = Unstable}
    ]

stmSem__ListIte = testCase "ListIte" $
    stmSem (ListIte [(FRel Gt (FVar FPDouble "X") (FInt 3), (FInt 0)), (FRel Lt (FVar FPDouble "X") (FInt (-2)), (FInt 2))] (FInt 1)) emptyInterp (emptyEnv) semConf (LDP []) [LDP []]
    `semEquiv`
    [ACeb {conds = Conds [Cond {realPathCond = Rel Gt (Var Real "X") (Int 3)
                                 ,fpPathCond = FRel Gt (FVar FPDouble "X") (FInt 3)
                                 ,realCond = BTrue
                                 ,fpCond = FBTrue}]
          ,rExprs  = RDeclRes [Int 0]
          ,fpExprs = FDeclRes [FInt 0]
          ,eExpr   = Just $ ErrRat 0
          ,decisionPath = LDP [0]
          ,cFlow = Stable}
    ,ACeb {conds = Conds [Cond {realPathCond = And (Rel Lt (Var Real "X") (Int  (-2)))
                                                   (Not (Rel Gt  (Var Real "X") (Int 3)))
                               ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "X") (FInt (-2))) (FNot (FRel Gt (FVar FPDouble "X") (FInt 3)))
                               ,realCond = BTrue
                               ,fpCond = FBTrue}]
          ,rExprs  = RDeclRes [Int 2]
          ,fpExprs = FDeclRes [FInt 2]
          ,eExpr = Just $ ErrRat 0
          ,decisionPath = LDP [1]
          ,cFlow = Stable}
    ,ACeb {conds = Conds [Cond {realPathCond = And (Not $ Rel Lt (Var Real "X") (Int  (-2)))
                                                   (Not $ Rel Gt  (Var Real "X") (Int 3))
                                 ,fpPathCond = FAnd (FNot $ FRel Lt (FVar FPDouble "X") (FInt (-2)))
                                                    (FNot $ FRel Gt (FVar FPDouble "X") (FInt 3))
                                 ,realCond = BTrue
                                 ,fpCond = FBTrue}]
          ,rExprs = RDeclRes [Int 1]
          ,fpExprs = FDeclRes [FInt 1]
          ,eExpr = Just $ ErrRat 0
          ,decisionPath = LDP [2]
          ,cFlow = Stable}
    ,ACeb {conds = Conds [
            Cond {realPathCond = And (Not (Rel Gt (Var Real "X") (Int 3))) (Not (Rel Lt (Var Real "X") (Int (-2))))
                 ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "X") (FInt (-2))) (FNot (FRel Gt (FVar FPDouble "X") (FInt 3)))
                 ,realCond = BTrue
                 ,fpCond = FBTrue}
           ,Cond {realPathCond = And (Not (Rel Gt (Var Real "X") (Int 3))) (Not (Rel Lt (Var Real "X") (Int (-2))))
                 ,fpPathCond = FRel Gt (FVar FPDouble "X") (FInt 3)
                 ,realCond = BTrue
                 ,fpCond = FBTrue}
           ,Cond {realPathCond = And (Rel Lt (Var Real "X") (Int (-2))) (Not (Rel Gt (Var Real "X") (Int 3)))
                 ,fpPathCond = FAnd (FNot (FRel Gt (FVar FPDouble "X") (FInt 3))) (FNot (FRel Lt (FVar FPDouble "X") (FInt (-2))))
                 ,realCond = BTrue
                 ,fpCond = FBTrue}
           ,Cond {realPathCond = And (Rel Lt  (Var Real "X") (Int (-2))) (Not (Rel Gt (Var Real "X") (Int 3)))
                 ,fpPathCond = FRel Gt (FVar FPDouble "X") (FInt 3)
                 ,realCond = BTrue
                 ,fpCond = FBTrue}
           ,Cond {realPathCond = Rel Gt (Var Real "X") (Int 3)
                 ,fpPathCond = FAnd (FNot (FRel Gt (FVar FPDouble "X") (FInt 3))) (FNot (FRel Lt (FVar FPDouble "X") (FInt (-2))))
                 ,realCond = BTrue
                 ,fpCond = FBTrue}
           ,Cond {realPathCond = Rel Gt (Var Real "X") (Int 3)
                 ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "X") (FInt (-2))) (FNot (FRel Gt (FVar FPDouble "X") (FInt 3)))
                 ,realCond = BTrue
                 ,fpCond = FBTrue}
           ]
          ,rExprs = RDeclRes [Int 0,Int 1,Int 2]
          ,fpExprs = FDeclRes [FInt 0,FInt 1,FInt 2]
          ,eExpr = Just $ MaxErr [MaxErr [MaxErr [MaxErr [MaxErr
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
    stmSem (Let [("X", FPDouble, FInt 5)] (BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FInt 3))) emptyInterp (emptyEnv)  semConf (LDP []) [LDP []]
    `semEquiv`
    [ACeb {conds   = trueConds
          ,rExprs  = RDeclRes [RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5}]
                     (BinaryOp AddOp (RealMark "X" ResValue) (Int 3))]
          ,fpExprs = FDeclRes [Let [("X", FPDouble, FInt 5)] (BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FInt 3))]
          ,eExpr   = Just $ RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5},
                            LetElem{letVar = "Err_X", letType = Real, letExpr = ErrRat 0}]
                     $ ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (Var Real "Err_X") (Int 3) (ErrRat 0)
          ,decisionPath = LDP []
          ,cFlow = Stable}]

stmSem__LetIn2 = testCase "LetIn2" $
    stmSem (Let [("X", FPDouble, FInt 5),("Y", FPDouble, FInt 1)]
           (BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FVar FPDouble "Y")))
           emptyInterp (emptyEnv) semConf (LDP []) [LDP []]
    `semEquiv`
    [ACeb {conds   = trueConds
          ,rExprs  = RDeclRes [RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5}]
                      (RLet [LetElem{letVar = "Y", letType = Real, letExpr = Int 1}]
                     (BinaryOp AddOp (RealMark "X" ResValue) (RealMark "Y" ResValue)))]
          ,fpExprs = FDeclRes [Let [("X", FPDouble, FInt 5)] (
                      Let [("Y", FPDouble, FInt 1)]
                      (BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FVar FPDouble "Y")))]
          ,eExpr   = Just $ RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5}
                          ,LetElem{letVar = "Err_X", letType = Real, letExpr = ErrRat 0}] (
                     RLet [LetElem{letVar = "Y", letType = Real, letExpr = Int 1}
                          ,LetElem{letVar = "Err_Y", letType = Real, letExpr = ErrRat 0}]
                     $ ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (Var Real "Err_X")
                                               (RealMark "Y" ResValue) (Var Real "Err_Y"))
          ,decisionPath = LDP []
          ,cFlow = Stable}]

stmSem__LetIn3 = testCase "LetIn3" $
    stmSem (Let [("X", FPDouble, Ite (FRel LtE (FVar FPDouble "Z") (FInt 0)) (FInt 1) (FInt 2))]
                (BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FInt 3))) emptyInterp (emptyEnv)  semConf (LDP []) [LDP []]
    `semEquiv`
     [ACeb {conds = Conds [Cond {realPathCond = Not (Rel LtE (Var Real "Z") (Int 0))
                               ,fpPathCond = FNot (FRel LtE (FVar FPDouble "Z") (FInt 0))
                               ,realCond = BTrue
                               ,fpCond = FBTrue}
                          ,Cond {realPathCond = Rel LtE (Var Real "Z") (Int 0)
                               ,fpPathCond = FRel LtE (FVar FPDouble "Z") (FInt 0)
                               ,realCond = BTrue
                               ,fpCond = FBTrue}]
            ,rExprs = RDeclRes [BinaryOp AddOp (Int 1) (Int 3)
                      ,BinaryOp AddOp (Int 2) (Int 3)]
            ,fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FInt 1) (FInt 3)
                       ,BinaryFPOp AddOp FPDouble (FInt 2) (FInt 3)]
            ,eExpr = Just $ MaxErr [ErrBinOp AddOp FPDouble (Int 1) (ErrRat 0) (Int 3) (ErrRat 0)
                            ,ErrBinOp AddOp FPDouble (Int 2) (ErrRat 0) (Int 3) (ErrRat 0)]
            ,decisionPath = LDP []
            ,cFlow = Stable}
     ,ACeb {conds = Conds [Cond {realPathCond = Not (Rel LtE (Var Real "Z") (Int 0))
                               ,fpPathCond = FRel LtE (FVar FPDouble "Z") (FInt 0)
                               ,realCond = BTrue
                               ,fpCond = FBTrue}
                          ,Cond {realPathCond = Rel LtE (Var Real "Z") (Int 0)
                               ,fpPathCond = FNot (FRel LtE (FVar FPDouble "Z") (FInt 0))
                               ,realCond = BTrue
                               ,fpCond = FBTrue}]
           ,rExprs = RDeclRes [BinaryOp AddOp (Int 1) (Int 3),BinaryOp AddOp (Int 2) (Int 3)]
           ,fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FInt 1) (FInt 3),BinaryFPOp AddOp FPDouble (FInt 2) (FInt 3)]
           ,eExpr = Just $ MaxErr [ErrBinOp AddOp FPDouble (Int 1)
                           (MaxErr [BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 2) (Int 1)))
                           ,BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 1) (Int 2)))]) (Int 3) (ErrRat 0)
                           ,ErrBinOp AddOp FPDouble (Int 2) (MaxErr [BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 2) (Int 1)))
                           ,BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 1) (Int 2)))]) (Int 3) (ErrRat 0)]
           ,decisionPath = LDP []
           ,cFlow = Unstable}]

stmSem__LetIn4 = testCase "LetIn4" $
    stmSem (Let [("X", FPDouble, FInt 5)
                ,("Y", FPDouble, BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FInt 2))]
    (BinaryFPOp AddOp FPDouble (FVar FPDouble "Y") (FInt 3))) emptyInterp (emptyEnv)  semConf (LDP []) [LDP []]
    `semEquiv`
    [ACeb {conds   = trueConds
          ,rExprs  = RDeclRes [RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5}]
                     (RLet [LetElem{letVar = "Y", letType = Real, letExpr = BinaryOp AddOp (RealMark "X" ResValue) (Int 2)}]
                     (BinaryOp AddOp (RealMark "Y" ResValue) (Int 3)))]
          ,fpExprs = FDeclRes [Let [("X", FPDouble, FInt 5)]
                     (Let [("Y", FPDouble, BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FInt 2))]
                      (BinaryFPOp AddOp FPDouble (FVar FPDouble "Y") (FInt 3)))]
          ,eExpr   = Just $ RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5},
                            LetElem{letVar = "Err_X", letType = Real, letExpr = ErrRat 0}] (
                     RLet [LetElem{letVar = "Y", letType = Real, letExpr = BinaryOp AddOp (RealMark "X" ResValue) (Int 2)},
                            LetElem{letVar = "Err_Y", letType = Real
                                   ,letExpr = ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (Var Real "Err_X") (Int 2) (ErrRat 0)}] (
                     ErrBinOp AddOp FPDouble (RealMark "Y" ResValue) (Var Real "Err_Y") (Int 3) (ErrRat 0)))
          ,decisionPath = LDP []
          ,cFlow = Stable}]

stmSem__LetIn5 = testCase "LetIn5" $
    stmSem (Let [("X", FPDouble, FInt 5)
                ,("Y", FPDouble, BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FInt 2))
                ,("Z", FPDouble, BinaryFPOp MulOp FPDouble (FVar FPDouble "Y") (FInt 7))]
    (BinaryFPOp AddOp FPDouble (FVar FPDouble "Z") (FInt 3))) emptyInterp (emptyEnv)  semConf (LDP []) [LDP []]
    `semEquiv`
    [ACeb {conds   = trueConds
          ,rExprs  = RDeclRes [RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5}]
                     (RLet [LetElem{letVar = "Y", letType = Real, letExpr = BinaryOp AddOp (RealMark "X" ResValue) (Int 2)}]
                     (RLet [LetElem{letVar = "Z", letType = Real, letExpr = BinaryOp MulOp (RealMark "Y" ResValue) (Int 7)}]
                     (BinaryOp AddOp (RealMark "Z" ResValue) (Int 3))))]
          ,fpExprs = FDeclRes [Let [("X", FPDouble, FInt 5)]
                     (Let [("Y", FPDouble, BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FInt 2))]
                     (Let [("Z", FPDouble, BinaryFPOp MulOp FPDouble (FVar FPDouble "Y") (FInt 7))]
                      (BinaryFPOp AddOp FPDouble (FVar FPDouble "Z") (FInt 3))))]
          ,eExpr   = Just $ RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5},
                            LetElem{letVar = "Err_X", letType = Real, letExpr = ErrRat 0}] (
                     RLet [LetElem{letVar = "Y", letType = Real, letExpr = BinaryOp AddOp (RealMark "X" ResValue) (Int 2)},
                            LetElem{letVar = "Err_Y", letType = Real
                                   ,letExpr = ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (Var Real "Err_X") (Int 2) (ErrRat 0)}] (
                     RLet [LetElem{letVar = "Z", letType = Real, letExpr = BinaryOp MulOp (RealMark "Y" ResValue) (Int 7)},
                            LetElem{letVar = "Err_Z", letType = Real
                                   ,letExpr = ErrBinOp MulOp FPDouble (RealMark "Y" ResValue) (Var Real "Err_Y") (Int 7) (ErrRat 0)}] (
                     ErrBinOp AddOp FPDouble (RealMark "Z" ResValue) (Var Real "Err_Z") (Int 3) (ErrRat 0))))
          ,decisionPath = LDP []
          ,cFlow = Stable}]


stmSem__LetIn6 = testCase "LetIn6" $
    stmSem (Let [("X", FPDouble, FInt 5)
                ,("Y", FPDouble, FEFun False "f" ResValue FPDouble [(FVar FPDouble "X")])]
    (BinaryFPOp AddOp FPDouble (FVar FPDouble "Y") (FInt 3)))
    interp
    (emptyEnv) semConf (LDP []) [LDP []]
    `semEquiv`
    [ACeb {conds   = trueConds
          ,rExprs  = RDeclRes [RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5}]
                     (RLet [LetElem{letVar = "Y", letType = Real, letExpr = Var Real "X"}]
                     (BinaryOp AddOp (RealMark "Y" ResValue) (Int 3)))]
          ,fpExprs = FDeclRes [Let [("X", FPDouble, FInt 5)]
                     (Let [("Y", FPDouble, FEFun False "f" ResValue FPDouble [FVar FPDouble "X"])]
                      (BinaryFPOp AddOp FPDouble (FVar FPDouble "Y") (FInt 3)))]
          ,eExpr   = Just $ RLet [LetElem{letVar = "X", letType = Real, letExpr = Int 5},
                            LetElem{letVar = "Err_X", letType = Real, letExpr = ErrRat 0}] (
                     RLet [LetElem{letVar = "Y", letType = Real, letExpr = Var Real "X"},
                            LetElem{letVar = "Err_Y", letType = Real
                                   ,letExpr = ErrRat 5}] (
                     ErrBinOp AddOp FPDouble (RealMark "Y" ResValue) (Var Real "Err_Y") (Int 3) (ErrRat 0)))
          ,decisionPath = LDP []
          ,cFlow = Stable}]
      where
        interp = Map.fromList [("f", (False, FPSingle, [Arg "x" FPDouble],fSemantics))]
        fSemantics = Map.fromList [(ResValue, [aceb])]
        aceb = ACeb {
                conds  = trueConds,
                rExprs = RDeclRes [Var Real "x"],
                fpExprs = FDeclRes [FVar FPDouble "x"],
                eExpr = Just $ ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }

stmSem__LetIn7 = testCase "LetIn7" $
    stmSem (Let [("A", FPDouble, (BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FVar FPDouble "Y")))
                ,("B", FPDouble, FEFun False "f" ResValue FPDouble [(FVar FPDouble "A")])]
          (FVar FPDouble "B"))
    interp
    (emptyEnv)  semConf (LDP []) [LDP []]
    `semEquiv`
    [ACeb {conds   = trueConds
          ,rExprs  = RDeclRes [RLet [LetElem{letVar = "A", letType = Real,
                            letExpr = BinaryOp AddOp (RealMark "X" ResValue) (RealMark "Y" ResValue)}]
                     (RLet [LetElem{letVar = "B", letType = Real,
                            letExpr = BinaryOp MulOp (RealMark "A" ResValue) (Int 2)}]
                     (RealMark "B" ResValue))]
          ,fpExprs = FDeclRes [Let [("A", FPDouble, BinaryFPOp AddOp FPDouble (FVar FPDouble "X") (FVar FPDouble "Y"))]
                     (Let [("B", FPDouble, FEFun False "f" ResValue FPDouble [(FVar FPDouble "A")])]
                      (FVar FPDouble "B"))]
          ,eExpr   = Just $ RLet [LetElem{letVar = "A", letType = Real,
                           letExpr = BinaryOp AddOp (RealMark "X" ResValue) (RealMark "Y" ResValue)},
                           LetElem{letVar = "Err_A", letType = Real,
                           letExpr = ErrBinOp AddOp FPDouble (RealMark "X" ResValue) (ErrorMark "X" ResValue FPDouble)
                                                            (RealMark "Y" ResValue) (ErrorMark "Y" ResValue FPDouble)}] (
                     RLet [LetElem{letVar = "B", letType = Real,
                           letExpr =  BinaryOp MulOp (RealMark "A" ResValue) (Int 2) },
                            LetElem{letVar = "Err_B", letType = Real
                                   ,letExpr = ErrBinOp MulOp FPDouble (RealMark "A" ResValue) (Var Real "Err_X") (Int 2) (ErrRat (0 % 1))}]
                                   (Var Real "Err_B"))
          ,decisionPath = LDP []
          ,cFlow = Stable}]
      where
        interp = Map.fromList [("f", (False, FPDouble, [Arg "X" FPDouble],fSemantics))]
        fSemantics = Map.fromList [(ResValue, [aceb])]
        aceb = ACeb {
                conds  = trueConds,
                rExprs = RDeclRes [BinaryOp MulOp (RealMark "X" ResValue) (Int 2)],
                fpExprs = FDeclRes [BinaryFPOp MulOp FPDouble (FVar FPDouble "X") (FInt 2)],
                eExpr = Just $ ErrBinOp MulOp FPDouble (RealMark "X" ResValue) (Var Real "Err_X") (Int 2) (ErrRat 0),
                decisionPath = root,
                cFlow = Stable
                }

stmSem__UnstWarning = testCase "UnstWarning" $
    stmSem UnstWarning emptyInterp (emptyEnv)  semConf (LDP []) [LDP []]
    `semEquiv`
    [ ACeb { conds  = trueConds,
             rExprs = RDeclRes [Int 0],
             fpExprs = FDeclRes [FInt 0],
             eExpr = Just $ ErrRat 0,
             decisionPath = LDP [],
             cFlow  = Stable
             } ]

stmSem__IteIntAdd = testCase "IteIntAdd" $
    stmSem (BinaryFPOp AddOp FPDouble (Ite (FRel Gt (FVar FPDouble "X") (FInt 0)) (FInt 0) (FInt 1))
                                      (FInt 3))
      emptyInterp (emptyEnv) semConf (LDP []) [LDP []]
    `semEquiv`
    [ACeb {conds = Conds [Cond {realPathCond = Not (Rel Gt (RealMark "X" ResValue) (Int 0))
                               ,fpPathCond = FNot (FRel Gt (FVar FPDouble "X") (FInt 0))
                               ,realCond = BTrue
                               ,fpCond = FBTrue}
                         ,Cond {realPathCond = Rel Gt (RealMark "X" ResValue) (Int 0)
                               ,fpPathCond = FRel Gt (FVar FPDouble "X") (FInt 0)
                               ,realCond = BTrue
                               ,fpCond = FBTrue}]
          ,rExprs = RDeclRes [BinaryOp AddOp (Int 0) (Int 3)
                    ,BinaryOp AddOp (Int 1) (Int 3)]
          ,fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FInt 0) (FInt 3)
                     ,BinaryFPOp AddOp FPDouble (FInt 1) (FInt 3)]
          ,eExpr = Just $ MaxErr [ErrBinOp AddOp FPDouble (Int 0) (ErrRat 0) (Int 3) (ErrRat 0)
                          ,ErrBinOp AddOp FPDouble (Int 1) (ErrRat (0 % 1)) (Int 3) (ErrRat (0 % 1))]
          ,decisionPath = LDP []
          ,cFlow = Stable}
    ,ACeb {conds = Conds [Cond {realPathCond = Not $ Rel Gt (RealMark "X" ResValue) (Int 0)
                               ,fpPathCond = FRel Gt (FVar FPDouble "X") (FInt 0)
                               ,realCond = BTrue
                               ,fpCond = FBTrue}
                         ,Cond {realPathCond = Rel Gt (RealMark "X" ResValue) (Int 0)
                               ,fpPathCond = FNot $ FRel Gt (FVar FPDouble "X") (FInt 0)
                               ,realCond = BTrue
                               ,fpCond = FBTrue}]
          ,rExprs = RDeclRes [BinaryOp AddOp (Int 0) (Int 3),BinaryOp AddOp (Int 1) (Int 3)]
          ,fpExprs = FDeclRes [BinaryFPOp AddOp FPDouble (FInt 0) (FInt 3),BinaryFPOp AddOp FPDouble (FInt 1) (FInt 3)]
          ,eExpr = Just $ MaxErr [ErrBinOp AddOp FPDouble (Int 0)
                  (MaxErr [BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 1) (Int 0)))
                          ,BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 0) (Int 1)))]) (Int 3) (ErrRat 0)
                          ,ErrBinOp AddOp FPDouble (Int 1) (MaxErr [BinaryOp AddOp (ErrRat 0) (UnaryOp AbsOp (BinaryOp SubOp (Int 1) (Int 0))),BinaryOp AddOp (ErrRat 0)
                          (UnaryOp AbsOp (BinaryOp SubOp (Int 0) (Int 1)))]) (Int 3) (ErrRat (0 % 1))]
          ,decisionPath = LDP []
          ,cFlow = Unstable}]

stmSem__ErrFun1 = testCase "ErrFun1" $
    stmSem (FEFun False "f" ResValue FPDouble []) interp (emptyEnv) semConfAbs (LDP []) [] @?=
    [ ACeb {conds  = trueConds,
            rExprs = RDeclRes [EFun "f" ResValue Real []],
            fpExprs = FDeclRes [FEFun False "f" ResValue FPDouble []],
            eExpr  = Just $ ErrFun "f" FPDouble ResValue [],
            decisionPath = LDP [],
            cFlow  = Stable
               } ]
  where
    interp = Map.fromList [("f", (False, FPDouble, [Arg "x" FPDouble],fSemantics))]
    fSemantics = Map.fromList [(ResValue, [aceb])]
    aceb = ACeb {
              conds  = Conds [Cond {realPathCond = Rel Lt (Int 8) (Int 9)
                                   ,fpPathCond = FRel Lt (FInt 8) (FInt 9)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
                             ,Cond {realPathCond = Rel Lt (Int 9) (Int 10)
                                   ,fpPathCond = FRel Lt (FInt 9) (FInt 10)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}],
              rExprs = RDeclRes [Int 9,Int 10],
              fpExprs = FDeclRes [FInt 9,FInt 10],
              eExpr = Just $ ErrRat 5,
              decisionPath = root,
              cFlow = Stable
              }

stmSem__ErrFun2 = testCase "ErrFun2" $
    stmSem (FEFun False "f" ResValue FPDouble [FEFun False "g" ResValue FPDouble []]) interp (emptyEnv) semConfAbs (LDP []) [] @?=
    [ ACeb {conds  = trueConds,
            rExprs = RDeclRes [EFun "f" ResValue Real [EFun "g" ResValue Real []]],
            fpExprs = FDeclRes [FEFun False "f" ResValue FPDouble [FEFun False "g" ResValue FPDouble []]],
            eExpr  = Just $ ErrFun "f" FPDouble ResValue [FEFun False "g" ResValue FPDouble []],
            decisionPath = LDP [],
            cFlow  = Stable
               } ]
  where
    interp = Map.fromList [("f", (False, FPDouble, [Arg "x" FPDouble],fSemantics)),
                           ("g", (False, FPDouble, [Arg "x" FPDouble],gSemantics))]
    fSemantics = Map.fromList [(ResValue, [acebf])]
    gSemantics = Map.fromList [(ResValue, [acebg])]
    acebf = ACeb {
              conds  = Conds [Cond {realPathCond = Rel Lt (Int 8) (Int 9)
                                   ,fpPathCond = FRel Lt (FInt 8) (FInt 9)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
                             ,Cond {realPathCond = Rel Lt (Int 9) (Int 10)
                                   ,fpPathCond = FRel Lt (FInt 9) (FInt 10)
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}],
              rExprs = RDeclRes [Int 9,Int 10],
              fpExprs = FDeclRes [FInt 9,FInt 10],
              eExpr = Just $ ErrFun "g" FPDouble ResValue [FVar FPDouble "x"],
              decisionPath = root,
              cFlow = Stable
              }
    acebg = ACeb {
              conds  = Conds [Cond {realPathCond = BTrue
                                   ,fpPathCond = FBTrue
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}
                             ,Cond {realPathCond = BTrue
                                   ,fpPathCond = FBTrue
                                   ,realCond = BTrue
                                   ,fpCond = FBTrue}],
              rExprs = RDeclRes [EFun "g" ResValue Real []],
              fpExprs = FDeclRes [FEFun False "g" ResValue FPDouble []],
              eExpr = Just $ ErrFun "g" FPDouble ResValue [FVar FPDouble "x"],
              decisionPath = root,
              cFlow = Stable
              }


stmSem__map = testCase "Map" $
    stmSem (FMap FPDouble "f" "l") interp (emptyEnv) semConf (LDP []) [] @?=
    [ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "l") (Int 9)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "l") (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Var Real "l") (Int 10)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "l") (FInt 10)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 9,Int 10],
                fpExprs = FDeclRes [FInt 9,FInt 10],
                eExpr = Just $ Int 1,
                decisionPath = root,
                cFlow = Stable
                }]
    where
      interp = Map.fromList [("f", (False, FPDouble, [Arg "x" FPDouble],fSemantics))]
      fSemantics = Map.fromList [(ResValue, [acebf])]
      acebf = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "x") (Int 9)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "x") (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Var Real "x") (Int 10)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "x") (FInt 10)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 9,Int 10],
                fpExprs = FDeclRes [FInt 9,FInt 10],
                eExpr = Just $ Int 1,
                decisionPath = root,
                cFlow = Stable
                }



-- stmSem__ErrFun3 = testCase "ErrFun2" $
--     stmSem (FEFun False "f" ResValue FPDouble
--       [FEFun False "g" ResValue FPDouble [FEFun False "d" ResValue FPDouble [FVar FPDouble "x"]]])
--       interp (emptyEnv) semConfAbs (LDP []) [] @?=
--     [ ACeb {conds  = trueConds,
--             rExprs = RDeclRes [EFun "f" ResValue Real [EFun "g" ResValue Real []]],
--             fpExprs = FDeclRes [FEFun False "f" ResValue FPDouble [FEFun False "g" ResValue FPDouble []]],
--             eExpr  = Just $ ErrFun "f" FPDouble ResValue [FEFun False "g" ResValue FPDouble []],
--             decisionPath = LDP [],
--             cFlow  = Stable
--                } ]
--   where
--     interp = Map.fromList [("f", (False, FPDouble, [Arg "x" FPDouble],fSemantics)),
--                            ("g", (False, FPDouble, [Arg "x" FPDouble],gSemantics)),
--                            ("d", (False, FPDouble, [Arg "x" FPDouble],dSemantics))]
--     fSemantics = Map.fromList [(ResValue, [acebf])]
--     gSemantics = Map.fromList [(ResValue, [acebg])]
--     dSemantics = Map.fromList [(ResValue, [acebd])]
--     acebf = ACeb {
--               conds  = Conds [Cond {realPathCond = Rel Lt (Int 8) (Int 9)
--                                    ,fpPathCond = FRel Lt (FInt 8) (FInt 9)
--                                    ,realCond = BTrue
--                                    ,fpCond = FBTrue}
--                              ,Cond {realPathCond = Rel Lt (Int 9) (Int 10)
--                                    ,fpPathCond = FRel Lt (FInt 9) (FInt 10)
--                                    ,realCond = BTrue
--                                    ,fpCond = FBTrue}],
--               rExprs = RDeclRes [Int 9,Int 10],
--               fpExprs = FDeclRes [FInt 9,FInt 10],
--               eExpr = Just $ ErrFun "f" FPDouble ResValue [FVar FPDouble "x"],
--               decisionPath = root,
--               cFlow = Stable
--               }
--     acebg = ACeb {
--               conds  = Conds [Cond {realPathCond = BTrue
--                                    ,fpPathCond = FBTrue
--                                    ,realCond = BTrue
--                                    ,fpCond = FBTrue}
--                              ,Cond {realPathCond = BTrue
--                                    ,fpPathCond = FBTrue
--                                    ,realCond = BTrue
--                                    ,fpCond = FBTrue}],
--               rExprs = RDeclRes [EFun "g" ResValue Real [FVar FPDouble "x"]],
--               fpExprs = FDeclRes [FEFun False "g" ResValue FPDouble [Var Real "x"]],
--               eExpr = Just $ ErrFun "g" FPDouble ResValue [FVar FPDouble "x"],
--               decisionPath = root,
--               cFlow = Stable
--               }
--     acebd = ACeb {
--               conds  = Conds [Cond {realPathCond = BTrue
--                                    ,fpPathCond = FBTrue
--                                    ,realCond = BTrue
--                                    ,fpCond = FBTrue}
--                              ,Cond {realPathCond = BTrue
--                                    ,fpPathCond = FBTrue
--                                    ,realCond = BTrue
--                                    ,fpCond = FBTrue}],
--               rExprs = RDeclRes [EFun "d" ResValue Real []],
--               fpExprs = FDeclRes [FEFun False "d" ResValue FPDouble []],
--               eExpr = Just $ ErrFun "d" FPDouble ResValue [FVar FPDouble "x"],
--               decisionPath = root,
--               cFlow = Stable
--               }

equivInterp__tests = testGroup "equivInterp tests"
    [equivInterp__test1
    ,equivInterp__test2
    ,equivInterp__test3
    ,equivInterp__test4
    ]

equivInterp__test1 = testCase "Equivalent Interpretation 1" $
    equivInterp interp1 interp2 @?= False
    where
      interp1 = Map.fromList [("f", (False, FPDouble, [Arg "x" FPDouble],fSemantics))]
      interp2 = Map.fromList [("f", (False, FPSingle, [Arg "x" FPDouble],fSemantics))]
      fSemantics = Map.fromList [(ResValue, [aceb])]
      aceb = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Int 8) (Int 9)
                                     ,fpPathCond = FRel Lt (FInt 8) (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Int 9) (Int 10)
                                     ,fpPathCond = FRel Lt (FInt 9) (FInt 10)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 9,Int 10],
                fpExprs = FDeclRes [FInt 9,FInt 10],
                eExpr = Just $ ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }

equivInterp__test2 = testCase "Equivalent Interpretation 2" $
    equivInterp interp1 interp2 @?= True
    where
      interp1 = Map.fromList [("f", (False, FPSingle, [Arg "x" FPDouble],fSemantics))
                ,("g", (False, FPDouble, [Arg "x" FPDouble],gSemantics))]
      interp2 = Map.fromList  [("g", (False, FPDouble, [Arg "x" FPDouble],gSemantics))
                ,("f", (False, FPSingle, [Arg "x" FPDouble],fSemantics))]
      fSemantics = Map.fromList [(ResValue, [faceb])]
      gSemantics = Map.fromList [(ResValue, [gaceb])]
      faceb = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Int 8) (Int 9)
                                     ,fpPathCond = FRel Lt (FInt 8) (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Int 9) (Int 10)
                                     ,fpPathCond = FRel Lt (FInt 9) (FInt 10)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 9,Int 10],
                fpExprs = FDeclRes [FInt 9,FInt 10],
                eExpr = Just $ ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }
      gaceb = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Int 6) (Int 9)
                                     ,fpPathCond = FRel Lt (FInt 8) (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Int 9) (Int 10)
                                     ,fpPathCond = FRel Lt (FInt 9) (FInt 10)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 9],
                fpExprs = FDeclRes [FInt 9],
                eExpr = Just $ ErrRat 5,
                decisionPath = root,
                cFlow = Unstable
                }

equivInterp__test3 = testCase "Equivalent Interpretation 3" $
    equivInterp interp1 interp2 @?= False
    where
      interp1 = Map.fromList [("f", (False, FPSingle, [Arg "y" FPDouble],fSemantics))
                ,("g", (False, FPDouble, [Arg "x" FPDouble],gSemantics))]
      interp2 = Map.fromList [("g", (False, FPDouble, [Arg "x" FPDouble],gSemantics))
                ,("f", (False, FPSingle, [Arg "x" FPDouble],fSemantics))]
      fSemantics = Map.fromList [(ResValue, [faceb])]
      gSemantics = Map.fromList [(ResValue, [gaceb])]
      faceb = ACeb {
                conds = Conds [Cond {realPathCond = Rel Lt (Int 8) (Int 9)
                                     ,fpPathCond = FRel Lt (FInt 8) (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                              ,Cond {realPathCond = Rel Lt (Int 9) (Int 10)
                                     ,fpPathCond = FRel Lt (FInt 9) (FInt 10)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 9,Int 10],
                fpExprs = FDeclRes [FInt 9,FInt 10],
                eExpr = Just $ ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }
      gaceb = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Int 6) (Int 9)
                                     ,fpPathCond = FRel Lt (FInt 8) (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Int 9) (Int 10)
                                     ,fpPathCond = FRel Lt (FInt 9) (FInt 10)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 9],
                fpExprs = FDeclRes [FInt 9],
                eExpr = Just $ ErrRat 5,
                decisionPath = root,
                cFlow = Unstable
                }

equivInterp__test4 = testCase "Equivalent Interpretation 3" $
    equivInterp interp1 interp2 @?= False
    where
      interp1 = Map.fromList [("f", (False, FPSingle, [Arg "x" FPDouble],fSemantics1))
                ,("g", (False, FPDouble, [Arg "x" FPDouble],gSemantics))]
      interp2 = Map.fromList [("g", (False, FPDouble, [Arg "x" FPDouble],gSemantics))
                ,("f", (False, FPSingle, [Arg "x" FPDouble],fSemantics2))]
      fSemantics1 = Map.fromList [(ResValue, facebs1)]
      fSemantics2 = Map.fromList [(ResValue, facebs2)]
      gSemantics = Map.fromList [(ResValue, gacebs)]
      facebs1 = [ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Int 8) (Int 9)
                                     ,fpPathCond = FRel Lt (FInt 8) (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Int 9) (Int 10)
                                     ,fpPathCond = FRel Lt (FInt 9) (FInt 10)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 9,Int 10],
                fpExprs = FDeclRes [FInt 9,FInt 10],
                eExpr = Just $ ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                },
                ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Int 8) (Int 9)
                                     ,fpPathCond = FRel Lt (FInt 8) (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Int 9) (Int 10)
                                     ,fpPathCond = FRel Lt (FInt 9) (FInt 10)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Var Real "v"],
                fpExprs = FDeclRes [FVar FPSingle "v"],
                eExpr = Just $ ErrRat 0,
                decisionPath = root,
                cFlow = Unstable
                }]
      facebs2 = [ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Int 8) (Int 9)
                                     ,fpPathCond = FRel Lt (FInt 8) (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Int 9) (Int 10)
                                     ,fpPathCond = FRel Lt (FInt 9) (FInt 10)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Var Real "v"],
                fpExprs = FDeclRes [FInt 9,FInt 10],
                eExpr = Just $ ErrRat 0,
                decisionPath = root,
                cFlow = Unstable
                },
                ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Int 8) (Int 9)
                                     ,fpPathCond = FRel Lt (FInt 8) (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Int 9) (Int 10)
                                     ,fpPathCond = FRel Lt (FInt 9) (FInt 10)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 9,Int 10],
                fpExprs = FDeclRes [FInt 9,FInt 10],
                eExpr = Just $ ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }]
      gacebs = [ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Int 6) (Int 9)
                                     ,fpPathCond = FRel Lt (FInt 8) (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Int 9) (Int 10)
                                     ,fpPathCond = FRel Lt (FInt 9) (FInt 10)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 9],
                fpExprs = FDeclRes [FInt 9],
                eExpr = Just $ ErrRat 5,
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
      actual = semEFun "f" ResValue formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
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
                conds  = Conds [Cond {realPathCond = Rel Lt (Int 0) (Int 1)
                                     ,fpPathCond = FRel Lt (FInt 0) (FInt 1)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Int 1) (Int 2)
                                     ,fpPathCond = FRel Lt (FInt 1) (FInt 2)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 1,Int 2],
                fpExprs = FDeclRes [FInt 3, FInt 2],
                eExpr = Just $ ErrRat 1,
                decisionPath = root,
                cFlow = Stable
                }
              xSemantics2 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Int 2) (Int 3)
                                     ,fpPathCond = FRel Lt (FInt 2) (FInt 3)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Int 3) (Int 4)
                                     ,fpPathCond = FRel Lt (FInt 3) (FInt 4)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 3,Int 4],
                fpExprs = FDeclRes [FInt 3,FInt 4],
                eExpr = Just $ ErrRat 2,
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics1 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Int 4) (Int 5)
                                     ,fpPathCond = FRel Lt (FInt 4) (FInt 5)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Int 5) (Int 6)
                                     ,fpPathCond = FRel Lt (FInt 5) (FInt 6)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 5,Int 6],
                fpExprs = FDeclRes [FInt 5,FInt 6],
                eExpr = Just $ ErrRat 3,
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics2 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Int 6) (Int 7)
                                     ,fpPathCond = FRel Lt (FInt 6) (FInt 7)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Int 7) (Int 8)
                                     ,fpPathCond = FRel Lt (FInt 7) (FInt 8)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 7,Int 8],
                fpExprs = FDeclRes [FInt 7,FInt 8],
                eExpr = Just $ ErrRat 4,
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1
                              ,fSemantics2
                              ]
            where
              fSemantics1 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Int 8) (Int 9)
                                     ,fpPathCond = FRel Lt (FInt 8) (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Int 9) (Int 10)
                                     ,fpPathCond = FRel Lt (FInt 9) (FInt 10)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 9,Int 10],
                fpExprs = FDeclRes [FInt 9,FInt 10],
                eExpr = Just $ ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }
              fSemantics2 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Int 10) (Int 11)
                                     ,fpPathCond = FRel Lt (FInt 10) (FInt 11)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Int 11) (Int 12)
                                     ,fpPathCond = FRel Lt (FInt 11) (FInt 12)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 11,Int 12],
                fpExprs = FDeclRes [FInt 11,FInt 12],
                eExpr = Just $ ErrRat 6,
                decisionPath = root,
                cFlow = Stable
                }
      expected = [
        ACeb {
            conds  = Conds [trueCond,trueCond,trueCond,trueCond,trueCond,trueCond,trueCond,trueCond],
            rExprs = RDeclRes [Int 9,Int 10],
            fpExprs = FDeclRes [FInt 9,FInt 10],
            eExpr = Just $ ErrRat (5 % 1),
            decisionPath = root,
            cFlow  = Stable
            },
        ACeb {
            conds = Conds [trueCond,trueCond,trueCond,trueCond,trueCond,trueCond,trueCond,trueCond],
            rExprs = RDeclRes [Int 9,Int 10],
            fpExprs = FDeclRes [FInt 9,FInt 10],
            eExpr = Just $ ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Conds [trueCond,trueCond,trueCond,trueCond,trueCond,trueCond,trueCond,trueCond],
            rExprs = RDeclRes [Int 9,Int 10],
            fpExprs = FDeclRes [FInt 9,FInt 10],
            eExpr = Just $ ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Conds [trueCond,trueCond,trueCond,trueCond,trueCond,trueCond,trueCond,trueCond],
            rExprs = RDeclRes [Int 9,Int 10],
            fpExprs = FDeclRes [FInt 9,FInt 10],
            eExpr = Just $ ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Conds [trueCond,trueCond,trueCond,trueCond,trueCond,trueCond,trueCond,trueCond],
            rExprs = RDeclRes [Int 11,Int 12],
            fpExprs = FDeclRes [FInt 11,FInt 12],
            eExpr = Just $ ErrRat (6 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Conds [trueCond,trueCond,trueCond,trueCond,trueCond,trueCond,trueCond,trueCond],
            rExprs = RDeclRes [Int 11,Int 12],
            fpExprs = FDeclRes [FInt 11,FInt 12],
            eExpr = Just $ ErrRat (6 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Conds [trueCond,trueCond,trueCond,trueCond,trueCond,trueCond,trueCond,trueCond],
            rExprs = RDeclRes [Int 11,Int 12],
            fpExprs = FDeclRes [FInt 11,FInt 12],
            eExpr = Just $ ErrRat (6 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Conds [trueCond,trueCond,trueCond,trueCond,trueCond,trueCond,trueCond,trueCond],
            rExprs = RDeclRes [Int 11,Int 12],
            fpExprs = FDeclRes [FInt 11,FInt 12],
            eExpr = Just $ ErrRat (6 % 1),
            decisionPath = root,
            cFlow = Stable
            }
        ]

semEFun__test2 = testCase "it correctly combines arguments-combinations conditions II" $
    actual @?= expected
    where
      actual = semEFun "f" ResValue formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
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
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "0") (Int 1)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "0") (FInt 1)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Var Real "1") (Int 2)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "1") (FInt 2)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 1,Int 2],
                fpExprs = FDeclRes [FInt 1,FInt 2],
                eExpr = Just $ ErrRat 1,
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics1 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "4") (Int 5)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "4") (FInt 5)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 5,Int 6],
                fpExprs = FDeclRes [FInt 5,FInt 6],
                eExpr = Just $ ErrRat 3,
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1]
            where
              fSemantics1 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "8") (Int 9)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "8") (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 9,Int 10],
                fpExprs = FDeclRes [FInt 9,FInt 10],
                eExpr = Just $ ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }
      expected = -- []
        [
          ACeb {
            conds = Conds [Cond {realPathCond = And (Rel Lt (Var Real "8") (Int 9))
                                                    (And (Rel Lt (Var Real "0") (Int 1))
                                                    (Rel Lt (Var Real "4") (Int 5)))
                                ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "8") (FInt 9))
                                                   (FAnd (FRel Lt (FVar FPDouble "0") (FInt 1))
                                                         (FRel Lt (FVar FPDouble "4") (FInt 5)))
                                ,realCond = BTrue
                                ,fpCond = FBTrue}
                          ,Cond {realPathCond = And (Rel Lt (Var Real "8") (Int 9))
                                                    (And (Rel Lt (Var Real "1") (Int 2))
                                                    (Rel Lt (Var Real "4") (Int 5)))
                                ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "8") (FInt 9))
                                                   (FAnd (FRel Lt (FVar FPDouble "1") (FInt 2))
                                                   (FRel Lt (FVar FPDouble "4") (FInt 5)))
                                ,realCond = BTrue
                                ,fpCond = FBTrue}],
            rExprs = RDeclRes [Int 9,Int 10],
            fpExprs = FDeclRes [FInt 9,FInt 10],
            eExpr = Just $ ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            }
        ]

semEFun__test3 = testCase "it correctly combines arguments-combinations conditions I" $
    actual @?= expected
    where
      actual = semEFun "f" ResValue formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
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
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "0") (Int 1)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "0") (FInt 1)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                                ,Cond {realPathCond = Rel Lt (Var Real "1") (Int 2)
                                      ,fpPathCond = FRel Lt (FVar FPDouble "1") (FInt 2)
                                      ,realCond = BTrue
                                      ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 1,Int 2],
                fpExprs = FDeclRes [FInt 1,FInt 2],
                eExpr = Just $ ErrRat 1,
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics1 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "4") (Int 5)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "4") (FInt 5)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                                ,Cond {realPathCond = Rel Lt (Var Real "6") (Int 7)
                                      ,fpPathCond = FRel Lt (FVar FPDouble "6") (FInt 7)
                                      ,realCond = BTrue
                                      ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 5,Int 6],
                fpExprs = FDeclRes [FInt 5,FInt 6],
                eExpr = Just $ ErrRat 3,
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1]
            where
              fSemantics1 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "8") (Int 9)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "8") (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 9,Int 10],
                fpExprs = FDeclRes [FInt 9,FInt 10],
                eExpr = Just $ ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }
      expected = [
        ACeb {
            conds = Conds [Cond {realPathCond = And (Rel Lt (Var Real "8") (Int 9))
                                                    (And (Rel Lt (Var Real "0") (Int 1))
                                                         (Rel Lt (Var Real "4") (Int 5)))
                                ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "8") (FInt 9))
                                                        (FAnd (FRel Lt (FVar FPDouble "0") (FInt 1))
                                                              (FRel Lt (FVar FPDouble "4") (FInt 5)))
                                ,realCond = BTrue
                                ,fpCond = FBTrue}
                          ,Cond {realPathCond = And (Rel Lt (Var Real "8") (Int 9))
                                                    (And (Rel Lt (Var Real "0") (Int 1))
                                                         (Rel Lt (Var Real "6") (Int 7)))
                                ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "8") (FInt 9))
                                                   (FAnd (FRel Lt (FVar FPDouble "0") (FInt 1))
                                                         (FRel Lt (FVar FPDouble "6") (FInt 7)))
                                ,realCond = BTrue
                                ,fpCond = FBTrue}
                          ,Cond {realPathCond = And (Rel Lt (Var Real "8") (Int 9))
                                                    (And (Rel Lt (Var Real "1") (Int 2))
                                                         (Rel Lt (Var Real "4") (Int 5)))
                                ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "8") (FInt 9))
                                                   (FAnd (FRel Lt (FVar FPDouble "1") (FInt 2))
                                                         (FRel Lt (FVar FPDouble "4") (FInt 5)))
                                ,realCond = BTrue
                                ,fpCond = FBTrue}
                          ,Cond {realPathCond = And (Rel Lt (Var Real "8") (Int 9))
                                                    (And (Rel Lt (Var Real "1") (Int 2))
                                                         (Rel Lt (Var Real "6") (Int 7)))
                                ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "8") (FInt 9))
                                                   (FAnd (FRel Lt (FVar FPDouble "1") (FInt 2))
                                                         (FRel Lt (FVar FPDouble "6") (FInt 7)))
                                ,realCond = BTrue
                                ,fpCond = FBTrue}],
              rExprs = RDeclRes [Int 9,Int 10],
              fpExprs = FDeclRes [FInt 9,FInt 10],
              eExpr = Just $ ErrRat (5 % 1),
              decisionPath = root,
              cFlow = Stable
            }
        ]


semEFun__test4 = testCase "it correctly combines argument combinations" $
    actual @?= expected
    where
      actual = semEFun "f" ResValue formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
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
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "0") (Int 1)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "0") (FInt 1)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Var Real "1") (Int 2)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "1") (FInt 2)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 1,Int 2],
                fpExprs = FDeclRes [FInt 1,FInt 2],
                eExpr = Just $ ErrRat 1,
                decisionPath = root,
                cFlow = Stable
                }
              xSemantics2 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "2") (Int 3)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "2") (FInt 3)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 3,Int 4],
                fpExprs = FDeclRes [FInt 3,FInt 4],
                eExpr = Just $ ErrRat 2,
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics1 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "4") (Int 5)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "4") (FInt 5)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 5,Int 6],
                fpExprs = FDeclRes [FInt 5,FInt 6],
                eExpr = Just $ ErrRat 3,
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1]
            where
              fSemantics1 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "8") (Int 9)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "8") (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 9,Int 10],
                fpExprs = FDeclRes [FInt 9,FInt 10],
                eExpr = Just $ ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }
      expected = [
        ACeb {
            conds = Conds [Cond {realPathCond = And (Rel Lt (Var Real "8") (Int 9))
                                                    (And (Rel Lt (Var Real "0") (Int 1))
                                                         (Rel Lt (Var Real "4") (Int 5)))
                                ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "8") (FInt 9))
                                                   (FAnd (FRel Lt (FVar FPDouble "0") (FInt 1))
                                                         (FRel Lt (FVar FPDouble "4") (FInt 5)))
                                ,realCond = BTrue
                                ,fpCond = FBTrue}
                         ,Cond {realPathCond = And (Rel Lt (Var Real "8") (Int 9))
                                                   (And (Rel Lt (Var Real "1") (Int 2))
                                                        (Rel Lt (Var Real "4") (Int 5)))
                                ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "8") (FInt 9))
                                                   (FAnd (FRel Lt (FVar FPDouble "1") (FInt 2))
                                                         (FRel Lt (FVar FPDouble "4") (FInt 5)))
                                ,realCond = BTrue
                                ,fpCond = FBTrue}],
            rExprs = RDeclRes [Int 9,Int 10],
            fpExprs = FDeclRes [FInt 9,FInt 10],
            eExpr = Just $ ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Conds [Cond {realPathCond = And (Rel Lt (Var Real "8") (Int 9))
                                                    (And (Rel Lt (Var Real "2") (Int 3))
                                                         (Rel Lt (Var Real "4") (Int 5)))
                                ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "8") (FInt 9))
                                                   (FAnd (FRel Lt (FVar FPDouble "2") (FInt 3))
                                                         (FRel Lt (FVar FPDouble "4") (FInt 5)))
                                ,realCond = BTrue
                                ,fpCond = FBTrue}],
            rExprs = RDeclRes [Int 9,Int 10],
            fpExprs = FDeclRes [FInt 9,FInt 10],
            eExpr = Just $ ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            }
        ]


semEFun__test5 = testCase "it correctly combines function semantics ACebS" $
    actual @?= expected
    where
      actual = semEFun "f" ResValue formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
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
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "0") (Int 1)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "0") (FInt 1)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Var Real "1") (Int 2)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "1") (FInt 2)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 1,Int 2],
                fpExprs = FDeclRes [FInt 1,FInt 2],
                eExpr = Just $ ErrRat 1,
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics1 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "4") (Int 5)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "4") (FInt 5)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 5,Int 6],
                fpExprs = FDeclRes [FInt 5,FInt 6],
                eExpr = Just $ ErrRat 3,
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1,fSemantics2]
            where
              fSemantics1 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "8") (Int 9)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "8") (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 9,Int 10],
                fpExprs = FDeclRes [FInt 9,FInt 10],
                eExpr = Just $ ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }
              fSemantics2 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "10") (Int 11)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "10") (FInt 11)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 11,Int 12],
                fpExprs = FDeclRes [FInt 11,FInt 12],
                eExpr = Just $ ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }
      expected = [
        ACeb {
            conds =  Conds [Cond {realPathCond = And (Rel Lt (Var Real "8") (Int 9))
                                                     (And (Rel Lt (Var Real "0") (Int 1))
                                                          (Rel Lt (Var Real "4") (Int 5)))
                                 ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "8") (FInt 9))
                                                    (FAnd (FRel Lt (FVar FPDouble "0") (FInt 1))
                                                          (FRel Lt (FVar FPDouble "4") (FInt 5)))
                                 ,realCond = BTrue
                                 ,fpCond = FBTrue}
                           ,Cond {realPathCond = And (Rel Lt (Var Real "8") (Int 9))
                                                     (And (Rel Lt (Var Real "1") (Int 2))
                                                          (Rel Lt (Var Real "4") (Int 5)))
                                  ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "8") (FInt 9))
                                                     (FAnd (FRel Lt (FVar FPDouble "1") (FInt 2))
                                                           (FRel Lt (FVar FPDouble "4") (FInt 5)))
                                  ,realCond = BTrue
                                  ,fpCond = FBTrue}],
            rExprs = RDeclRes [Int 9,Int 10],
            fpExprs = FDeclRes [FInt 9,FInt 10],
            eExpr = Just $ ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Conds [Cond {realPathCond = And (Rel Lt (Var Real "10") (Int 11))
                                                    (And (Rel Lt (Var Real "0") (Int 1))
                                                         (Rel Lt (Var Real "4") (Int 5)))
                                 ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "10") (FInt 11))
                                                    (FAnd (FRel Lt (FVar FPDouble "0") (FInt 1))
                                                          (FRel Lt (FVar FPDouble "4") (FInt 5)))
                                 ,realCond = BTrue
                                 ,fpCond = FBTrue}
                           ,Cond {realPathCond = And (Rel Lt (Var Real "10") (Int 11))
                                                     (And (Rel Lt (Var Real "1") (Int 2))
                                                          (Rel Lt (Var Real "4") (Int 5)))
                                  ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "10") (FInt 11))
                                                     (FAnd (FRel Lt (FVar FPDouble "1") (FInt 2))
                                                           (FRel Lt (FVar FPDouble "4") (FInt 5)))
                                  ,realCond = BTrue
                                  ,fpCond = FBTrue}],
            rExprs = RDeclRes [Int 11,Int 12],
            fpExprs = FDeclRes [FInt 11,FInt 12],
            eExpr = Just $ ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            }
        ]

semEFun__test6 = testCase "it correctly combines function semantics ACebS" $
    actual @?= expected
    where
      actual = semEFun "f" ResValue formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
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
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "0") (Int 1)
                                  ,fpPathCond = FRel Lt (FVar FPDouble "0") (FInt 1)
                                  ,realCond = BTrue
                                  ,fpCond = FBTrue}
                               ,Cond {realPathCond = Rel Lt (Var Real "1") (Int 2)
                                  ,fpPathCond = FRel Lt (FVar FPDouble "1") (FInt 2)
                                  ,realCond = BTrue
                                  ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 1,Int 2],
                fpExprs = FDeclRes [FInt 1,FInt 2],
                eExpr = Just $ ErrRat 1,
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics1 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "4") (Int 5)
                                  ,fpPathCond = FRel Lt (FVar FPDouble "4") (FInt 5)
                                  ,realCond = BTrue
                                  ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 5,Int 6],
                fpExprs = FDeclRes [FInt 5,FInt 6],
                eExpr = Just $ ErrRat 3,
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1,fSemantics2]
            where
              fSemantics1 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "8") (Int 9)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "8") (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 9,Int 10],
                fpExprs = FDeclRes [FInt 9,FInt 10],
                eExpr = Just $ ErrRat 5,
                decisionPath = root,
                cFlow = Unstable
                }
              fSemantics2 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "10") (Int 11)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "10") (FInt 11)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 11,Int 12],
                fpExprs = FDeclRes [FInt 11,FInt 12],
                eExpr = Just $ ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }
      expected = [
        ACeb {
            conds = Conds [Cond {realPathCond = And (Rel Lt (Var Real "8") (Int 9))
                                                    (And (Rel Lt (Var Real "0") (Int 1))
                                                         (Rel Lt (Var Real "4") (Int 5)))
                                  ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "8") (FInt 9))
                                                     (FAnd (FRel Lt (FVar FPDouble "0") (FInt 1))
                                                           (FRel Lt (FVar FPDouble "4") (FInt 5)))
                                  ,realCond = BTrue
                                  ,fpCond = FBTrue}
                               ,Cond {realPathCond = And (Rel Lt (Var Real "8") (Int 9))
                                                         (And (Rel Lt (Var Real "1") (Int 2))
                                                              (Rel Lt (Var Real "4") (Int 5)))
                                  ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "8") (FInt 9))
                                                     (FAnd (FRel Lt (FVar FPDouble "1") (FInt 2))
                                                           (FRel Lt (FVar FPDouble "4") (FInt 5)))
                                  ,realCond = BTrue
                                  ,fpCond = FBTrue}],
            rExprs = RDeclRes [Int 9,Int 10],
            fpExprs = FDeclRes [FInt 9,FInt 10],
            eExpr = Just $ ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Unstable
            },
        ACeb {
            conds = Conds [Cond {realPathCond = And (Rel Lt (Var Real "10") (Int 11))
                                                    (And (Rel Lt (Var Real "0") (Int 1))
                                                         (Rel Lt (Var Real "4") (Int 5)))
                                ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "10") (FInt 11))
                                                   (FAnd (FRel Lt (FVar FPDouble "0") (FInt 1))
                                                         (FRel Lt (FVar FPDouble "4") (FInt 5)))
                                ,realCond = BTrue
                                ,fpCond = FBTrue}
                          ,Cond {realPathCond = And (Rel Lt (Var Real "10") (Int 11))
                                                    (And (Rel Lt (Var Real "1") (Int 2))
                                                         (Rel Lt (Var Real "4") (Int 5)))
                                ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "10") (FInt 11))
                                                   (FAnd (FRel Lt (FVar FPDouble "1") (FInt 2))
                                                         (FRel Lt (FVar FPDouble "4") (FInt 5)))
                                ,realCond = BTrue
                                ,fpCond = FBTrue}],
            rExprs = RDeclRes [Int 11,Int 12],
            fpExprs = FDeclRes [FInt 11,FInt 12],
            eExpr = Just $ ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            }
        ]

semEFun__test7 = testCase "it correctly combines function semantics ACebS" $
    actual @?= expected
    where
      actual = semEFun "f" ResValue formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
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
                conds = Conds [Cond {realPathCond = Rel Lt (Var Real "0") (Int 1)
                                    ,fpPathCond = FRel Lt (FVar FPDouble "0") (FInt 1)
                                    ,realCond = BTrue
                                    ,fpCond = FBTrue}
                              ,Cond {realPathCond = Rel Lt (Var Real "1") (Int 2)
                                    ,fpPathCond = FRel Lt (FVar FPDouble "1") (FInt 2)
                                    ,realCond = BTrue
                                    ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 1,Int 2],
                fpExprs = FDeclRes [FInt 1,FInt 2],
                eExpr = Just $ ErrRat 1,
                decisionPath = root,
                cFlow = Unstable
                }
              ySemantics1 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "4") (Int 5)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "4") (FInt 5)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 5,Int 6],
                fpExprs = FDeclRes [FInt 5,FInt 6],
                eExpr = Just $ ErrRat 3,
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1]
            where
              fSemantics1 = ACeb {
                conds  = Conds [Cond {realPathCond = Rel Lt (Var Real "8") (Int 9)
                                     ,fpPathCond = FRel Lt (FVar FPDouble "8") (FInt 9)
                                     ,realCond = BTrue
                                     ,fpCond = FBTrue}],
                rExprs = RDeclRes [Int 9,Int 10],
                fpExprs = FDeclRes [FInt 9,FInt 10],
                eExpr = Just $ ErrRat 5,
                decisionPath = root,
                cFlow = Stable
                }
      expected = [
        ACeb {
            conds = Conds [Cond {realPathCond = And (Rel Lt (Var Real "8") (Int 9))
                                                    (And (Rel Lt (Var Real "0") (Int 1))
                                                         (Rel Lt (Var Real "4") (Int 5)))
                                ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "8") (FInt 9))
                                                   (FAnd (FRel Lt (FVar FPDouble "0") (FInt 1))
                                                         (FRel Lt (FVar FPDouble "4") (FInt 5)))
                                ,realCond = BTrue
                                ,fpCond = FBTrue}
                          ,Cond {realPathCond = And (Rel Lt (Var Real "8") (Int 9))
                                                    (And (Rel Lt (Var Real "1") (Int 2))
                                                         (Rel Lt (Var Real "4") (Int 5)))
                                ,fpPathCond = FAnd (FRel Lt (FVar FPDouble "8") (FInt 9))
                                                   (FAnd (FRel Lt (FVar FPDouble "1") (FInt 2))
                                                         (FRel Lt (FVar FPDouble "4") (FInt 5)))
                                ,realCond = BTrue
                                ,fpCond = FBTrue}],
            rExprs = RDeclRes [Int 9,Int 10],
            fpExprs = FDeclRes [FInt 9,FInt 10],
            eExpr = Just $ ErrRat (5 % 1),
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
