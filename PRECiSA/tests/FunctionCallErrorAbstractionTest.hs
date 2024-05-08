module FunctionCallErrorAbstractionTest where

import Control.Exception (tryJust,AssertionFailed(..), asyncExceptionFromException)
import Control.Monad
import Data.Maybe (fromJust,fromMaybe)
import Data.Bifunctor
import qualified Data.Map as Map
import Data.Map (fromList)
import Foreign.C.String
import Foreign.C.Types (CUInt)
import System.IO (hFlush,stdout)
import GHC.IO.Handle (hFlush)
import Test.Tasty
import Test.Tasty.HUnit

import AbsPVSLang
import AbsSpecLang
import AbstractDomain
import AbstractSemantics
import Common.ControlFlow
import Common.DecisionPath
import PVSTypes
import Kodiak.Kodiak
import Kodiak.Runnable
import Kodiak.Runner
import qualified Kodiak.Expression as K
import Kodiak.ExpressionTest
import Operators
import Translation.Float2Real
import Translation.Real2Float
import FunctionCallErrorAbstraction


rfcewc = replaceFunCallErr

semconf = SemConf {improveError = False
                  ,assumeTestStability = False
                  ,mergeUnstables = True
                  ,unfoldFunCalls = False}

testFunctionCallErrorAbstraction = testGroup "Function Call Error Abstraction"
  [testAexprEnclosure
  ,testFAexprEnclosure
  ,testreplaceFunCallErr
  ]

testAexprEnclosure = testGroup "aexprEnclosure"
  [aexprEnclosure__test1
  ,aexprEnclosure__test2
  ,aexprEnclosure__test3
  ]

aexprEnclosure__test1 = testCase "aexprEnclosure__test1" $
  aexprEnclosure True semconf emptyInterp emptyEnv 2 7 [] (Int 1)
  `returnsValue` (1, 1)

aexprEnclosure__test2 = testCase "aexprEnclosure__test1" $
  aexprEnclosure True semconf emptyInterp emptyEnv 2 7 [] (BinaryOp AddOp (Int 1) (Int 2))
  `returnsValue` (3, 3)

aexprEnclosure__test3 = testCase "aexprEnclosure__test3" $
  aexprEnclosure True semconf emptyInterp emptyEnv 2 7 [VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 10)] (Var Real "x")
  `returnsValue` (0, 10)

testFAexprEnclosure = testGroup "faexprEnclosure"
  [faexprEnclosure__test1
  ,faexprEnclosure__test2
  ,faexprEnclosure__test3
  ]

faexprEnclosure__test1 = testCase "faexprEnclosure__test1" $
  faexprEnclosure semconf emptyInterp emptyEnv 2 7 [VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 10)] (FInt 1) `returnsValue` (1, 1)
    where
      spec = Spec [SpecBind "g" [VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 1)]]

faexprEnclosure__test2 = testCase "faexprEnclosure__test2" $
  faexprEnclosure semconf emptyInterp emptyEnv  2 7 [VarBind "x" ResValue TInt (LBInt 0) (UBInt 10)] (BinaryFPOp AddOp FPDouble (FInt 1) (FInt 2)) `returnsValue` (3, 3)
    where
      spec = Spec [SpecBind "g" [VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 1)]]

faexprEnclosure__test3 = testCase "faexprEnclosure__test3" $
  faexprEnclosure semconf emptyInterp emptyEnv 2 7 [VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 10)] (FVar FPDouble "x")
  `returnsValue` (-0.0000000000000008881784197001252, 10)
    where
      spec = Spec [SpecBind "g" [VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 1)]]

testreplaceFunCallErr = testGroup "ReplaceFunCallErr"
  [testCase1
  ,testCase2
  ,testCase3
  ,testCase4
  ,testCase5
  ,testCase6
  ,testCase7
  ,testCase8
  ,testCase9
  ,testCase10
  -- ,testCase11
  ]
  where
    testCase1 = testCase "testFunctionCallErrorAbstraction 1" $
      rfcewc True semconf emptyInterp emptyEnv [] [VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 1)] (Int 1) `returnsValue` (Int 1)

    testCase2 = testCase "testFunctionCallErrorAbstraction 2" $
      rfcewc True semconf interp emptyEnv [] [VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 1)] (ErrFun "f" FPDouble ResValue [FVar FPDouble "x"]) `returnsValue` Rat 3
      where
        interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], sem))]
          where
            sem = Map.fromList [
                (ResValue
                ,[ACeb {
                  conds   = trueConds,
                  rExprs  = RDeclRes [Int 5],
                  fpExprs = FDeclRes [FInt 5],
                  eExpr   = Just $ Int 2,
                  decisionPath = root,
                  cFlow  = Stable }
                  ,ACeb {
                  conds   = trueConds,
                  rExprs  = RDeclRes [Int 5],
                  fpExprs = FDeclRes [FInt 5],
                  eExpr   = Just $ Int 3,
                  decisionPath = root,
                  cFlow  = Stable }
              ])]


    testCase3 = testCase "testFunctionCallErrorAbstraction 3" $
      rfcewc True semconf interp emptyEnv [] [VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 1)] (ErrFun "f" FPDouble ResValue [FInt 3]) `returnsValue` Rat 3
      where
        interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], sem))]
          where
            sem = Map.fromList [
              (ResValue
              ,[ACeb {
                conds   = trueConds,
                rExprs  = undefined,
                fpExprs = undefined,
                eExpr   = Just $ Var FPDouble "x",
                decisionPath = root,
                cFlow  = Stable }])
              ]

    testCase4 = testCase "testFunctionCallErrorAbstraction 4" $
      rfcewc True semconf interp emptyEnv [] [VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 4)] (ErrFun "f" FPDouble ResValue [FVar FPDouble "x"]) `returnsValue` Rat 4
      where
        interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], sem))]
          where
            sem = Map.fromList [
              (ResValue
              ,[ACeb {
                conds   = trueConds,
                rExprs  = undefined,
                fpExprs = undefined,
                eExpr   = Just $ Var FPDouble "x",
                decisionPath = root,
                cFlow  = Stable }])
              ]

    testCase5 = testCase "testFunctionCallErrorAbstraction 5" $
      rfcewc True semconf interp emptyEnv [] [VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 4)] (ErrFun "f" FPDouble ResValue [BinaryFPOp AddOp FPDouble (FVar FPDouble "x") (FInt 2)]) `returnsValue` Rat (6755399441055745/1125899906842624)
      where
        interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], sem))]
          where
            sem = Map.fromList [
              (ResValue
              ,[ACeb {
                conds   = trueConds,
                rExprs  = undefined,
                fpExprs = undefined,
                eExpr   = Just $ Var FPDouble "x",
                decisionPath = root,
                cFlow  = Stable }])
              ]

    testCase6 = testCase "testFunctionCallErrorAbstraction 6" $
      rfcewc True semconf interp emptyEnv [] [VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 4)] (ErrFun "f" FPDouble ResValue [FEFun False "d" ResValue FPDouble [FVar FPDouble "x"]]) `returnsValue` Rat 5
      where
        interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], semf))
                              ,("d",(False, FPDouble, [Arg "x" FPDouble], semd))]
          where
            semf = Map.fromList [
              (ResValue
              ,[ACeb {
                conds   = trueConds,
                rExprs  = RDeclRes [Var Real "x"],
                fpExprs = undefined,
                eExpr   = Just $ Var FPDouble "x",
                decisionPath = root,
                cFlow  = Stable }])
              ]
            semd = Map.fromList [
              (ResValue
              ,[ACeb {
                conds   = trueConds,
                rExprs  = RDeclRes [Var Real "x"],
                fpExprs = undefined,
                eExpr   = Just $ Rat 1,
                decisionPath = root,
                cFlow  = Stable }])
              ]

    testCase7 = testCase "testFunctionCallErrorAbstraction 7" $
      rfcewc True semconf interp emptyEnv [] [VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 4)] (EFun "d" ResValue FPDouble [Var Real "x"]) `returnsValue` Interval 3 15
      where
        interp = Map.fromList [("d",(False, FPDouble, [Arg "x" FPDouble], sem))]
          where
            sem = Map.fromList [
              (ResValue
              ,[ACeb {
                conds   = trueConds,
                rExprs  = RDeclRes [BinaryOp AddOp (Var Real "x") (Int 7)],
                fpExprs = undefined,
                eExpr   = Just $ Var FPDouble "x",
                decisionPath = root,
                cFlow  = Stable }])
              ]

    testCase8 = testCase "testFunctionCallErrorAbstraction 8" $
      rfcewc True semconf interp emptyEnv [] [VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 4)] (EFun "d" ResValue Real [RealMark "x" ResValue]) `returnsValue` Interval (-1) 5
      where
        interp = Map.fromList [("d",(False, FPDouble, [Arg "x" FPDouble], semd))]
          where
            semd = Map.fromList [
              (ResValue
              ,[ACeb {
                conds   = trueConds,
                rExprs  = RDeclRes [Var Real "x"],
                fpExprs = undefined,
                eExpr   = Just $ Rat 1,
                decisionPath = root,
                cFlow  = Stable }])
              ]

    testCase9 = testCase "testFunctionCallErrorAbstraction 9" $
      rfcewc True semconf interp emptyEnv [] [VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 4)] (EFun "f" ResValue Real [EFun "d" ResValue Real [RealMark "x" ResValue]]) `returnsValue` Interval (-2) 6
      where
        interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], semf))
                              ,("d",(False, FPDouble, [Arg "x" FPDouble], semd))]
          where
            semf = Map.fromList [
              (ResValue
              ,[ACeb {
                conds   = trueConds,
                rExprs  = RDeclRes [Var Real "x"],
                fpExprs = undefined,
                eExpr   = Just $ Rat 1,
                decisionPath = root,
                cFlow  = Stable }])
              ]
            semd = Map.fromList [
              (ResValue
              ,[ACeb {
                conds   = trueConds,
                rExprs  = RDeclRes [Var Real "x"],
                fpExprs = undefined,
                eExpr   = Just $ Rat 1,
                decisionPath = root,
                cFlow  = Stable }])
              ]

    testCase10 = testCase "testFunctionCallErrorAbstraction 10" $
      rfcewc True semconf interp emptyEnv [] [VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 4)] (ErrFun "f" FPDouble ResValue [BinaryFPOp AddOp FPDouble (FInt 2) (FEFun False "d" ResValue FPDouble [FVar FPDouble "x"])]) `returnsValue` Rat 7
      where

        interp = Map.fromList [("f",(False, FPDouble, [Arg "x" FPDouble], semf))
                              ,("d",(False, FPDouble, [Arg "x" FPDouble], semd))]
          where
            semf = Map.fromList [
              (ResValue
              ,[ACeb {
                conds   = trueConds,
                rExprs  = RDeclRes [Var Real "x"],
                fpExprs = undefined,
                eExpr   = Just $ Var FPDouble "x",
                decisionPath = root,
                cFlow  = Stable }])
              ]
            semd = Map.fromList [
              (ResValue
              ,[ACeb {
                conds   = trueConds,
                rExprs  = RDeclRes [Var Real "x"],
                fpExprs = undefined,
                eExpr   = Just $ Rat 1,
                decisionPath = root,
                cFlow  = Stable }])
              ]

    testCase11 = testCase "testFunctionCallErrorAbstraction 11" $
      rfcewc True semconf interp env [] [VarBind "vz" ResValue FPDouble (LBInt 0) (UBInt 100)] expr `returnsValue` Rat 7
      where
        interp = fromList
          [("d",(False,FPDouble,[Arg "y" FPDouble],fromList [(ResValue,[ACeb {conds = Conds [Cond {realPathCond = BTrue, fpPathCond = FBTrue, realCond = BTrue, fpCond = FBTrue}], rExprs = RDeclRes [RealMark "y" ResValue], fpExprs = FDeclRes [FVar FPDouble "y"], eExpr = Just (ErrorMark "y" ResValue FPDouble), decisionPath = LDP [], cFlow = Stable}])]))
          ,("f",(False,FPDouble,[Arg "r" FPDouble],fromList [(ResValue,[ACeb {conds = Conds [Cond {realPathCond = BTrue, fpPathCond = FBTrue, realCond = BTrue, fpCond = FBTrue}], rExprs = RDeclRes [EFun "d" ResValue Real [Var Real "r"]], fpExprs = FDeclRes [FEFun False "d" ResValue FPDouble [FVar FPDouble "r"]], eExpr = Just (ErrFun "d" FPDouble ResValue [FVar FPDouble "r"]), decisionPath = LDP [], cFlow = Stable}])]))
          ,("g",(False,FPDouble,[Arg "vz" FPDouble],fromList [(ResValue,[ACeb {conds = Conds [Cond {realPathCond = BTrue, fpPathCond = FBTrue, realCond = BTrue, fpCond = FBTrue}], rExprs = RDeclRes [EFun "f" ResValue Real [Var Real "vz"]], fpExprs = FDeclRes [FEFun False "f" ResValue FPDouble [FVar FPDouble "vz"]], eExpr = Just (ErrFun "f" FPDouble ResValue [FVar FPDouble "vz"]), decisionPath = LDP [], cFlow = Stable}])]))
          ]
        env = emptyEnv
        locVars = []
        fname = "g"
        expr = ErrFun "f" FPDouble ResValue [FVar FPDouble "vz"]

-- returnsValue :: IO AExpr -> AExpr -> Assertion
returnsValue actual expected =
  actual >>= \out -> (out == expected) @?
        ("Returned value: " ++ show out ++ " is different than expected: " ++ show expected)