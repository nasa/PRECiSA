module SMT.ModelTest where

import Test.Tasty
import Test.Tasty.HUnit

import AbstractSemantics
import AbstractDomain
import AbsPVSLang
import AbsSpecLang
import FPrec
import SMT.PrettyPrinter
import PPExt
import Control.Monad.State
import SMT.GenerateSMTModel


import AbsPVSLang
import AbstractDomain
import PPExt



testModel :: TestTree
testModel = testGroup "Model" 
  [testreplaceFreshVarInFAExpr
  ,testreplaceFreshVarInFBExpr
  ,testreplaceFreshVarInAExpr
  ,testreplaceFreshVarInBExpr
  ]


testreplaceFreshVarInFAExpr :: TestTree
testreplaceFreshVarInFAExpr = testGroup "replaceFreshVarInFAExpr"
  [testCase "FInt returns itself" $
   (test $ FInt 3) @?= FInt 3
  ,testCase "FCnst returns itself" $
   (test $ FCnst FPDouble 3.2) @?= FCnst FPDouble 3.2
  ,testCase "RtoD(Int) returns itself" $
   (test $ RtoD (Int 3)) @?= RtoD (Int 3)
  ,testCase "RtoS(Int) returns itself" $
   (test $ RtoS (Int 3)) @?= RtoS (Int 3)
  ,testCase "RtoD(Rat) returns itself" $
   (test $ RtoD (Rat 3)) @?= RtoD (Rat 3)
  ,testCase "RtoS(Rat) returns itself" $
   (test $ RtoS (Rat 3)) @?= RtoS (Rat 3)
  ,testCase "FVar returns itself" $
   (test $ FVar FPDouble "u") @?= FVar FPDouble "u"
  ,testCase "FAExpr returns a fresh variable if not replaced yet" $
   (test $ FSub FPDouble (FVar FPDouble "u") (FVar FPDouble "v")) @?= FVar FPDouble "Temp_2"
  ,testCase "FAExpr returns a previous variable if already replaced" $
   (test $ FAdd FPDouble (FVar FPDouble "y") (FVar FPDouble "z")) @?= FVar FPDouble "Temp_1"
  ]
    where
      test x = fst $ runState (replaceFreshVarInFAExpr x) state
      state  = (2,[("Temp_1", FAdd FPDouble (FVar FPDouble "y") (FVar FPDouble "z"), FPDouble)])

testreplaceFreshVarInFBExpr :: TestTree
testreplaceFreshVarInFBExpr = testGroup "testreplaceFreshVarInFBExpr"
  [testCase "FBTrue returns itself" $
   (test $ FBTrue) @?= FBTrue
  ,testCase "FBFalse returns itself" $
   (test $ FBFalse) @?= FBFalse
  ,testCase "FNot propagates replacements" $
   (test $ FNot
    (FGt
     (FMul FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
     (FSub FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))) )
    @?=
    FNot
    (FGt
     (FVar FPDouble "Temp_2")
     (FVar FPDouble "Temp_3"))
  ,testCase "FAnd propagates replacements" $
   (test $ FAnd 
    (FEq
     (FAdd FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
     (FSub FPDouble (FVar FPDouble "y") (FVar FPDouble "z")))
    (FNeq
     (FSub FPDouble (FVar FPDouble "y") (FVar FPDouble "z")) 
     (FAdd FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))) )
   @?=
   FAnd 
    (FEq
     (FVar FPDouble "Temp_1")
     (FVar FPDouble "Temp_2"))
    (FNeq
     (FVar FPDouble "Temp_2")
     (FVar FPDouble "Temp_1"))
  ,testCase "FOr propagates replacements" $
   (test $ FOr 
    (FEq
     (FAdd FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
     (FSub FPDouble (FVar FPDouble "y") (FVar FPDouble "z")))
    (FNeq
     (FSub FPDouble (FVar FPDouble "y") (FVar FPDouble "z")) 
     (FAdd FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))) )
   @?=
   FOr 
    (FEq
     (FVar FPDouble "Temp_1")
     (FVar FPDouble "Temp_2"))
    (FNeq
     (FVar FPDouble "Temp_2")
     (FVar FPDouble "Temp_1"))
  ,testCase "FGt propagates replacements" $
   (test $ FGt
    (FAdd FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
    (FSub FPDouble (FVar FPDouble "y") (FVar FPDouble "z")) )
   @?=
   FGt (FVar FPDouble "Temp_1") (FVar FPDouble "Temp_2")
  ,testCase "FGtE propagates replacements" $
   (test $ FGtE
    (FAdd FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
    (FSub FPDouble (FVar FPDouble "y") (FVar FPDouble "z")) )
   @?=
   FGtE (FVar FPDouble "Temp_1") (FVar FPDouble "Temp_2")
  ,testCase "FLt propagates replacements" $
   (test $ FLt
    (FAdd FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
    (FSub FPDouble (FVar FPDouble "y") (FVar FPDouble "z")) )
   @?=
   FLt (FVar FPDouble "Temp_1") (FVar FPDouble "Temp_2")
  ,testCase "FLtE propagates replacements" $
   (test $ FLtE
    (FAdd FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
    (FSub FPDouble (FVar FPDouble "y") (FVar FPDouble "z")) )
   @?=
   FLtE (FVar FPDouble "Temp_1") (FVar FPDouble "Temp_2")
  ,testCase "FNeq propagates replacements" $
   (test $ FNeq
    (FAdd FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
    (FSub FPDouble (FVar FPDouble "y") (FVar FPDouble "z")) )
   @?=
   FNeq (FVar FPDouble "Temp_1") (FVar FPDouble "Temp_2")
  ,testCase "FEq propagates replacements" $
   (test $ FEq
    (FAdd FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
    (FSub FPDouble (FVar FPDouble "y") (FVar FPDouble "z")) )
   @?=
   FEq (FVar FPDouble "Temp_1") (FVar FPDouble "Temp_2")
  ] 
    where
      test x = fst $ runState (replaceFreshVarInFBExpr x) state
      state  = (2,[("Temp_1", FAdd FPDouble (FVar FPDouble "y") (FVar FPDouble "z"), FPDouble)])
      
testreplaceFreshVarInAExpr :: TestTree
testreplaceFreshVarInAExpr = testGroup "replaceFreshVarInAExpr"
  [testCase "Int returns itself" $
   (test $ Int 3) @?= Int 3
  ,testCase "Rat returns itself" $
   (test $ Rat 3.2) @?= Rat 3.2
  ,testCase "Var returns itself" $
   (test $ Var Real "u") @?= Var Real "u"
  ,testCase "RealMark returns itself" $
   (test $ RealMark "X") @?= RealMark "X"
  ,testCase "ErrorMark returns itself" $
   (test $ ErrorMark "X" FPDouble) @?= ErrorMark "X" FPDouble 
  ,testCase "AExpr returns a fresh variable if not replaced yet" $
   (test $ Sub (Var Real "u") (Var Real "v")) @?= Var Real "Real_Temp_2"
  ,testCase "AExpr returns a previous variable if already replaced" $
   (test $ Add (Var Real "y") (Var Real "z")) @?= Var Real "Real_Temp_1"
  ,testCase "Mul returns a fresh variable if not replaced" $
   (test $ Mul (Var Real "y") (Var Real "z")) @?= Var Real "Real_Temp_2"
  ,testCase "Sub returns a fresh variable if not replaced" $
   (test $ Sub (Var Real "y") (Var Real "z")) @?= Var Real "Real_Temp_2"
  ]
    where
      test x = fst $ runState (replaceFreshVarInAExpr x) state
      state  = (2,[("Real_Temp_1", Add (Var Real "y") (Var Real "z"))])

testreplaceFreshVarInBExpr :: TestTree
testreplaceFreshVarInBExpr = testGroup "testreplaceFreshVarInBExpr"
  [testCase "BTrue returns itself" $
   (test $ BTrue) @?= BTrue
  ,testCase "BFalse returns itself" $
   (test $ BFalse) @?= BFalse
  ,testCase "Not propagates replacements" $
   (test $ Not (Gt (Mul (Var Real "y") (Var Real "z")) (Sub (Var Real "y") (Var Real "z"))) )
       @?= Not (Gt (Var Real "Real_Temp_2") (Var Real "Real_Temp_3"))
  ,testCase "And propagates replacements" $
   (test $ And (Eq  (Add (Var Real "y") (Var Real "z")) (Sub (Var Real "y") (Var Real "z")))
               (Neq (Sub (Var Real "y") (Var Real "z")) (Add (Var Real "y") (Var Real "z"))) )
       @?= And (Eq  (Var Real "Real_Temp_1") (Var Real "Real_Temp_2"))
               (Neq (Var Real "Real_Temp_2") (Var Real "Real_Temp_1"))
  ,testCase "Or propagates replacements" $
   (test $ Or (Eq  (Add (Var Real "y") (Var Real "z")) (Sub (Var Real "y") (Var Real "z")))
              (Neq (Sub (Var Real "y") (Var Real "z")) (Add (Var Real "y") (Var Real "z"))) )
       @?= Or ( Eq (Var Real "Real_Temp_1") (Var Real "Real_Temp_2"))
              (Neq (Var Real "Real_Temp_2") (Var Real "Real_Temp_1"))
  ,testCase "Gt propagates replacements" $
   (test $ Gt (Add (Var Real "y") (Var Real "z")) (Sub (Var Real "y") (Var Real "z")) )
       @?= Gt (Var Real "Real_Temp_1") (Var Real "Real_Temp_2")
  ,testCase "Gt propagates replacements" $
   (test $ Gt (Mul (Var Real "y") (Var Real "z")) (Sub (Var Real "y") (Var Real "z")) )
       @?= Gt (Var Real "Real_Temp_2") (Var Real "Real_Temp_3")
  ,testCase "GtE propagates replacements" $
   (test $ GtE (Add (Var Real "y") (Var Real "z")) (Sub (Var Real "y") (Var Real "z")) )
       @?= GtE (Var Real "Real_Temp_1") (Var Real "Real_Temp_2")
  ,testCase "Lt propagates replacements" $
   (test $ Lt (Add (Var Real "y") (Var Real "z")) (Sub (Var Real "y") (Var Real "z")) )
       @?= Lt (Var Real "Real_Temp_1") (Var Real "Real_Temp_2")
  ,testCase "LtE propagates replacements" $
   (test $ LtE (Add (Var Real "y") (Var Real "z")) (Sub (Var Real "y") (Var Real "z")) )
       @?= LtE (Var Real "Real_Temp_1") (Var Real "Real_Temp_2")
  ,testCase "Neq propagates replacements" $
   (test $ Neq (Add (Var Real "y") (Var Real "z"))(Sub (Var Real "y") (Var Real "z")) )
       @?= Neq (Var Real "Real_Temp_1") (Var Real "Real_Temp_2")
  ,testCase "Eq propagates replacements" $
   (test $ Eq (Add (Var Real "y") (Var Real "z")) (Sub (Var Real "y") (Var Real "z")) )
       @?= Eq (Var Real "Real_Temp_1") (Var Real "Real_Temp_2")
  ] 
    where
      test x = fst $ runState (replaceFreshVarInBExpr x) state
      state  = (2,[("Real_Temp_1", Add (Var Real "y") (Var Real "z"))])


--testGenSMTConstraints :: TestTree
--testGenSMTConstraints = testGroup "testGenSMTConstraints"
--  [testCase "BTrue returns itself" $
--   (test $ BTrue) @?= BTrue
--  ] 
--  where
--      test x = fst $ runState (replaceFreshVarInBExpr x) state
-- testGenerateErrorConstraints :: TestTree







