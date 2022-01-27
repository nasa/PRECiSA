-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module SMT.ModelTest where

import Test.Tasty
import Test.Tasty.HUnit

import PVSTypes
import Control.Monad.State hiding (state)
import SMT.GenerateSMTModel
import Operators
import AbsPVSLang

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
   (test $ ToFloat FPDouble (Int 3)) @?= ToFloat FPDouble (Int 3)
  ,testCase "RtoS(Int) returns itself" $
   (test $ ToFloat FPSingle (Int 3)) @?= ToFloat FPSingle (Int 3)
  ,testCase "RtoD(Rat) returns itself" $
   (test $ ToFloat FPDouble (Rat 3)) @?= ToFloat FPDouble (Rat 3)
  ,testCase "RtoS(Rat) returns itself" $
   (test $ ToFloat FPSingle (Rat 3)) @?= ToFloat FPSingle (Rat 3)
  ,testCase "FVar returns itself" $
   (test $ FVar FPDouble "u") @?= FVar FPDouble "u"
  ,testCase "FAExpr returns a fresh variable if not replaced yet" $
   (test $ BinaryFPOp SubOp FPDouble (FVar FPDouble "u") (FVar FPDouble "v")) @?= FVar FPDouble "Temp_2"
  ,testCase "FAExpr returns a previous variable if already replaced" $
   (test $ BinaryFPOp AddOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z")) @?= FVar FPDouble "Temp_1"
  ]
    where
      test x = fst $ runState (replaceFreshVarInFAExpr x) state
      state  = (2,[("Temp_1", BinaryFPOp AddOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z"), FPDouble)])

testreplaceFreshVarInFBExpr :: TestTree
testreplaceFreshVarInFBExpr = testGroup "testreplaceFreshVarInFBExpr"
  [testCase "FBTrue returns itself" $
   (test $ FBTrue) @?= FBTrue
  ,testCase "FBFalse returns itself" $
   (test $ FBFalse) @?= FBFalse
  ,testCase "FNot propagates replacements" $
   (test $ FNot
    (FRel Gt
     (BinaryFPOp MulOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
     (BinaryFPOp SubOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))) )
    @?=
    FNot
    (FRel Gt
     (FVar FPDouble "Temp_2")
     (FVar FPDouble "Temp_3"))
  ,testCase "FAnd propagates replacements" $
   (test $ FAnd
    (FRel Eq
     (BinaryFPOp AddOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
     (BinaryFPOp SubOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z")))
    (FRel Neq
     (BinaryFPOp SubOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
     (BinaryFPOp AddOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))) )
   @?=
   FAnd
    (FRel Eq
     (FVar FPDouble "Temp_1")
     (FVar FPDouble "Temp_2"))
    (FRel Neq
     (FVar FPDouble "Temp_2")
     (FVar FPDouble "Temp_1"))
  ,testCase "FOr propagates replacements" $
   (test $ FOr
    (FRel Eq
     (BinaryFPOp AddOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
     (BinaryFPOp SubOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z")))
    (FRel Neq
     (BinaryFPOp SubOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
     (BinaryFPOp AddOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))) )
   @?=
   FOr
    (FRel Eq
     (FVar FPDouble "Temp_1")
     (FVar FPDouble "Temp_2"))
    (FRel Neq
     (FVar FPDouble "Temp_2")
     (FVar FPDouble "Temp_1"))
  ,testCase "FRel Gt propagates replacements" $
   (test $ FRel Gt
    (BinaryFPOp AddOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
    (BinaryFPOp SubOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z")) )
   @?=
   FRel Gt (FVar FPDouble "Temp_1") (FVar FPDouble "Temp_2")
  ,testCase "FRel GtE propagates replacements" $
   (test $ FRel GtE
    (BinaryFPOp AddOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
    (BinaryFPOp SubOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z")) )
   @?=
   FRel GtE (FVar FPDouble "Temp_1") (FVar FPDouble "Temp_2")
  ,testCase "FRel Lt propagates replacements" $
   (test $ FRel Lt
    (BinaryFPOp AddOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
    (BinaryFPOp SubOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z")) )
   @?=
   FRel Lt (FVar FPDouble "Temp_1") (FVar FPDouble "Temp_2")
  ,testCase "FRel LtE propagates replacements" $
   (test $ FRel LtE
    (BinaryFPOp AddOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
    (BinaryFPOp SubOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z")) )
   @?=
   FRel LtE (FVar FPDouble "Temp_1") (FVar FPDouble "Temp_2")
  ,testCase "FRel Neq propagates replacements" $
   (test $ FRel Neq
    (BinaryFPOp AddOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
    (BinaryFPOp SubOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z")) )
   @?=
   FRel Neq (FVar FPDouble "Temp_1") (FVar FPDouble "Temp_2")
  ,testCase "FRel Eq propagates replacements" $
   (test $ FRel Eq
    (BinaryFPOp AddOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z"))
    (BinaryFPOp SubOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z")) )
   @?=
   FRel Eq (FVar FPDouble "Temp_1") (FVar FPDouble "Temp_2")
  ]
    where
      test x = fst $ runState (replaceFreshVarInFBExpr x) state
      state  = (2,[("Temp_1", BinaryFPOp AddOp FPDouble (FVar FPDouble "y") (FVar FPDouble "z"), FPDouble)])

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
   (test $ BinaryOp SubOp (Var Real "u") (Var Real "v")) @?= Var Real "Real_Temp_2"
  ,testCase "AExpr returns a previous variable if already replaced" $
   (test $ BinaryOp AddOp (Var Real "y") (Var Real "z")) @?= Var Real "Real_Temp_1"
  ,testCase "Mul returns a fresh variable if not replaced" $
   (test $ BinaryOp MulOp (Var Real "y") (Var Real "z")) @?= Var Real "Real_Temp_2"
  ,testCase "Sub returns a fresh variable if not replaced" $
   (test $ BinaryOp SubOp (Var Real "y") (Var Real "z")) @?= Var Real "Real_Temp_2"
  ]
    where
      test x = fst $ runState (replaceFreshVarInAExpr x) state
      state  = (2,[("Real_Temp_1", BinaryOp AddOp (Var Real "y") (Var Real "z"))])

testreplaceFreshVarInBExpr :: TestTree
testreplaceFreshVarInBExpr = testGroup "testreplaceFreshVarInBExpr"
  [testCase "BTrue returns itself" $
   (test $ BTrue) @?= BTrue
  ,testCase "BFalse returns itself" $
   (test $ BFalse) @?= BFalse
  ,testCase "Not propagates replacements" $
   (test $ Not (Rel Gt (BinaryOp MulOp (Var Real "y") (Var Real "z")) (BinaryOp SubOp (Var Real "y") (Var Real "z"))) )
       @?= Not (Rel Gt (Var Real "Real_Temp_2") (Var Real "Real_Temp_3"))
  ,testCase "And propagates replacements" $
   (test $ And (Rel Eq  (BinaryOp AddOp (Var Real "y") (Var Real "z")) (BinaryOp SubOp (Var Real "y") (Var Real "z")))
               (Rel Neq (BinaryOp SubOp (Var Real "y") (Var Real "z")) (BinaryOp AddOp (Var Real "y") (Var Real "z"))) )
       @?= And (Rel Eq  (Var Real "Real_Temp_1") (Var Real "Real_Temp_2"))
               (Rel Neq (Var Real "Real_Temp_2") (Var Real "Real_Temp_1"))
  ,testCase "Or propagates replacements" $
   (test $ Or (Rel Eq  (BinaryOp AddOp (Var Real "y") (Var Real "z")) (BinaryOp SubOp (Var Real "y") (Var Real "z")))
              (Rel Neq (BinaryOp SubOp (Var Real "y") (Var Real "z")) (BinaryOp AddOp (Var Real "y") (Var Real "z"))) )
       @?= Or (Rel Eq (Var Real "Real_Temp_1") (Var Real "Real_Temp_2"))
              (Rel Neq (Var Real "Real_Temp_2") (Var Real "Real_Temp_1"))
  ,testCase "Gt propagates replacements" $
   (test $ Rel Gt (BinaryOp AddOp (Var Real "y") (Var Real "z")) (BinaryOp SubOp (Var Real "y") (Var Real "z")) )
       @?= Rel Gt (Var Real "Real_Temp_1") (Var Real "Real_Temp_2")
  ,testCase "Gt propagates replacements" $
   (test $ Rel Gt (BinaryOp MulOp (Var Real "y") (Var Real "z")) (BinaryOp SubOp (Var Real "y") (Var Real "z")) )
       @?= Rel Gt (Var Real "Real_Temp_2") (Var Real "Real_Temp_3")
  ,testCase "GtE propagates replacements" $
   (test $ Rel GtE (BinaryOp AddOp (Var Real "y") (Var Real "z")) (BinaryOp SubOp (Var Real "y") (Var Real "z")) )
       @?= Rel GtE (Var Real "Real_Temp_1") (Var Real "Real_Temp_2")
  ,testCase "Lt propagates replacements" $
   (test $ Rel Lt (BinaryOp AddOp (Var Real "y") (Var Real "z")) (BinaryOp SubOp (Var Real "y") (Var Real "z")) )
       @?= Rel Lt (Var Real "Real_Temp_1") (Var Real "Real_Temp_2")
  ,testCase "LtE propagates replacements" $
   (test $ Rel LtE (BinaryOp AddOp (Var Real "y") (Var Real "z")) (BinaryOp SubOp (Var Real "y") (Var Real "z")) )
       @?= Rel LtE (Var Real "Real_Temp_1") (Var Real "Real_Temp_2")
  ,testCase "Neq propagates replacements" $
   (test $ Rel Neq (BinaryOp AddOp (Var Real "y") (Var Real "z"))(BinaryOp SubOp (Var Real "y") (Var Real "z")) )
       @?= Rel Neq (Var Real "Real_Temp_1") (Var Real "Real_Temp_2")
  ,testCase "Eq propagates replacements" $
   (test $ Rel Eq (BinaryOp AddOp (Var Real "y") (Var Real "z")) (BinaryOp SubOp (Var Real "y") (Var Real "z")) )
       @?= Rel Eq (Var Real "Real_Temp_1") (Var Real "Real_Temp_2")
  ]
    where
      test x = fst $ runState (replaceFreshVarInBExpr x) state
      state  = (2,[("Real_Temp_1", BinaryOp AddOp (Var Real "y") (Var Real "z"))])


--testGenSMTConstraints :: TestTree
--testGenSMTConstraints = testGroup "testGenSMTConstraints"
--  [testCase "BTrue returns itself" $
--   (test $ BTrue) @?= BTrue
--  ]
--  where
--      test x = fst $ runState (replaceFreshVarInBExpr x) state
-- testGenerateErrorConstraints :: TestTree







