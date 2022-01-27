-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module Translation.Real2FloatTest where

import AbsPVSLang
import Common.TypesUtils
import PVSTypes
import Operators
import Translation.Real2Float
import Test.Tasty
import Test.Tasty.HUnit
import UtilsTest (fromDouble2Rat)

testReal2Float:: TestTree
testReal2Float = testGroup "Real2Float"
    [testReal2fpAexpr__tests
    ,testReal2fpBexpr__tests
    ,real2fpDecl__tests
    ,real2fpActArgs__tests
    ]

real2fpActArgs__tests :: TestTree
real2fpActArgs__tests = testGroup "real2fpActArgs"
  [real2fpActArgs__test1
  ,real2fpActArgs__test2
  ,real2fpActArgs__test3
  ]

real2fpActArgs__test1 = testCase "real2fpActArgs__test1" $
  real2fpActArgs False FPDouble [] [Arg "x" Real] [BinaryOp AddOp (Int 2) (Rat (fromDouble2Rat 0.2))]
  @?=
  [BinaryFPOp AddOp FPDouble (TypeCast TInt FPDouble $ FInt 2) (ToFloat FPDouble $ Rat (fromDouble2Rat 0.2))]

real2fpActArgs__test2 = testCase "real2fpActArgs__test2" $
  real2fpActArgs False FPDouble [] [Arg "x" TInt] [BinaryOp AddOp (Int 2) (Int 3)]
  @?=
  [BinaryFPOp AddOp TInt (FInt 2) (FInt 3)]

real2fpActArgs__test3 = testCase "real2fpActArgs__test3" $
  real2fpActArgs False FPDouble [] [Arg "x" Real] [BinaryOp AddOp (Int 2) (Int 3)]
  @?=
  [TypeCast TInt FPDouble $ BinaryFPOp AddOp TInt (FInt 2) (FInt 3)]

testReal2fpAexpr__tests :: TestTree
testReal2fpAexpr__tests = testGroup "testReal2fpAexpr"
    [testReal2fpAexpr__test1
    ,testReal2fpAexpr__test2
    ,testReal2fpAexpr__test3
    ,testReal2fpAexpr__test4
    ,testReal2fpAexpr__test5
    ,testReal2fpAexpr__test6
    ,testReal2fpAexpr__test7
    ]

testReal2fpAexpr__test1 = testCase "real2fpAexpr(Int + Rat) = cast(Int) + Rat" $
  real2fpAexpr False FPDouble [] (BinaryOp AddOp (Int 2) (Rat (fromDouble2Rat 0.2)))
  @?=
  BinaryFPOp AddOp FPDouble (TypeCast TInt FPDouble (FInt 2)) (ToFloat FPDouble (Rat $ fromDouble2Rat 0.2))

testReal2fpAexpr__test2 = testCase "real2fpAexpr(IntSum + Rat) = cast(IntSum) + Rat" $
  real2fpAexpr False FPDouble [] (BinaryOp AddOp (BinaryOp AddOp (Int 2) (Int 4)) (Rat (fromDouble2Rat 0.2)))
  @?=
  BinaryFPOp AddOp FPDouble (TypeCast TInt FPDouble (BinaryFPOp AddOp TInt (FInt 2) (FInt 4))) (ToFloat FPDouble (Rat $ fromDouble2Rat 0.2))

testReal2fpAexpr__test3 = testCase "real2fpAexpr(Rat + Int) = Rat + cast(Int)" $
  real2fpAexpr False FPDouble [] (BinaryOp AddOp (Rat (fromDouble2Rat 0.2)) (Int 2))
  @?=
  BinaryFPOp AddOp FPDouble (ToFloat FPDouble (Rat $ fromDouble2Rat 0.2)) (TypeCast TInt FPDouble (FInt 2))

testReal2fpAexpr__test4 = testCase "real2fpAexpr(Rat + Rat) = Rat + Rat" $
  real2fpAexpr False FPDouble [] (BinaryOp AddOp (Rat (fromDouble2Rat 0.2)) (Rat (fromDouble2Rat 0.4)))
  @?=
  BinaryFPOp AddOp FPDouble (ToFloat FPDouble (Rat $ fromDouble2Rat 0.2)) (ToFloat FPDouble (Rat $ fromDouble2Rat 0.4))

testReal2fpAexpr__test5 = testCase "real2fpAexpr(Int + Int) = Int + Int" $
  real2fpAexpr False TInt [] (BinaryOp AddOp (Int 2) (Int 5))
  @?=
  BinaryFPOp AddOp TInt (FInt 2) (FInt 5)

testReal2fpAexpr__test6 = testCase "real2fpAexpr(If x>0 then x else 5) = If x>0 then x else (double) 5" $
  real2fpAexpr False FPDouble [] (RIte (Rel Gt (Var Real "x") (Int 0)) (Var Real "x") (Int 5))
  @?=
  Ite (FRel Gt (FVar FPDouble "x") (TypeCast TInt FPDouble $ FInt 0))
      (FVar FPDouble "x")
      (TypeCast TInt FPDouble (FInt 5))

testReal2fpAexpr__test7 = testCase "real2fpAexpr(If p(x) then 4 else 5) = If x>0 then x else (double) 5" $
  real2fpAexpr False FPDouble [RPred "p" [Arg "y" Real] (RBExpr BTrue)] (RIte (EPred "p" [Var Real "x"]) (Int 4) (Int 5))
  @?=
  TypeCast TInt FPDouble (Ite (FEPred True Original "p" [FVar FPDouble "x"]) (FInt 4) (FInt 5))


testReal2fpBexpr__tests :: TestTree
testReal2fpBexpr__tests = testGroup "testReal2fpBexpr"
    [testReal2fpBexpr__test1
    ,testReal2fpBexpr__test2
    ,testReal2fpBexpr__test3
    ,testReal2fpBexpr__test4
    ]

testReal2fpBexpr__test1 = testCase "real2fpBexpr(Int < Rat) = cast(Int) < Rat" $
  real2fpBexpr False FPDouble [] (Rel Lt (Int 2) (Rat (fromDouble2Rat 0.2)))
  @?=
  FRel Lt (TypeCast TInt FPDouble (FInt 2)) (ToFloat FPDouble (Rat $ fromDouble2Rat 0.2))

testReal2fpBexpr__test2 = testCase "real2fpBexpr(Int < Rat) = cast(Int) < Rat" $
  real2fpBexpr False FPDouble [] (Rel LtE (Int 2) (Rat (fromDouble2Rat 0.2)))
  @?=
  FRel LtE (TypeCast TInt FPDouble (FInt 2)) (ToFloat FPDouble (Rat $ fromDouble2Rat 0.2))

testReal2fpBexpr__test3 = testCase "real2fpBexpr(Rat > IntSum) = Rat > cast(IntSum)" $
  real2fpBexpr False FPDouble [] (Rel Gt (Rat (fromDouble2Rat 0.2)) (BinaryOp AddOp (Int 2) (Int 4)))
  @?=
  FRel Gt (ToFloat FPDouble (Rat $ fromDouble2Rat 0.2)) (TypeCast TInt FPDouble $ BinaryFPOp AddOp TInt (FInt 2) (FInt 4))

testReal2fpBexpr__test4 = testCase "real2fpBexpr(Rat >= IntSum) = Rat > cast(Int)" $
  real2fpBexpr False FPDouble [] (Rel GtE (Rat (fromDouble2Rat 0.2)) (BinaryOp AddOp (Int 2) (Int 4)))
  @?=
  FRel GtE (ToFloat FPDouble (Rat $ fromDouble2Rat 0.2)) (TypeCast TInt FPDouble $ BinaryFPOp AddOp TInt (FInt 2) (FInt 4))

real2fpDecl__tests :: TestTree
real2fpDecl__tests = testGroup "real2fpDecl"
    [real2fpDecl__test1
    ,real2fpDecl__test2
    ,real2fpDecl__test3
    -- ,real2fpDecl__test4
    -- ,real2fpDecl__test5
    -- ,real2fpDecl__test6
    -- ,real2fpDecl__test7
    ]

real2fpDecl__test1 = testCase "real2fpDecl 1" $
  real2fpDecl FPDouble [RDecl TInt "f" [Arg "x" Real] (RIte (EPred "p" [Var Real "x"]) (Int 4) (Int 5))
                       ,RPred "p" [Arg "y" Real] (RBExpr BTrue)]
                       (RDecl TInt "f" [Arg "x" Real] (RIte (EPred "p" [Var Real "x"]) (Int 4) (Int 5)))
  @?=
  Decl True TInt "f" [Arg "x" FPDouble] (Ite (FEPred True Original "p" [FVar FPDouble "x"])
                                            (FInt 4)
                                            (FInt 5))

real2fpDecl__test2 = testCase "real2fpDecl 2" $
  real2fpDecl FPDouble [RDecl TInt "f" [Arg "x" Real] (RIte (EPred "p" [Var Real "x"]) (Int 4) (Int 5))
                       ,RPred "p" [Arg "y" Real] (RBExpr BTrue)]
                       (RDecl FPDouble "f" [Arg "x" Real] (RIte (EPred "p" [Var Real "x"]) (Int 4) (Int 5)))
  @?=
  Decl True FPDouble "f" [Arg "x" FPDouble] (TypeCast TInt FPDouble (Ite (FEPred True Original "p" [FVar FPDouble "x"])
                                                                    (FInt 4) (FInt 5)))

real2fpDecl__test3 = testCase "real2fpDecl 3" $
  real2fpDecl FPDouble [RDecl TInt "f" [Arg "x" Real] (RIte (EPred "p" [Var Real "x"]) (Int 4) (Int 5))
                       ,RPred "p" [Arg "y" Real] (RBExpr BTrue)]
                       (RDecl FPDouble "f" [Arg "x" Real] (RIte (EPred "p" [Var Real "x"]) (Var Real "x") (Int 5)))
  @?=
  Decl True FPDouble "f" [Arg "x" FPDouble] (Ite (FEPred True Original "p" [FVar FPDouble "x"])
                                                 (FVar FPDouble "x")
                                                 (TypeCast TInt FPDouble $ FInt 5))