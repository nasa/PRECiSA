-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
 
-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
 
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
    
    
{-# LANGUAGE MultiParamTypeClasses #-}

module Kodiak.GeneratorTest where

import Control.Monad.Except
import Test.Tasty
import Test.Tasty.HUnit

import PVSTypes
import AbsPVSLang
import qualified Kodiak.Expression as K
import Kodiak.Generator
import Operators

testKodiakGenerator :: TestTree
testKodiakGenerator = testGroup "Kodiak Expression Generators"
    [testKodiakAExprGenerator
    ,testKodiakBExprGenerator
    ,testIntegerExpressionSimplifier
    ]

testKodiakAExprGenerator :: TestTree
testKodiakAExprGenerator = testGroup "Kodiak AExpr Generator" $
    let x = "x" in
    [testGroup "Real"
        [testGroup "Constants"
            [testCase "Int" $
                (kodiakize $ Int 1)
                @?=
                (return $ K.Cnst 1)
            ,testCase "Rat" $
                (kodiakize $ Rat 2)
                @?=
                (return $ K.Cnst 2)
            ]
        ,testGroup "Variables"
            [testGroup "Var"
                [testCase "Int" $
                    (kodiakize $ Var TInt x)
                    @?=
                    (return $ K.Var x)
                ,testCase "Real" $
                    (kodiakize $ Var Real x)
                    @?=
                    (return $ K.Var x)
                ,testCase "Other variable types fail" $
                    (kodiakize $ Var FPSingle x)
                    @?=
                    (throwError $ NonSupportedExpr $ Var FPSingle x)
                ]
            ,testCase "RealMark" $
                (kodiakize $ RealMark x)
                @?=
                (return $ K.Var x)
            ]
        ,testGroup "Binary operators"
            [testCase "Add" $
                (kodiakize $ BinaryOp AddOp (Rat 2) (Rat 1))
                @?=
                (return $ K.Add (K.Cnst 2) (K.Cnst 1))
            ,testCase "Sub" $
                (kodiakize $ BinaryOp SubOp (Rat 1) (Rat 2))
                @?=
                (return $ K.Sub (K.Cnst 1) (K.Cnst 2))
            ,testCase "Mul" $
                (kodiakize $ BinaryOp MulOp (Rat 1) (Rat 2))
                @?=
                (return $ K.Mul (K.Cnst 1) (K.Cnst 2))
            ,testCase "Div" $
                (kodiakize $ BinaryOp DivOp (Rat 1) (Rat 2))
                @?=
                (return $ K.Div (K.Cnst 1) (K.Cnst 2))
            ]
        ,testGroup "Unary operators"
            [testCase "Abs" $
                (kodiakize $ UnaryOp AbsOp (Rat (-1)))
                @?=
                (return $ K.Abs (K.Cnst (-1)))
            ,testCase "Sqrt" $
                (kodiakize $ UnaryOp SqrtOp (Rat 2))
                @?=
                (return $ K.Sqrt (K.Cnst 2))
            ,testCase "Neg" $
                (kodiakize $ UnaryOp NegOp (Rat 2))
                @?=
                (return $ K.Neg (K.Cnst 2))
            ,testCase "Ln" $
                (kodiakize $ UnaryOp LnOp (Rat 2))
                @?=
                (return $ K.Ln (K.Cnst 2))
            ,testCase "Expo" $
                (kodiakize $ UnaryOp ExpoOp (Rat 2))
                @?=
                (return $ K.Exp (K.Cnst 2))
            ,testCase "Sin" $
                (kodiakize $ UnaryOp SinOp (Rat 2))
                @?=
                (return $ K.Sin (K.Cnst 2))
            ,testCase "Cos" $
                (kodiakize $ UnaryOp CosOp (Rat 2))
                @?=
                (return $ K.Cos (K.Cnst 2))
            ,testCase "ATan" $
                (kodiakize $ UnaryOp AtanOp (Rat 2))
                @?=
                (return $ K.ATan (K.Cnst 2))
            ,testCase "Floor" $
                (kodiakize $ UnaryOp FloorOp (Rat 2))
                @?=
                (return $ K.Floor (K.Cnst 2))
            ,testGroup "HalfUlp" $
                [testCase "Single precision" $
                    (kodiakize $ HalfUlp (Rat 3) FPSingle)
                    @?=
                    (return $ K.Div (K.Ulp FPSingle (K.Cnst 3)) (K.Cnst 2))
                ,testCase "Double precision" $
                    (kodiakize $ HalfUlp (Rat 3) FPDouble)
                    @?=
                    (return $ K.Div (K.Ulp FPDouble (K.Cnst 3)) (K.Cnst 2))
                ]
            ]
        ,testGroup "N-ary operators"
            [testGroup "MaxErr"
                [testCase "0 expressions fails" $
                    (kodiakize $ MaxErr [])
                    @?=
                    (throwError $ EmptyNAryExpr $ MaxErr [])
                ,testCase "1 expression" $
                    (kodiakize $ MaxErr [Rat 1] )
                    @?=
                    (return $ K.Cnst 1)
                ,testCase "N expressions" $
                    (kodiakize $ MaxErr [Rat 1,Rat 2] )
                    @?=
                    (return $ K.Max [K.Cnst 1,K.Cnst 2])
                ]
            ]
        ,testGroup "Integer operators"
            [testCase "IMod" $
                (kodiakize $ BinaryOp ModOp (Int 4) (Int 3))
                @?=
                (return $ K.Cnst 1)
            ]
        ]
    ,testGroup "FPDouble"
        [testGroup "Constants"
            [testCase "FInt" $
                (kodiakize $ FInt 1)
                @?=
                (return $ K.Cnst 1)
            ,testGroup "FCnst"
                [testCase "FPDouble" $
                    (kodiakize $ FCnst FPDouble 2)
                    @?=
                    (return $ K.Cnst 2)
                ,testCase "FPSingle" $
                    (kodiakize $ FCnst FPSingle 2)
                    @?=
                    (return $ K.Cnst 2)
                ,testCase "Real constant fails" $
                    let e = FCnst Real 2 in
                    (kodiakize e)
                    @?=
                    (throwError $ NonSupportedExpr e)
                ]
            ]
        ,testGroup "Variables"
            [testGroup "FVar"
                [testCase "Int" $
                    (kodiakize $ FVar TInt x)
                    @?=
                    (return $ K.Var x)
                ,testCase "FPDouble" $
                    (kodiakize $ FVar FPDouble x)
                    @?=
                    (return $ K.Var x)
                ,testCase "FPSingle variable fails" $
                    (kodiakize $ FVar FPSingle x)
                    @?=
                    (throwError $ NonSupportedExpr $ FVar FPSingle x)
                ,testCase "Real variable fails" $
                    (kodiakize $ FVar Real x)
                    @?=
                    (throwError $ NonSupportedExpr $ FVar Real x)
                ]
            ]
        ,testGroup "Binary operators"
            [testCase "FAdd" $
                (kodiakize $ BinaryFPOp AddOp FPDouble (FCnst FPDouble 2) (FCnst FPDouble 1))
                @?=
                (return $ K.Add (K.Cnst 2) (K.Cnst 1))
            ,testCase "FSub" $
                (kodiakize $ BinaryFPOp SubOp FPDouble (FCnst FPDouble 1) (FCnst FPDouble 2))
                @?=
                (return $ K.Sub (K.Cnst 1) (K.Cnst 2))
            ,testCase "FMul" $
                (kodiakize $ BinaryFPOp MulOp FPDouble (FCnst FPDouble 1) (FCnst FPDouble 2))
                @?=
                (return $ K.Mul (K.Cnst 1) (K.Cnst 2))
            ,testCase "FDiv" $
                (kodiakize $ BinaryFPOp DivOp FPDouble (FCnst FPDouble 1) (FCnst FPDouble 2))
                @?=
                (return $ K.Div (K.Cnst 1) (K.Cnst 2))
            ]
        ,testGroup "Unary operators"
            [testCase "FAbs" $
                (kodiakize $ UnaryFPOp AbsOp FPDouble (FCnst FPDouble (-1)))
                @?=
                (return $ K.Abs (K.Cnst (-1)))
            ,testCase "FSqrt" $
                (kodiakize $ UnaryFPOp SqrtOp FPDouble (FCnst FPDouble 2))
                @?=
                (return $ K.Sqrt (K.Cnst 2))
            ,testCase "FNeg" $
                (kodiakize $ UnaryFPOp NegOp FPDouble (FCnst FPDouble 2))
                @?=
                (return $ K.Neg (K.Cnst 2))
            ,testCase "FLn" $
                (kodiakize $ UnaryFPOp LnOp FPDouble (FCnst FPDouble 2))
                @?=
                (return $ K.Ln (K.Cnst 2))
            ,testCase "FExpo" $
                (kodiakize $ UnaryFPOp ExpoOp FPDouble (FCnst FPDouble 2))
                @?=
                (return $ K.Exp (K.Cnst 2))
            ,testCase "FSin" $
                (kodiakize $ UnaryFPOp SinOp FPDouble (FCnst FPDouble 2))
                @?=
                (return $ K.Sin (K.Cnst 2))
            ,testCase "FCos" $
                (kodiakize $ UnaryFPOp CosOp FPDouble (FCnst FPDouble 2))
                @?=
                (return $ K.Cos (K.Cnst 2))
            ,testCase "FAtan" $
                (kodiakize $ UnaryFPOp AtanOp FPDouble (FCnst FPDouble 2))
                @?=
                (return $ K.ATan (K.Cnst 2))
            ,testCase "FFloor" $
                (kodiakize $ UnaryFPOp FloorOp FPDouble (FCnst FPDouble 2))
                @?=
                (return $ K.Floor (K.Cnst 2))
            ,testGroup "RtoD"
                [testCase "Int" $
                    (kodiakize $ ToFloat FPDouble $ Int 0)
                    @?=
                    (return $ K.Cnst 0)
                ,testCase "Rat" $
                    (kodiakize $ ToFloat FPDouble $ Rat 1)
                    @?=
                    (return $ K.Cnst 1)
                ]
            ]
        ,testGroup "N-ary operators"
            [testGroup "FMaxErr"
                [testCase "0 expressions fails" $
                    (kodiakize $ FMax [])
                    @?=
                    (throwError $ EmptyNAryExpr $ FMax [])
                ,testCase "1 expression" $
                    (kodiakize $ FMax [FCnst FPDouble 1] )
                    @?=
                    (return $ K.Cnst 1)
                ,testCase "N expressions" $
                    (kodiakize $ FMax [FCnst FPDouble 1,FCnst FPDouble 2] )
                    @?=
                    (return $ K.Max [K.Cnst 1,K.Cnst 2])
                ]
            ]
        ,testGroup "Integer operators"
            [testCase "FISub" $
                (kodiakize $ BinaryFPOp SubOp TInt (FInt 4) (FInt 3))
                @?=
                (return $ K.Cnst 1)
            ,testCase "FIMod" $
                (kodiakize $ BinaryFPOp ModOp TInt (FInt 1) (FInt 4))
                @?=
                (return $ K.Cnst 1)
            ]
        ]
    ]

testKodiakBExprGenerator :: TestTree
testKodiakBExprGenerator = testGroup "Kodiak BExpr Generator"
    [testGroup "Real"
        [testGroup "Constants"
            [testCase "BTrue" $
                (kodiakize $ BTrue)
                @?=
                (return $ K.True)
            ,testCase "BFalse" $
                (kodiakize $ BFalse)
                @?=
                (return $ K.False)
            ]
        ,testGroup "Boolean operators"
            [testCase "Not" $
                (kodiakize $ Not BTrue)
                @?=
                (return $ K.Not K.True)
            ,testCase "And" $
                (kodiakize $ And BFalse BTrue)
                @?=
                (return $ K.And K.False K.True)
            ,testCase "Or" $
                (kodiakize $ Or BTrue BFalse)
                @?=
                (return $ K.Or K.True K.False)
            ]
        ,testGroup "Relational operators"
            [testCase "Eq" $
                (kodiakize $ Rel Eq (Rat 1) (Rat 2))
                @?=
                (return $ K.Eq (K.Cnst 1) (K.Cnst 2))
            ,testCase "Neq" $
                (kodiakize $ Rel Neq (Rat 1) (Rat 2))
                @?=
                (return $ K.NEq (K.Cnst 1) (K.Cnst 2))
            ,testCase "Lt" $
                (kodiakize $ Rel Lt (Rat 1) (Rat 2))
                @?=
                (return $ K.LT (K.Cnst 1) (K.Cnst 2))
            ,testCase "LtE" $
                (kodiakize $ Rel LtE (Rat 1) (Rat 2))
                @?=
                (return $ K.LE (K.Cnst 1) (K.Cnst 2))
            ,testCase "Gt" $
                (kodiakize $ Rel Gt (Rat 1) (Rat 2))
                @?=
                (return $ K.GT (K.Cnst 1) (K.Cnst 2))
            ,testCase "GtE" $
                (kodiakize $ Rel GtE (Rat 1) (Rat 2))
                @?=
                (return $ K.GE (K.Cnst 1) (K.Cnst 2))
            ]
        ]
    ,testGroup "FP"
        [testGroup "Constants"
            [testCase "BTrue" $
                (kodiakize $ FBTrue)
                @?=
                (return $ K.True)
            ,testCase "BFalse" $
                (kodiakize $ FBFalse)
                @?=
                (return $ K.False)
            ]
        ,testGroup "Boolean operators"
            [testCase "FNot" $
                (kodiakize $ FNot FBTrue)
                @?=
                (return $ K.Not K.True)
            ,testCase "FAnd" $
                (kodiakize $ FAnd FBFalse FBTrue)
                @?=
                (return $ K.And K.False K.True)
            ,testCase "FOr" $
                (kodiakize $ FOr FBTrue FBFalse)
                @?=
                (return $ K.Or K.True K.False)
            ]
        ,testGroup "Relational operators"
            [testCase "FEq" $
                (kodiakize $ FRel Eq (FCnst FPDouble 1) (FCnst FPDouble 2))
                @?=
                (return $ K.Eq (K.Cnst 1) (K.Cnst 2))
            ,testCase "FNeq" $
                (kodiakize $ FRel Neq (FCnst FPDouble 1) (FCnst FPDouble 2))
                @?=
                (return $ K.NEq (K.Cnst 1) (K.Cnst 2))
            ,testCase "FLt" $
                (kodiakize $ FRel Lt (FCnst FPDouble 1) (FCnst FPDouble 2))
                @?=
                (return $ K.LT (K.Cnst 1) (K.Cnst 2))
            ,testCase "FLtE" $
                (kodiakize $ FRel LtE (FCnst FPDouble 1) (FCnst FPDouble 2))
                @?=
                (return $ K.LE (K.Cnst 1) (K.Cnst 2))
            ,testCase "FGt" $
                (kodiakize $ FRel Gt (FCnst FPDouble 1) (FCnst FPDouble 2))
                @?=
                (return $ K.GT (K.Cnst 1) (K.Cnst 2))
            ,testCase "FGtE" $
                (kodiakize $ FRel GtE (FCnst FPDouble 1) (FCnst FPDouble 2))
                @?=
                (return $ K.GE (K.Cnst 1) (K.Cnst 2))
            ]
        ]
    ]

testIntegerExpressionSimplifier :: TestTree
testIntegerExpressionSimplifier = testGroup "Integer (Sub-)Expression Simplifier" $
    let evalRI' x = evalRI x :: Except (ToKodiakError AExpr) Integer
        evalFI' x = evalFI x :: Except (ToKodiakError FAExpr) Integer in
    [testGroup "Real embedded"
        [testCase "Int" $
            (evalRI' $ Int 2)
            @?=
            (return 2)
        ,testCase "IMod" $
            (evalRI' $ BinaryOp ModOp (Int 2) (Int 3))
            @?=
            (return 2)
        ,testCase "Invalid" $
            (evalRI' $ BinaryOp ModOp (Rat 2) (Int 3))
            @?=
            (throwError $ InvalidIntegerExpr (Rat 2))
        ]
    ,testGroup "FP embedded"
        [testCase "FInt" $
            (evalFI' $ FInt 2)
            @?=
            (return 2)
        ,testCase "FIMod" $
            (evalFI' $ BinaryFPOp ModOp TInt (FInt 2) (FInt 3))
            @?=
            (return 2)
        ,testGroup "RtoD nested of Real"
            [testCase "Valid" $
                (evalFI' $ ToFloat FPDouble $ BinaryOp ModOp (Int 2) (Int 3))
                @?=
                (return 2)
            ,testCase "Invalid" $
                (evalFI' $ ToFloat FPDouble $ BinaryOp ModOp (Rat 2) (Int 3))
                @?=
                (throwError $ AExprError $ InvalidIntegerExpr $ Rat 2)
            ]
        ,testCase "Invalid" $
            let cnst = FCnst FPDouble 2 in
            (evalFI' $ BinaryFPOp ModOp TInt cnst (FInt 3))
            @?=
            (throwError $ InvalidIntegerExpr cnst)
        ]
    ]