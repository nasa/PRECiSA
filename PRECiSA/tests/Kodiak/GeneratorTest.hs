{-# LANGUAGE MultiParamTypeClasses #-}

module Kodiak.GeneratorTest where

import Control.Monad.Except
import Test.Tasty
import Test.Tasty.HUnit

import FPrec
import AbsPVSLang
import qualified Kodiak.Expression as K
import Kodiak.Generator

testKodiakGenerator :: TestTree
testKodiakGenerator = testGroup "Kodiak Expression Generators"
    [testKodiakAExprGenerator
    ,testKodiakBExprGenerator
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
                (kodiakize $ Add (Rat 2) (Rat 1))
                @?=
                (return $ K.Add (K.Cnst 2) (K.Cnst 1))
            ,testCase "Sub" $
                (kodiakize $ Sub (Rat 1) (Rat 2))
                @?=
                (return $ K.Sub (K.Cnst 1) (K.Cnst 2))
            ,testCase "Mul" $
                (kodiakize $ Mul (Rat 1) (Rat 2))
                @?=
                (return $ K.Mul (K.Cnst 1) (K.Cnst 2))
            ,testCase "Div" $
                (kodiakize $ Div (Rat 1) (Rat 2))
                @?=
                (return $ K.Div (K.Cnst 1) (K.Cnst 2))
            ]
        ,testGroup "Unary operators"
            [testCase "Abs" $
                (kodiakize $ Abs (Rat (-1)))
                @?=
                (return $ K.Abs (K.Cnst (-1)))
            ,testCase "Sqrt" $
                (kodiakize $ Sqrt (Rat 2))
                @?=
                (return $ K.Sqrt (K.Cnst 2))
            ,testCase "Neg" $
                (kodiakize $ Neg (Rat 2))
                @?=
                (return $ K.Neg (K.Cnst 2))
            ,testCase "Ln" $
                (kodiakize $ Ln (Rat 2))
                @?=
                (return $ K.Ln (K.Cnst 2))
            ,testCase "Expo" $
                (kodiakize $ Expo (Rat 2))
                @?=
                (return $ K.Exp (K.Cnst 2))
            ,testCase "Sin" $
                (kodiakize $ Sin (Rat 2))
                @?=
                (return $ K.Sin (K.Cnst 2))
            ,testCase "Cos" $
                (kodiakize $ Cos (Rat 2))
                @?=
                (return $ K.Cos (K.Cnst 2))
            ,testCase "ATan" $
                (kodiakize $ ATan (Rat 2))
                @?=
                (return $ K.ATan (K.Cnst 2))
            ,testCase "Floor" $
                (kodiakize $ Floor (Rat 2))
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
        -- ,testGroup "Embedded expressions"
        --    [testCase "AE" $
        --        (kodiakize $ AE (Rat 1))
        --        @?=
        --        (return $ K.Cnst 1)
        --    ,testCase "EE" $
        --        (kodiakize $ EE (Rat 1))
        --        @?=
        --        (return $ K.Cnst 1)
        --    ]
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
                (kodiakize $ FAdd FPDouble (FCnst FPDouble 2) (FCnst FPDouble 1))
                @?=
                (return $ K.Add (K.Cnst 2) (K.Cnst 1))
            ,testCase "FSub" $
                (kodiakize $ FSub FPDouble (FCnst FPDouble 1) (FCnst FPDouble 2))
                @?=
                (return $ K.Sub (K.Cnst 1) (K.Cnst 2))
            ,testCase "FMul" $
                (kodiakize $ FMul FPDouble (FCnst FPDouble 1) (FCnst FPDouble 2))
                @?=
                (return $ K.Mul (K.Cnst 1) (K.Cnst 2))
            ,testCase "FDiv" $
                (kodiakize $ FDiv FPDouble (FCnst FPDouble 1) (FCnst FPDouble 2))
                @?=
                (return $ K.Div (K.Cnst 1) (K.Cnst 2))
            ]
        ,testGroup "Unary operators"
            [testCase "FAbs" $
                (kodiakize $ FAbs FPDouble (FCnst FPDouble (-1)))
                @?=
                (return $ K.Abs (K.Cnst (-1)))
            ,testCase "FSqrt" $
                (kodiakize $ FSqrt FPDouble (FCnst FPDouble 2))
                @?=
                (return $ K.Sqrt (K.Cnst 2))
            ,testCase "FNeg" $
                (kodiakize $ FNeg FPDouble (FCnst FPDouble 2))
                @?=
                (return $ K.Neg (K.Cnst 2))
            ,testCase "FLn" $
                (kodiakize $ FLn FPDouble (FCnst FPDouble 2))
                @?=
                (return $ K.Ln (K.Cnst 2))
            ,testCase "FExpo" $
                (kodiakize $ FExpo FPDouble (FCnst FPDouble 2))
                @?=
                (return $ K.Exp (K.Cnst 2))
            ,testCase "FSin" $
                (kodiakize $ FSin FPDouble (FCnst FPDouble 2))
                @?=
                (return $ K.Sin (K.Cnst 2))
            ,testCase "FCos" $
                (kodiakize $ FCos FPDouble (FCnst FPDouble 2))
                @?=
                (return $ K.Cos (K.Cnst 2))
            ,testCase "FAtan" $
                (kodiakize $ FAtan FPDouble (FCnst FPDouble 2))
                @?=
                (return $ K.ATan (K.Cnst 2))
            ,testCase "FFloor" $
                (kodiakize $ FFloor FPDouble (FCnst FPDouble 2))
                @?=
                (return $ K.Floor (K.Cnst 2))
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
        ]
        -- ,testGroup "Embedded expressions"
        --     [testCase "AE" $
        --         (kodiakize $ AE (Rat 1))
        --         @?=
        --         (return $ K.Cnst 1)
        --     ,testCase "EE" $
        --         (kodiakize $ EE (Rat 1))
        --         @?=
        --         (return $ K.Cnst 1)
        --     ]
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
                (kodiakize $ Eq (Rat 1) (Rat 2))
                @?=
                (return $ K.Eq (K.Cnst 1) (K.Cnst 2))
            ,testCase "Neq" $
                (kodiakize $ Neq (Rat 1) (Rat 2))
                @?=
                (return $ K.NEq (K.Cnst 1) (K.Cnst 2))
            ,testCase "Lt" $
                (kodiakize $ Lt (Rat 1) (Rat 2))
                @?=
                (return $ K.LT (K.Cnst 1) (K.Cnst 2))
            ,testCase "LtE" $
                (kodiakize $ LtE (Rat 1) (Rat 2))
                @?=
                (return $ K.LE (K.Cnst 1) (K.Cnst 2))
            ,testCase "Gt" $
                (kodiakize $ Gt (Rat 1) (Rat 2))
                @?=
                (return $ K.GT (K.Cnst 1) (K.Cnst 2))
            ,testCase "GtE" $
                (kodiakize $ GtE (Rat 1) (Rat 2))
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
                (kodiakize $ FEq (FCnst FPDouble 1) (FCnst FPDouble 2))
                @?=
                (return $ K.Eq (K.Cnst 1) (K.Cnst 2))
            ,testCase "FNeq" $
                (kodiakize $ FNeq (FCnst FPDouble 1) (FCnst FPDouble 2))
                @?=
                (return $ K.NEq (K.Cnst 1) (K.Cnst 2))
            ,testCase "FLt" $
                (kodiakize $ FLt (FCnst FPDouble 1) (FCnst FPDouble 2))
                @?=
                (return $ K.LT (K.Cnst 1) (K.Cnst 2))
            ,testCase "FLtE" $
                (kodiakize $ FLtE (FCnst FPDouble 1) (FCnst FPDouble 2))
                @?=
                (return $ K.LE (K.Cnst 1) (K.Cnst 2))
            ,testCase "FGt" $
                (kodiakize $ FGt (FCnst FPDouble 1) (FCnst FPDouble 2))
                @?=
                (return $ K.GT (K.Cnst 1) (K.Cnst 2))
            ,testCase "FGtE" $
                (kodiakize $ FGtE (FCnst FPDouble 1) (FCnst FPDouble 2))
                @?=
                (return $ K.GE (K.Cnst 1) (K.Cnst 2))
            ]
        ]
    ]