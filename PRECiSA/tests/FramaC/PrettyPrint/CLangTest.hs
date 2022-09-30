module FramaC.PrettyPrint.CLangTest where

import Common.TypesUtils
import FramaC.CLang
import FramaC.Types
import PPExt (prettyDoc, render)
import Test.Tasty
import Test.Tasty.HUnit

test =
  testGroup
    "PrettyPrint of CLang AST"
    [ testGroup "Stm" $
        [ testGroup
            "VarAssign"
            $ let cases =
                    [ ( "single precision integer constant",
                        VarAssign (Float SinglePrec) "res" $ FPCnst SinglePrec 1,
                        "res_single = 1.0;"
                      ),
                      ( "double precision integer constant",
                        VarAssign (Float DoublePrec) "res" $ FPCnst SinglePrec 1,
                        "res_double = 1.0;"
                      ),
                      ( "double precision rational non-fp representable constant",
                        VarAssign (Float DoublePrec) "res" $ FPCnst SinglePrec (0.1 :: Rational),
                        "res_double = 0.1;"
                      ),
                      ( "double precision rational fp representable constant",
                        VarAssign (Float DoublePrec) "res" $ FPCnst SinglePrec (0.5 :: Rational),
                        "res_double = 0.5;"
                      )
                    ]
               in [testCase m (i `isPrettiedAs` o) | (m, i, o) <- cases]
        ],
      testGroup "AExpr" $
        [ testGroup "Var" $
            let cases =
                  [ ("double precision", Var (Float DoublePrec) "x", "x_double"),
                    ("single precision", Var (Float SinglePrec) "x", "x_single"),
                    ("integer", Var Int "y", "y"),
                    ("bool", Var Boolean "z", "z"),
                    ("maybe single", Var (MaybeStruct $ Float SinglePrec) "x", "x_single"),
                    ("maybe double", Var (MaybeStruct $ Float DoublePrec) "x", "x_double"),
                    ("maybe integer", Var (MaybeStruct Int) "x", "x"),
                    ("maybe bool", Var (MaybeStruct Boolean) "b", "b")
                  ]
             in [testCase m (i `isPrettiedAs` o) | (m, i, o) <- cases]
        ]
    ]

class IsPrettiedAs a where
  isPrettiedAs :: a -> String -> Assertion

instance IsPrettiedAs Stm where
  isPrettiedAs stm = flip (assertEqual "") prettyRenderRes
    where
      prettyRenderRes = render $ prettyDoc stm

instance IsPrettiedAs AExpr where
  isPrettiedAs stm = flip (assertEqual "") prettyRenderRes
    where
      prettyRenderRes = render $ prettyDoc stm