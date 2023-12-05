module MapFPCoreLangASTTest where

import qualified Operators as Op
import Test.Tasty
import Test.Tasty.HUnit
import AbsFPCoreLang
import AbsPVSLang
import MapFPCoreLangAST
import PVSTypes

testMapFPCoreLangAST = testGroup "MapFPCoreLangAST"
  [ fpcore2FAExpr__tests
  ]

fpcore2FAExpr__tests = testGroup "fpcore2FAExpr"
  [ fpcore2FAExpr_FVar
  , fpcore2FAExpr_Ite
  , fpcore2FAExpr_Let
  , fpcore2FAExpr_LetStar
  , fpcore2FAExpr_Lte
  ]

fpcore2FAExpr_FVar = testCase "FVar" $
  fpcore2FAExpr [] [] (ExSym (Symbol "test"))
  @?=
  FVar FPDouble "test"

fpcore2FAExpr_Ite = testCase "Ite" $
  fpcore2FAExpr [] [] (ExIf (ExOp LTOp (ExNum (NRat (Rational "5/1"))) [(ExNum (NRat (Rational "8/1")))]) (ExNum (NRat (Rational "4/1"))) (ExNum (NRat (Rational "2/1"))))
  @?=
  Ite (FRel Op.Lt (FCnst FPDouble 5) (FCnst FPDouble 8)) (FCnst FPDouble 4) (FCnst FPDouble 2)

fpcore2FAExpr_Let = testCase "Let" $
  fpcore2FAExpr [] [] (ExLet [(SymExPair (Symbol "vara") (ExNum (NRat (Rational "5/1")))), (SymExPair (Symbol "varb") (ExNum (NRat (Rational "3/1"))))] (ExNum (NRat (Rational "2/1"))))
  @?=
  Let [("varb", FPDouble, (FCnst FPDouble 3)), ("vara", FPDouble, (FCnst FPDouble 5))] (FCnst FPDouble 2)

fpcore2FAExpr_LetStar = testCase "LetStar" $
  fpcore2FAExpr [] [] (ExLetStar [(SymExPair (Symbol "vara") (ExNum (NRat (Rational "5/1")))), (SymExPair (Symbol "varb") (ExNum (NRat (Rational "3/1"))))] (ExNum (NRat (Rational "2/1"))))
  @?=
  Let [("vara", FPDouble, (FCnst FPDouble 5))] (Let [("varb", FPDouble, (FCnst FPDouble 3))] (FCnst FPDouble 2))

fpcore2FAExpr_Lte = testCase "Lte" $
  fpcoreOpCall2BExpr [] [] LTEOp [(ExNum (NRat (Rational "0/1"))), (ExSym (Symbol "x")), (ExNum (NRat (Rational "100/1")))]
  @?=
  AbsPVSLang.FAnd (AbsPVSLang.FRel Op.LtE (FCnst FPDouble 0) (FVar FPDouble "x")) (AbsPVSLang.FRel Op.LtE (FVar FPDouble "x") (FCnst FPDouble 100))
