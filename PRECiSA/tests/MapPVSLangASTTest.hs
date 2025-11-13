module MapPVSLangASTTest where

import qualified AbsPVSLang as PVS
import qualified AbsRawPVSLang as Raw
import qualified MapPVSLangAST as MAP
import qualified Operators as OP
import Test.Tasty
import Test.Tasty.HUnit

testMapPVSLangAST =
  testGroup
    "MapPVSLangAST"
    [ test__raw2FAExpr,
      test__raw2FBExprStm,
      test__raw2CollExpr
    ]

test__raw2FAExpr =
  testGroup
    "raw2FAExpr"
    [ test
        ( "let",
          Raw.Let [Raw.LetElem (Raw.Id "y") (Raw.Int 1), Raw.LetElem (Raw.Id "z") (Raw.ExprAdd (Raw.ExprId (Raw.Id "y")) (Raw.Int 2))] (Raw.ExprAdd (Raw.ExprAdd (Raw.ExprId (Raw.Id "z")) (Raw.ExprId (Raw.Id "y"))) (Raw.ExprId (Raw.Id "x"))),
          PVS.Let [("y", PVS.FPDouble, PVS.FInt 1), ("z", PVS.FPDouble, PVS.BinaryFPOp OP.AddOp PVS.FPDouble (PVS.FVar PVS.FPDouble "y") (PVS.FInt 2))] (PVS.BinaryFPOp OP.AddOp PVS.FPDouble (PVS.BinaryFPOp OP.AddOp PVS.FPDouble (PVS.FVar PVS.FPDouble "z") (PVS.FVar PVS.FPDouble "y")) (PVS.FVar PVS.FPDouble "x")),
          ([("x", PVS.FPDouble)], [], [])
        )
    , test
        ( "let",
          Raw.ListIf (Raw.Gt (Raw.ExprId (Raw.Id "x")) (Raw.Rat 1.0)) (Raw.Rat 1.0) [Raw.ElsIf (Raw.Lt (Raw.ExprId (Raw.Id "x")) (Raw.ExprNeg (Raw.Rat 1.0))) (Raw.ExprNeg (Raw.Rat 1.0))] (Raw.Rat 0.0),
          PVS.ListIte [(PVS.FRel OP.Gt (PVS.FVar PVS.FPDouble "x") (PVS.ToFloat PVS.FPDouble (PVS.Rat (fromInteger 1))),PVS.ToFloat PVS.FPDouble (PVS.Rat (fromInteger 1))),(PVS.FRel OP.Lt (PVS.FVar PVS.FPDouble "x") (PVS.ToFloat PVS.FPDouble (PVS.Rat (fromInteger (-1)))),PVS.ToFloat PVS.FPDouble (PVS.Rat (fromInteger (-1))))] (PVS.ToFloat PVS.FPDouble (PVS.Rat (fromInteger 0))),
          ([("x", PVS.FPDouble)], [], [])
        )
    , test
        ( "tuple",
          Raw.TupleIndex (Raw.Id "x") 1,
          PVS.FTupleElem PVS.FPDouble "x" 1,
          ([("x",PVS.Tuple [PVS.FPDouble,PVS.FPDouble])], [], [])
        )
    , test
        ( "tuple function return",
          Raw.TupleFunIndex (Raw.Id "f") [Raw.ExprId (Raw.Id "x")] 1,
          PVS.FEFun False "f" (PVS.ResTupleIndex 1) PVS.FPDouble [PVS.FVar (PVS.Tuple [PVS.FPDouble,PVS.FPDouble]) "x"],
          ([("x",PVS.Tuple [PVS.FPDouble,PVS.FPDouble])], [("f",PVS.Tuple [PVS.FPDouble,PVS.FPDouble]),("example",PVS.FPDouble)], [])
        )
    , test
        ( "record",
          Raw.RecordField (Raw.Id "x") (Raw.Id "b"),
          PVS.FRecordElem PVS.FPDouble "x" "b",
          ([("x",PVS.Record [("a",PVS.FPDouble),("b",PVS.FPDouble)])], [], [])
        )
    , test
        ( "record function return",
          Raw.RecordFunField (Raw.Id "f") [Raw.ExprId (Raw.Id "x")] (Raw.Id "b"),
          PVS.FEFun False "f" (PVS.ResRecordField "b") PVS.FPDouble [PVS.FVar (PVS.Record [("a",PVS.FPDouble),("b",PVS.FPDouble)]) "x"],
          ([("x",PVS.Record [("a",PVS.FPDouble),("b",PVS.FPDouble)])], [("f",PVS.Record [("a",PVS.FPDouble),("b",PVS.FPDouble)]),("example",PVS.FPDouble)], [])
        )
    ]
  where
    test (name, e, e', (vEnv, fEnv, tyEnv)) = testCase name $ MAP.raw2FAExpr tyEnv vEnv fEnv e @?= e'

test__raw2FBExprStm =
  testGroup
    "raw2FBExprStm"
    [ testCase "Let" $
        MAP.raw2FBExprStm [] [("x",PVS.FPDouble)] [] (Raw.Let [Raw.LetElem (Raw.Id "y") (Raw.ExprAdd (Raw.ExprId (Raw.Id "x")) (Raw.Int 2)), Raw.LetElem (Raw.Id "z") (Raw.ExprSub (Raw.ExprId (Raw.Id "y")) (Raw.Int 1))] (Raw.Gt (Raw.ExprId (Raw.Id "z")) (Raw.Int 3)))
          @?= PVS.BLet [("y", PVS.FPDouble, PVS.BinaryFPOp OP.AddOp PVS.FPDouble (PVS.FVar PVS.FPDouble "x") (PVS.FInt 2)), ("z", PVS.FPDouble, PVS.BinaryFPOp OP.SubOp PVS.FPDouble (PVS.FVar PVS.FPDouble "y") (PVS.FInt 1))] (PVS.BExpr (PVS.FRel OP.Gt (PVS.FVar PVS.FPDouble "z") (PVS.FInt 3)))
    , testCase "If" $
        MAP.raw2FBExprStm [] [("x",PVS.FPDouble)] []
          (Raw.If (Raw.Gt (Raw.ExprId (Raw.Id "x")) (Raw.Rat 1.0)) Raw.BTrue Raw.BFalse)
          @?=
            (PVS.BIte (PVS.FRel OP.Gt (PVS.FVar PVS.FPDouble "x") (PVS.ToFloat PVS.FPDouble (PVS.Rat (fromInteger 1)))) (PVS.BExpr PVS.FBTrue) (PVS.BExpr PVS.FBFalse))
    , testCase "ListIf" $
        MAP.raw2FBExprStm [] [("x",PVS.FPDouble)] []
          (Raw.ListIf (Raw.Gt (Raw.ExprId (Raw.Id "x")) (Raw.Rat 1.0)) Raw.BTrue [Raw.ElsIf (Raw.Lt (Raw.ExprId (Raw.Id "x")) (Raw.ExprNeg (Raw.Rat 1.0))) Raw.BTrue] Raw.BFalse)
          @?=
            (PVS.BListIte [(PVS.FRel OP.Gt (PVS.FVar PVS.FPDouble "x") (PVS.ToFloat PVS.FPDouble (PVS.Rat (fromInteger 1))),PVS.BExpr PVS.FBTrue),(PVS.FRel OP.Lt (PVS.FVar PVS.FPDouble "x") (PVS.ToFloat PVS.FPDouble (PVS.Rat (fromInteger (-1)))),PVS.BExpr PVS.FBTrue)] (PVS.BExpr PVS.FBFalse))
    ]

test__raw2CollExpr =
  testGroup
    "raw2CollExpr"
    [ testCase "With" $
      MAP.raw2CollExpr [] [("x",PVS.Array [PVS.TInt] PVS.FPDouble)] []
        (Raw.With (Raw.ExprId (Raw.Id "x")) (Raw.Int 1) (Raw.Rat 1.0))
        @?=
          PVS.ArrayUpdate (PVS.CollVar (PVS.Array [PVS.TInt] PVS.FPDouble) "x") (PVS.FInt 1) (PVS.ToFloat PVS.FPDouble (PVS.Rat 1))
    , testCase "Let" $
      MAP.raw2CollExpr [] [("x",PVS.Tuple [PVS.FPDouble,PVS.FPDouble])] []
        (Raw.Let [Raw.LetElem (Raw.Id "y") (Raw.ExprId (Raw.Id "x")),Raw.LetElem (Raw.Id "z") (Raw.ExprId (Raw.Id "y"))] (Raw.ExprId (Raw.Id "x")))
        @?=
          PVS.CLet [("y",PVS.FPDouble,PVS.FVar (PVS.Tuple [PVS.FPDouble,PVS.FPDouble]) "x"),("z",PVS.FPDouble,PVS.FVar PVS.FPDouble "y")] (PVS.CollVar (PVS.Tuple [PVS.FPDouble,PVS.FPDouble]) "x")
    , testCase "If" $
      MAP.raw2CollExpr [] [("x",PVS.Tuple [PVS.FPDouble,PVS.FPDouble]),("y",PVS.Tuple [PVS.FPDouble,PVS.FPDouble])] []
        (Raw.If Raw.BTrue (Raw.ExprId (Raw.Id "x")) (Raw.ExprId (Raw.Id "x")))
        @?=
          PVS.CIte PVS.FBTrue (PVS.CollVar (PVS.Tuple [PVS.FPDouble,PVS.FPDouble]) "x") (PVS.CollVar (PVS.Tuple [PVS.FPDouble,PVS.FPDouble]) "x")
    , testCase "ListIf" $
      MAP.raw2CollExpr [] [("x",PVS.Tuple [PVS.FPDouble,PVS.FPDouble]),("y",PVS.Tuple [PVS.FPDouble,PVS.FPDouble])] []
        (Raw.ListIf Raw.BFalse (Raw.ExprId (Raw.Id "x")) [Raw.ElsIf Raw.BTrue (Raw.ExprId (Raw.Id "y"))] (Raw.ExprId (Raw.Id "x")))
        @?=
          PVS.CListIte [(PVS.FBFalse,PVS.CollVar (PVS.Tuple [PVS.FPDouble,PVS.FPDouble]) "x"),(PVS.FBTrue,PVS.CollVar (PVS.Tuple [PVS.FPDouble,PVS.FPDouble]) "y")] (PVS.CollVar (PVS.Tuple [PVS.FPDouble,PVS.FPDouble]) "x")
    , testCase "Record" $
      MAP.raw2CollExpr [] [("x",PVS.FPDouble)] []
        (Raw.RecordExpr [Raw.RecordElem (Raw.Id "a") (Raw.Rat 1.0),Raw.RecordElem (Raw.Id "b") (Raw.Rat 0.0)])
        @?=
          PVS.RecordExpr [("a",Left (PVS.ToFloat PVS.FPDouble (PVS.Rat 1))),("b",Left (PVS.ToFloat PVS.FPDouble (PVS.Rat 0)))]
    , testCase "Tuple" $
      MAP.raw2CollExpr [] [("x",PVS.FPDouble)] []
        (Raw.TupleExpr [Raw.ExprId (Raw.Id "x"),Raw.ExprId (Raw.Id "x")])
        @?=
          PVS.TupleExpr [Left (PVS.FVar PVS.FPDouble "x"),Left (PVS.FVar PVS.FPDouble "x")]
    , testCase "Id" $
      MAP.raw2CollExpr [] [("x",PVS.Tuple [PVS.FPDouble,PVS.FPDouble])] []
        (Raw.ExprId (Raw.Id "x"))
        @?=
          PVS.CollVar (PVS.Tuple [PVS.FPDouble,PVS.FPDouble]) "x"
    , testCase "FCall" $
      MAP.raw2CollExpr [] [("x",PVS.FPDouble)] [("f",PVS.Tuple [PVS.FPDouble,PVS.FPDouble]),("example",PVS.Tuple [PVS.FPDouble,PVS.FPDouble])]
        (Raw.Call (Raw.Id "f") [Raw.ExprId (Raw.Id "x")])
        @?=
          PVS.CollFun False "f" (PVS.Tuple [PVS.FPDouble,PVS.FPDouble]) [PVS.FVar PVS.FPDouble "x"]
    ]