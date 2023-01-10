module FramaC.GenerateACSLTest where

import qualified AbsPVSLang as PVS
import Common.TypesUtils
import FramaC.ACSLTypes
import FramaC.ACSLlang
import FramaC.GenerateACSL
import qualified FramaC.Types as C
import Operators
import PPExt (prettyDoc, render)
import qualified PVSTypes as PVS
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Eq)

true = PredBExpr BTrue

test =testGroup  "ACSL Generation" [
   behaviorStructure__test
  ,behaviorStructurePred__test
  ,isFiniteHypothesis__test
  ,predPostCond__test
  ,behaviorStablePaths__test
  ]

behaviorStablePaths__test = testGroup "behaviorStablePaths"
  [ testCase "behaviorStablePaths with let" $
    behaviorStablePaths (\_ -> False) "greater_than" (Just TauPlus) (Float DoublePrec)
                        [PVS.Arg "a" PVS.Real, PVS.Arg "b" PVS.Real]
                        [("E_0",PVS.Let [("a_minus_b",PVS.FPDouble,PVS.BinaryFPOp SubOp PVS.FPDouble
                        (PVS.FVar PVS.FPDouble "a") (PVS.FVar PVS.FPDouble "b"))](PVS.FVar PVS.FPDouble"a_minus_b"),PVS.FBTrue)]
                        [][]
                        [PVS.FVar PVS.FPDouble "a"
                        ,PVS.FVar PVS.FPDouble "b"
                        ,PVS.FVar PVS.FPDouble "E_0"
                        ,PVS.FVar PVS.FPDouble "a_minus_b"
                        ,PVS.TypeCast PVS.TInt PVS.FPDouble (PVS.FInt 0)]
    @?=
      Ensures (Forall [("a",Real),("b",Real)] (Implies (FPredLet (Float DoublePrec) "a_minus_b" (BinaryFPOp SubOp (Float DoublePrec) (FVar (Float DoublePrec) "a") (FVar (Float DoublePrec) "b")) (PredLet "a_minus_b" (BinaryOp SubOp (Var Real "a") (Var Real "b")) (FErrorDiseq (FVar (Float DoublePrec) "a_minus_b") (Var Real "a_minus_b") (FVar (Float DoublePrec) "E_0")))) (Implies (PredAnd (PredAnd (PredAnd (PredAnd (PredAnd (IsFiniteFP (FVar (Float DoublePrec) "a")) (IsFiniteFP (FVar (Float DoublePrec) "b"))) (IsFiniteFP (FVar (Float DoublePrec) "E_0"))) (IsFiniteFP (FVar (Float DoublePrec) "a_minus_b"))) (IsFiniteFP (FTypeCast Int (Float DoublePrec) (FInt 0)))) (IsValid Result)) (Pred "greater_than_tauplus_stable_paths" [Var Real "a",Var Real "b",Var (Float DoublePrec) "a",Var (Float DoublePrec) "b"]))))
  ]

behaviorStructure__test = testGroup "behaviorStructure"
  [ testCase "no args no conditions no expression list" $
      behaviorStructure (\_ -> False) (Float DoublePrec) "f" [] []
        @?= Ensures (Implies (PredAnd (IsFiniteFP FResult) (PredBExpr BTrue)) (PredFBExpr $ FRel Eq FResult (FEFun "f_fp" (Float DoublePrec) []))),
    testCase "no args conditions no expression list" $
      behaviorStructure (\_ -> True) (Float DoublePrec) "f" [] []
        @?= Ensures (Implies(PredAnd (IsValid Result) (PredAnd (IsFiniteFP (FValue FResult)) (PredBExpr BTrue))) (PredFBExpr $ FRel Eq (FValue FResult) (FEFun "f_fp" (Float DoublePrec) [])))
  ]

behaviorStructurePred__test = testGroup  "behaviorStructurePred"
  [ testCase "tauplus no args no conditions no expression list" $
      behaviorStructurePred (\_ -> False) (Float DoublePrec) TauPlus "f" [] [] [] [] []
        @?=
          Ensures (Implies
            (PredBExpr BTrue)
            (Implies (AExprPred Result)
                     (PredAnd (AExprPred (EFun "f" Real []))
                              (FAExprPred (FEFun "f_fp" (Float DoublePrec) [])))))
  , testCase "tauplus no args with conditions no expression list" $
      behaviorStructurePred (\_ -> True) (Float DoublePrec) TauPlus "f" [] [] [] [] []
        @?=
          Ensures (Implies (IsValid Result)
            (Implies (AExprPred (Value Result))
                     (PredAnd (AExprPred (EFun "f" Real []))
                              (FAExprPred (FEFun "f_fp" (Float DoublePrec) [])))))
  , testCase "tauminus no args no conditions no expression list" $
      behaviorStructurePred (\_ -> False) (Float DoublePrec) TauMinus "f" [] [] [] [] []
        @?=
          Ensures (Implies (PredBExpr BTrue)
            (Implies (AExprPred Result)
            (PredAnd (PredNot $ AExprPred (EFun "f" Real []))
                     (PredNot $ FAExprPred (FEFun "f_fp" (Float DoublePrec) [])))))
  , testCase "tauminus no args with conditions no expression list" $
      behaviorStructurePred (\_ -> True) (Float DoublePrec) TauMinus "f" [] [] [] [] []
        @?=
          Ensures (Implies (IsValid Result)
          (Implies (AExprPred (Value Result))
                   (PredAnd (PredNot $ AExprPred (EFun "f" Real []))
                            (PredNot $ FAExprPred (FEFun "f_fp" (Float DoublePrec) [])))))
  , testCase "original no args no conditions no expression list" $
      behaviorStructurePred (\_ -> False) (Float DoublePrec) Original "f" [] [] [] [] []
        @?=
          Ensures (Implies (PredBExpr BTrue)
          (PredAnd
            (Implies (AExprPred Result)
            (PredAnd (AExprPred (EFun "f" Real []))
                     (FAExprPred (FEFun "f_fp" (Float DoublePrec) []))))
            (Implies (PredNot $ AExprPred Result)
            (PredAnd (PredNot $ AExprPred (EFun "f" Real []))
                     (PredNot $ FAExprPred (FEFun "f_fp" (Float DoublePrec) []))))))
  , testCase "original no args with conditions no expression list" $
      behaviorStructurePred (\_ -> True) (Float DoublePrec) Original "f" [] [] [] [] []
        @?=
          Ensures (Implies (IsValid Result)
          (PredAnd
          (Implies (AExprPred (Value Result))
                   (PredAnd (AExprPred (EFun "f" Real []))
                            (FAExprPred (FEFun "f_fp" (Float DoublePrec) []))))
          (Implies (PredNot $ AExprPred (Value Result))
                   (PredAnd (PredNot $ AExprPred (EFun "f" Real []))
                            (PredNot $ FAExprPred (FEFun "f_fp" (Float DoublePrec) []))))))
  ]

isFiniteHypothesis__test = testGroup
  "isFiniteHypothesis"
  [ testCase "no conditions and an empty expression list" $
      isFiniteHypothesis False [] @?= PredBExpr BTrue,
    testCase "with conditions and an empty expression list" $
      isFiniteHypothesis True [] @?= PredBExpr BTrue,
    testCase "no conditions and one expression" $
      isFiniteHypothesis False [PVS.FVar PVS.FPDouble "x"]
        @?= IsFiniteFP (FVar (Float DoublePrec) "x"),
    testCase "with conditions and one expression" $
      isFiniteHypothesis True [PVS.FVar PVS.FPDouble "x"]
        @?= IsFiniteFP (FVar (Float DoublePrec) "x")
  ]

predPostCond__test = testGroup "predPostCond"
  [ testCase "no conditions, tauplus and an empty expression list " $
      predPostCond (\_ -> False) TauPlus "f" []
        @?= Implies (AExprPred Result)
                    (AExprPred $ EFun "f" Real [])
  , testCase "no conditions, tauminus and an empty expression list " $
      predPostCond (\_ -> False) TauMinus "f" []
        @?= Implies (AExprPred Result)
                    (PredNot $ AExprPred $ EFun "f" Real [])
  , testCase "no conditions, original and an empty expression list " $
      predPostCond (\_ -> False) Original "f" []
        @?= Iff (AExprPred Result)
                (AExprPred $ EFun "f" Real [])
  , testCase "with conditions, tauplus and an empty expression list " $
      predPostCond (\ _ -> True) TauPlus "f" []
        @?= Implies (AExprPred $ Value Result)
                    (AExprPred $ EFun "f" Real [])
  , testCase "with conditions, tauminus and an empty expression list " $
      predPostCond (\_ -> True) TauMinus "f" []
        @?= Implies (AExprPred $ Value Result)
                    (PredNot $ AExprPred $ EFun "f" Real [])
  , testCase "with conditions, original and an empty expression list " $
      predPostCond (\_ -> True) Original "f" []
        @?= Iff (AExprPred $ Value Result)
                   (AExprPred $ EFun "f" Real [])
  ]