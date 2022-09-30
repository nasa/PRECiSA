module FramaC.GenerateACSLTest where

import qualified AbsPVSLang as PVS
import Common.TypesUtils
import FramaC.ACSLTypes
import FramaC.ACSLlang
import FramaC.GenerateACSL
import qualified FramaC.Types as C
import Operators (RelOp (..))
import PPExt (prettyDoc, render)
import qualified PVSTypes as PVS
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Eq)

true = PredBExpr BTrue

test =
  testGroup
    "ACSL Generation"
    [ testGroup
        "behaviorStructure"
        [ testCase "no args no conditions no expression list" $
            behaviorStructure (\_ -> False) (Float DoublePrec) "f" [] []
              @?= Ensures (Implies true (PredFBExpr $ FRel Eq FResult (FEFun "f_fp" (Float DoublePrec) []))),
          testCase "no args conditions no expression list" $
            behaviorStructure (\_ -> True) (Float DoublePrec) "f" [] []
              @?= Ensures (Implies (IsValid Result) (PredFBExpr $ FRel Eq (FValue FResult) (FEFun "f_fp" (Float DoublePrec) [])))
        ],
      testGroup
        "behaviorStructurePred"
        [ testCase "tauplus no args no conditions no expression list" $
            behaviorStructurePred (\_ -> False) (Float DoublePrec) TauPlus "f" [] [] [] [] []
              @?=
                Ensures (Implies
                  (PredBExpr BTrue)
                  (PredAnd (AExprPred (EFun "f" Real [])) (FAExprPred (FEFun "f_fp" (Float DoublePrec) []))))
        , testCase "tauplus no args with conditions no expression list" $
            behaviorStructurePred (\_ -> True) (Float DoublePrec) TauPlus "f" [] [] [] [] []
              @?=
                Ensures (Implies
                  (IsValid Result)
                  (PredAnd (AExprPred (EFun "f" Real [])) (FAExprPred (FValue (FEFun "f_fp" (Float DoublePrec) [])))))
        ],
      testGroup
        "isFiniteHypothesis"
        [ testCase "no conditions and an empty expression list" $
            isFiniteHypothesis False [] @?= PredBExpr BTrue,
          testCase "with conditions and an empty expression list" $
            isFiniteHypothesis True [] @?= PredBExpr BTrue,
          testCase "no conditions and one expression" $
            isFiniteHypothesis False [PVS.FVar PVS.FPDouble "x"]
              @?= PredAnd
                (IsFiniteFP FResult)
                (IsFiniteFP (FVar (Float DoublePrec) "x")),
          testCase "with conditions and one expression" $
            isFiniteHypothesis True [PVS.FVar PVS.FPDouble "x"]
              @?= PredAnd
                (IsFiniteFP (FValue FResult))
                (IsFiniteFP (FVar (Float DoublePrec) "x"))
        ],
      testGroup
        "predPostCond"
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
    ]
