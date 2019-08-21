module Common.Test where

import Common.DecisionPathTest
import Common.ControlFlowTest
import Common.ShowRationalTest
import Test.Tasty

testCommon :: TestTree
testCommon = testGroup "Common"
        [testCommonDecisionPath
        ,testControlFlow
        ,testShowRational
        ]
        