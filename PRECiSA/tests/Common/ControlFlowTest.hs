module Common.ControlFlowTest where

import Common.ControlFlow
import Test.Tasty
import Test.Tasty.HUnit

testControlFlow :: TestTree
testControlFlow = testGroup "Common.ControlFlow"
    [testMergeControlFlow
    ]

testMergeControlFlow :: TestTree
testMergeControlFlow = testGroup "mergeControlFlow"
    [mergeControlFlow__test1
    ,mergeControlFlow__test2
    ,mergeControlFlow__test3
    ,mergeControlFlow__test4
    ]

mergeControlFlow__test1 = testCase "Stable merge Stable = Stable" $
    mergeControlFlow Stable Stable @?= Stable

mergeControlFlow__test2 = testCase "Stable merge Unstable = Unstable" $
    mergeControlFlow Stable Unstable @?= Unstable

mergeControlFlow__test3 = testCase "Unstable merge Stable = Unstable" $
    mergeControlFlow Unstable Stable @?= Unstable

mergeControlFlow__test4 = testCase "Unstable merge Unstable = Unstable" $
    mergeControlFlow Unstable Unstable @?= Unstable