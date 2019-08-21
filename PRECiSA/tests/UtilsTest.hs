module UtilsTest where

import Utils
import Test.Tasty
import Test.Tasty.HUnit

testUtils :: TestTree
testUtils = testGroup "Utils"
    [testSetEq
    ,testCombos
    ,testFoldlWithDefault
    ,testIsPow2
    ]


testSetEq :: TestTree
testSetEq = testGroup "testSetEq"
    [testSetEq__test1
    ,testSetEq__test2
    ,testSetEq__test3
    ,testSetEq__test4
    ]

testSetEq__test1 = testCase "[1,2,3] == [2,3,1]" $
    setEq [1,2,3] [2,3,1] @?= True

testSetEq__test2 = testCase "[1,2,3] == [2,3,1]" $
    setEq [1,2,1] [2,1] @?= True

testSetEq__test3 = testCase "[1,2,3,4] != [2,3,1]" $
    setEq [1,2,3,4] [2,3,1] @?= False

testSetEq__test4 = testCase "[1,2,3] != [2,3,1]" $
    setEq [1,2,1] [2,1,3] @?= False

testCombos :: TestTree
testCombos = testGroup "testCombos"
    [testCombos__test1
    ,testCombos__test2
    ,testCombos__test3
    ,testCombos__test4
    ]

testCombos__test1 = testCase "combos [[1,2,3],[4,5]] = [[1,4],[1,5],[2,4],[2,5],[3,4],[3,5]]" $
    combos [[1,2,3],[4,5]] @?= [[1,4],[1,5],[2,4],[2,5],[3,4],[3,5]]

testCombos__test2 = testCase "combos [[1,2,1],[3]] == [[1,3],[2,3],[1,3]]" $
    combos [[1,2,1],[3]] @?= [[1,3],[2,3],[1,3]]

testCombos__test3 = testCase "combos [[1,2,3],[]] == []" $
    combos [[1,2,3],[]] @?= []

testCombos__test4 = testCase "combos [[1,2,3],[4,5],[7]] == [[1,4,7],[1,5,7],[2,4,7],[2,5,7],[3,4,7],[3,5,7]]" $
    combos [[1,2,3],[4,5],[7]] @?= [[1,4,7],[1,5,7],[2,4,7],[2,5,7],[3,4,7],[3,5,7]]

testIsPow2 :: TestTree
testIsPow2 = testGroup "testSetEq"
    [testIsPow2__test1
    ,testIsPow2__test2
    ,testIsPow2__test3
    ,testIsPow2__test4
    ]

testIsPow2__test1 = testCase "2 is power of 2" $
    isPow2 2 @?= True

testIsPow2__test2 = testCase "16 is power of 2" $
    setEq [1,2,1] [2,1] @?= True

testIsPow2__test3 = testCase "3 is not power of 2" $
    isPow2 3 @?= False

testIsPow2__test4 = testCase "12 is not power of 2" $
    isPow2 12 @?= False

testFoldlWithDefault :: TestTree
testFoldlWithDefault = testGroup "testSetEq"
    [testFoldlWithDefault__test1
    ,testFoldlWithDefault__test2
    ,testFoldlWithDefault__test3
    ,testFoldlWithDefault__test4
    ]

testFoldlWithDefault__test1 = testCase "fold with default returns base case for []" $
    foldlWithDefault (+) 0 [] @?= 0

testFoldlWithDefault__test2 = testCase "foldlWithDefault (+) 0 [1,2] = 3" $
    foldlWithDefault (+) 0 [1,2] @?= 3

testFoldlWithDefault__test3 = testCase "foldlWithDefault (+) 0 [1,2,4] = 7" $
    foldlWithDefault (+) 0 [1,2,4] @?= 7

testFoldlWithDefault__test4 = testCase "foldlWithDefault (*) 0 [0,1,2] = 0" $
    foldlWithDefault (*) 0 [0,1,2] @?= 0

