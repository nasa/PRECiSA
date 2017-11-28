module Common.DecisionPathTest where

import Common.DecisionPath hiding (root)
import qualified Common.DecisionPath as DP
import Test.Tasty
import Test.Tasty.HUnit


root = DP.root :: LDecisionPath

testDecisionPath = testGroup "DecisionPath" [
  maxCommonPrefix__tests,
  maxCommonPrefixOfList__tests,
  isPrefix__tests,
  isPrefixInList__tests,
  existsPrefixInList__tests
  ]

maxCommonPrefix__tests = testGroup "maxCommonPrefix tests"
  [maxCommonPrefix__test1
  ,maxCommonPrefix__test2
  ,maxCommonPrefix__test3
  ]

maxCommonPrefix__test1 = testCase "0100 /\\ 0000 = 0" $
    maxCommonPrefix dp1 dp2 @?= root ~> False
    where
      dp1 = root ~> False ~> True ~> False ~> False
      dp2 = root ~> False ~> False ~> False ~> False

maxCommonPrefix__test2 = testCase "0100 /\\ 0101 = 010" $
    maxCommonPrefix dp1 dp2 @?= root ~> False ~> True ~> False
    where
      dp1 = root ~> False ~> True ~> False ~> False
      dp2 = root ~> False ~> True ~> False ~> True

maxCommonPrefix__test3 = testCase "0100 /\\ 011 = 01" $
    maxCommonPrefix dp1 dp2 @?= root ~> False ~> True
    where
      dp1 = root ~> False ~> True ~> False ~> False
      dp2 = root ~> False ~> True ~> True

maxCommonPrefixOfList__tests = testGroup "maxCommonPrefixOfList tests" [
  maxCommonPrefixOfList__test1,
  maxCommonPrefixOfList__test2
  ]

maxCommonPrefixOfList__test1 = testCase "[0110,0100,0111] = 01" $
    maxCommonPrefixOfList dpList @?= root ~> False ~> True
    where
      dpList = [
        root ~> False ~> True ~> True ~> False,
        root ~> False ~> True ~> False ~> False,
        root ~> False ~> True ~> True ~> True
        ]

maxCommonPrefixOfList__test2 = testCase "root = root" $
    maxCommonPrefixOfList dpList @?= root
    where
      dpList = []

isPrefix__tests = testGroup "isPrefix tests"
  [isPrefix__test1
  ,isPrefix__test2
  ,isPrefix__test3
  ,isPrefix__test4
  ]

isPrefix__test1 = testCase "101 `isPrefix` 1011" $
    (dp1 `isPrefix` dp2) @?= True
    where
      dp1 = root ~> True ~> False ~> True
      dp2 = root ~> True ~> False ~> True ~> True

isPrefix__test2 = testCase "not $ 101 `isPrefix` 100" $
    (dp1 `isPrefix` dp2) @?= False
    where
      dp1 = root ~> True ~> False ~> True
      dp2 = root ~> True ~> False ~> False

isPrefix__test3 = testCase "root `isPrefix` 100" $
    (dp1 `isPrefix` dp2) @?= True
    where
      dp1 = root
      dp2 = root ~> True ~> False ~> False

isPrefix__test4 = testCase "root `isPrefix` root" $
    (root `isPrefix` root) @?= True

isPrefixInList__tests = testGroup "isPrefixInList tests"
  [isPrefixInList__test1
  ,isPrefixInList__test2
  ,isPrefixInList__test3
  ,isPrefixInList__test4
  ,isPrefixInList__test5
  ]

isPrefixInList__test1 = testCase "0 `isPrefixInList` [10,01]" $
    (dp `isPrefixInList` dpList) @?= True
    where
      dp = root ~> False
      dpList = [root ~> True ~> False, root ~> False ~> True]

isPrefixInList__test2 = testCase "not $ 0 `isPrefixInList` [10,11]" $
    (dp `isPrefixInList` dpList) @?= False
    where
      dp = root ~> False
      dpList = [root ~> True ~> False, root ~> True ~> True]

isPrefixInList__test3 = testCase "not $ 0 `isPrefixInList` []" $
    (dp `isPrefixInList` dpList) @?= False
    where
      dp = root ~> False
      dpList = []

isPrefixInList__test4 = testCase "not $ root `isPrefixInList` []" $
    (dp `isPrefixInList` dpList) @?= False
    where
      dp = root
      dpList = []

isPrefixInList__test5 = testCase "root `isPrefixInList` [10,01]" $
    (dp `isPrefixInList` dpList) @?= True
    where
      dp = root
      dpList = [root ~> True ~> False, root ~> False ~> True]


existsPrefixInList__tests = testGroup "existsPrefixInList tests"
  [existsPrefixInList__test1
  ,existsPrefixInList__test2
  ,existsPrefixInList__test3
  ,existsPrefixInList__test4
  ,existsPrefixInList__test5
  ,existsPrefixInList__test6
  ]

existsPrefixInList__test1 = testCase "011 `existsPrefixInList` [10,0]" $
    (dp `existsPrefixInList` dpList) @?= True
    where
      dp = root ~> False ~> True ~> True
      dpList = [root ~> True ~> False, root ~> False ]

existsPrefixInList__test2 = testCase "not $ 0 `existsPrefixInList` [10,11]" $
    (dp `existsPrefixInList` dpList) @?= False
    where
      dp = root ~> False
      dpList = [root ~> True ~> False, root ~> True ~> True]

existsPrefixInList__test3 = testCase "not $ 0 `existsPrefixInList` []" $
    (dp `existsPrefixInList` dpList) @?= False
    where
      dp = root ~> False
      dpList = []

existsPrefixInList__test4 = testCase "not $ root `existsPrefixInList` []" $
    (dp `existsPrefixInList` dpList) @?= False
    where
      dp = root
      dpList = []

existsPrefixInList__test5 = testCase "root `existsPrefixInList` [root,01]" $
    (dp `existsPrefixInList` dpList) @?= True
    where
      dp = root
      dpList = [root, root ~> False ~> True]

existsPrefixInList__test6 = testCase "not $ root `existsPrefixInList` [10,011]" $
    (dp `existsPrefixInList` dpList) @?= False
    where
      dp = root
      dpList = [root ~> True ~> False, root ~> False ~> True ~> True]

existsPrefixInList__test7 = testCase "1 `existsPrefixInList` [root,01]" $
    (dp `existsPrefixInList` dpList) @?= True
    where
      dp = root ~> False
      dpList = [root, root ~> False ~> True]
