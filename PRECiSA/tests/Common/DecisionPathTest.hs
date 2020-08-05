-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
 
-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
 
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
  
  
module Common.DecisionPathTest where

import Common.DecisionPath hiding (root)
import qualified Common.DecisionPath as DP
import Test.Tasty
import Test.Tasty.HUnit

testCommonDecisionPath :: TestTree
testCommonDecisionPath = testGroup "Common.DecisionPath"
    [testDecisionPath
    ]

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
    maxCommonPrefix dp1 dp2 @?= root ~> 0
    where
      dp1 = root ~> 0 ~> 1 ~> 0 ~> 0
      dp2 = root ~> 0 ~> 0 ~> 0 ~> 0

maxCommonPrefix__test2 = testCase "0100 /\\ 0101 = 010" $
    maxCommonPrefix dp1 dp2 @?= root ~> 0 ~> 1 ~> 0
    where
      dp1 = root ~> 0 ~> 1 ~> 0 ~> 0
      dp2 = root ~> 0 ~> 1 ~> 0 ~> 1

maxCommonPrefix__test3 = testCase "0100 /\\ 011 = 01" $
    maxCommonPrefix dp1 dp2 @?= root ~> 0 ~> 1
    where
      dp1 = root ~> 0 ~> 1 ~> 0 ~> 0
      dp2 = root ~> 0 ~> 1 ~> 1

maxCommonPrefixOfList__tests = testGroup "maxCommonPrefixOfList tests" [
  maxCommonPrefixOfList__test1,
  maxCommonPrefixOfList__test2
  ]

maxCommonPrefixOfList__test1 = testCase "[0110,0100,0111] = 01" $
    maxCommonPrefixOfList dpList @?= root ~> 0 ~> 1
    where
      dpList = [
        root ~> 0 ~> 1 ~> 1 ~> 0,
        root ~> 0 ~> 1 ~> 0 ~> 0,
        root ~> 0 ~> 1 ~> 1 ~> 1
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
      dp1 = root ~> 1 ~> 0 ~> 1
      dp2 = root ~> 1 ~> 0 ~> 1 ~> 1

isPrefix__test2 = testCase "not $ 101 `isPrefix` 100" $
    (dp1 `isPrefix` dp2) @?= False
    where
      dp1 = root ~> 1 ~> 0 ~> 1
      dp2 = root ~> 1 ~> 0 ~> 0

isPrefix__test3 = testCase "root `isPrefix` 100" $
    (dp1 `isPrefix` dp2) @?= True
    where
      dp1 = root
      dp2 = root ~> 1 ~> 0 ~> 0

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
      dp = root ~> 0
      dpList = [root ~> 1 ~> 0, root ~> 0 ~> 1]

isPrefixInList__test2 = testCase "not $ 0 `isPrefixInList` [10,11]" $
    (dp `isPrefixInList` dpList) @?= False
    where
      dp = root ~> 0
      dpList = [root ~> 1 ~> 0, root ~> 1 ~> 1]

isPrefixInList__test3 = testCase "not $ 0 `isPrefixInList` []" $
    (dp `isPrefixInList` dpList) @?= False
    where
      dp = root ~> 0
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
      dpList = [root ~> 1 ~> 0, root ~> 0 ~> 1]


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
      dp = root ~> 0 ~> 1 ~> 1
      dpList = [root ~> 1 ~> 0, root ~> 0 ]

existsPrefixInList__test2 = testCase "not $ 0 `existsPrefixInList` [10,11]" $
    (dp `existsPrefixInList` dpList) @?= False
    where
      dp = root ~> 0
      dpList = [root ~> 1 ~> 0, root ~> 1 ~> 1]

existsPrefixInList__test3 = testCase "not $ 0 `existsPrefixInList` []" $
    (dp `existsPrefixInList` dpList) @?= False
    where
      dp = root ~> 0
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
      dpList = [root, root ~> 0 ~> 1]

existsPrefixInList__test6 = testCase "not $ root `existsPrefixInList` [10,011]" $
    (dp `existsPrefixInList` dpList) @?= False
    where
      dp = root
      dpList = [root ~> 1 ~> 0, root ~> 0 ~> 1 ~> 1]

existsPrefixInList__test7 = testCase "1 `existsPrefixInList` [root,01]" $
    (dp `existsPrefixInList` dpList) @?= True
    where
      dp = root ~> 0
      dpList = [root, root ~> 0 ~> 1]
