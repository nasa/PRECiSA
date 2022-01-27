-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


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
    setEq [1 :: Int,2,3] [2,3,1] @?= True

testSetEq__test2 = testCase "[1,2,3] == [2,3,1]" $
    setEq [1 :: Int,2,1] [2,1] @?= True

testSetEq__test3 = testCase "[1,2,3,4] != [2,3,1]" $
    setEq [1 :: Int,2,3,4] [2,3,1] @?= False

testSetEq__test4 = testCase "[1,2,3] != [2,3,1]" $
    setEq [1 :: Int,2,1] [2,1,3] @?= False

testCombos :: TestTree
testCombos = testGroup "testCombos"
    [testCombos__test1
    ,testCombos__test2
    ,testCombos__test3
    ,testCombos__test4
    ]

testCombos__test1 = testCase "combos [[1,2,3],[4,5]] = [[1,4],[1,5],[2,4],[2,5],[3,4],[3,5]]" $
    combos [[1 :: Int,2,3],[4,5]] @?= [[1,4],[1,5],[2,4],[2,5],[3,4],[3,5]]

testCombos__test2 = testCase "combos [[1,2,1],[3]] == [[1,3],[2,3],[1,3]]" $
    combos [[1 :: Int,2,1],[3]] @?= [[1,3],[2,3],[1,3]]

testCombos__test3 = testCase "combos [[1,2,3],[]] == []" $
    combos [[1 :: Int,2,3],[]] @?= []

testCombos__test4 = testCase "combos [[1,2,3],[4,5],[7]] == [[1,4,7],[1,5,7],[2,4,7],[2,5,7],[3,4,7],[3,5,7]]" $
    combos [[1 :: Int,2,3],[4,5],[7]] @?= [[1,4,7],[1,5,7],[2,4,7],[2,5,7],[3,4,7],[3,5,7]]

testIsPow2 :: TestTree
testIsPow2 = testGroup "testSetEq"
    [testIsPow2__test1
    ,testIsPow2__test2
    ,testIsPow2__test3
    ,testIsPow2__test4
    ]

testIsPow2__test1 = testCase "2 is power of 2" $
    isPow2 (2 :: Double) @?= True

testIsPow2__test2 = testCase "16 is power of 2" $
    isPow2 (16 :: Double) @?= True

testIsPow2__test3 = testCase "3 is not power of 2" $
    isPow2 (3 :: Double) @?= False

testIsPow2__test4 = testCase "12 is not power of 2" $
    isPow2 (12 :: Double) @?= False

testFoldlWithDefault :: TestTree
testFoldlWithDefault = testGroup "testSetEq"
    [testFoldlWithDefault__test1
    ,testFoldlWithDefault__test2
    ,testFoldlWithDefault__test3
    ,testFoldlWithDefault__test4
    ]

testFoldlWithDefault__test1 = testCase "fold with default returns base case for []" $
    foldlWithDefault (+) (0 :: Int) [] @?= 0

testFoldlWithDefault__test2 = testCase "foldlWithDefault (+) 0 [1,2] = 3" $
    foldlWithDefault (+) (0 :: Int) [1,2] @?= 3

testFoldlWithDefault__test3 = testCase "foldlWithDefault (+) 0 [1,2,4] = 7" $
    foldlWithDefault (+) (0 :: Int) [1,2,4] @?= 7

testFoldlWithDefault__test4 = testCase "foldlWithDefault (*) 0 [0,1,2] = 0" $
    foldlWithDefault (*) (0 :: Int) [0,1,2] @?= 0

fromDouble2Rat :: Double -> Rational
fromDouble2Rat d = toRational d