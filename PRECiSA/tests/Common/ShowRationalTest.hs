module Common.ShowRationalTest where

import Common.ShowRational
import Test.Tasty
import Test.Tasty.HUnit

testShowRational :: TestTree
testShowRational = testGroup "Common.ShowRational"
    [testShowRationalFun
    ]


testShowRationalFun :: TestTree
testShowRationalFun = testGroup "testShowRationalFun"
    [testShowRationalFun__test1
    ,testShowRationalFun__test2
    ,testShowRationalFun__test3
    -- ,testShowRationalFun__test4
    ]

testShowRationalFun__test1 = testCase "showRational 0.1 = 0.1000000000000000055511151231257827021181583404541015625" $
    showRational Nothing (toRational 0.1) @?= "0.1000000000000000055511151231257827021181583404541015625"

testShowRationalFun__test2 = testCase "showRational 0.1 = 0.100" $
    showRational (Just 3) (toRational 0.1) @?= "0.100"

testShowRationalFun__test3 = testCase "showRational 0.25 = 0.25" $
    showRational Nothing (toRational 0.25) @?= "0.25"
