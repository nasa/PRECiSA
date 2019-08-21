module SMT.PrettyPrinterTest where

import Test.Tasty
import Test.Tasty.HUnit

import AbsPVSLang
import AbsSpecLang
import AbstractDomain
import PPExt
import SMT.PrettyPrinter
import Foreign.C
import Numeric (fromRat,showFFloat)
import Debug.Trace

testSMTPrettyPrinter :: TestTree
testSMTPrettyPrinter = testGroup "SMT.PrettyPrinter"
    [testPrettySMT
    ,testPrettySMTCondition
    ,testPrettySMTUBound
    ,testPrettySMTLBound
    ]


testPrettySMT :: TestTree
testPrettySMT = testGroup "SMT.PrettyPrinter.prettySMT" [
    testCase "[] returns false" $
        (test $ Cond []) @?= "false",
    testCase "[(BTrue, FBTrue)] returns empty" $
        (test $ Cond [(BTrue, FBTrue)]) @?= ""
    ]
    where
        test = render . prettySMT

testPrettySMTCondition :: TestTree
testPrettySMTCondition = testGroup "SMT.PrettyPrinter.prettySMTCondition"
    [testCase "(BTrue, FBTrue) returns empty" $
        (test $ (BTrue, FBTrue)) @?= ""
    ,testCase "(BFalse, FBFalse) returns false" $
        (test $ (BFalse, FBFalse)) @?= "false"
    ,testCase "(BFalse, FBTrue) returns false" $
        (test $ (BFalse, FBTrue)) @?= "false"
    ,testCase "(BTrue, FBFalse) returns false" $
        (test $ (BTrue, FBFalse)) @?= "false"
    ]
    where
        test = render . prettySMTCondition

testPrettySMTUBound :: TestTree
testPrettySMTUBound = testGroup "SMT.PrettyPrinter.testPrettySMTUBound"
    [testCase "(UBInt 1) returns 1.0" $
        (test $ UBInt 1) @?= "1.0"
    ,testCase "(UBDouble 0.25) returns 0.25" $
        (test $ UBDouble (toRational 0.25)) @?= "0.25"
    ,testCase "(UBDouble 0.1) returns 0.1" $
        (test $ UBDouble (toRational 0.1)) @?=  "0.1000000000000000055511151231257827021181583404541015625"
    ]
    where
        test = render . prettySMTUBound

testPrettySMTLBound :: TestTree
testPrettySMTLBound = testGroup "SMT.PrettyPrinter.testPrettySMTUBound"
    [testCase "(UBInt 1) returns 1.0" $
        (test $ UBInt 1) @?= "1.0"
    ,testCase "(UBDouble 0.25) returns 0.25" $
        (test $ UBDouble (toRational 0.25)) @?= "0.25"
    ,testCase "(UBDouble 0.1) returns 0.1" $
        (test $ UBDouble (toRational 0.1)) @?= "0.1000000000000000055511151231257827021181583404541015625"
    ]
    where
        test = render . prettySMTUBound