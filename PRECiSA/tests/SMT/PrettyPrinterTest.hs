-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module SMT.PrettyPrinterTest where

import Test.Tasty
import Test.Tasty.HUnit

import AbsPVSLang
import AbsSpecLang
import AbstractDomain
import PPExt
import SMT.PrettyPrinter
import UtilsTest (fromDouble2Rat)

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
        (test $ UBDouble (fromDouble2Rat 0.25)) @?= "0.25"
    ,testCase "(UBDouble 0.1) returns 0.1" $
        (test $ UBDouble (fromDouble2Rat 0.1)) @?=  "0.1000000000000000055511151231257827021181583404541015625"
    ]
    where
        test = render . prettySMTUBound

testPrettySMTLBound :: TestTree
testPrettySMTLBound = testGroup "SMT.PrettyPrinter.testPrettySMTUBound"
    [testCase "(UBInt 1) returns 1.0" $
        (test $ UBInt 1) @?= "1.0"
    ,testCase "(UBDouble 0.25) returns 0.25" $
        (test $ UBDouble (fromDouble2Rat 0.25)) @?= "0.25"
    ,testCase "(UBDouble 0.1) returns 0.1" $
        (test $ UBDouble (fromDouble2Rat 0.1)) @?= "0.1000000000000000055511151231257827021181583404541015625"
    ]
    where
        test = render . prettySMTUBound