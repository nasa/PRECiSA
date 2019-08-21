module SMT.Test where

import SMT.ModelTest
import SMT.PrettyPrinterTest

import Test.Tasty

testSMT :: TestTree
testSMT = testGroup "SMT"
        [testModel
        ,testSMTPrettyPrinter
        ]
        