module Main where

import Test.Tasty

import AbstractDomainTest
import AbstractSemanticsTest
import SMT.Test
import Common.Test
import Kodiak.Test
import Kodiak.GeneratorTest
import AbsPVSLangTest
import UtilsTest
import AbsPVSLangTest
import TransformationTest
import TransformationUtilsTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [testGroup "" []
    ,testAbstractSemantics
    ,testAbstractDomain
    ,testAbsPVSLang
    ,testSMT
    ,testCommon
    ,testUtils
    ,testTransformation
    ,testTransformationUtils
    ,testKodiak
    ,testKodiakGenerator
    ]

