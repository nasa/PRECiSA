module Test where

import Test.Tasty
import Test.Tasty.HUnit

import AbstractDomain
import AbstractSemantics
import AbstractSemanticsTest
import AbstractDomainTest
import AbsPVSLangTest
import Common.DecisionPathTest

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
    [testAbstractSemantics
    ,testAbstractDomain
    ,testDecisionPath
    ,testAbsPVSLang
    ]

