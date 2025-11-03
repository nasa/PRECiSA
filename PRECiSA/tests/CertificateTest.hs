module CertificateTest where

import AbsPVSLang

import Prelude hiding ((<>))
import Test.Tasty
import Test.Tasty.HUnit
import TestUtils (throwsException)
import Text.PrettyPrint

testCertificate = testGroup "Certificate"
  [testGroup "linearizeMax"
    [testCase "1" $
      linearizeMax [text "A", text "B", text "C"] @?= text "max(A,max(B,C))"
    ,testCase "2" $
      linearizeMax [text "D", text "A", text "B", text "C"] @?= text "max(D,max(A,max(B,C)))"
    ,testCase "3" $
      linearizeMax [text "B", text "C"] @?= text "max(B,C)"
    ,testCase "4" $
      linearizeMax [text "C"] @?= text "C"
    ,testCase "5" $
      throwsException $ linearizeMax []
    ]
  ]