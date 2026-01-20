{-# LANGUAGE QuasiQuotes #-}
module CertificateTest where

import AbsPVSLang
import Certificate.Symbolic
import Common.ControlFlow (ControlFlow(..))

import Data.List (dropWhile)
import Prelude hiding ((<>))
import Test.Tasty
import Test.Tasty.HUnit
import TestUtils (throwsException)
import Text.PrettyPrint
import Text.RawString.QQ (r)

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
  ,testSymbolicCertificate
  ]

testSymbolicCertificate = testGroup "Symbolic"
  [testGroup "prPvsProof"
    [testCase "for stable" $
      prPvsProof "example" 0 Stable `isCloseTo`
      [r|
%|- example_0: PROOF
%|- (prove-symbolic-certificate$)
%|- QED|]
    ,testCase "for unstable" $
      prPvsProof "example" 0 Unstable `isCloseTo`
      [r|
%|- example_0: PROOF
%|- (prove-symbolic-certificate$ t)
%|- QED|]
    ]

  ,testGroup "prIsFinite"
    [testCase "for integer literals" $
      prIsFinite (FInt 2) `isCloseTo` [r|
finite?_double(round_double(2))
AND
abs(DtoR(round_double(2)) - 2) <= 0|]
    ]
  ]
  where
    isCloseTo doc str = render doc @?= dropWhile (=='\n') str