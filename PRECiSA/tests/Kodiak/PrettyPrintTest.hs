-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
 
-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
 
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
  
  
module Kodiak.PrettyPrintTest where

import Kodiak.Expression
import Kodiak.PrettyPrint

import Test.Tasty
import Test.Tasty.HUnit
import Text.PrettyPrint

testAll :: TestTree
testAll = testGroup "Kodiak Expression Pretty Printer" [
  testPrettyAExpr
  ]

testPrettyAExpr :: TestTree
testPrettyAExpr = testGroup "Kodiak AExpr Pretty Printer" $
  [testGroup "Cnst"
    [testCase "exact" $
      (prettyAExpr $ Cnst 1)
      @?=
      text "val(dec(1,1))"
    ,testCase "exact overflow" $
      (prettyAExpr $ Cnst 2147483648)
      @?=
      text "val(ival(hex_val(\"0x1p31\"),hex_val(\"0x1p31\")))"
    ,testCase "0.1" $
      (prettyAExpr $ Cnst 0.1)
      @?=
      text "val(ival(hex_val(\"0x1.9999999999999p-4\"),hex_val(\"0x1.999999999999ap-4\")))"
    ]
    ,testCase "Var" $
      (prettyAExpr $ Var "myvar")
      @?=
      text "myvar"
    ,testCase "Add" $
      (prettyAExpr $ Add (Cnst 0) (Cnst 1))
      @?=
      text "( val(dec(0,1)) + val(dec(1,1)) )"
    ,testCase "Sub" $
      (prettyAExpr $ Sub (Cnst 0) (Cnst 1))
      @?=
      text "( val(dec(0,1)) - val(dec(1,1)) )"
    ,testCase "Mul" $
      (prettyAExpr $ Mul (Cnst 0) (Cnst 1))
      @?=
      text "( val(dec(0,1)) * val(dec(1,1)) )"
    ,testCase "Div" $
      (prettyAExpr $ Div (Cnst 0) (Cnst 1))
      @?=
      text "( val(dec(0,1)) / val(dec(1,1)) )"
    ,testCase "Neg" $
      (prettyAExpr $ Neg (Cnst 0))
      @?=
      text "( - val(dec(0,1)) )"
    ,testCase "Floor" $
      (prettyAExpr $ Floor (Cnst 0))
      @?=
      text "Floor(val(dec(0,1)))"
    ,testCase "Sqrt" $
      (prettyAExpr $ Sqrt (Cnst 0))
      @?=
      text "Sqrt(val(dec(0,1)))"
    ,testCase "Sqrt" $
      (prettyAExpr $ Sqrt (Cnst 0))
      @?=
      text "Sqrt(val(dec(0,1)))"
    ,testCase "Abs" $
      (prettyAExpr $ Abs (Cnst 0))
      @?=
      text "Abs(val(dec(0,1)))"
    ,testCase "Sin" $
      (prettyAExpr $ Sin (Cnst 0))
      @?=
      text "Sin(val(dec(0,1)))"
    ,testCase "Cos" $
      (prettyAExpr $ Cos (Cnst 0))
      @?=
      text "Cos(val(dec(0,1)))"
    ,testCase "ATan" $
      (prettyAExpr $ ATan (Cnst 0))
      @?=
      text "ATan(val(dec(0,1)))"
    ,testCase "Ln" $
      (prettyAExpr $ Ln (Cnst 0))
      @?=
      text "Ln(val(dec(0,1)))"
    ,testCase "Exp" $
      (prettyAExpr $ Exp (Cnst 0))
      @?=
      text "Exp(val(dec(0,1)))"
    ,testGroup "Ulp"
      [testCase "Single precision" $
        (prettyAExpr $ Ulp FPSingle (Cnst 0))
        @?=
        text "SUlp(val(dec(0,1)))"
      ,testCase "Double precision" $
        (prettyAExpr $ Ulp FPDouble (Cnst 0))
        @?=
        text "DUlp(val(dec(0,1)))"
      ]
    ,testCase "Max" $
      (prettyAExpr $ Max [(Cnst 0),(Cnst 1)])
      @?=
      text "Max({ val(dec(0,1)) , val(dec(1,1)) })"
  ]