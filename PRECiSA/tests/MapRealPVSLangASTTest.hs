-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
 
-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
 
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
  
  
module MapRealPVSLangASTTest where

import Test.Tasty
import Test.Tasty.HUnit
import AbsRawRealPVSLang
import AbsPVSLang
import MapRealPVSLangAST
import PVSTypes

testMapRealPVSLangAST = testGroup "MapRealPVSLangAST"
    [raw2AExpr__tests
    ]

raw2AExpr__tests = testGroup "raw2AExpr"
  [ raw2AExpr_LetList1
  , raw2AExpr_LetList2
  ]

raw2AExpr_LetList1 = testCase "letList1" $
    raw2AExpr [] [] (AbsRawRealPVSLang.Let [AbsRawRealPVSLang.LetElem (Id "x") (AbsRawRealPVSLang.Int 1)
                                     ,AbsRawRealPVSLang.LetElem (Id "y") (AbsRawRealPVSLang.Int 3)]
                                     (AbsRawRealPVSLang.Int 2))
    @?=
    AbsPVSLang.RLet [AbsPVSLang.LetElem{letVar ="x"
                                       ,letType = TInt
                                       ,letExpr = AbsPVSLang.Int 1}
                    ,AbsPVSLang.LetElem{letVar = "y"
                                       ,letType = TInt
                                       ,letExpr = AbsPVSLang.Int 3}]
                    (AbsPVSLang.Int 2)

raw2AExpr_LetList2 = testCase "letList2" $
    raw2AExpr [] [] (AbsRawRealPVSLang.Let [AbsRawRealPVSLang.LetElemType (Id "x") TypeInt
                                            (AbsRawRealPVSLang.Int 1)
                                           ,AbsRawRealPVSLang.LetElemType (Id "y") TypeInt
                                            (AbsRawRealPVSLang.Int 3)]
                                     (AbsRawRealPVSLang.Int 2))
    @?=
    AbsPVSLang.RLet [AbsPVSLang.LetElem{letVar ="x"
                                       ,letType = TInt
                                       ,letExpr = AbsPVSLang.Int 1}
                    ,AbsPVSLang.LetElem{letVar = "y"
                                       ,letType = TInt
                                       ,letExpr = AbsPVSLang.Int 3}]
                    (AbsPVSLang.Int 2)