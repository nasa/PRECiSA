-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


{-# LANGUAGE NamedFieldPuns #-}

module Kodiak.PaverTest where

import Test.Tasty
import Test.Tasty.HUnit

import AbsSpecLang
import PVSTypes
import qualified Kodiak.Runnable   as KR
import qualified Kodiak.Expression as K
import Kodiak.Paver

testKodiakPaver :: TestTree
testKodiakPaver = testGroup "Kodiak Paver"
    [testCase "Paves a simple constant" $
        let name = "True"
            input = Input name K.True [] 10 (-10) in
        KR.run input () `checkOutputIs` name
    ,testCase "Paves a relation" $
        let name       = "XGreaterThan0"
            input = Input { name
                          , expression = K.GT (K.Var "X") (K.Cnst 0)
                          , bindings   = [VarBind "X" FPDouble (LBInt $ -1) (UBInt 1)]
                          , maxDepth   = 10
                          , precision  = 8
                          } in
        KR.run input () `checkOutputIs` name
    ,testCase "Paves a complex boolean expression" $
        let name       = "XGreaterThan0"
            input = Input { name
                          , expression = K.And (K.GT (K.Var "X") (K.Cnst 0))
                                               (K.LT (K.Var "Y")
                                                     (K.Add (K.Var "X")
                                                     (K.Cnst 0.2)))
                          , bindings   = [VarBind "X" FPDouble (LBInt $ -1) (UBInt 1)
                                         ,VarBind "Y" FPDouble (LBInt $ -1) (UBInt 1)]
                          , maxDepth   = 10
                          , precision  = 8
                          } in
        KR.run input () `checkOutputIs` name
    ]
    where
        checkOutputIs actual expected =
            actual >>=
                \out ->
                    (out == Output expected)
                        @? ("Output: " ++ show out ++ " is different than expected: " ++ show expected)