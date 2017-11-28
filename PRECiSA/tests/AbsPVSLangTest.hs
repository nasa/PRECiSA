module AbsPVSLangTest where

import Test.Tasty
import Test.Tasty.HUnit
import AbsPVSLang


testAbsPVSLang = testGroup "AbstractSemantics" [rewriteEquivEExpr__tests, equivEExpr__tests]


rewriteEquivEExpr__tests = testGroup "rewriteEquivEExpr tests" $  map (\(msg, i, o) -> testCase msg (rewriteEquivEExpr i @?= o)) rewriteEquivEExpr_testsIOs


rewriteEquivEExpr_testsIOs =
           [("ErrMulPow2L"
                ,ErrMulPow2L 2 (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y"))
                ,ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")
            )
            ,("ErrMulPow2R"
                ,ErrMulPow2R 2 (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y"))
                ,ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")
            )
            ,("ErrAbs"
                ,ErrAbs (Int 3) (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y"))
                ,ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")
            )
            ,("ErrAbs"
                ,ErrNeg (Int 3) (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y"))
                ,ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")
            )
            ,("MaxErr"
                ,MaxErr [ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y"),ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")]
                ,ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")
            )
            ,("MaxErr_ErrNeg"
                ,MaxErr [ErrNeg (Int 3) (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")),ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")]
                ,ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")
            )
            ,("ErrAdd_ErrMulPow2L"
                ,ErrAdd (RealMark "Z") (ErrMulPow2L 2 (ErrSub (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y"))) (RealMark "K") (ErrorMark "K")
                ,ErrAdd (RealMark "Z") (ErrSub (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")) (RealMark "K") (ErrorMark "K")
            )
            ,("ErrAdd_ErrMulPow2L"
                ,ErrAdd (RealMark "Z") (ErrMulPow2L 2 (ErrSub (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y"))) (RealMark "K") (ErrorMark "K")
                ,ErrAdd (RealMark "Z") (ErrSub (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")) (RealMark "K") (ErrorMark "K")
            )
            ,("ErrSub_ErrMulPow2L"
                ,ErrSub (RealMark "Z") (ErrMulPow2L 2 (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y"))) (RealMark "K") (ErrorMark "K")
                ,ErrSub (RealMark "Z") (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")) (RealMark "K") (ErrorMark "K")
            )
            ,("ErrMul_ErrMulPow2L"
                ,ErrMul (RealMark "Z") (ErrMulPow2L 2 (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y"))) (RealMark "K") (ErrorMark "K")
                ,ErrMul (RealMark "Z") (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")) (RealMark "K") (ErrorMark "K")
            )
            ,("ErrDiv_ErrMulPow2L"
                ,ErrDiv (RealMark "Z") (ErrMulPow2L 2 (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y"))) (RealMark "K") (ErrorMark "K")
                ,ErrDiv (RealMark "Z") (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")) (RealMark "K") (ErrorMark "K")
            )
            ,("ErrFloor_ErrMulPow2L"
                ,ErrFloor (RealMark "Z") (ErrMulPow2L 2 (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")))
                ,ErrFloor (RealMark "Z") (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")) 
            )
            ,("ErrFloor0_ErrMulPow2L"
                ,ErrFloor0 (RealMark "Z") (ErrMulPow2L 2 (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")))
                ,ErrFloor0 (RealMark "Z") (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")) 
            )
            ,("ErrSqrt_ErrMulPow2L"
                ,ErrSqrt (RealMark "Z") (ErrMulPow2L 2 (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")))
                ,ErrSqrt (RealMark "Z") (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")) 
            )
            ,("ErrSin_ErrMulPow2L"
                ,ErrSin (RealMark "Z") (ErrMulPow2L 2 (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")))
                ,ErrSin (RealMark "Z") (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")) 
            )
            ,("ErrCos_ErrMulPow2L"
                ,ErrCos (RealMark "Z") (ErrMulPow2L 2 (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")))
                ,ErrCos (RealMark "Z") (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y"))
            )
            ,("ErrTan_ErrMulPow2L"
                ,ErrTan (RealMark "Z") (ErrMulPow2L 2 (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")))
                ,ErrTan (RealMark "Z") (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")) 
            )
            ,("ErrAsin_ErrMulPow2L"
                ,ErrAsin (RealMark "Z") (ErrMulPow2L 2 (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")))
                ,ErrAsin (RealMark "Z") (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y"))
            )
            ,("ErrAcos_ErrMulPow2L"
                ,ErrAcos (RealMark "Z") (ErrMulPow2L 2 (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")))
                ,ErrAcos (RealMark "Z") (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y"))
            )
            ,("ErrAtan_ErrMulPow2L"
                ,ErrAtan (RealMark "Z") (ErrMulPow2L 2 (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")))
                ,ErrAtan (RealMark "Z") (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y"))
            )
            ,("ErrAtanT_ErrMulPow2L"
                ,ErrAtanT (RealMark "Z") (ErrMulPow2L 2 (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")))
                ,ErrAtanT (RealMark "Z") (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y"))
            )
            ,("ErrLn_ErrMulPow2L"
                ,ErrLn (RealMark "Z") (ErrMulPow2L 2 (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")))
                ,ErrLn (RealMark "Z") (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")) 
            )
            ,("ErrExpo_ErrMulPow2L"
                ,ErrExpo (RealMark "Z") (ErrMulPow2L 2 (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")))
                ,ErrExpo (RealMark "Z") (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")) 
            )
            ]


equivEExpr__tests = testGroup "equivEExpr tests" $  map (\(msg, ee1, ee2, o) -> testCase msg (equivEExpr ee1 ee2 @?= o)) equivEExpr_testsIOs

equivEExpr_testsIOs =
            [("ErrMulPow2L_Add"
                ,ErrMulPow2L 2 (ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y"))
                ,ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")
                ,True
            )
            ,("ErrMul_Add"
                ,ErrMul (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")
                ,ErrAdd (RealMark "X") (ErrorMark "X") (RealMark "Y") (ErrorMark "Y")
                ,False
            )
            ]


