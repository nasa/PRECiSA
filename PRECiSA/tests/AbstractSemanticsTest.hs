module AbstractSemanticsTest where

import Common.ControlFlow
import Common.DecisionPath
import Common.PVSProof
import Data.Ratio
import Test.Tasty
import Test.Tasty.HUnit

import AbsPVSLang
import AbstractDomain
import AbstractSemantics
import FPrec


testAbstractSemantics = testGroup "AbstractSemantics" [semEFun__tests]


semEFun__tests = testGroup "semEFun tests" [semEFun__test1
                                           ,semEFun__test2
                                           ,semEFun__test3
                                           ,semEFun__test4
                                           ,semEFun__test5
                                           ]

semEFun__test1 = testCase "it correctly combines ACeBS" $
    actual @?= expected
    where
      actual = semEFun "f" formalParams actualParams semanticArgumentCombinations functionSemantics 1 FPSingle (LDP [])
        where
          functionName = "f"
          formalParams = [VarId "x"
                         ,VarId "y"
                         ]
          actualParams = [FVar "a"
                         ,FVar "b"
                         ]
          semanticArgumentCombinations =
            [
              [xSemantics1,ySemantics1],
              [xSemantics1,ySemantics2],
              [xSemantics2,ySemantics1],
              [xSemantics2,ySemantics2]
            ]
            where
              xSemantics1 = dummyACeb {
                conds  = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1))
                         ,(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))
                         ],
                rExprs = [Int 1,Int 2],
                eExpr  = ErrRat (toRational 1),
                decisionPath = root
                }
              xSemantics2 = dummyACeb {
                conds  = Cond [(Lt (Int 2) (Int 3),FLt (FInt 2) (FInt 3))
                         ,(Lt (Int 3) (Int 4),FLt (FInt 3) (FInt 4))
                         ],
                rExprs = [Int 3,Int 4],
                eExpr  = ErrRat (toRational 2),
                decisionPath = root
                }
              ySemantics1 = dummyACeb {
                conds  = Cond [(Lt (Int 4) (Int 5),FLt (FInt 4) (FInt 5))
                         ,(Lt (Int 5) (Int 6),FLt (FInt 5) (FInt 6))
                         ],
                rExprs = [Int 5,Int 6],
                eExpr  = ErrRat (toRational 3),
                decisionPath = root
                }
              ySemantics2 = dummyACeb {
                conds  = Cond [(Lt (Int 6) (Int 7),FLt (FInt 6) (FInt 7))
                         ,(Lt (Int 7) (Int 8),FLt (FInt 7) (FInt 8))
                         ],
                rExprs = [Int 7,Int 8],
                eExpr  = ErrRat (toRational 4),
                decisionPath = root
                }
          functionSemantics = [fSemantics1
                              ,fSemantics2
                              ]
            where
              fSemantics1 = dummyACeb {
                conds  = Cond [(Lt (Int 8) (Int 9), FLt (FInt 8) (FInt 9))
                         ,(Lt (Int 9) (Int 10),FLt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9,Int 10],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root
                }
              fSemantics2 = dummyACeb {
                conds  = Cond [(Lt (Int 10) (Int 11),FLt (FInt 10) (FInt 11)),
                          (Lt (Int 11) (Int 12),FLt (FInt 11) (FInt 12))
                         ],
                rExprs = [Int 11,Int 12],
                eExpr  = ErrRat (toRational 6),
                decisionPath = root
                }
      expected = [
        ACeb {
            conds  = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 9,Int 10],
            eExpr  = ErrRat (5 % 1),
            decisionPath = root,
            cFlow  = Stable,
            proof = SFunR "f" 1 [ErrRat (1 % 1),ErrRat (3 % 1)] [Int 1,Int 2,Int 5,Int 6] [SIntR,SIntR]
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 9,Int 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable,
            proof = SFunR "f" 1 [ErrRat (1 % 1),ErrRat (4 % 1)] [Int 1,Int 2,Int 7,Int 8] [SIntR,SIntR]
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 9,Int 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable,
            proof = SFunR "f" 1 [ErrRat (2 % 1),ErrRat (3 % 1)] [Int 3,Int 4,Int 5,Int 6] [SIntR,SIntR]
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 9,Int 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable,
            proof = SFunR "f" 1 [ErrRat (2 % 1),ErrRat (4 % 1)] [Int 3,Int 4,Int 7,Int 8] [SIntR,SIntR]
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 11,Int 12],
            eExpr = ErrRat (6 % 1),
            decisionPath = root,
            cFlow = Stable,
            proof = SFunR "f" 1 [ErrRat (1 % 1),ErrRat (3 % 1)] [Int 1,Int 2,Int 5,Int 6] [SIntR,SIntR]
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 11,Int 12],
            eExpr = ErrRat (6 % 1),
            decisionPath = root,
            cFlow = Stable,
            proof = SFunR "f" 1 [ErrRat (1 % 1),ErrRat (4 % 1)] [Int 1,Int 2,Int 7,Int 8] [SIntR,SIntR]
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 11,Int 12],
            eExpr = ErrRat (6 % 1),
            decisionPath = root,
            cFlow = Stable,
            proof = SFunR "f" 1 [ErrRat (2 % 1),ErrRat (3 % 1)] [Int 3,Int 4,Int 5,Int 6] [SIntR,SIntR]
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 11,Int 12],
            eExpr = ErrRat (6 % 1),
            decisionPath = root,
            cFlow = Stable,
            proof = SFunR "f" 1 [ErrRat (2 % 1),ErrRat (4 % 1)] [Int 3,Int 4,Int 7,Int 8] [SIntR,SIntR]
            }
        ]

semEFun__test2 = testCase "it correctly combines arguments-combinations conditions II" $
    actual @?= expected
    where
      actual = semEFun "f" formalParams actualParams semanticArgumentCombinations functionSemantics 1 FPSingle (LDP [])
        where
          functionName = "f"
          formalParams = [VarId "x"
                         ,VarId "y"
                         ]
          actualParams = [FVar "a"
                         ,FVar "b"
                         ]
          semanticArgumentCombinations = [[xSemantics1,ySemantics1]]
            where
              xSemantics1 = dummyACeb {
                conds  = Cond [(Lt (Var "0") (Int 1),FLt (FVar "0") (FInt 1))
                         ,(Lt (Var "1") (Int 2),FLt (FVar "1") (FInt 2))
                         ],
                rExprs = [Int 1,Int 2],
                eExpr  = ErrRat (toRational 1),
                decisionPath = root
                }
              ySemantics1 = dummyACeb {
                conds  = Cond [(Lt (Var "4") (Int 5),FLt (FVar "4") (FInt 5))],
                rExprs = [Int 5,Int 6],
                eExpr  = ErrRat (toRational 3),
                decisionPath = root
                }
          functionSemantics = [fSemantics1]
            where
              fSemantics1 = dummyACeb {
                conds  = Cond [(Lt (Var "8") (Int 9), FLt (FVar "8") (FInt 9))],
                rExprs = [Int 9,Int 10],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root
                }
      expected = -- []
        [
          ACeb {
            conds = Cond [
                (And (Lt (Var "8") (Int 9)) (And (Lt (Var "0") (Int 1)) (Lt (Var "4") (Int 5))),FAnd (FLt (FVar "8") (FInt 9)) (FAnd (FLt (FVar "0") (FInt 1)) (FLt (FVar "4") (FInt 5)))),
                (And (Lt (Var "8") (Int 9)) (And (Lt (Var "1") (Int 2)) (Lt (Var "4") (Int 5))),FAnd (FLt (FVar "8") (FInt 9)) (FAnd (FLt (FVar "1") (FInt 2)) (FLt (FVar "4") (FInt 5))))],
            rExprs = [Int 9,Int 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable,
            proof = SFunR "f" 1 [ErrRat (1 % 1),ErrRat (3 % 1)] [Int 1,Int 2,Int 5,Int 6] [SIntR,SIntR]
            }
        ]

semEFun__test3 = testCase "it correctly combines arguments-combinations conditions I" $
    actual @?= expected
    where
      actual = semEFun "f" formalParams actualParams semanticArgumentCombinations functionSemantics 1 FPSingle (LDP [])
        where
          functionName = "f"
          formalParams = [VarId "x"
                         ,VarId "y"
                         ]
          actualParams = [FVar "a"
                         ,FVar "b"
                         ]
          semanticArgumentCombinations = [[xSemantics1,ySemantics1]]
            where
              xSemantics1 = dummyACeb {
                conds  = Cond [(Lt (Var "0") (Int 1),FLt (FVar "0") (FInt 1))
                         ,(Lt (Var "1") (Int 2),FLt (FVar "1") (FInt 2))
                         ],
                rExprs = [Int 1,Int 2],
                eExpr  = ErrRat (toRational 1)
                }
              ySemantics1 = dummyACeb {
                conds  = Cond [(Lt (Var "4") (Int 5),FLt (FVar "4") (FInt 5))
                         ,(Lt (Var "6") (Int 7),FLt (FVar "6") (FInt 7))],
                rExprs = [Int 5,Int 6],
                eExpr  = ErrRat (toRational 3)
                }
          functionSemantics = [fSemantics1]
            where
              fSemantics1 = dummyACeb {
                conds  = Cond [(Lt (Var "8") (Int 9), FLt (FVar "8") (FInt 9))],
                rExprs = [Int 9,Int 10],
                eExpr  = ErrRat (toRational 5)
                }
      expected = [
        ACeb {
            conds = Cond [
                (And (Lt (Var "8") (Int 9)) (And (Lt (Var "0") (Int 1)) (Lt (Var "4") (Int 5))),FAnd (FLt (FVar "8") (FInt 9)) (FAnd (FLt (FVar "0") (FInt 1)) (FLt (FVar "4") (FInt 5)))),
                (And (Lt (Var "8") (Int 9)) (And (Lt (Var "0") (Int 1)) (Lt (Var "6") (Int 7))),FAnd (FLt (FVar "8") (FInt 9)) (FAnd (FLt (FVar "0") (FInt 1)) (FLt (FVar "6") (FInt 7)))),
                (And (Lt (Var "8") (Int 9)) (And (Lt (Var "1") (Int 2)) (Lt (Var "4") (Int 5))),FAnd (FLt (FVar "8") (FInt 9)) (FAnd (FLt (FVar "1") (FInt 2)) (FLt (FVar "4") (FInt 5)))),
                (And (Lt (Var "8") (Int 9)) (And (Lt (Var "1") (Int 2)) (Lt (Var "6") (Int 7))),FAnd (FLt (FVar "8") (FInt 9)) (FAnd (FLt (FVar "1") (FInt 2)) (FLt (FVar "6") (FInt 7))))
                ],
              rExprs = [Int 9,Int 10],
              eExpr = ErrRat (5 % 1),
              decisionPath = root,
              cFlow = Stable,
              proof = SFunR "f" 1 [ErrRat (1 % 1),ErrRat (3 % 1)] [Int 1,Int 2,Int 5,Int 6] [SIntR,SIntR]
            }
        ] 


semEFun__test4 = testCase "it correctly combines argument combinations" $
    actual @?= expected
    where
      actual = semEFun "f" formalParams actualParams semanticArgumentCombinations functionSemantics 1 FPSingle (LDP [])
        where
          functionName = "f"
          formalParams = [VarId "x"
                         ,VarId "y"
                         ]
          actualParams = [FVar "a"
                         ,FVar "b"
                         ]
          semanticArgumentCombinations =
            [
              [xSemantics1,ySemantics1],
              [xSemantics2,ySemantics1]
            ]
            where
              xSemantics1 = dummyACeb {
                conds  = Cond [
                    (Lt (Var "0") (Int 1),FLt (FVar "0") (FInt 1)),
                    (Lt (Var "1") (Int 2),FLt (FVar "1") (FInt 2))
                    ],
                rExprs = [Int 1,Int 2],
                eExpr  = ErrRat (toRational 1),
                decisionPath = root
                }
              xSemantics2 = dummyACeb {
                conds  = Cond [(Lt (Var "2") (Int 3),FLt (FVar "2") (FInt 3))],
                rExprs = [Int 3,Int 4],
                eExpr  = ErrRat (toRational 2),
                decisionPath = root
                }
              ySemantics1 = dummyACeb {
                conds  = Cond [(Lt (Var "4") (Int 5),FLt (FVar "4") (FInt 5))],
                rExprs = [Int 5,Int 6],
                eExpr  = ErrRat (toRational 3),
                decisionPath = root
                }
          functionSemantics = [fSemantics1]
            where
              fSemantics1 = dummyACeb {
                conds  = Cond [(Lt (Var "8") (Int 9), FLt (FVar "8") (FInt 9))],
                rExprs = [Int 9,Int 10],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root
                }
      expected = [
        ACeb {
            conds = Cond [
                (And (Lt (Var "8") (Int 9)) (And (Lt (Var "0") (Int 1)) (Lt (Var "4") (Int 5))),FAnd (FLt (FVar "8") (FInt 9)) (FAnd (FLt (FVar "0") (FInt 1)) (FLt (FVar "4") (FInt 5)))),
                (And (Lt (Var "8") (Int 9)) (And (Lt (Var "1") (Int 2)) (Lt (Var "4") (Int 5))),FAnd (FLt (FVar "8") (FInt 9)) (FAnd (FLt (FVar "1") (FInt 2)) (FLt (FVar "4") (FInt 5))))
                ],
            rExprs = [Int 9,Int 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable,
            proof = SFunR "f" 1 [ErrRat (1 % 1),ErrRat (3 % 1)] [Int 1,Int 2,Int 5,Int 6] [SIntR,SIntR]
            },
        ACeb {
            conds = Cond [(And (Lt (Var "8") (Int 9)) (And (Lt (Var "2") (Int 3)) (Lt (Var "4") (Int 5))),FAnd (FLt (FVar "8") (FInt 9)) (FAnd (FLt (FVar "2") (FInt 3)) (FLt (FVar "4") (FInt 5))))],
            rExprs = [Int 9,Int 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable,
            proof = SFunR "f" 1 [ErrRat (2 % 1),ErrRat (3 % 1)] [Int 3,Int 4,Int 5,Int 6] [SIntR,SIntR]
            }
        ]


semEFun__test5 = testCase "it correctly combines function semantics ACebS" $
    actual @?= expected
    where
      actual = semEFun "f" formalParams actualParams semanticArgumentCombinations functionSemantics 1 FPSingle (LDP [])
        where
          functionName = "f"
          formalParams = [VarId "x"
                         ,VarId "y"
                         ]
          actualParams = [FVar "a"
                         ,FVar "b"
                         ]
          semanticArgumentCombinations = [[xSemantics1,ySemantics1]]
            where
              xSemantics1 = dummyACeb {
                conds  = Cond [
                    (Lt (Var "0") (Int 1),FLt (FVar "0") (FInt 1)),
                    (Lt (Var "1") (Int 2),FLt (FVar "1") (FInt 2))
                    ],
                rExprs = [Int 1,Int 2],
                eExpr  = ErrRat (toRational 1),
                decisionPath = root
                }
              ySemantics1 = dummyACeb {
                conds  = Cond [(Lt (Var "4") (Int 5),FLt (FVar "4") (FInt 5))],
                rExprs = [Int 5,Int 6],
                eExpr  = ErrRat (toRational 3),
                decisionPath = root
                }
          functionSemantics = [fSemantics1,fSemantics2]
            where
              fSemantics1 = dummyACeb {
                conds  = Cond [(Lt (Var "8") (Int 9), FLt (FVar "8") (FInt 9))],
                rExprs = [Int 9,Int 10],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root
                }
              fSemantics2 = dummyACeb {
                conds  = Cond [(Lt (Var "10") (Int 11), FLt (FVar "10") (FInt 11))],
                rExprs = [Int 11,Int 12],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root
                }
      expected = [
        ACeb {
            conds =  Cond [
                (And (Lt (Var "8") (Int 9)) (And (Lt (Var "0") (Int 1)) (Lt (Var "4") (Int 5))),FAnd (FLt (FVar "8") (FInt 9)) (FAnd (FLt (FVar "0") (FInt 1)) (FLt (FVar "4") (FInt 5)))),
                (And (Lt (Var "8") (Int 9)) (And (Lt (Var "1") (Int 2)) (Lt (Var "4") (Int 5))),FAnd (FLt (FVar "8") (FInt 9)) (FAnd (FLt (FVar "1") (FInt 2)) (FLt (FVar "4") (FInt 5))))
                ],
            rExprs = [Int 9,Int 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable,
            proof = SFunR "f" 1 [ErrRat (1 % 1),ErrRat (3 % 1)] [Int 1,Int 2,Int 5,Int 6] [SIntR,SIntR]
            },
        ACeb {
            conds = Cond [
                (And (Lt (Var "10") (Int 11)) (And (Lt (Var "0") (Int 1)) (Lt (Var "4") (Int 5))),FAnd (FLt (FVar "10") (FInt 11)) (FAnd (FLt (FVar "0") (FInt 1)) (FLt (FVar "4") (FInt 5)))),
                (And (Lt (Var "10") (Int 11)) (And (Lt (Var "1") (Int 2)) (Lt (Var "4") (Int 5))),FAnd (FLt (FVar "10") (FInt 11)) (FAnd (FLt (FVar "1") (FInt 2)) (FLt (FVar "4") (FInt 5))))
                ],
            rExprs = [Int 11,Int 12],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable,
            proof = SFunR "f" 1 [ErrRat (1 % 1),ErrRat (3 % 1)] [Int 1,Int 2,Int 5,Int 6] [SIntR,SIntR]
            }
        ]
