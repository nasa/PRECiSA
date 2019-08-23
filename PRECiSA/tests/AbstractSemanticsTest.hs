module AbstractSemanticsTest where

import Common.ControlFlow
import Common.DecisionPath
import Data.Ratio
import Test.Tasty
import Test.Tasty.HUnit

import AbsPVSLang
import AbstractDomain
import AbstractSemantics
import FPrec


testAbstractSemantics = testGroup "AbstractSemantics"
    [semEFun__tests
    ,equivInterp__tests
    ]


equivInterp__tests = testGroup "equivInterp tests"
    [equivInterp__test1
    ,equivInterp__test2
    ,equivInterp__test3
    ,equivInterp__test4
    ]

equivInterp__test1 = testCase "Equivalent Interpretation 1" $
    equivInterp interp1 interp2 @?= False
    where
      interp1 = [("f", (FPDouble, [Arg "x" FPDouble],fSemantics))]
      interp2 = [("f", (FPSingle, [Arg "x" FPDouble],fSemantics))]
      fSemantics = [ACeb {
                conds  = Cond [(Lt (Int 8) (Int 9), FLt (FInt 8) (FInt 9))
                         ,(Lt (Int 9) (Int 10),FLt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root,
                cFlow = Stable
                }]

equivInterp__test2 = testCase "Equivalent Interpretation 2" $
    equivInterp interp1 interp2 @?= True
    where
      interp1 = [("f", (FPSingle, [Arg "x" FPDouble],fSemantics)),("g", (FPDouble, [Arg "x" FPDouble],gSemantics))]
      interp2 = [("g", (FPDouble, [Arg "x" FPDouble],gSemantics)),("f", (FPSingle, [Arg "x" FPDouble],fSemantics))]
      fSemantics = [ACeb {
                conds  = Cond [(Lt (Int 8) (Int 9), FLt (FInt 8) (FInt 9))
                         ,(Lt (Int 9) (Int 10),FLt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root,
                cFlow = Stable
                }]
      gSemantics = [ACeb {
                conds  = Cond [(Lt (Int 6) (Int 9), FLt (FInt 8) (FInt 9))
                         ,(Lt (Int 9) (Int 10),FLt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9],
                fpExprs = [FInt 9],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root,
                cFlow = Unstable
                }]

equivInterp__test3 = testCase "Equivalent Interpretation 3" $
    equivInterp interp1 interp2 @?= False
    where
      interp1 = [("f", (FPSingle, [Arg "y" FPDouble],fSemantics)),("g", (FPDouble, [Arg "x" FPDouble],gSemantics))]
      interp2 = [("g", (FPDouble, [Arg "x" FPDouble],gSemantics)),("f", (FPSingle, [Arg "x" FPDouble],fSemantics))]
      fSemantics = [ACeb {
                conds  = Cond [(Lt (Int 8) (Int 9), FLt (FInt 8) (FInt 9))
                         ,(Lt (Int 9) (Int 10),FLt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root,
                cFlow = Stable
                }]
      gSemantics = [ACeb {
                conds  = Cond [(Lt (Int 6) (Int 9), FLt (FInt 8) (FInt 9))
                         ,(Lt (Int 9) (Int 10),FLt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9],
                fpExprs = [FInt 9],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root,
                cFlow = Unstable
                }]

equivInterp__test4 = testCase "Equivalent Interpretation 3" $
    equivInterp interp1 interp2 @?= False
    where
      interp1 = [("f", (FPSingle, [Arg "x" FPDouble],fSemantics1)),("g", (FPDouble, [Arg "x" FPDouble],gSemantics))]
      interp2 = [("g", (FPDouble, [Arg "x" FPDouble],gSemantics)) ,("f", (FPSingle, [Arg "x" FPDouble],fSemantics2))]
      fSemantics1 = [ACeb {
                conds  = Cond [(Lt (Int 8) (Int 9), FLt (FInt 8) (FInt 9))
                         ,(Lt (Int 9) (Int 10),FLt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root,
                cFlow = Stable
                },
                ACeb {
                conds  = Cond [(Lt (Int 8) (Int 9), FLt (FInt 8) (FInt 9))
                         ,(Lt (Int 9) (Int 10),FLt (FInt 9) (FInt 10))
                         ],
                rExprs = [Var Real "v"],
                fpExprs = [FVar FPSingle "v"],
                eExpr  = ErrRat (toRational 0),
                decisionPath = root,
                cFlow = Unstable
                }]
      fSemantics2 = [ACeb {
                conds  = Cond [(Lt (Int 8) (Int 9), FLt (FInt 8) (FInt 9))
                         ,(Lt (Int 9) (Int 10),FLt (FInt 9) (FInt 10))
                         ],
                rExprs = [Var Real "v"],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat (toRational 0),
                decisionPath = root,
                cFlow = Unstable
                },
                ACeb {
                conds  = Cond [(Lt (Int 8) (Int 9), FLt (FInt 8) (FInt 9))
                         ,(Lt (Int 9) (Int 10),FLt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root,
                cFlow = Stable
                }]
      gSemantics = [ACeb {
                conds  = Cond [(Lt (Int 6) (Int 9), FLt (FInt 8) (FInt 9))
                         ,(Lt (Int 9) (Int 10),FLt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9],
                fpExprs = [FInt 9],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root,
                cFlow = Unstable
                }]

semEFun__tests = testGroup "semEFun tests"
    [semEFun__test1
    ,semEFun__test2
    ,semEFun__test3
    ,semEFun__test4
    ,semEFun__test5
    ]

semEFun__test1 = testCase "it correctly combines ACeBS" $
    actual @?= expected
    where
      actual = semEFun "f" formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
        where
          functionName = "f"
          formalParams = [Arg "x" FPDouble
                         ,Arg "y" FPDouble
                         ]
          actualParams = [FVar FPDouble "a"
                         ,FVar FPDouble "b"
                         ]
          semanticArgumentCombinations =
            [
              [xSemantics1,ySemantics1],
              [xSemantics1,ySemantics2],
              [xSemantics2,ySemantics1],
              [xSemantics2,ySemantics2]
            ]
            where
              xSemantics1 = ACeb {
                conds  = Cond [(Lt (Int 0) (Int 1),FLt (FInt 0) (FInt 1))
                         ,(Lt (Int 1) (Int 2),FLt (FInt 1) (FInt 2))
                         ],
                rExprs = [Int 1,Int 2],
                fpExprs = [FInt 3, FInt 2],
                eExpr  = ErrRat (toRational 1),
                decisionPath = root,
                cFlow = Stable
                }
              xSemantics2 = ACeb {
                conds  = Cond [(Lt (Int 2) (Int 3),FLt (FInt 2) (FInt 3))
                         ,(Lt (Int 3) (Int 4),FLt (FInt 3) (FInt 4))
                         ],
                rExprs = [Int 3,Int 4],
                fpExprs = [FInt 3,FInt 4],
                eExpr  = ErrRat (toRational 2),
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics1 = ACeb {
                conds  = Cond [(Lt (Int 4) (Int 5),FLt (FInt 4) (FInt 5))
                         ,(Lt (Int 5) (Int 6),FLt (FInt 5) (FInt 6))
                         ],
                rExprs = [Int 5,Int 6],
                fpExprs = [FInt 5,FInt 6],
                eExpr  = ErrRat (toRational 3),
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics2 = ACeb {
                conds  = Cond [(Lt (Int 6) (Int 7),FLt (FInt 6) (FInt 7))
                         ,(Lt (Int 7) (Int 8),FLt (FInt 7) (FInt 8))
                         ],
                rExprs = [Int 7,Int 8],
                fpExprs = [FInt 7,FInt 8],
                eExpr  = ErrRat (toRational 4),
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1
                              ,fSemantics2
                              ]
            where
              fSemantics1 = ACeb {
                conds  = Cond [(Lt (Int 8) (Int 9), FLt (FInt 8) (FInt 9))
                         ,(Lt (Int 9) (Int 10),FLt (FInt 9) (FInt 10))
                         ],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root,
                cFlow = Stable
                }
              fSemantics2 = ACeb {
                conds  = Cond [(Lt (Int 10) (Int 11),FLt (FInt 10) (FInt 11)),
                          (Lt (Int 11) (Int 12),FLt (FInt 11) (FInt 12))
                         ],
                rExprs = [Int 11,Int 12],
                fpExprs = [FInt 11,FInt 12],
                eExpr  = ErrRat (toRational 6),
                decisionPath = root,
                cFlow = Stable
                }
      expected = [
        ACeb {
            conds  = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 9,Int 10],
            fpExprs = [FInt 9,FInt 10],
            eExpr  = ErrRat (5 % 1),
            decisionPath = root,
            cFlow  = Stable
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 9,Int 10],
            fpExprs = [FInt 9,FInt 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 9,Int 10],
            fpExprs = [FInt 9,FInt 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 9,Int 10],
            fpExprs = [FInt 9,FInt 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 11,Int 12],
            fpExprs = [FInt 11,FInt 12],
            eExpr = ErrRat (6 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 11,Int 12],
            fpExprs = [FInt 11,FInt 12],
            eExpr = ErrRat (6 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 11,Int 12],
            fpExprs = [FInt 11,FInt 12],
            eExpr = ErrRat (6 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Cond [(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue),(BTrue,FBTrue)],
            rExprs = [Int 11,Int 12],
            fpExprs = [FInt 11,FInt 12],
            eExpr = ErrRat (6 % 1),
            decisionPath = root,
            cFlow = Stable
            }
        ]

semEFun__test2 = testCase "it correctly combines arguments-combinations conditions II" $
    actual @?= expected
    where
      actual = semEFun "f" formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
        where
          functionName = "f"
          formalParams = [Arg "x" FPDouble
                         ,Arg "y" FPDouble
                         ]
          actualParams = [FVar FPDouble "a"
                         ,FVar FPDouble "b"
                         ]
          semanticArgumentCombinations = [[xSemantics1,ySemantics1]]
            where
              xSemantics1 = ACeb {
                conds  = Cond [
                          (Lt (Var Real "0") (Int 1),FLt (FVar FPDouble "0") (FInt 1))
                         ,(Lt (Var Real "1") (Int 2),FLt (FVar FPDouble "1") (FInt 2))
                         ],
                rExprs = [Int 1,Int 2],
                fpExprs = [FInt 1,FInt 2],
                eExpr  = ErrRat (toRational 1),
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics1 = ACeb {
                conds  = Cond [(Lt (Var Real "4") (Int 5),FLt (FVar FPDouble "4") (FInt 5))],
                rExprs = [Int 5,Int 6],
                fpExprs = [FInt 5,FInt 6],
                eExpr  = ErrRat (toRational 3),
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1]
            where
              fSemantics1 = ACeb {
                conds  = Cond [(Lt (Var Real "8") (Int 9), FLt (FVar FPDouble "8") (FInt 9))],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root,
                cFlow = Stable
                }
      expected = -- []
        [
          ACeb {
            conds = Cond [
                (And (Lt (Var Real "8") (Int 9)) (And (Lt (Var Real "0") (Int 1)) (Lt (Var Real "4") (Int 5)))
                ,FAnd (FLt (FVar FPDouble "8") (FInt 9)) (FAnd (FLt (FVar FPDouble "0") (FInt 1)) (FLt (FVar FPDouble "4") (FInt 5)))),
                (And (Lt (Var Real "8") (Int 9)) (And (Lt (Var Real "1") (Int 2)) (Lt (Var Real "4") (Int 5)))
                ,FAnd (FLt (FVar FPDouble "8") (FInt 9)) (FAnd (FLt (FVar FPDouble "1") (FInt 2)) (FLt (FVar FPDouble "4") (FInt 5))))],
            rExprs = [Int 9,Int 10],
            fpExprs = [FInt 9,FInt 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            }
        ]

semEFun__test3 = testCase "it correctly combines arguments-combinations conditions I" $
    actual @?= expected
    where
      actual = semEFun "f" formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
        where
          functionName = "f"
          formalParams = [Arg "x" FPDouble
                         ,Arg "y" FPDouble
                         ]
          actualParams = [FVar FPDouble "a"
                         ,FVar FPDouble "b"
                         ]
          semanticArgumentCombinations = [[xSemantics1,ySemantics1]]
            where
              xSemantics1 = ACeb {
                conds  = Cond [(Lt (Var Real "0") (Int 1),FLt (FVar FPDouble "0") (FInt 1))
                         ,(Lt (Var Real "1") (Int 2),FLt (FVar FPDouble "1") (FInt 2))
                         ],
                rExprs = [Int 1,Int 2],
                fpExprs = [FInt 1,FInt 2],
                eExpr  = ErrRat (toRational 1),
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics1 = ACeb {
                conds  = Cond [(Lt (Var Real "4") (Int 5),FLt (FVar FPDouble "4") (FInt 5))
                         ,(Lt (Var Real "6") (Int 7),FLt (FVar FPDouble "6") (FInt 7))],
                rExprs = [Int 5,Int 6],
                fpExprs = [FInt 5,FInt 6],
                eExpr  = ErrRat (toRational 3),
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1]
            where
              fSemantics1 = ACeb {
                conds  = Cond [(Lt (Var Real "8") (Int 9), FLt (FVar FPDouble "8") (FInt 9))],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root,
                cFlow = Stable
                }
      expected = [
        ACeb {
            conds = Cond [
                (And (Lt (Var Real "8") (Int 9)) (And (Lt (Var Real "0") (Int 1)) (Lt (Var Real "4") (Int 5)))
                ,FAnd (FLt (FVar FPDouble "8") (FInt 9)) (FAnd (FLt (FVar FPDouble "0") (FInt 1)) (FLt (FVar FPDouble "4") (FInt 5)))),
                (And (Lt (Var Real "8") (Int 9)) (And (Lt (Var Real "0") (Int 1)) (Lt (Var Real "6") (Int 7)))
                ,FAnd (FLt (FVar FPDouble "8") (FInt 9)) (FAnd (FLt (FVar FPDouble "0") (FInt 1)) (FLt (FVar FPDouble "6") (FInt 7)))),
                (And (Lt (Var Real "8") (Int 9)) (And (Lt (Var Real "1") (Int 2)) (Lt (Var Real "4") (Int 5)))
                ,FAnd (FLt (FVar FPDouble "8") (FInt 9)) (FAnd (FLt (FVar FPDouble "1") (FInt 2)) (FLt (FVar FPDouble "4") (FInt 5)))),
                (And (Lt (Var Real "8") (Int 9)) (And (Lt (Var Real "1") (Int 2)) (Lt (Var Real "6") (Int 7)))
                ,FAnd (FLt (FVar FPDouble "8") (FInt 9)) (FAnd (FLt (FVar FPDouble "1") (FInt 2)) (FLt (FVar FPDouble "6") (FInt 7))))
                ],
              rExprs = [Int 9,Int 10],
              fpExprs = [FInt 9,FInt 10],
              eExpr = ErrRat (5 % 1),
              decisionPath = root,
              cFlow = Stable
            }
        ] 


semEFun__test4 = testCase "it correctly combines argument combinations" $
    actual @?= expected
    where
      actual = semEFun "f" formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
        where
          functionName = "f"
          formalParams = [Arg "x" FPDouble
                         ,Arg "y" FPDouble
                         ]
          actualParams = [FVar FPDouble "a"
                         ,FVar FPDouble "b"
                         ]
          semanticArgumentCombinations =
            [
              [xSemantics1,ySemantics1],
              [xSemantics2,ySemantics1]
            ]
            where
              xSemantics1 = ACeb {
                conds  = Cond [
                    (Lt (Var Real "0") (Int 1),FLt (FVar FPDouble "0") (FInt 1)),
                    (Lt (Var Real "1") (Int 2),FLt (FVar FPDouble "1") (FInt 2))
                    ],
                rExprs = [Int 1,Int 2],
                fpExprs = [FInt 1,FInt 2],
                eExpr  = ErrRat (toRational 1),
                decisionPath = root,
                cFlow = Stable
                }
              xSemantics2 = ACeb {
                conds  = Cond [(Lt (Var Real "2") (Int 3),FLt (FVar FPDouble "2") (FInt 3))],
                rExprs = [Int 3,Int 4],
                fpExprs = [FInt 3,FInt 4],
                eExpr  = ErrRat (toRational 2),
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics1 = ACeb {
                conds  = Cond [(Lt (Var Real "4") (Int 5),FLt (FVar FPDouble "4") (FInt 5))],
                rExprs = [Int 5,Int 6],
                fpExprs = [FInt 5,FInt 6],
                eExpr  = ErrRat (toRational 3),
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1]
            where
              fSemantics1 = ACeb {
                conds  = Cond [(Lt (Var Real "8") (Int 9), FLt (FVar FPDouble "8") (FInt 9))],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root,
                cFlow = Stable
                }
      expected = [
        ACeb {
            conds = Cond [
                (And (Lt (Var Real "8") (Int 9)) (And (Lt (Var Real "0") (Int 1)) (Lt (Var Real "4") (Int 5)))
                ,FAnd (FLt (FVar FPDouble "8") (FInt 9)) (FAnd (FLt (FVar FPDouble "0") (FInt 1)) (FLt (FVar FPDouble "4") (FInt 5)))),
                (And (Lt (Var Real "8") (Int 9)) (And (Lt (Var Real "1") (Int 2)) (Lt (Var Real "4") (Int 5)))
                ,FAnd (FLt (FVar FPDouble "8") (FInt 9)) (FAnd (FLt (FVar FPDouble "1") (FInt 2)) (FLt (FVar FPDouble "4") (FInt 5))))
                ],
            rExprs = [Int 9,Int 10],
            fpExprs = [FInt 9,FInt 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Cond [
                  (And (Lt (Var Real "8") (Int 9)) (And (Lt (Var Real "2") (Int 3)) (Lt (Var Real "4") (Int 5)))
                  ,FAnd (FLt (FVar FPDouble "8") (FInt 9)) (FAnd (FLt (FVar FPDouble "2") (FInt 3)) (FLt (FVar FPDouble "4") (FInt 5))))],
            rExprs = [Int 9,Int 10],
            fpExprs = [FInt 9,FInt 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            }
        ]


semEFun__test5 = testCase "it correctly combines function semantics ACebS" $
    actual @?= expected
    where
      actual = semEFun "f" formalParams actualParams semanticArgumentCombinations functionSemantics 1 (LDP [])
        where
          functionName = "f"
          formalParams = [Arg "x" FPDouble
                         ,Arg "y" FPDouble
                         ]
          actualParams = [FVar FPDouble "a"
                         ,FVar FPDouble "b"
                         ]
          semanticArgumentCombinations = [[xSemantics1,ySemantics1]]
            where
              xSemantics1 = ACeb {
                conds  = Cond [
                    (Lt (Var Real "0") (Int 1),FLt (FVar FPDouble "0") (FInt 1)),
                    (Lt (Var Real "1") (Int 2),FLt (FVar FPDouble "1") (FInt 2))
                    ],
                rExprs = [Int 1,Int 2],
                fpExprs = [FInt 1,FInt 2],
                eExpr  = ErrRat (toRational 1),
                decisionPath = root,
                cFlow = Stable
                }
              ySemantics1 = ACeb {
                conds  = Cond [(Lt (Var Real "4") (Int 5),FLt (FVar FPDouble "4") (FInt 5))],
                rExprs = [Int 5,Int 6],
                fpExprs = [FInt 5,FInt 6],
                eExpr  = ErrRat (toRational 3),
                decisionPath = root,
                cFlow = Stable
                }
          functionSemantics = [fSemantics1,fSemantics2]
            where
              fSemantics1 = ACeb {
                conds  = Cond [(Lt (Var Real "8") (Int 9), FLt (FVar FPDouble "8") (FInt 9))],
                rExprs = [Int 9,Int 10],
                fpExprs = [FInt 9,FInt 10],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root,
                cFlow = Stable
                }
              fSemantics2 = ACeb {
                conds  = Cond [(Lt (Var Real "10") (Int 11), FLt (FVar FPDouble "10") (FInt 11))],
                rExprs = [Int 11,Int 12],
                fpExprs = [FInt 11,FInt 12],
                eExpr  = ErrRat (toRational 5),
                decisionPath = root,
                cFlow = Stable
                }
      expected = [
        ACeb {
            conds =  Cond [
                (And (Lt (Var Real "8") (Int 9)) (And (Lt (Var Real "0") (Int 1)) (Lt (Var Real "4") (Int 5)))
                ,FAnd (FLt (FVar FPDouble "8") (FInt 9)) (FAnd (FLt (FVar FPDouble "0") (FInt 1)) (FLt (FVar FPDouble "4") (FInt 5)))),
                (And (Lt (Var Real "8") (Int 9)) (And (Lt (Var Real "1") (Int 2)) (Lt (Var Real "4") (Int 5)))
                ,FAnd (FLt (FVar FPDouble "8") (FInt 9)) (FAnd (FLt (FVar FPDouble "1") (FInt 2)) (FLt (FVar FPDouble "4") (FInt 5))))
                ],
            rExprs = [Int 9,Int 10],
            fpExprs = [FInt 9,FInt 10],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            },
        ACeb {
            conds = Cond [
                (And (Lt (Var Real "10") (Int 11)) (And (Lt (Var Real "0") (Int 1)) (Lt (Var Real "4") (Int 5))),FAnd (FLt (FVar FPDouble "10") (FInt 11)) (FAnd (FLt (FVar FPDouble "0") (FInt 1)) (FLt (FVar FPDouble "4") (FInt 5)))),
                (And (Lt (Var Real "10") (Int 11)) (And (Lt (Var Real "1") (Int 2)) (Lt (Var Real "4") (Int 5))),FAnd (FLt (FVar FPDouble "10") (FInt 11)) (FAnd (FLt (FVar FPDouble "1") (FInt 2)) (FLt (FVar FPDouble "4") (FInt 5))))
                ],
            rExprs = [Int 11,Int 12],
            fpExprs = [FInt 11,FInt 12],
            eExpr = ErrRat (5 % 1),
            decisionPath = root,
            cFlow = Stable
            }
        ]
