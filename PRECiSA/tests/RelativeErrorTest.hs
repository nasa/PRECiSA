module RelativeErrorTest where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson (encode, toJSON)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List (isInfixOf, isPrefixOf)
import Data.Ratio ((%))
import Numeric.IEEE (succIEEE)

import AbsPVSLang
import AbsSpecLang
import AbstractDomain (ACeb(..), Condition(..), Conditions(..), FResult(..), RResult(..), trueCond)
import AnalysisResult (FunSummary(..), RelErrorResult(..))
import Common.DecisionPath (root)
import Common.ControlFlow (ControlFlow(..))
import JSON (toAnalysisResultFun)
import Kodiak.Paver (SearchParameters(..))
import Operators
import PPExt (render, text)
import Certificate.Symbolic (prPvsLemma, prPvsRelLemma)
import Certificate.Numerical (prPvsNumRelLemma, prPvsNumRelProof)
import RelativeError
import TestUtils

testRelativeError = testGroup "Relative Error"
  [testClassifyRatio
  ,testIsZeroError
  ,testRatioExpr
  ,testLiteralDenomContainsZero
  ,testOneLineMessage
  ,testSafeQuotient
  ,testComputeRelError
  ,testRelErrorJSON
  ,testFunSummaryJSON
  ,testRelLemma
  ,testNumRelLemma
  ,testNumRelProof
  ]

testClassifyRatio = testGroup "classifyRatio"
  [testCase "finite bound is kept" $
     classifyRatio 1.5e-16 @?= RelFinite 1.5e-16
  ,testCase "zero is finite" $
     classifyRatio 0 @?= RelFinite 0
  ,testCase "infinity becomes RelInfinite" $
     classifyRatio (1/0) @?= RelInfinite
  ,testCase "NaN becomes RelInfinite" $
     classifyRatio (0/0) @?= RelInfinite
  ,testCase "negative infinity becomes RelInfinite" $
     classifyRatio (-1/0) @?= RelInfinite
  ]

testIsZeroError = testGroup "isZeroError"
  [testCase "Int 0 is zero" $
     isZeroError (Int 0) @?= True
  ,testCase "Rat 0 is zero" $
     isZeroError (Rat 0) @?= True
  ,testCase "Int 1 is not zero" $
     isZeroError (Int 1) @?= False
  ,testCase "a variable is not zero" $
     isZeroError (Var Real "x") @?= False
  ]

testRatioExpr = testGroup "ratioExpr"
  [testCase "single alternative divides by its absolute value" $
     ratioExpr (Var Real "e") [Var Real "r"]
       @?= BinaryOp DivOp (Var Real "e") (UnaryOp AbsOp (Var Real "r"))
  ,testCase "several alternatives take the maximum of the ratios" $
     ratioExpr (Var Real "e") [Var Real "r1", Var Real "r2"]
       @?= MaxErr [BinaryOp DivOp (Var Real "e") (UnaryOp AbsOp (Var Real "r1"))
                  ,BinaryOp DivOp (Var Real "e") (UnaryOp AbsOp (Var Real "r2"))]
  ,testCase "no alternatives is an error" $
     throwsException $ ratioExpr (Var Real "e") []
  ]

testLiteralDenomContainsZero = testGroup "literalDenomContainsZero"
  [testCase "Int 0 contains zero"      $ literalDenomContainsZero (Int 0) @?= True
  ,testCase "Rat 0 contains zero"      $ literalDenomContainsZero (Rat 0) @?= True
  ,testCase "Int 3 does not"           $ literalDenomContainsZero (Int 3) @?= False
  ,testCase "a variable is not literal"$ literalDenomContainsZero (Var Real "x") @?= False
  ,testCase "an interval straddling zero contains zero" $
     literalDenomContainsZero (Interval (-1) 2) @?= True
  ,testCase "a strictly positive interval does not" $
     literalDenomContainsZero (Interval 1 2) @?= False
  ]

-- The failure message ends up verbatim in the JSON field
-- relativeStableErrorFailure, which the VSCode extension shows to a user, so
-- it must be one line and must not name a source location: 'show' on an
-- 'ErrorCall' appends the HasCallStack backtrace, and the file:line in it goes
-- stale the moment the file it names changes.
--
-- Tested on the formatter directly rather than through 'computeRelError' so
-- the pin does not depend on a Kodiak run, on which constructors
-- 'Kodiak.Runner.run'' happens to lack a case for, or on GHC's exact backtrace
-- layout.
testOneLineMessage = testGroup "oneLineMessage"
  [testCase "a call stack is stripped, the message kept" $
     oneLineMessage
       ("KodiakRunnable instance for AExpr, VariableMap and PReal undefined\n"
     ++ "CallStack (from HasCallStack):\n"
     ++ "  error, called at src/Kodiak/Runner.hs:504:20 in precisa:Kodiak.Runner")
       @?= "KodiakRunnable instance for AExpr, VariableMap and PReal undefined"

  -- The Kodiak shim's own messages are already one line and carry the only
  -- diagnostic detail there is; nothing may be trimmed off them.
  ,testCase "a single-line message is left alone" $
     oneLineMessage "Kodiak (eval): sqrt expects a nonnegative interval"
       @?= "Kodiak (eval): sqrt expects a nonnegative interval"

  ,testCase "no newline survives a multi-line message" $
     assertBool "a newline leaked through"
       (not ("\n" `isInfixOf` oneLineMessage "first\nsecond\nthird"))

  ,testCase "surrounding whitespace is trimmed" $
     oneLineMessage "  spaced out \t\r\nCallStack (from HasCallStack):\n  error"
       @?= "spaced out"

  ,testCase "an empty message stays empty" $
     oneLineMessage "" @?= ""
  ]

-- The quotient maxE / d is a REPORTED BOUND and the constant of a PVS lemma, so
-- it may never be rounded down: a bound one ulp below the true quotient is not
-- a bound. 1/3 is the witness that hardware division is not good enough -- it
-- rounds to nearest, and for 1/3 nearest is BELOW the exact value.
testSafeQuotient = testGroup "safeQuotient"
  [testCase "1/3 rounds down in Double, so the helper must step up" $ do
     -- the premise of the test: plain division really does land below 1/3
     assertBool "premise broken: 1/3 does not round down in this Double"
                (toRational (1/3 :: Double) < (1 % 3))
     assertBool ("expected a value above the Double quotient, got "
                 ++ show (safeQuotient 1 3))
                (safeQuotient 1 3 > (1/3 :: Double))

  ,testCase "the result is never below the exact quotient" $
     assertBool "the returned Double is below the exact quotient"
                (toRational (safeQuotient 1 3) >= (1 % 3))

  ,testCase "it steps up by exactly one ulp, not more" $
     safeQuotient 1 3 @?= succIEEE (1/3 :: Double)

  -- ... and an exactly representable quotient is left alone: rounding up an
  -- exact value would loosen every bound that did not need it.
  ,testCase "an exact quotient is returned unchanged" $
     safeQuotient 1 4 @?= 0.25

  ,testCase "an exact quotient of realistic magnitudes is unchanged" $
     safeQuotient 1.0e-13 2 @?= 5.0e-14

  ,testCase "the exact-value check is inclusive, not strict" $
     assertBool "an exact quotient was stepped up anyway"
                (toRational (safeQuotient 3 8) == (3 % 8))
  ]

params :: SearchParameters
params = SP { maximumDepth = 7, minimumPrecision = 14 }

binds :: Integer -> Integer -> [VarBind]
binds lo hi = [VarBind "x" ResValue FPDouble (LBInt lo) (UBInt hi)]

-- | @q(x) = x*x - x + 1@, whose true minimum on [0,1] is 0.75 but whose naive
--   interval enclosure there is [0,2] -- x occurs twice, so the enclosure
--   straddles zero even though q does not come near it. Kodiak will not divide
--   by it; that is exactly what the denominator-floor fallback is for.
q :: AExpr
q = BinaryOp AddOp (BinaryOp SubOp (BinaryOp MulOp (Var Real "x") (Var Real "x"))
                                   (Var Real "x"))
                   (Int 1)

testComputeRelError = testGroup "computeRelError"
  [testCase "zero error short-circuits without running Kodiak" $ do
     r <- computeRelError params "f" (binds 2 3) (Int 0) 0 [Var Real "x"]
     r @?= Right (RelFinite 0)

  ,testCase "denominator bounded away from zero gives a finite bound" $ do
     -- max (1 / abs x) over x in [2,4] = 0.5
     r <- computeRelError params "f" (binds 2 4) (Int 1) 1 [Var Real "x"]
     case r of
       Right (RelFinite ub) -> assertBool ("expected ~0.5, got " ++ show ub)
                                          (ub >= 0.5 && ub <= 0.6)
       other -> assertFailure ("expected a finite bound, got " ++ show other)

  -- Both routes fail here, and that is right: abs(x) really does reach zero
  -- inside [-1,1], so the divisor enclosure contains zero AND no positive floor
  -- exists for the fallback to divide by. A finite bound here would be unsound.
  ,testCase "a divisor that genuinely reaches zero gives RelInfinite" $ do
     r <- computeRelError params "f" (binds (-1) 1) (Int 1) 1 [Var Real "x"]
     r @?= Right RelInfinite

  -- The DENOMINATOR-FLOOR FALLBACK. q(x) = x*x - x + 1 is nowhere zero on
  -- [0,1] -- its true minimum is 0.75 -- but each x occurs more than once, so
  -- the naive interval enclosure at the TOP box is [0,2] and Kodiak refuses to
  -- divide by it. Minimizing abs(q) involves no division, cannot trip that
  -- guard, and does profit from subdivision, so it proves a positive floor and
  -- maxE / floor is reported: 1 / 0.75 = 1.333...
  ,testCase "a floor on the denominator rescues a ratio Kodiak would not divide" $ do
     r <- computeRelError params "f" (binds 0 1) (Int 1) 1 [q]
     case r of
       Right (RelFinite ub) -> assertBool ("expected 1.33 <= ub <= 2, got " ++ show ub)
                                          (ub >= 1.33 && ub <= 2)
       other -> assertFailure ("expected a finite bound from the fallback, got "
                               ++ show other)

  -- The fallback divides the ABSOLUTE bound it is handed, so the bound it
  -- reports must scale with it. Same box, same floor, ten times the numerator.
  ,testCase "the fallback bound scales with the absolute bound it is given" $ do
     r1 <- computeRelError params "f" (binds 0 1) (Int 1) 1 [q]
     r2 <- computeRelError params "f" (binds 0 1) (Int 1) 10 [q]
     case (r1, r2) of
       (Right (RelFinite a), Right (RelFinite b)) ->
         assertBool ("expected b ~ 10*a, got " ++ show (a,b))
                    (b >= 9.9 * a && b <= 10.1 * a)
       other -> assertFailure ("expected two finite bounds, got " ++ show other)

  -- A floor of zero proves nothing about the quotient. abs(q) does reach zero
  -- on [-1,1] (q(x) = x*x - x - 1 has a root there), so the minimization
  -- succeeds and returns a NON-POSITIVE floor -- which must stay 'RelInfinite'
  -- rather than become a division by (or near) zero.
  ,testCase "a floor that is not positive keeps the bound infinite" $ do
     r <- computeRelError params "f" (binds (-1) 1) (Int 1) 1
            [BinaryOp SubOp (BinaryOp SubOp (BinaryOp MulOp (Var Real "x") (Var Real "x"))
                                            (Var Real "x"))
                            (Int 1)]
     r @?= Right RelInfinite

  ,testCase "literal zero denominator gives RelInfinite without aborting" $ do
     r <- computeRelError params "f" (binds 2 4) (Int 1) 1 [Int 0]
     r @?= Right RelInfinite

  ,testCase "correlated numerator and denominator stay tight" $ do
     -- E = x, r = x*x over [1,1000]. Jointly max (x / x^2) = 1 at x=1.
     -- Bounding separately would give max x / min x^2 = 1000.
     r <- computeRelError params "f" (binds 1 1000) (Var Real "x") 1000
                          [BinaryOp MulOp (Var Real "x") (Var Real "x")]
     case r of
       -- The lower bound matters too: a bug that collapses the numerator would
       -- give 0 and sail past an upper-bound-only check.
       Right (RelFinite ub) -> assertBool ("expected 1 <= ub <= 20, got " ++ show ub)
                                          (ub >= 1 && ub <= 20)
       other -> assertFailure ("expected a finite bound, got " ++ show other)

  ,testCase "a Kodiak failure is an error, not RelInfinite" $ do
     r <- computeRelError params "f" (binds 2 4)
            (UnaryOp SqrtOp (UnaryOp NegOp (Var Real "x"))) 1 [Var Real "x"]
     case r of
       Left msg -> assertBool msg ("sqrt" `isInfixOf` msg)
       other    -> assertFailure ("expected Left, got " ++ show other)

  ,testCase "no real expression for the path is an error, not a bound" $ do
     r <- computeRelError params "f" (binds 2 4) (Int 1) 1 []
     r @?= Left "no real expression for this path"

  -- Regression: Kodiak folds these divisors to a value interval containing
  -- zero, so its construction-time check fires. Predicting that from the
  -- PRECiSA AST is impossible; building the division through the guarded
  -- builder is what keeps the process alive. Before that fix both aborted the
  -- test process with SIGABRT.
  ,testCase "a divisor Kodiak folds to zero gives RelInfinite, no abort" $ do
     r <- computeRelError params "f" (binds 2 4) (Int 1) 1
                          [BinaryOp SubOp (Var Real "x") (Var Real "x")]
     r @?= Right RelInfinite

  ,testCase "a negated zero literal divisor gives RelInfinite, no abort" $ do
     r <- computeRelError params "f" (binds 2 4) (Int 1) 1 [UnaryOp NegOp (Rat 0)]
     r @?= Right RelInfinite

  -- Regression: real result expressions reach 'computeRelError' RAW -- they
  -- are never unfolded and never simplified -- so they can hold constructors
  -- 'Kodiak.Runner.run'' has no case for, and its catch-all raises an
  -- 'ErrorCall'. Relative error is opt-in and must never sink an otherwise
  -- successful absolute analysis, so that has to come back as 'Left' with the
  -- process still standing. Before the fix this killed precisa outright on any
  -- program with a function call and --unfold-fun-calls off.
  ,testCase "an unsupported real expression is a soft failure, not a crash" $ do
     r <- computeRelError params "f" (binds 2 4) (Int 1) 1
            [EFun "g" ResValue Real [Var Real "x"]]
     case r of
       Left msg -> do assertBool msg ("relative error unavailable" `isInfixOf` msg)
                      assertBool msg ("EFun" `isInfixOf` msg)
                      -- and the whole way through, not just in the formatter:
                      -- no backtrace and no newline reach the JSON field
                      assertBool msg (not ("CallStack" `isInfixOf` msg))
                      assertBool msg (not ("\n" `isInfixOf` msg))
       other    -> assertFailure ("expected Left, got " ++ show other)

  -- ... and the process really is still usable afterwards, which is the whole
  -- point of failing soft rather than aborting.
  ,testCase "a later path still computes after an unsupported one failed" $ do
     _ <- computeRelError params "f" (binds 2 4) (Int 1) 1
            [EFun "g" ResValue Real [Var Real "x"]]
     r <- computeRelError params "f" (binds 2 4) (Int 1) 1 [Var Real "x"]
     case r of
       Right (RelFinite ub) -> assertBool ("expected ~0.5, got " ++ show ub)
                                          (ub >= 0.5 && ub <= 0.6)
       other -> assertFailure ("expected a finite bound, got " ++ show other)
  ]

testRelErrorJSON = testGroup "relative error JSON encoding"
  [testCase "finite bound encodes as the bare number" $
     encode (toJSON (RelFinite 1.5e-16)) @?= encode (toJSON (1.5e-16 :: Double))
  ,testCase "infinite bound encodes as a string" $
     encode (toJSON RelInfinite) @?= encode (toJSON ("infinity" :: String))
  ]

-- | A one-line summary carrying the given stable relative error outcome.
summaryWith :: RelErrorResult -> FunSummary
summaryWith rel = FunSummary { fsName        = "f"
                             , fsField       = ResValue
                             , fsStable      = 1.0
                             , fsUnstable    = Nothing
                             , fsRelStable   = rel
                             , fsRelUnstable = RelErrorOff
                             }

encodedSummary :: RelErrorResult -> String
encodedSummary = unpack . encode . toAnalysisResultFun . summaryWith

-- | True when the encoding carries the given key. Matched with its quotes and
--   its colon so that a key cannot be confused with one it is a prefix of.
hasKey :: String -> String -> Bool
hasKey key s = ("\"" ++ key ++ "\":") `isInfixOf` s

-- | An encoded summary whose two sides carry DIFFERENT outcomes, which is what
--   the per-side failure keys exist to distinguish.
encodedBothSides :: RelErrorResult -> RelErrorResult -> String
encodedBothSides stable unstable =
  unpack . encode . toAnalysisResultFun $
    (summaryWith stable) { fsUnstable = Just 2.0, fsRelUnstable = unstable }

-- | As 'encodedSummary', but with the absolute errors chosen by the caller.
encodedSummary' :: Double -> Maybe Double -> RelErrorResult -> String
encodedSummary' stable unstable rel =
  unpack . encode . toAnalysisResultFun $
    (summaryWith rel) { fsStable = stable, fsUnstable = unstable }

-- The VSCode extension consumes a fixed contract: relativeStableError is a
-- number, the string "infinity", or ABSENT. Absent means no bound is
-- available, whether because the feature is off or because it failed.
--
-- Keys are matched WITH their quotes and trailing colon throughout, because
-- "relativeStableError" is a proper substring of "relativeStableErrorFailure":
-- an unquoted isInfixOf cannot tell a bound from a failure report.
testFunSummaryJSON = testGroup "FunSummary JSON encoding"
  [testCase "the feature being off emits no relative key at all" $ do
     let s = encodedSummary RelErrorOff
     assertBool s (not (hasKey "relativeStableError" s))
     assertBool s (not (hasKey "relativeStableErrorFailure" s))
     assertBool s (not (hasKey "relativeUnstableErrorFailure" s))

  ,testCase "a finite bound emits relativeStableError and no failure" $ do
     let s = encodedSummary (RelErrorBound (RelFinite 1.5e-16))
     assertBool s (hasKey "relativeStableError" s)
     assertBool s (not (hasKey "relativeStableErrorFailure" s))

  ,testCase "an infinite bound emits the string \"infinity\"" $ do
     let s = encodedSummary (RelErrorBound RelInfinite)
     assertBool s ("\"relativeStableError\":\"infinity\"" `isInfixOf` s)
     assertBool s (not (hasKey "relativeStableErrorFailure" s))

  ,testCase "a failure emits relativeStableErrorFailure and NO bound" $ do
     let s = encodedSummary (RelErrorFailed "kodiak fell over")
     assertBool s (hasKey "relativeStableErrorFailure" s)
     assertBool s ("kodiak fell over" `isInfixOf` s)
     assertBool s (not (hasKey "relativeStableError" s))

  -- The reason the failure keys are per side. One shared key reported this
  -- program as both bounded and failed, with nothing to say which side broke.
  ,testCase "a failure on one side leaves the other side's bound intact" $ do
     let s = encodedBothSides (RelErrorBound (RelFinite 1.5e-16))
                              (RelErrorFailed "kodiak fell over")
     assertBool s (hasKey "relativeStableError" s)
     assertBool s (not (hasKey "relativeStableErrorFailure" s))
     assertBool s (hasKey "relativeUnstableErrorFailure" s)
     assertBool s (not (hasKey "relativeUnstableError" s))

  ,testCase "the sides' failure keys do not leak into each other" $ do
     let s = encodedBothSides (RelErrorFailed "stable side broke")
                              (RelErrorBound (RelFinite 2.5e-16))
     assertBool s (hasKey "relativeStableErrorFailure" s)
     assertBool s ("stable side broke" `isInfixOf` s)
     assertBool s (not (hasKey "relativeStableError" s))
     assertBool s (hasKey "relativeUnstableError" s)
     assertBool s (not (hasKey "relativeUnstableErrorFailure" s))

  -- The regression baseline is byte-for-byte, so with the feature off the
  -- encoding must be EXACTLY what it was before the feature existed. Compared
  -- whole, not by isInfixOf: "\"stableError\":1" is happily satisfied by
  -- "\"stableError\":1.0", which is precisely the drift that has to be caught.
  ,testCase "with the feature off the encoding is byte-for-byte the old one" $
     encodedSummary RelErrorOff
       @?= "{\"function\":\"f\",\"stableError\":1,\"unstableError\":null}"

  -- Integral and very large doubles are where aeson's two number paths differ:
  -- via Scientific (the class default toEncoding, i.e. value . toJSON) 0 and 2
  -- render as "0" and "2"; via E.double they render as "0.0" and "2.0", and
  -- 1.6072157233039065e40 renders in exponent form rather than in full. Both
  -- are numerically right and byte-wise wrong, so pin the bytes.
  ,testCase "integral doubles keep their integral rendering" $
     encodedSummary' 0 (Just 2) RelErrorOff
       @?= "{\"function\":\"f\",\"stableError\":0,\"unstableError\":2}"

  ,testCase "a large double keeps its expanded rendering" $
     encodedSummary' 1.6072157233039065e40 Nothing RelErrorOff
       @?= "{\"function\":\"f\",\"stableError\":"
        ++ "16072157233039065000000000000000000000000,\"unstableError\":null}"
  ]

-- The symbolic bridge lemma. It is range free: 'rel' is universally quantified
-- and the bound on the error function is a hypothesis, so the numeric
-- certificate can later instantiate 'rel' with the computed constant.
--
-- The bridge lemma is proved by citing the absolute error lemma for the same
-- path, so it must repeat that lemma's hypotheses verbatim. Deliberately no
-- rational literals appear in this ACeb: real programs put things like
-- finite?_double((5 / 128)) in the hypotheses, and those slashes would mask
-- the "no division" assertion below.
relAceb :: ACeb
relAceb = ACeb { conds        = Conds []
               , rExprs       = RDeclRes [BinaryOp AddOp (Var Real "x") (Var Real "y")]
               , fpExprs      = FDeclRes [BinaryFPOp AddOp FPDouble
                                            (FVar FPDouble "x") (FVar FPDouble "y")]
               , eExpr        = Just (ErrRat 1)
               , decisionPath = root
               , cFlow        = Stable
               }

relLemmaArgs :: [Arg]
relLemmaArgs = [Arg "x" FPDouble, Arg "y" FPDouble]

renderedRelLemma :: String
renderedRelLemma = render (prPvsRelLemma "f" "f_real" ResValue relLemmaArgs
                                         relAceb FPDouble 0 [])

-- The absolute lemma for the very same path, for comparing hypotheses.
renderedAbsLemma :: String
renderedAbsLemma = render (prPvsLemma "f" "f_real" ResValue relLemmaArgs
                                      relAceb FPDouble 0 [])

testRelLemma = testGroup "prPvsRelLemma"
  [testCase "the lemma is named f_rel_<path> and is a LEMMA" $ do
     let s = renderedRelLemma
     assertBool s ("f_rel_0" `isInfixOf` s)
     assertBool s ("LEMMA" `isInfixOf` s)

  ,testCase "rel is universally quantified as a real" $ do
     let s = renderedRelLemma
     assertBool s ("rel: real" `isInfixOf` s)

  ,testCase "the error function application appears as the hypothesis" $ do
     let s = renderedRelLemma
     assertBool s ("f_0_error(e_x, e_y, r_x, r_y)" `isInfixOf` s)

  ,testCase "the multiplicative bound appears on both sides of IMPLIES" $ do
     let s = renderedRelLemma
         (hyp, concl) = breakOn "IMPLIES" s
     assertBool s ("IMPLIES" `isInfixOf` s)
     assertBool ("hypothesis: " ++ hyp) ("rel * abs(" `isInfixOf` hyp)
     assertBool ("conclusion: " ++ concl) ("rel * abs(" `isInfixOf` concl)

  ,testCase "the conclusion bounds the absolute difference" $ do
     let s = renderedRelLemma
     assertBool s ("abs(DtoR(f(x, y)) - f_real(r_x, r_y))" `isInfixOf` s)

  -- The multiplicative form exists precisely so that no division TCC is
  -- generated. A quotient anywhere in the statement defeats the whole design.
  ,testCase "no division appears anywhere in the lemma" $ do
     let s = renderedRelLemma
     assertBool s (not ("/" `isInfixOf` s))

  -- The hypotheses are the whole point: without them the bridge lemma is not
  -- just unprovable from the absolute lemma, it is false, because nothing
  -- stops the floating-point result from overflowing.
  ,testCase "the finiteness hypotheses are carried over" $ do
     let s = renderedRelLemma
     assertBool s ("finite?_double" `isInfixOf` s)

  ,testCase "the argument error bindings are carried over" $ do
     let s = renderedRelLemma
     assertBool s ("abs(DtoR(x) - r_x) <= e_x" `isInfixOf` s)
     assertBool s ("abs(DtoR(y) - r_y) <= e_y" `isInfixOf` s)

  -- The strongest form of the same check: the bridge lemma's hypothesis block
  -- must be the absolute lemma's block plus exactly one extra conjunct.
  ,testCase "the hypotheses are the absolute lemma's plus the ratio bound" $ do
     let absHyps = fst (breakOn "IMPLIES" renderedAbsLemma)
         relHyps = fst (breakOn "IMPLIES" renderedRelLemma)
         extra   = "\nAND\nf_0_error(e_x, e_y, r_x, r_y) <= rel * abs(f_real(r_x, r_y))\n"
         -- drop the differing FORALL line from each
         body t  = snd (breakOn "):" t)
     assertBool ("abs: " ++ absHyps ++ "\nrel: " ++ relHyps)
                (body relHyps == body absHyps ++ dropWhile (== '\n') extra)
  ]

-- | Split a string at the first occurrence of a separator, dropping it.
breakOn :: String -> String -> (String, String)
breakOn sep = go ""
  where
    n = length sep
    go acc [] = (reverse acc, "")
    go acc str@(c:cs)
      | sep `isPrefixOf` str = (reverse acc, drop n str)
      | otherwise            = go (c:acc) cs

-- The numeric relative error lemma: the bridge lemma with 'rel' instantiated
-- to the constant Kodiak computed, stated over the numeric hypotheses (the
-- input ranges and the path condition) exactly as the numeric absolute lemma
-- is.
numRelArgs :: [Arg]
numRelArgs = [Arg "x" FPDouble, Arg "y" FPDouble]

-- The range bounds are INTEGER valued on purpose. A rational bound would be
-- printed as "a / b", and those slashes would mask the division checks below,
-- whose whole point is that the relative bound is stated multiplicatively and
-- so generates no nonzero-divisor TCC.
numRelRanges :: [VarBind]
numRelRanges = [VarBind "x" ResValue FPDouble (LBInt 0) (UBInt 1)
               ,VarBind "y" ResValue FPDouble (LBInt 2) (UBInt 4)]

-- | The bound as Kodiak would hand it over: an ordinary Double.
numRelBound :: Double
numRelBound = 1.5e-16

-- | Its exact rational rendering. 'toRational' is exact on a Double, so this
--   is the very value Kodiak returned, not a re-rounded decimal.
numRelConstant :: String
numRelConstant = "6084722881095501 / 40564819207303340847894502572032"

renderedNumRelLemma :: String
renderedNumRelLemma = render (prPvsNumRelLemma (text "f_rel_c_0") "f" "f_real" ResValue
                                               numRelArgs (Conds []) numRelBound
                                               numRelRanges FPDouble [])

-- | A path condition that is not trivially true, so that 'prPvsNumRelLemma'
--   has to conjoin it, exactly as 'prPvsNumLemma' does.
nonTrivialCond :: Conditions
nonTrivialCond = Conds [trueCond { realPathCond = Rel Lt (Var Real "r_x") (Int 1) }]

-- | Remove the first occurrence of a substring; a no-op when absent.
removeFirst :: String -> String -> String
removeFirst sep str = let (before, after) = breakOn sep str in before ++ after

testNumRelLemma = testGroup "prPvsNumRelLemma"
  [testCase "the lemma is named <f>_rel_c_<path> and is a LEMMA" $ do
     let s = renderedNumRelLemma
     assertBool s ("f_rel_c_0 : LEMMA" `isInfixOf` s)

  ,testCase "the quantifier block matches the numeric absolute lemma's" $ do
     let s = renderedNumRelLemma
     assertBool s ("FORALL(r_x, r_y: real, x: double, y: double):" `isInfixOf` s)

  -- The constant must be the EXACT rational form of the Double Kodiak
  -- returned; a re-rounded decimal could round down and certify a bound that
  -- was never established.
  ,testCase "the computed constant appears, exactly" $ do
     let s = renderedNumRelLemma
     assertBool s (numRelConstant `isInfixOf` s)

  ,testCase "the bound is multiplicative, not a quotient" $ do
     let concl = snd (breakOn "IMPLIES" renderedNumRelLemma)
     assertBool concl ("* abs(" `isInfixOf` concl)
     assertBool concl ("abs(DtoR(f(x, y)) - f_real(r_x, r_y)) <=" `isInfixOf` concl)
     assertBool concl ("* abs(f_real(r_x, r_y))" `isInfixOf` concl)

  -- The numeric hypotheses are what make this a concrete certificate rather
  -- than the range-free bridge lemma.
  ,testCase "the input ranges appear as hypotheses" $ do
     let s = renderedNumRelLemma
     assertBool s ("r_x ## [|0,1|]" `isInfixOf` s)
     assertBool s ("r_y ## [|2,4|]" `isInfixOf` s)

  ,testCase "rel is no longer quantified: it has been instantiated" $ do
     let s = renderedNumRelLemma
     assertBool s (not ("rel: real" `isInfixOf` s))

  -- Division in PVS emits a nonzero-divisor TCC, which the multiplicative
  -- form exists to avoid. The ranges are integer valued (see 'numRelRanges'),
  -- so the ONLY legitimate slashes left in the rendered lemma are the two
  -- half-ulp argument bindings and the rational literal for the constant;
  -- strip those three and nothing divided may remain.
  ,testCase "no division is introduced anywhere in the lemma" $ do
     let s = foldr removeFirst renderedNumRelLemma
                   [numRelConstant, "ulp_double(r_x)/2", "ulp_double(r_y)/2"]
     assertBool s (not ("/" `isInfixOf` s))

  -- Same check, sharper, on the part that states the bound: after the single
  -- rational literal is removed the conclusion holds no division at all.
  ,testCase "the conclusion divides by nothing but its own literal" $ do
     let concl = snd (breakOn "IMPLIES" renderedNumRelLemma)
     assertBool concl (not ("/" `isInfixOf` removeFirst numRelConstant concl))

  -- A non-trivial path condition is conjoined just as 'prPvsNumLemma' does it;
  -- a trivially true one is dropped rather than printed as "TRUE AND ...".
  ,testCase "a non-trivial path condition is carried into the hypotheses" $ do
     let s = render (prPvsNumRelLemma (text "f_rel_c_1") "f" "f_real" ResValue
                                      numRelArgs nonTrivialCond
                                      numRelBound numRelRanges FPDouble [])
     assertBool s ("Dqlt(r_x,1)" `isInfixOf` s)
     -- and it sits before IMPLIES, i.e. among the hypotheses
     assertBool s ("Dqlt(r_x,1)" `isInfixOf` fst (breakOn "IMPLIES" s))
  ]

renderedNumRelProof :: String
renderedNumRelProof = render (prPvsNumRelProof (text "f_rel_c_0") (text "f_rel_0")
                                               numRelBound 14 7)

-- | The constant as it appears in the LEMMA: everything between "<= " and the
--   " * abs(" that starts the multiplicative factor.
constantInConclusion :: String
constantInConclusion =
  fst (breakOn " * abs(" (snd (breakOn "<= " (snd (breakOn "IMPLIES" renderedNumRelLemma)))))

-- | The constant as it appears in the PROOF SCRIPT: the quoted PVS term
--   handed to prove-relative-lemma.
constantInProof :: String
constantInProof = fst (breakOn "\"" (snd (breakOn "\"" renderedNumRelProof)))

testNumRelProof = testGroup "prPvsNumRelProof"
  [testCase "the proof cites prove-relative-lemma with the symbolic bridge lemma" $ do
     let s = renderedNumRelProof
     assertBool s ("%|- f_rel_c_0 : PROOF" `isInfixOf` s)
     assertBool s ("(prove-relative-lemma f_rel_0 " `isInfixOf` s)
     assertBool s ("%|- QED" `isInfixOf` s)

  ,testCase "the precision and max depth are passed through, in that order" $ do
     let s = renderedNumRelProof
     assertBool s ("14 7)" `isInfixOf` s)

  ,testCase "the bound is passed as a quoted PVS term" $ do
     let s = renderedNumRelProof
     assertBool s (("\"" ++ numRelConstant ++ "\"") `isInfixOf` s)

  -- THE check. The bridge lemma is instantiated with the term in the proof
  -- script, and the goal states the term in the conclusion; if the two are not
  -- the same characters the instantiated lemma does not match the goal and the
  -- proof cannot close. Both come from 'prettyRatNumError', and this pins that
  -- they stay in lockstep -- a silent, proof-breaking divergence otherwise.
  ,testCase "the proof script's constant matches the conclusion's, exactly" $ do
     assertBool ("conclusion: " ++ show constantInConclusion
                 ++ "\nproof: " ++ show constantInProof)
                (constantInProof == constantInConclusion)
     -- and neither is empty, so the comparison above is not vacuous
     assertBool "the constant was not extracted" (constantInProof == numRelConstant)
  ]

