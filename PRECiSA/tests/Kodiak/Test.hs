module Kodiak.Test where

import Control.Exception (tryJust,AssertionFailed(..))
import Control.Monad
import Data.Either (isLeft)
import Foreign.C.String
import Test.Tasty
import Test.Tasty.HUnit

import AbsPVSLang
import FPrec
import Kodiak.Kodiak
import Kodiak.KodiakRunnable
import Kodiak.KodiakRunner
import qualified Kodiak.Expression as K

testKodiak :: TestTree
testKodiak = testGroup "Kodiak"
    [testBooleanExpressions
    ,testPaver
    ,testBooleanExpressionKodiakRunnable
    ,testKodiakExpressionsAreKodiakRunnable
    ]

testBooleanExpressions :: TestTree
testBooleanExpressions = testGroup "Boolean Expressions"
    [testBooleanConstants
    ,testBooleanOperators
    ,testRelationalOperators
    ]

testBooleanConstants :: TestTree
testBooleanConstants = testGroup "Constants"
    [testCase "True can be created" $
        checkSelfEquality bool_create_true @? isNotSelfEqualLabel
    ,testCase "False can be created" $
        checkSelfEquality bool_create_false @? isNotSelfEqualLabel
    ,testCase "Possibly can be created" $
        checkSelfEquality bool_create_possibly @? isNotSelfEqualLabel
    ,testCase "WithinEps can be created" $
        checkSelfEquality bool_create_within_eps @? isNotSelfEqualLabel
    ,testCase "True is distinct to the rest" $
        checkDistincness bool_create_true [bool_create_false,bool_create_possibly,bool_create_within_eps] @? isNotDistinctLabel
    ,testCase "False is distinct to the rest" $
        checkDistincness bool_create_false [bool_create_true,bool_create_possibly,bool_create_within_eps] @? isNotDistinctLabel
    ,testCase "Possibly is distinct to the rest" $
        checkDistincness bool_create_possibly [bool_create_true,bool_create_false,bool_create_within_eps] @? isNotDistinctLabel
    ,testCase "WithinEps is distinct to the rest" $
        checkDistincness bool_create_within_eps [bool_create_true,bool_create_false,bool_create_possibly] @? isNotDistinctLabel
    ]

checkSelfEquality le = le >>= \e -> bool_equal_to_ e e
checkDistincness le les = and <$> mapM (fmap not . join . liftM2 bool_equal_to_ le) les
isNotSelfEqualLabel = "Is not equal to itself"
isNotDistinctLabel = "Is not distinct to the rest"

testBooleanOperators :: TestTree
testBooleanOperators = testGroup "Operators" $
    let createNotTrue          = bool_create_true >>= bool_create_not
        createAndTrueFalse     = do t <- bool_create_true
                                    f <- bool_create_false
                                    bool_create_and t f
        createOrTrueFalse      = do t <- bool_create_true
                                    f <- bool_create_false
                                    or <- bool_create_or t f
                                    return or
        createImpliesTrueFalse = do t <- bool_create_true
                                    f <- bool_create_false
                                    implies <- bool_create_implies t f
                                    return implies in
    [testGroup "Not" $
        [testCase "it can be created" $
            checkSelfEquality createNotTrue @? isNotSelfEqualLabel
        ,testCase "it is distinct from a constant" $
            checkDistincness createNotTrue [bool_create_true,bool_create_false,bool_create_possibly,bool_create_within_eps] @? isNotSelfEqualLabel
        ]
    ,testGroup "And" $
        [testCase "it can be created" $
            checkSelfEquality createAndTrueFalse @? isNotSelfEqualLabel
        ,testCase "it is distinct from a constant" $
            checkDistincness createAndTrueFalse [bool_create_true,bool_create_false,bool_create_possibly,bool_create_within_eps] @? isNotSelfEqualLabel
        ]
    ,testGroup "Or" $
        [testCase "it can be created" $
            checkSelfEquality createOrTrueFalse @? isNotSelfEqualLabel
        ,testCase "it is distinct from a constant" $
            checkDistincness createOrTrueFalse [bool_create_true,bool_create_false,bool_create_possibly,bool_create_within_eps] @? isNotSelfEqualLabel
        ]
    ,testGroup "Implies" $
        [testCase "it can be created" $
            checkSelfEquality createImpliesTrueFalse @? isNotSelfEqualLabel
        ,testCase "it is distinct from a constant" $
            checkDistincness createImpliesTrueFalse [bool_create_true,bool_create_false,bool_create_possibly,bool_create_within_eps] @? isNotSelfEqualLabel
        ]
    ]

testRelationalOperators :: TestTree
testRelationalOperators = testGroup "Relational Operators"
    [testGroup "Equal To" $
        let createEquals =
                do
                    zeroI <- interval_create 0 0
                    zeroR <- real_create_value zeroI
                    bool_create_equal_to zeroR zeroR in
        [testCase "it can be created" $
            checkSelfEquality createEquals @? isNotSelfEqualLabel
        ,testCase "it is distinct from a constant" $
            checkDistincness createEquals [bool_create_true,bool_create_false,bool_create_possibly,bool_create_within_eps] @? isNotSelfEqualLabel
        ]
    ,testGroup "Less Than" $
        let createLessThan =
                do
                    zeroI <- interval_create 0 0
                    zeroR <- real_create_value zeroI
                    bool_create_less_than zeroR zeroR in
        [testCase "it can be created" $
            checkSelfEquality createLessThan @? isNotSelfEqualLabel
        ,testCase "it is distinct from a constant" $
            checkDistincness createLessThan [bool_create_true,bool_create_false,bool_create_possibly,bool_create_within_eps] @? isNotSelfEqualLabel
        ]
    ,testGroup "Less Than or Equal To" $
        let createLessThanOrEqualTo =
                do
                    zeroI <- interval_create 0 0
                    zeroR <- real_create_value zeroI
                    bool_create_less_than_or_equal_to zeroR zeroR in
        [testCase "it can be created" $
            checkSelfEquality createLessThanOrEqualTo @? isNotSelfEqualLabel
        ,testCase "it is distinct from a constant" $
            checkDistincness createLessThanOrEqualTo [bool_create_true,bool_create_false,bool_create_possibly,bool_create_within_eps] @? isNotSelfEqualLabel
        ]
    ,testGroup "Less Than" $
        let createLessThan =
                do
                    zeroI <- interval_create 0 0
                    zeroR <- real_create_value zeroI
                    bool_create_less_than zeroR zeroR in
        [testCase "it can be created" $
            checkSelfEquality createLessThan @? isNotSelfEqualLabel
        ,testCase "it is distinct from a constant" $
            checkDistincness createLessThan [bool_create_true,bool_create_false,bool_create_possibly,bool_create_within_eps] @? isNotSelfEqualLabel
        ]
    ,testGroup "Less Than or Equal To" $
        let createLessThanOrEqualTo =
                do
                    zeroI <- interval_create 0 0
                    zeroR <- real_create_value zeroI
                    bool_create_less_than_or_equal_to zeroR zeroR in
        [testCase "it can be created" $
            checkSelfEquality createLessThanOrEqualTo @? isNotSelfEqualLabel
        ,testCase "it is distinct from a constant" $
            checkDistincness createLessThanOrEqualTo [bool_create_true,bool_create_false,bool_create_possibly,bool_create_within_eps] @? isNotSelfEqualLabel
        ]
    ]

testPaver :: TestTree
testPaver = testGroup "Paver" $
    let createPaver = do name <- newCString "Example"
                         paver_create name
        createVariable p = do var <- newCString "X"
                              lb <- interval_create 0 0
                              ub <- interval_create 1 1
                              paver_register_variable p var lb ub
                              return var
        createBool varName = do var <- real_create_variable 0 varName
                                half <- interval_create_from_rational 1 2 >>= real_create_value
                                bool_create_greater_than_or_equal_to var half
        failLabel = "something unexpected happened"
    in
    [testCase "paver_create" $
        (createPaver >>= \x -> return True) @? failLabel
    ,testCase "paver_print" $
        do p <- createPaver
           return True
        @? failLabel
    ,testCase "paver_register_variable" $
        do p <- createPaver
           createVariable p
           return True
        @? failLabel
    ,testCase "paver_set_maxdepth" $
        do p <- createPaver
           paver_set_maxdepth p 10
           return True
        @? failLabel
    ,testCase "paver_set_precision" $
        do p <- createPaver
           paver_set_precision p (-10)
           return True
        @? failLabel
    ,testCase "paver_pave" $
        do p <- createPaver
           varName <- createVariable p
           boolExp <- createBool varName
           paver_set_maxdepth p 10
           paver_set_precision p (-10)
           paver_pave p boolExp
           return True
        @? failLabel
    ,testCase "paver_save_paving" $
        do p <- createPaver
           varName <- createVariable p
           boolExp <- createBool varName
           paver_set_maxdepth p 2
           paver_set_precision p (-1)
           paver_pave p boolExp
           name <- newCString "test-paving.eraseme"
           paver_save_paving p name
           return True
        @? failLabel
    ]

testBooleanExpressionKodiakRunnable :: TestTree
testBooleanExpressionKodiakRunnable = testGroup "KodiakRunnable Boolean Expressions" $
    let failLabel = "Something unexpected happened!"
        zero = interval_create 0 0 >>= real_create_value
        one  = interval_create 1 1 >>= real_create_value in
    [testGroup "Real Boolean Expression" $
        [testCase "BTrue" $
            bool_create_true
            `areIdenticalBoolExpression`
            run BTrue (VMap [])
        ,testCase "BFalse" $
            bool_create_false
            `areIdenticalBoolExpression`
            run BFalse (VMap [])
        ,testCase "Not" $
            (join $ bool_create_not <$> bool_create_true)
            `areIdenticalBoolExpression`
            run (Not BTrue) (VMap [])
        ,testCase "Or" $
            (join $ bool_create_or <$> bool_create_true <*> bool_create_false)
            `areIdenticalBoolExpression`
            run (Or BTrue BFalse) (VMap [])
        ,testCase "And" $
            (join $ bool_create_and <$> bool_create_true <*> bool_create_false)
            `areIdenticalBoolExpression`
            run (And BTrue BFalse) (VMap [])
        ,testCase "Eq" $
            (join $ bool_create_equal_to <$> zero <*> one)
            `areIdenticalBoolExpression`
            run (Eq (Int 0) (Int 1)) (VMap [])
        ,testCase "Neq" $
            (join $ bool_create_not <$> (join $ bool_create_equal_to <$> zero <*> one))
            `areIdenticalBoolExpression`
            run (Neq (Int 0) (Int 1)) (VMap [])
        ,testCase "Lt" $
            (join $ bool_create_less_than <$> zero <*> one)
            `areIdenticalBoolExpression`
            run (Lt (Int 0) (Int 1)) (VMap [])
        ,testCase "LtE" $
            (join $ bool_create_less_than_or_equal_to <$> zero <*> one)
            `areIdenticalBoolExpression`
            run (LtE (Int 0) (Int 1)) (VMap [])
        ,testCase "Gt" $
            (join $ bool_create_greater_than <$> zero <*> one)
            `areIdenticalBoolExpression`
            run (Gt (Int 0) (Int 1)) (VMap [])
        ,testCase "GtE" $
            (join $ bool_create_greater_than_or_equal_to <$> zero <*> one)
            `areIdenticalBoolExpression`
            run (GtE (Int 0) (Int 1)) (VMap [])
        ]
    ]
    where
        areIdenticalBoolExpression lhs rhs =
            do pLHS <- lhs
               pRHS <- rhs
               bool_equal_to_ pLHS pRHS
            @? "the boolean expression generated is different than expected"

testKodiakExpressionsAreKodiakRunnable :: TestTree
testKodiakExpressionsAreKodiakRunnable = testGroup "Kodiak Expressions are KodiakRunnable"
    [testKodiakArithmeticExpressionsAreKodiakRunnable
    ,testKodiakBooleanExpressionsAreKodiakRunnable
    ]

testKodiakArithmeticExpressionsAreKodiakRunnable :: TestTree
testKodiakArithmeticExpressionsAreKodiakRunnable = testGroup "Kodiak Arithmetic Expressions are KodiakRunnable" $
    let vMap = VMap [("x",0)]
        createVariableX0  = newCString "x" >>= real_create_variable 0
        createCnstTwo     = interval_create_from_rational (fromInteger 2) (fromInteger 1) >>= real_create_value
        createVectorTwoX0 = do v <- real_vector_create
                               createCnstTwo    >>= real_vector_add v 
                               createVariableX0 >>= real_vector_add v 
                               return v
    in
    [testCase "Cnst" $
        createCnstTwo
        `areIdenticalExpressions`
        run (K.Cnst 2) emptyMap
    ,testGroup "Var" $
        [testCase "Known variable is transformed" $
            createVariableX0
            `areIdenticalExpressions`
            run (K.Var "x") vMap
        ,testCase "Unknown variable raises exception" $
            arisesException $ run (K.Var "y") vMap
        ]
    ,testCase "Add" $
        do
            var0 <- createVariableX0
            cnst2 <- createCnstTwo
            real_create_addition var0 cnst2
        `areIdenticalExpressions`
        run (K.Add (K.Var "x") (K.Cnst 2)) vMap
    ,testCase "Sub" $
        do
            var0 <- createVariableX0
            cnst2 <- createCnstTwo
            real_create_subtraction var0 cnst2
        `areIdenticalExpressions`
        run (K.Sub (K.Var "x") (K.Cnst 2)) vMap
    ,testCase "Mul" $
        do
            var0 <- createVariableX0
            cnst2 <- createCnstTwo
            real_create_multiplication var0 cnst2
        `areIdenticalExpressions`
        run (K.Mul (K.Var "x") (K.Cnst 2)) vMap
    ,testCase "Div" $
        do
            var0 <- createVariableX0
            cnst2 <- createCnstTwo
            real_create_division var0 cnst2
        `areIdenticalExpressions`
        run (K.Div (K.Var "x") (K.Cnst 2)) vMap
    ,testCase "Neg" $
        (createCnstTwo >>= real_create_negation)
        `areIdenticalExpressions`
        run (K.Neg (K.Cnst 2)) vMap
    ,testCase "Floor" $
        (createCnstTwo >>= real_create_floor)
        `areIdenticalExpressions`
        run (K.Floor (K.Cnst 2)) vMap
    ,testCase "Sqrt" $
        (createCnstTwo >>= real_create_sqrt)
        `areIdenticalExpressions`
        run (K.Sqrt (K.Cnst 2)) vMap
    ,testCase "Abs" $
        (createCnstTwo >>= real_create_absolute_value)
        `areIdenticalExpressions`
        run (K.Abs (K.Cnst 2)) vMap
    ,testCase "Sin" $
        (createCnstTwo >>= real_create_sine)
        `areIdenticalExpressions`
        run (K.Sin (K.Cnst 2)) vMap
    ,testCase "Cos" $
        (createCnstTwo >>= real_create_cosine)
        `areIdenticalExpressions`
        run (K.Cos (K.Cnst 2)) vMap
    ,testCase "ATan" $
        (createCnstTwo >>= real_create_arctangent)
        `areIdenticalExpressions`
        run (K.ATan (K.Cnst 2)) vMap
    ,testCase "Ln" $
        (createCnstTwo >>= real_create_elogarithm)
        `areIdenticalExpressions`
        run (K.Ln (K.Cnst 2)) vMap
    ,testCase "Exp" $
        (createCnstTwo >>= real_create_eexponent)
        `areIdenticalExpressions`
        run (K.Exp (K.Cnst 2)) vMap
    ,testGroup "Ulp"
        [testCase "Single" $
            (createCnstTwo >>= real_create_single_ulp)
            `areIdenticalExpressions`
            run (K.Ulp FPSingle (K.Cnst 2)) vMap
        ,testCase "Single" $
            (createCnstTwo >>= real_create_double_ulp)
            `areIdenticalExpressions`
            run (K.Ulp FPDouble (K.Cnst 2)) vMap
        ]
    ,testCase "Max" $
        (createVectorTwoX0 >>= real_create_maximum)
        `areIdenticalExpressions`
        run (K.Max [K.Cnst 2,K.Var "x"]) vMap
    ]
    where
        emptyMap = VMap []
        areIdenticalExpressions lhs rhs =
            do pLHS <- lhs
               pRHS <- rhs
               real_equal_to_ pLHS pRHS
            @? "the arithmetic expression generated is different than expected"
        arisesException act = (tryJust (guard . isAssertionFailed) act >>= (return . isLeft))
            @? "the action does not throw an AssertionFailed exception"
            where
                isAssertionFailed (AssertionFailed _) = True

testKodiakBooleanExpressionsAreKodiakRunnable :: TestTree
testKodiakBooleanExpressionsAreKodiakRunnable = testGroup "Kodiak Boolean Expressions are KodiakRunnable" $
    let failLabel = "Something unexpected happened!"
        vmap = VMap []
        zero = interval_create 0 0 >>= real_create_value
        one  = interval_create 1 1 >>= real_create_value in
    [testCase "True" $
        bool_create_true
        `areIdenticalExpression`
        run K.True vmap
    ,testCase "False" $
        bool_create_false
        `areIdenticalExpression`
        run K.False vmap
    ,testCase "Not" $
        (join $ bool_create_not <$> bool_create_true)
        `areIdenticalExpression`
        run (K.Not K.True) vmap
    ,testCase "Or" $
        (join $ bool_create_or <$> bool_create_true <*> bool_create_false)
        `areIdenticalExpression`
        run (K.Or K.True K.False) vmap
    ,testCase "And" $
        (join $ bool_create_and <$> bool_create_true <*> bool_create_false)
        `areIdenticalExpression`
        run (K.And K.True K.False) vmap
    ,testCase "Eq" $
        (join $ bool_create_equal_to <$> zero <*> one)
        `areIdenticalExpression`
        run (K.Eq (K.Cnst 0) (K.Cnst 1)) vmap
    ,testCase "NEq" $
        (join $ bool_create_not <$> (join $ bool_create_equal_to <$> zero <*> one))
        `areIdenticalExpression`
        run (K.NEq (K.Cnst 0) (K.Cnst 1)) vmap
    ,testCase "LT" $
        (join $ bool_create_less_than <$> zero <*> one)
        `areIdenticalExpression`
        run (K.LT (K.Cnst 0) (K.Cnst 1)) vmap
    ,testCase "LE" $
        (join $ bool_create_less_than_or_equal_to <$> zero <*> one)
        `areIdenticalExpression`
        run (K.LE (K.Cnst 0) (K.Cnst 1)) vmap
    ,testCase "GT" $
        (join $ bool_create_greater_than <$> zero <*> one)
        `areIdenticalExpression`
        run (K.GT (K.Cnst 0) (K.Cnst 1)) vmap
    ,testCase "GE" $
        (join $ bool_create_greater_than_or_equal_to <$> zero <*> one)
        `areIdenticalExpression`
        run (K.GE (K.Cnst 0) (K.Cnst 1)) vmap
    ]
    where
        areIdenticalExpression lhs rhs =
            do pLHS <- lhs
               pRHS <- rhs
               bool_equal_to_ pLHS pRHS
            @? "the boolean expression generated is different than expected"
