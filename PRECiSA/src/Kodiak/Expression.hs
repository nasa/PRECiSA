{-# LANGUAGE MultiParamTypeClasses  #-}

module Kodiak.Expression (
    module Kodiak.Expression,
    module FPrec
) where

import FPrec
import Kodiak.Kodiak
import Kodiak.KodiakRunnable
import Kodiak.KodiakRunner hiding (runBExpr)

import Control.Exception (throw,AssertionFailed(..),assert)
import Data.Ratio (numerator,denominator)
import Data.Bits.Floating (nextUp,nextDown)
import Foreign.C
import Prelude hiding (True,False,LT,LE,GT,GE)

data AExpr
    = Cnst Rational
    | Var  VarName
    | Add   AExpr AExpr
    | Sub   AExpr AExpr
    | Mul   AExpr AExpr
    | Div   AExpr AExpr
    | Neg   AExpr
    | Floor AExpr
    | Sqrt  AExpr
    | Abs   AExpr
    | Sin   AExpr
    | Cos   AExpr
    | ATan  AExpr
    | Ln    AExpr
    | Exp   AExpr
    | Ulp FPrec AExpr
    | Max [AExpr]
    deriving (Show,Eq)

instance KodiakRunnable AExpr VariableMap PReal where
    run = runAExpr

runAExpr e vmap@(VMap vMap)
    | Cnst r  <- e = do
        let dr = fromRational r :: CDouble
        pi <- if (toRational dr == r)
            then interval_create dr dr
            else let rdr = toRational dr in
                if (rdr > r)
                then do let lb = nextDown dr
                        assert (toRational lb < rdr) $
                            interval_create lb dr
                else do let ub = nextUp dr
                        assert (rdr < toRational ub) $
                            interval_create dr ub
        real_create_value pi

    | Var v   <- e = case lookup v vMap of
                        Just i -> newCString v >>= real_create_variable i
                        Nothing -> throw $ AssertionFailed $ "Kodiak.Var " ++ v ++ " name not found"

    | Add l r <- e = runBinary l r real_create_addition

    | Sub l r <- e = runBinary l r real_create_subtraction

    | Mul l r <- e = runBinary l r real_create_multiplication

    | Div l r <- e = runBinary l r real_create_division

    | Neg   r <- e = runUnary r real_create_negation

    | Floor r <- e = runUnary r real_create_floor

    | Sqrt r  <- e = runUnary r real_create_sqrt

    | Abs r   <- e = runUnary r real_create_absolute_value

    | Sin r   <- e = runUnary r real_create_sine

    | Cos r   <- e = runUnary r real_create_cosine

    | ATan r  <- e = runUnary r real_create_arctangent

    | Ln r    <- e = runUnary r real_create_elogarithm

    | Exp r   <- e = runUnary r real_create_eexponent

    | Ulp p r <- e = runUnary r $ case p of FPSingle -> real_create_single_ulp
                                            FPDouble -> real_create_double_ulp

    | Max rs  <- e = do v <- real_vector_create
                        mapM_ (\r -> runAExpr r vmap >>= real_vector_add v) rs
                        real_create_maximum v

    where
        runBinary l r op = do pl <- runAExpr l vmap
                              pr <- runAExpr r vmap
                              op pl pr

        runUnary r op = runAExpr r vmap >>= op

data BExpr
    = True
    | False
    | Not BExpr
    | And BExpr BExpr
    | Or  BExpr BExpr
    | Eq  AExpr AExpr
    | NEq AExpr AExpr
    | LT  AExpr AExpr
    | LE  AExpr AExpr
    | GT  AExpr AExpr
    | GE  AExpr AExpr
    deriving (Show,Eq)

instance KodiakRunnable BExpr VariableMap PBool where
    run = runBExpr

runBExpr :: BExpr -> VariableMap -> IO PBool
runBExpr e vmap@(VMap vMap)
    | True    <- e = bool_create_true

    | False   <- e = bool_create_false

    | Not b   <- e = runBExpr b vmap >>= bool_create_not

    | Or l r  <- e = runBooleanBinary l r bool_create_or

    | And l r <- e = runBooleanBinary l r bool_create_and

    | Eq l r  <- e = runBinary l r bool_create_equal_to

    | NEq l r <- e = runBinary l r bool_create_equal_to >>= bool_create_not

    | LT l r  <- e = runBinary l r bool_create_less_than

    | LE l r  <- e = runBinary l r bool_create_less_than_or_equal_to

    | GT l r  <- e = runBinary l r bool_create_greater_than

    | GE l r  <- e = runBinary l r bool_create_greater_than_or_equal_to

    where
        runBinary l r op = do pl <- runAExpr l vmap
                              pr <- runAExpr r vmap
                              op pl pr

        runBooleanBinary l r op = do pl <- runBExpr l vmap
                                     pr <- runBExpr r vmap
                                     op pl pr
