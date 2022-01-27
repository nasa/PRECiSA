-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


{-# LANGUAGE MultiParamTypeClasses  #-}

module Kodiak.Expression (
    module Kodiak.Expression,
    module PVSTypes
) where

import PVSTypes
import Kodiak.Kodiak
import Kodiak.Runnable
import Kodiak.Runner hiding (runBExpr)
import Common.TypesUtils
import Control.Exception (throw,AssertionFailed(..),assert)
import Data.Bits.Floating (nextUp,nextDown)
import Foreign.C
import Prelude hiding (True,False,LT,GT)

data AExpr
    = Cnst Rational
    | Var  VarName
    | Let VarName AExpr AExpr
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
    | Ulp PVSType AExpr
    | Max [AExpr]
    deriving (Show,Eq)

instance KodiakRunnable AExpr VariableMap PReal where
    run = runAExpr

runAExpr :: AExpr -> VariableMap -> IO PReal
runAExpr e vmap@(VMap vMap)
    | Cnst r  <- e = do
        let dr = fromRational r :: CDouble
        pint <- if (toRational dr == r)
            then interval_create dr dr
            else let rdr = toRational dr in
                if (rdr > r)
                then do let lb = nextDown dr
                        assert (toRational lb < rdr) $
                            interval_create lb dr
                else do let ub = nextUp dr
                        assert (rdr < toRational ub) $
                            interval_create dr ub
        real_create_value pint

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
                                            _ -> error $ "Kodiak Ulp AExpr is not defined for PVSType: " ++ show p

    | Max rs  <- e = do v <- real_vector_create
                        mapM_ (\r -> runAExpr r vmap >>= real_vector_add v) rs
                        real_create_maximum v

    | otherwise = error $ "Kodiak does not support AExpr: " ++ show e

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
runBExpr e vmap
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
