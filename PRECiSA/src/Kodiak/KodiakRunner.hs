-- Notices:
--
-- Copyright 2017 United States Government as represented by the Administrator of the National Aeronautics and Space Administration.
-- All Rights Reserved.
--
-- Disclaimers:
--
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED,
-- IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT
-- SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS,
-- HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
--
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT,
-- ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING
-- FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH
-- MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


{-# LANGUAGE MultiParamTypeClasses  #-}

module Kodiak.KodiakRunner where

import Kodiak.Kodiak
import Kodiak.KodiakRunnable
import AbsPVSLang
import AbsSpecLang

import Control.Exception (throw,AssertionFailed(..))
import Control.Monad (when,mapM_,(>=>))
import Debug.Trace (trace)
import Foreign.C
import Foreign.C.String

data KodiakInput = KI { name :: String,
                        expression :: EExpr,
                        bindings :: [VarBind],
                        maxDepth :: CUInt,
                        precision :: CUInt }

variableMapFromBinds binds = VMap $ zip variableNames [(0::CUInt)..]
  where variableNames = map (\(VarBind s _ _) -> s) binds

fromLBoundToRational :: LBound -> Rational
fromLBoundToRational (LBInt i) = toRational i
fromLBoundToRational (LBDouble r) = toRational r
fromLBoundToRational LInf = error "not implemented for LInf TODO: Refactor"

fromUBoundToRational :: UBound -> Rational
fromUBoundToRational (UBInt i) = toRational i
fromUBoundToRational (UBDouble r) = toRational r
fromUBoundToRational UInf = error "not implemented for UInf TODO: Refactor"

instance KodiakRunnable VarBind PMinMaxSystem () where
  run (VarBind name lowerBound upperBound) pSys =
    do
      cName <- newCString name
      pLb <- interval_create cLowerBound cLowerBound
      pUb <- interval_create cUpperBound cUpperBound
      minmax_system_register_variable pSys cName pLb pUb
        where
          cLowerBound = fromRational $ fromLBoundToRational lowerBound
          cUpperBound = fromRational $ fromUBoundToRational upperBound

instance KodiakRunnable KodiakInput () KodiakResult where
  run kInput _ =
    do
      let sysName = name kInput
          errorExpr     = expression kInput
          varRanges     = bindings kInput
          variableMap   = variableMapFromBinds varRanges
          thisPrecision = precision kInput
          thisMaxDepth  = maxDepth kInput
      cName <- newCString sysName
      pSys <- minmax_system_create cName
      minmax_system_set_maxdepth pSys thisMaxDepth
      minmax_system_set_precision pSys (negate (fromInteger $ toInteger thisPrecision))
      mapM_ (flip run pSys) varRanges
      pExpr <- run errorExpr variableMap
      minmax_system_maximize pSys pExpr
      lb <- minmax_system_maximum_lower_bound pSys >>= fmap (fromRational . toRational) . return
      ub <- minmax_system_maximum_upper_bound pSys >>= fmap (fromRational . toRational) . return
      return $ KR { maximumLowerBound = lb,
                    maximumUpperBound = ub }

newtype VariableMap = VMap [(String,CUInt)] deriving Show

lookup' str (VMap mappings) = case lookup str mappings of
                                Just x -> x
                                Nothing -> error $ "lookup': tried to search \"" ++ str ++ "\" in \"" ++ show mappings ++ "\""


instance KodiakRunnable AExpr VariableMap PReal where
  run e m = do kodiakVariables <- mapM createKodiakVariable variableToNumberMap
               run' e kodiakVariables
                 where
                   VMap variableToNumberMap = m

                   createKodiakVariable (name,id) = do cName <- newCString name
                                                       pVariable <- real_create_variable id cName
                                                       return (name,pVariable)
                   runBinaryOperator l r vmap kodiakFunction = do pl <- run' l vmap
                                                                  pr <- run' r vmap
                                                                  kodiakFunction pl pr
                   debugBinaryOperator l r vmap kodiakFunction str =
                     do pl <- run' l vmap
                        pr <- run' r vmap
                        putStrLn ""
                        putStrLn $ "BEGIN DEBUGGING " ++ str
                        putStrLn "From:"
                        putStr " l="
                        real_print pl
                        putStr " r="
                        real_print pr
                        returnP <- kodiakFunction pl pr
                        putStrLn "...we get:"
                        real_print returnP 
                        putStrLn $ "END DEBUGGING " ++ str
                        putStrLn ""
                        putStrLn ""
                        return returnP

                   runUnaryOperator e vmap kodiakFunction = do p <- run' e vmap
                                                               kodiakFunction p

                   debugUnaryOperator e vmap kodiakFunction str =
                     do p <- run' e vmap
                        putStrLn ""
                        putStrLn $ "BEGIN DEBUGGING " ++ str
                        putStrLn "From:"
                        putStr " e="
                        real_print p
                        returnP <- kodiakFunction p
                        putStrLn "...we get:"
                        real_print returnP 
                        putStrLn $ "END DEBUGGING " ++ str
                        putStrLn ""
                        putStrLn ""
                        return returnP

                   runBinaryErrorOperator l le r re vmap kodiakFunction = do pl  <- run' l vmap
                                                                             ple <- run' le vmap
                                                                             pr  <- run' r vmap
                                                                             pre <- run' re vmap
                                                                             kodiakFunction pl ple pr pre

                   debugBinaryErrorOperator l le r re vmap kodiakFunction str =
                     do pl  <- run' l vmap
                        ple <- run' le vmap
                        pr  <- run' r vmap
                        pre <- run' re vmap
                        putStrLn ""
                        putStrLn $ "BEGIN DEBUGGING " ++ str
                        putStrLn "From:"
                        putStr " pl="
                        real_print pl
                        putStr " ple="
                        real_print ple
                        putStr " pr="
                        real_print pr
                        putStr " pre="
                        real_print pre
                        returnP <- kodiakFunction pl ple pr pre
                        putStrLn "...we get:"
                        real_print returnP 
                        putStrLn $ "END DEBUGGING " ++ str
                        putStrLn ""
                        putStrLn ""
                        return returnP

                   runUnaryErrorOperator l le vmap kodiakFunction = do pl  <- run' l vmap
                                                                       ple <- run' le vmap
                                                                       kodiakFunction pl ple

                   debugUnaryErrorOperator l le vmap kodiakFunction str =
                     do pl  <- run' l vmap
                        ple <- run' le vmap
                        putStrLn ""
                        putStrLn $ "BEGIN DEBUGGING " ++ str
                        putStrLn "From:"
                        putStr " pl="
                        real_print pl
                        putStr " ple="
                        real_print ple
                        returnP <- kodiakFunction pl ple
                        putStrLn "...we get:"
                        real_print returnP 
                        putStrLn $ "END DEBUGGING " ++ str
                        putStrLn ""
                        putStrLn ""
                        return returnP

                   run' (Add l r) vmap =
                     runBinaryOperator l r vmap real_create_addition
                   run' (Sub l r) vmap =
                     runBinaryOperator l r vmap real_create_subtraction
                   run' (Mul l r) vmap =
                     runBinaryOperator l r vmap real_create_multiplication
                   run' (Div l r) vmap =
                     runBinaryOperator l r vmap real_create_division
                   run' (AE e) vmap = run' e vmap
                   run' (EE e) vmap = run' e vmap
                   run' (Int i) vmap = run' (Double $ toRational i) vmap
                   run' (Double r) vmap = interval_create lb ub >>= real_create_value
                     where (lb,ub) = rat2interval r
                   run' (Var name) vmap = run' (RealMark name) vmap
                   run' (RealMark name) vmap = case lookup name vmap of
                                                 Just pVar -> return pVar
                                                 Nothing -> (throw $ AssertionFailed $ "TODO: RealMark " ++ name ++ " name not found") >> return undefined
                   run' (ErrorMark name) vmap = (throw $ AssertionFailed "TODO: ErrorMark should not been used") >> return undefined
                   run' (Abs e) vmap = runUnaryOperator e vmap real_create_absolute_value
                   run' (Ln e) vmap = runUnaryOperator e vmap real_create_elogarithm
                   run' (Expo e) vmap = runUnaryOperator e vmap real_create_eexponent
                   run' (Sin e) vmap = runUnaryOperator e vmap real_create_sine
                   run' (Cos e) vmap = runUnaryOperator e vmap real_create_cosine
                   run' (Floor e) vmap = runUnaryOperator e vmap real_create_floor
                   run' (DUlp e) vmap =
                     runUnaryOperator e vmap real_create_double_ulp
                   run' (Neg e) vmap =
                     runUnaryOperator e vmap real_create_negation
                   run' (HalfUlp e) vmap = run' (Div (DUlp e) (Double $ toRational 2)) vmap
                   run' (ErrAdd l le r re) vmap =
                     runBinaryErrorOperator l le r re vmap real_create_error_addition
                   run' (ErrSub l le r re) vmap =
                     runBinaryErrorOperator l le r re vmap real_create_error_subtraction

                   run' (ErrMul l le r re) vmap =
                     runBinaryErrorOperator l le r re vmap real_create_error_multiplication
                   run' (ErrDiv l le r re) vmap =
                     runBinaryErrorOperator l le r re vmap real_create_error_division
                   run' (ErrNeg expr error) vmap =
                     runUnaryErrorOperator expr error vmap real_create_error_negation
                   run' (ErrRat rational) vmap = run' (Double rational) vmap
                   run' (ErrMulPow2L i e) vmap = do pe <- run' e vmap
                                                    real_create_error_power_of_two_multiplication (fromInteger i) pe
                   run' (ErrMulPow2R i e) vmap = do pe <- run' e vmap
                                                    real_create_error_power_of_two_multiplication (fromInteger i) pe
                   run' (ErrExpo l le) vmap = runUnaryErrorOperator l le vmap real_create_error_eexponent
                   run' (ErrLn l le) vmap = runUnaryErrorOperator l le vmap real_create_error_elogarithm
                   run' (ErrSin l le) vmap = runUnaryErrorOperator l le vmap real_create_error_sine
                   run' (ErrCos l le) vmap = runUnaryErrorOperator l le vmap real_create_error_cosine
                   run' (ErrAtan l le) vmap = runUnaryErrorOperator l le vmap real_create_error_arctangent
                   run' (ErrFloor l le) vmap = runUnaryErrorOperator l le vmap real_create_error_floor
                   run' (ErrFloor0 l le) vmap = runUnaryErrorOperator l le vmap real_create_error_floor_tight
                   run' (MaxErr es) vmap = let
                     buildVector = do pVector <- real_vector_create
                                      mapM_ (flip run' vmap >=> real_vector_add pVector) es
                                      return pVector
                     in if length es > 1
                        then buildVector >>= real_create_maximum
                        else run' (head es) vmap
                   run' e _ = error $ "KodiakRunnable instance for AExpr, VariableMap and PReal undefined for " ++ show e
 

runnableTest = run expr2 varMap >>= real_print
  where
    varMap = VMap [("x", 0),("y", 1)]
    expr1 = Var "helo"
    expr2 = Var "y"
