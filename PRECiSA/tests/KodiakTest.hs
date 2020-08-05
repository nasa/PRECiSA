-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
 
-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
 
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
  
  
{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C.String
import System.IO

import Kodiak

disableBuffering = hSetBuffering stdout NoBuffering

main = do
  intervalFeedback
  realExpressionsFeedback
  realExpressionVectorFeedback
  realErrorExpressionsFeedback
  minmaxSystemFeedback
  putStrLn "Main: exit"


{----------------------------------------------------------------------}
{-                                                                    -}
{- C Interval functions                                               -}
{-                                                                    -}
{----------------------------------------------------------------------}

intervalFeedback = do
  putStrLn "Main: creating interval [-2.0,2.0]"
  pInterval <- interval_create (-2.0) 2.0
  putStrLn "Main: print interval [-2.0,2.0]"
  interval_print pInterval
  putStrLn "Main: creating interval [-2/3]"
  pInterval' <- interval_create_from_rational (-2) 3
  putStrLn "Main: print interval [-2/3]"
  interval_print pInterval'


{----------------------------------------------------------------------}
{-                                                                    -}
{- C Real expressions functions                                       -}
{-                                                                    -}
{----------------------------------------------------------------------}

realExpressionsFeedback = do
  disableBuffering
  putStrLn ""
  putStrLn "----------------------------------------"
  putStrLn "Real Expressions"
  putStrLn "----------------------------------------"
  pInterval <- interval_create (-2.0) 2.0
  putStrLn "Main: creating real value [-2.0,2.0]"
  pRealValue <- real_create_value pInterval 
  putStrLn "Main: print real value [-2.0,2.0]"
  real_print pRealValue
  putStrLn "Main: creating real variable var(2,\"Y\")"
  cString <- newCString "Y"
  pRealVariable <- real_create_variable 2 cString
  putStrLn "Main: print real variable var(2,\"Y\")"
  real_print pRealVariable
  putStrLn "Main: creating real expression [-2.0,2.0] + var(2,\"Y\")"
  pRealAddition <- real_create_addition pRealValue pRealVariable
  putStrLn "Main: print real expression [-2.0,2.0] + var(2,\"Y\")"
  real_print pRealAddition
  putStrLn "Main: creating real expression [-2.0,2.0] - var(2,\"Y\")"
  pRealSubtraction <- real_create_subtraction pRealValue pRealVariable
  putStrLn "Main: print real expression [-2.0,2.0] - var(2,\"Y\")"
  real_print pRealSubtraction
  putStrLn "Main: creating real expression [-2.0,2.0] * var(2,\"Y\")"
  pRealMultiplication <- real_create_multiplication pRealValue pRealVariable
  putStrLn "Main: print real expression [-2.0,2.0] * var(2,\"Y\")"
  real_print pRealMultiplication
  putStrLn "Main: creating real expression [-2.0,2.0] / var(2,\"Y\")"
  pRealDivision <- real_create_division pRealValue pRealVariable
  putStrLn "Main: print real expression [-2.0,2.0] / var(2,\"Y\")"
  real_print pRealDivision
  putStrLn "Main: creating real expression DUlp([-2.0,2.0])"
  pRealDUlp <- real_create_double_ulp pRealValue
  putStrLn "Main: print real expression DUlp([-2.0,2.0])"
  real_print pRealDUlp


{----------------------------------------------------------------------}
{-                                                                    -}
{- C Real expression Vector functions                                 -}
{-                                                                    -}
{----------------------------------------------------------------------}

realExpressionVectorFeedback = do
  disableBuffering
  putStrLn ""
  putStrLn "----------------------------------------"
  putStrLn "Real Expression Vector"
  putStrLn "----------------------------------------"
  pInterval <- interval_create (-2.0) 2.0
  putStrLn "Main: creating real value [-2.0,2.0]"
  pRealValue <- real_create_value pInterval
  putStrLn "Main: print real value [-2.0,2.0]"
  real_print pRealValue
  putStrLn "Main: creating real variable var(2,\"Y\")"
  cString <- newCString "Y"
  pRealVariable <- real_create_variable 2 cString
  putStrLn "Main: print real variable var(2,\"Y\")"
  real_print pRealVariable
  putStrLn "Main: creating empty real expression"
  pVector <- real_vector_create
  real_vector_print pVector
  putStrLn "Main: addding val([-2.0,2.0]) to vector"
  real_vector_add pVector pRealValue
  real_vector_print pVector
  putStrLn "Main: addding var(2,\"Y\") to vector"
  real_vector_add pVector pRealVariable
  real_vector_print pVector
  putStrLn "Main: printing real expression vector{[-2.0,2.0],var(2,\"Y\")}"
  real_vector_print pVector
  putStrLn "Main: creating maximum expression from real expression vector{[-2.0,2.0],var(2,\"Y\")}"
  maxExpression <- real_create_maximum pVector
  real_print maxExpression


{----------------------------------------------------------------------}
{-                                                                    -}
{- C Real Error expressions functions                                 -}
{-                                                                    -}
{----------------------------------------------------------------------}

realErrorExpressionsFeedback = do
  disableBuffering
  putStrLn ""
  putStrLn "----------------------------------------"
  putStrLn "Real Error Expressions"
  putStrLn "----------------------------------------"
  pReal1 <- interval_create 1.0 1.0 >>= real_create_value
  pReal2 <- interval_create 2.0 2.0 >>= real_create_value
  pReal3 <- interval_create 5.0 5.0 >>= real_create_value
  pReal4 <- interval_create 7.0 7.0 >>= real_create_value
  putStr "Left operand: " >> real_print pReal1
  putStr "Left operand error: " >> real_print pReal2
  putStr "Right operand: " >> real_print pReal3
  putStr "Right operand error: " >> real_print pReal4
  putStr "Negation error: " >> real_create_error_negation pReal1 pReal2 >>= real_print
  putStr "Addition error: " >> real_create_error_addition pReal1 pReal2 pReal3 pReal4 >>= real_print
  putStr "Subtraction error: " >> real_create_error_subtraction pReal1 pReal2 pReal3 pReal4 >>= real_print
  putStr "Multiplication error: " >> real_create_error_multiplication pReal1 pReal2 pReal3 pReal4 >>= real_print
  putStr "Division error: " >> real_create_error_division pReal1 pReal2 pReal3 pReal4 >>= real_print


{----------------------------------------------------------------------}
{-                                                                    -}
{- C MinMax System functions                                          -}
{-                                                                    -}
{----------------------------------------------------------------------}

minmaxSystemFeedback = do
  disableBuffering
  putStrLn ""
  putStrLn "----------------------------------------"
  putStrLn "MinMax Systems"
  putStrLn "----------------------------------------"
  systemName <- newCString "A single variable trivial system"
  minmaxSystem <- minmax_system_create systemName
  variableName <- newCString "variableName"
  lowerBound <- interval_create 1.0 1.0
  upperBound <- interval_create 200.0 200.0
  minmax_system_register_variable minmaxSystem variableName lowerBound upperBound
  minmax_system_set_maxdepth minmaxSystem 9
  minmax_system_set_precision minmaxSystem (-16)
  anotherVariableName <- newCString "variableName"
  anExpression <- real_create_variable 0 anotherVariableName
  minmax_system_maximize minmaxSystem anExpression
  putStr "MinMax System: " >> minmax_system_print minmaxSystem
  putStr "Maximum's lower/upper bound: ["
  minmax_system_maximum_lower_bound minmaxSystem >>= putStr . show
  putStr ","
  minmax_system_maximum_upper_bound minmaxSystem >>= putStr . show
  putStrLn "]"

  systemName <- newCString "A dual variable simple system"
  minmaxSystem <- minmax_system_create systemName
  variableXName <- newCString "X"
  xLowerBound <- interval_create 1.0 1.0
  xUpperBound <- interval_create 20.0 20.0
  minmax_system_register_variable minmaxSystem variableXName xLowerBound xUpperBound
  variableYName <- newCString "Y"
  yLowerBound <- interval_create 30.0 30.0
  yUpperBound <- interval_create 70.0 70.0
  minmax_system_register_variable minmaxSystem variableYName yLowerBound yUpperBound
  minmax_system_set_maxdepth minmaxSystem 9
  minmax_system_set_precision minmaxSystem (-16)
  variableX <- real_create_variable 0 variableXName
  variableY <- real_create_variable 1 variableYName
  xPlusY <- real_create_addition variableX variableY
  fullExpression <- real_create_division variableX xPlusY
  minmax_system_maximize minmaxSystem fullExpression
  putStr "MinMax System: " >> minmax_system_print minmaxSystem
  putStr "Maximum's lower/upper bound: ["
  minmax_system_maximum_lower_bound minmaxSystem >>= putStr . show
  putStr ","
  minmax_system_maximum_upper_bound minmaxSystem >>= putStr . show
  putStrLn "]"

  systemName <- newCString "A single variable example-system"
  minmaxSystem <- minmax_system_create systemName
  variableXName <- newCString "X"
  lowerBound <- interval_create 0.0 0.0
  upperBound <- interval_create 999.0 999.0
  minmax_system_register_variable minmaxSystem variableXName lowerBound upperBound
  minmax_system_set_maxdepth minmaxSystem 9
  minmax_system_set_precision minmaxSystem (-40)
  variableX <- real_create_variable 0 variableXName
  value0 <- interval_create 0.0 0.0 >>= real_create_value
  value1 <- interval_create 1.0 1.0 >>= real_create_value
  value2 <- interval_create 2.0 2.0 >>= real_create_value
  dUlpX <- real_create_double_ulp variableX
  dUlpXDividedBy2 <- real_create_division dUlpX value2
  addError <- real_create_error_addition variableX dUlpXDividedBy2 value1 value0
  xPlus1 <- real_create_addition variableX value1
  fullExpression <- real_create_error_division variableX dUlpXDividedBy2 xPlus1 addError
  minmax_system_maximize minmaxSystem fullExpression
  putStr "MinMax System: " >> minmax_system_print minmaxSystem
  putStr "Maximum's lower/upper bound: ["
  minmax_system_maximum_lower_bound minmaxSystem >>= putStr . show
  putStr ","
  minmax_system_maximum_upper_bound minmaxSystem >>= putStr . show
  putStrLn "]"

