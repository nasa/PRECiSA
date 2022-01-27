-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


{-# LANGUAGE ForeignFunctionInterface #-}

module Kodiak.Kodiak where

import Foreign
import Foreign.C.Types
import Foreign.C.String

newtype PInterval     = PInterval     (Ptr ())
newtype PReal         = PReal         (Ptr ())
newtype PBool         = PBool         (Ptr ())
newtype PRealVector   = PRealVector   (Ptr ())
newtype PMinMaxSystem = PMinMaxSystem (Ptr ())
newtype PPaver        = PPaver        (Ptr ())


{----------------------------------------------------------------------}
{-                                                                    -}
{- C Interval functions                                               -}
{-                                                                    -}
{----------------------------------------------------------------------}

foreign import ccall unsafe ""
  interval_create :: CDouble -> CDouble -> IO PInterval

foreign import ccall unsafe ""
  interval_create_from_rational :: CInt -> CUInt -> IO PInterval

foreign import ccall unsafe ""
  interval_print :: PInterval -> IO ()


{----------------------------------------------------------------------}
{-                                                                    -}
{- C Real expressions functions                                       -}
{-                                                                    -}
{----------------------------------------------------------------------}

foreign import ccall unsafe ""
  real_print :: PReal -> IO ()

foreign import ccall unsafe ""
  real_to_string :: PReal -> IO CString

foreign import ccall unsafe ""
  real_equal_to :: PReal -> PReal -> IO CInt

real_equal_to_ :: PReal -> PReal -> IO Bool
real_equal_to_ p1 p2 = real_equal_to p1 p2 >>= \res -> return (res /= 0)

foreign import ccall unsafe ""
  real_create_value :: PInterval -> IO PReal

foreign import ccall unsafe ""
  real_create_variable :: CUInt -> CString -> IO PReal

foreign import ccall unsafe ""
  real_create_local_variable :: CString -> IO PReal

foreign import ccall unsafe ""
  real_create_letin :: CString -> PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_vector_create :: IO PRealVector

foreign import ccall unsafe ""
  real_vector_add :: PRealVector -> PReal -> IO ()

foreign import ccall unsafe ""
  real_vector_print :: PRealVector -> IO ()

foreign import ccall unsafe ""
  real_create_maximum :: PRealVector -> IO PReal

foreign import ccall unsafe ""
  real_create_absolute_value :: PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_sine :: PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_arcsine :: PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_cosine :: PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_arccosine :: PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_tangent :: PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_arctangent :: PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_floor :: PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_double_ulp :: PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_single_ulp :: PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_negation :: PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_addition :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_subtraction :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_multiplication :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_division :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_sqrt :: PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_eexponent :: PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_elogarithm :: PReal -> IO PReal


{----------------------------------------------------------------------}
{-                                                                    -}
{- C Boolean expressions functions                                    -}
{-                                                                    -}
{----------------------------------------------------------------------}

foreign import ccall unsafe ""
  bool_create_true :: IO PBool

foreign import ccall unsafe ""
  bool_create_false :: IO PBool

foreign import ccall unsafe ""
  bool_create_possibly :: IO PBool

foreign import ccall unsafe ""
  bool_create_within_eps :: IO PBool

foreign import ccall unsafe ""
  bool_print :: PBool -> IO ()

foreign import ccall unsafe ""
  bool_equal_to :: PBool -> PBool -> IO CInt

bool_equal_to_ :: PBool -> PBool -> IO Bool
bool_equal_to_ p1 p2 = bool_equal_to p1 p2 >>= \res -> return (res /= 0)

foreign import ccall unsafe ""
  bool_create_not :: PBool -> IO PBool

foreign import ccall unsafe ""
  bool_create_and :: PBool -> PBool -> IO PBool

foreign import ccall unsafe ""
  bool_create_or :: PBool -> PBool -> IO PBool

foreign import ccall unsafe ""
  bool_create_implies :: PBool -> PBool -> IO PBool

foreign import ccall unsafe ""
  bool_create_equal_to :: PReal -> PReal -> IO PBool

foreign import ccall unsafe ""
  bool_create_less_than :: PReal -> PReal -> IO PBool

foreign import ccall unsafe ""
  bool_create_less_than_or_equal_to :: PReal -> PReal -> IO PBool

foreign import ccall unsafe ""
  bool_create_greater_than :: PReal -> PReal -> IO PBool

foreign import ccall unsafe ""
  bool_create_greater_than_or_equal_to :: PReal -> PReal -> IO PBool


{----------------------------------------------------------------------}
{-                                                                    -}
{- C Real Error expressions functions                                 -}
{-                                                                    -}
{----------------------------------------------------------------------}

foreign import ccall unsafe ""
  real_create_error_negation :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_error_addition :: PReal -> PReal -> PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_error_subtraction :: PReal -> PReal -> PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_error_multiplication :: PReal -> PReal -> PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_error_division :: PReal -> PReal -> PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_error_sine :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_error_arcsine :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_error_cosine :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_error_arccosine :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_error_tangent :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_error_arctangent :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_error_arctangent_tight :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_error_eexponent :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_error_elogarithm :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_error_floor :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_error_floor_tight :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_error_sqrt :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_error_power_of_two_multiplication :: CInt -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_single_error_negation :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_single_error_addition :: PReal -> PReal -> PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_single_error_subtraction :: PReal -> PReal -> PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_single_error_multiplication :: PReal -> PReal -> PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_single_error_division :: PReal -> PReal -> PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_single_error_sine :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_single_error_cosine :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_single_error_arctangent :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_single_error_arctangent_tight :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_single_error_eexponent :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_single_error_elogarithm :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_single_error_floor :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_single_error_floor_tight :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_single_error_sqrt :: PReal -> PReal -> IO PReal

foreign import ccall unsafe ""
  real_create_single_error_power_of_two_multiplication :: CInt -> PReal -> IO PReal


{----------------------------------------------------------------------}
{-                                                                    -}
{- C MinMaxSystem                                                     -}
{-                                                                    -}
{----------------------------------------------------------------------}

foreign import ccall unsafe ""
  minmax_system_create :: CString -> IO PMinMaxSystem

foreign import ccall unsafe ""
  minmax_system_print :: PMinMaxSystem -> IO ()

foreign import ccall unsafe ""
  minmax_system_register_variable :: PMinMaxSystem -> CString -> PInterval -> PInterval -> IO ()

foreign import ccall unsafe ""
  minmax_system_set_maxdepth :: PMinMaxSystem -> CUInt -> IO ()

foreign import ccall unsafe ""
  minmax_system_set_precision :: PMinMaxSystem -> CInt -> IO ()

foreign import ccall unsafe ""
  minmax_system_maximize :: PMinMaxSystem -> PReal -> IO ()

foreign import ccall unsafe ""
  minmax_system_maximum_lower_bound :: PMinMaxSystem -> IO CDouble

foreign import ccall unsafe ""
  minmax_system_maximum_upper_bound :: PMinMaxSystem -> IO CDouble


{----------------------------------------------------------------------}
{-                                                                    -}
{- C Paver                                                            -}
{-                                                                    -}
{----------------------------------------------------------------------}

foreign import ccall unsafe ""
  paver_create :: CString -> IO PPaver

foreign import ccall unsafe ""
  paver_print :: PPaver -> IO ()

foreign import ccall unsafe ""
  paver_register_variable :: PPaver -> CString -> PInterval -> PInterval -> IO ()

foreign import ccall unsafe ""
  paver_set_maxdepth :: PPaver -> CUInt -> IO ()

foreign import ccall unsafe ""
  paver_set_precision :: PPaver -> CInt -> IO ()

foreign import ccall unsafe ""
  paver_pave :: PPaver -> PBool -> IO ()

foreign import ccall unsafe ""
  paver_save_paving :: PPaver -> CString -> IO ()
