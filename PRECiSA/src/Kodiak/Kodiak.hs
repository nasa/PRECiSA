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


{-# LANGUAGE ForeignFunctionInterface #-}

module Kodiak.Kodiak where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import System.IO

newtype PInterval     = PInterval     (Ptr ())
newtype PReal         = PReal         (Ptr ())
newtype PRealVector   = PRealVector   (Ptr ())
newtype PMinMaxSystem = PMinMaxSystem (Ptr ())


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
  real_to_string :: PReal -> IO (CString)

foreign import ccall unsafe ""
  real_create_value :: PInterval -> IO (PReal)

foreign import ccall unsafe ""
  real_create_variable :: CUInt -> CString -> IO (PReal)

foreign import ccall unsafe ""
  real_vector_create :: IO (PRealVector)

foreign import ccall unsafe ""
  real_vector_add :: PRealVector -> PReal -> IO ()

foreign import ccall unsafe ""
  real_vector_print :: PRealVector -> IO ()

foreign import ccall unsafe ""
  real_create_maximum :: PRealVector -> IO (PReal)

foreign import ccall unsafe ""
  real_create_absolute_value :: PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_sine :: PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_cosine :: PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_floor :: PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_double_ulp :: PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_negation :: PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_addition :: PReal -> PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_subtraction :: PReal -> PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_multiplication :: PReal -> PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_division :: PReal -> PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_eexponent :: PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_elogarithm :: PReal -> IO (PReal)

{----------------------------------------------------------------------}
{-                                                                    -}
{- C Real Error expressions functions                                 -}
{-                                                                    -}
{----------------------------------------------------------------------}

foreign import ccall unsafe ""
  real_create_error_negation :: PReal -> PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_error_addition :: PReal -> PReal -> PReal -> PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_error_subtraction :: PReal -> PReal -> PReal -> PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_error_multiplication :: PReal -> PReal -> PReal -> PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_error_division :: PReal -> PReal -> PReal -> PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_error_sine :: PReal -> PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_error_cosine :: PReal -> PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_error_arctangent :: PReal -> PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_error_eexponent :: PReal -> PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_error_elogarithm :: PReal -> PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_error_floor :: PReal -> PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_error_floor_tight :: PReal -> PReal -> IO (PReal)

foreign import ccall unsafe ""
  real_create_error_power_of_two_multiplication :: CInt -> PReal -> IO (PReal)


{----------------------------------------------------------------------}
{-                                                                    -}
{- C Real Error expressions functions                                 -}
{-                                                                    -}
{----------------------------------------------------------------------}

foreign import ccall unsafe ""
  minmax_system_create :: CString -> IO (PMinMaxSystem)

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
  minmax_system_maximum_lower_bound :: PMinMaxSystem -> IO (CDouble)

foreign import ccall unsafe ""
  minmax_system_maximum_upper_bound :: PMinMaxSystem -> IO (CDouble)

