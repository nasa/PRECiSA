-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

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
  real_create_minimum :: PRealVector -> IO PReal

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
  minmax_system_minmax :: PMinMaxSystem -> PReal -> IO ()

foreign import ccall unsafe ""
  minmax_system_maximum_lower_bound :: PMinMaxSystem -> IO CDouble

foreign import ccall unsafe ""
  minmax_system_maximum_upper_bound :: PMinMaxSystem -> IO CDouble

foreign import ccall unsafe ""
  minmax_system_minimum_lower_bound :: PMinMaxSystem -> IO CDouble

foreign import ccall unsafe ""
  minmax_system_minimum_upper_bound :: PMinMaxSystem -> IO CDouble


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

{----------------------------------------------------------------------}
{-                                                                    -}
{- PRECiSA C++ shim over Kodiak (see cbits/kodiak_shim.cpp)           -}
{-                                                                    -}
{----------------------------------------------------------------------}

-- Kodiak reports failures by throwing @kodiak::Growl@. A C++ exception cannot
-- be caught from Haskell, so it escapes the FFI boundary and terminates the
-- process. The entry points below go through a PRECiSA-owned C++ wrapper that
-- catches it and reports a status code plus the exception message.
--
-- The @c_precisa_*@ foreign imports are the raw boundary and are not meant to
-- be called directly: the guarded wrappers underneath are the intended API.
-- They exist to enforce, structurally rather than by comment, the contract
-- that a failed system must never be read from or reused (see below).

-- | Outcome of a Kodiak call made through the shim.
--
-- Division by an interval that contains zero is singled out because it is a
-- legitimate mathematical outcome (an unbounded result), not a failure. Every
-- other Kodiak exception is a genuine error carrying its message, and the two
-- must never be conflated: reporting a real failure as an unbounded bound
-- would turn a crash into a plausible-looking answer.
data KodiakStatus = KodiakOk | KodiakDivByZero | KodiakError String
  deriving (Show, Eq)

-- | Size of the buffer handed to the shim for the exception message.
kodiakErrorBufferSize :: Int
kodiakErrorBufferSize = 256

-- | Allocate the message buffer, run a shim call, and decode its status code.
withKodiakStatus :: (CString -> CInt -> IO CInt) -> IO KodiakStatus
withKodiakStatus call =
  allocaBytes kodiakErrorBufferSize $ \errBuf -> do
    status <- call errBuf (fromIntegral kodiakErrorBufferSize)
    case status of
      0 -> return KodiakOk
      1 -> return KodiakDivByZero
      2 -> KodiakError <$> peekCString errBuf
      _ -> error $ "Kodiak.Kodiak: unexpected status code " ++ show status
                ++ " from the PRECiSA Kodiak shim (cbits/kodiak_shim.cpp);"
                ++ " the C and Haskell sides have drifted."

-- | Run a shim call, producing its result only when the call succeeded.
kodiakGuarded :: (CString -> CInt -> IO CInt) -> IO a -> IO (Either KodiakStatus a)
kodiakGuarded call readResult = do
  status <- withKodiakStatus call
  case status of
    KodiakOk -> Right <$> readResult
    _        -> return (Left status)

foreign import ccall unsafe "precisa_minmax_system_maximize"
  c_precisa_minmax_system_maximize
    :: PMinMaxSystem -> PReal -> CString -> CInt -> IO CInt

foreign import ccall unsafe "precisa_minmax_system_minmax"
  c_precisa_minmax_system_minmax
    :: PMinMaxSystem -> PReal -> CString -> CInt -> IO CInt

foreign import ccall unsafe "precisa_real_create_division"
  c_precisa_real_create_division
    :: PReal -> PReal -> Ptr (Ptr ()) -> CString -> CInt -> IO CInt

foreign import ccall unsafe "precisa_minmax_system_maximum_lower_bound"
  c_precisa_minmax_system_maximum_lower_bound
    :: PMinMaxSystem -> Ptr CDouble -> CString -> CInt -> IO CInt

foreign import ccall unsafe "precisa_minmax_system_maximum_upper_bound"
  c_precisa_minmax_system_maximum_upper_bound
    :: PMinMaxSystem -> Ptr CDouble -> CString -> CInt -> IO CInt

foreign import ccall unsafe "precisa_minmax_system_minimum_lower_bound"
  c_precisa_minmax_system_minimum_lower_bound
    :: PMinMaxSystem -> Ptr CDouble -> CString -> CInt -> IO CInt

foreign import ccall unsafe "precisa_minmax_system_minimum_upper_bound"
  c_precisa_minmax_system_minimum_upper_bound
    :: PMinMaxSystem -> Ptr CDouble -> CString -> CInt -> IO CInt

-- | Maximize @pExpr@ over the system's box.
--
-- @Left 'KodiakDivByZero'@ means Kodiak's divisor guard fired: a legitimate
-- unbounded result. @Left ('KodiakError' msg)@ is a genuine failure.
--
-- On either failure the system is DEAD and is deliberately not returned:
--
--   * its bounds cover only part of the box (branch-and-bound aborted
--     mid-recursion) and are not a valid enclosure, so they must not be read;
--   * reusing it is silently unsound, because @MinMaxSystem::acc_@ (the
--     pruning accumulator) is never reset by @minmax()@, so a second run
--     prunes against stale bounds and reports a too-small maximum with no
--     diagnostic.
--
-- Simply dropping it is safe: Kodiak is value-typed with intrusively
-- refcounted @Real@ nodes.
maximizeGuarded :: PMinMaxSystem -> PReal -> IO (Either KodiakStatus ())
maximizeGuarded pSys pExpr =
  kodiakGuarded (c_precisa_minmax_system_maximize pSys pExpr) (return ())

-- | Minimize AND maximize @pExpr@ over the system's box in a single run, so
-- that both the minimum's lower bound and the maximum's upper bound become
-- readable afterwards.
--
-- Statuses mean exactly what they mean for 'maximizeGuarded', and the failure
-- contract is identical: this is the same branch-and-bound evaluation, so on a
-- non-'KodiakOk' status the system is DEAD -- its partial bounds are not an
-- enclosure and its @acc_@ pruning accumulator is stale -- and it is
-- deliberately not returned.
minmaxGuarded :: PMinMaxSystem -> PReal -> IO (Either KodiakStatus ())
minmaxGuarded pSys pExpr =
  kodiakGuarded (c_precisa_minmax_system_minmax pSys pExpr) (return ())

-- | Build a division expression, catching the construction-time divisor guard
-- (@Real.cpp:214@) that fires when the divisor is a literal interval
-- containing zero.
realCreateDivision :: PReal -> PReal -> IO (Either KodiakStatus PReal)
realCreateDivision num den =
  alloca $ \pOut ->
    kodiakGuarded (c_precisa_real_create_division num den pOut)
                  (PReal <$> peek pOut)

-- | Read a bound through the shim. The four @MinMax@ getters throw whenever
-- the corresponding point set is empty, which is exactly the state left behind
-- by a failed maximization -- and also by an infeasible box on the success
-- path -- so they cannot be called raw.
kodiakBound :: (PMinMaxSystem -> Ptr CDouble -> CString -> CInt -> IO CInt)
            -> PMinMaxSystem -> IO (Either KodiakStatus CDouble)
kodiakBound cCall pSys = alloca $ \pOut -> kodiakGuarded (cCall pSys pOut) (peek pOut)

maximumLowerBoundGuarded :: PMinMaxSystem -> IO (Either KodiakStatus CDouble)
maximumLowerBoundGuarded = kodiakBound c_precisa_minmax_system_maximum_lower_bound

maximumUpperBoundGuarded :: PMinMaxSystem -> IO (Either KodiakStatus CDouble)
maximumUpperBoundGuarded = kodiakBound c_precisa_minmax_system_maximum_upper_bound

-- | Read the minimum's lower bound, as the min-max path does after
-- 'minmaxGuarded'.
minimumLowerBoundGuarded :: PMinMaxSystem -> IO (Either KodiakStatus CDouble)
minimumLowerBoundGuarded = kodiakBound c_precisa_minmax_system_minimum_lower_bound

-- | Unused today, wrapped pre-emptively for symmetry with the maximum
-- getters. See the note above @PRECISA_WRAP_BOUND@ in @cbits/kodiak_shim.cpp@.
minimumUpperBoundGuarded :: PMinMaxSystem -> IO (Either KodiakStatus CDouble)
minimumUpperBoundGuarded = kodiakBound c_precisa_minmax_system_minimum_upper_bound
