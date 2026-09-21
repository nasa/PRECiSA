-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- 'ToJSON RelError' below is an orphan on purpose: 'RelError' is an analysis
-- value and RelativeError must not acquire a dependency on aeson just to be
-- printed. JSON encoding is this module's whole job, so the instance lives
-- here.
{-# OPTIONS_GHC -Wno-orphans #-}

module JSON where

import GHC.Generics
import Data.Aeson
import Data.ByteString.Lazy.Internal
import AbsPVSLang (ResultField(..))
import AnalysisResult (FunSummary(..), RelErrorResult(..))
import RelativeError (RelError(..))

-- | One entry of the @results@ array.
--
--   The VSCode extension consumes a FIXED contract for the relative error:
--   @relativeStableError@ and @relativeUnstableError@ are each a JSON number,
--   or the string @"infinity"@, or ABSENT. Absent means no bound is available
--   -- the feature is off, or the analysis failed. A failure is additionally
--   reported in @relativeStableErrorFailure@ / @relativeUnstableErrorFailure@,
--   and must never appear as a bound.
--
--   The failure keys are PER SIDE, mirroring the value keys, because the two
--   sides fail independently: a program can have a proved bound on its stable
--   paths and a Kodiak failure on its unstable ones. A single shared failure
--   key made that case indistinguishable from a total failure. For each side
--   exactly one of the value key and the failure key is present, or -- with the
--   feature off -- neither.
data AnalysisResultFun = AnalysisResultFun {
    function :: String,
    stableError :: Double,
    unstableError :: Maybe Double,
    relativeStableError :: Maybe RelError,
    relativeUnstableError :: Maybe RelError,
    relativeStableErrorFailure :: Maybe String,
    relativeUnstableErrorFailure :: Maybe String
} deriving (Generic, Show)

data AnalysisResult = AnalysisResult {
    results :: [AnalysisResultFun],
    certFile :: String,
    numCertFile :: String
} deriving (Generic, Show)

instance ToJSON RelError where
  toJSON (RelFinite ub) = toJSON ub
  toJSON RelInfinite    = toJSON ("infinity" :: String)

-- Written out rather than derived so that a 'Nothing' relative field OMITS its
-- key instead of emitting null: absent is what the extension reads as "no
-- bound". The first three pairs must keep emitting exactly as the derived
-- instance did, @unstableError: null@ included -- the regression baseline
-- compares byte for byte.
--
-- ONLY 'toJSON' IS DEFINED, ON PURPOSE. 'toEncoding' is left to its class
-- default of @value . toJSON@, which is exactly the route the empty derived
-- instance took before this feature existed. Writing 'toEncoding' directly
-- instead would build the output with @E.double@ rather than through
-- 'Data.Scientific.Scientific', and that renders the SAME NUMBER
-- DIFFERENTLY: @0@ becomes @0.0@, @2@ becomes @2.0@, and
-- @16072157233039065000000000000000000000000@ becomes @1.6072157233039065e40@.
-- Numerically equal, byte-wise not -- and with @--relative-error@ off the
-- output must be byte-identical to before the feature.
--
-- Going through 'toJSON' means the keys come out in 'Data.Aeson.KeyMap' order
-- rather than in the order written here, so ADDING a relative key can reorder
-- the absolute ones. That is fine: consumers parse this, and the contract is
-- which keys are present, not where. It costs nothing with the feature off,
-- where the key set is exactly the pre-feature one.
instance ToJSON AnalysisResultFun
  where
    toJSON r = object $
         [ "function"      .= function r
         , "stableError"   .= stableError r
         , "unstableError" .= unstableError r ]
      ++ maybe [] (\v -> ["relativeStableError"   .= v]) (relativeStableError r)
      ++ maybe [] (\v -> ["relativeUnstableError" .= v]) (relativeUnstableError r)
      ++ maybe [] (\v -> ["relativeStableErrorFailure"   .= v]) (relativeStableErrorFailure r)
      ++ maybe [] (\v -> ["relativeUnstableErrorFailure" .= v]) (relativeUnstableErrorFailure r)

instance ToJSON AnalysisResult
  where
    toEncoding (AnalysisResult res certFileName numCertFileName) =
      pairs ("results" .= res <> "certFile" .= certFileName <> "numCertFile" .= numCertFileName)

toAnalysisResultFun :: FunSummary -> AnalysisResultFun
toAnalysisResultFun fs =
  AnalysisResultFun { function = fsName fs ++ printFieldName (fsField fs),
                      stableError = fsStable fs,
                      unstableError = fsUnstable fs,
                      relativeStableError = relBound (fsRelStable fs),
                      relativeUnstableError = relBound (fsRelUnstable fs),
                      relativeStableErrorFailure   = relFailure (fsRelStable fs),
                      relativeUnstableErrorFailure = relFailure (fsRelUnstable fs)
                    }
  where
    -- A failure is NOT a bound: it leaves the value field absent.
    relBound RelErrorOff        = Nothing
    relBound (RelErrorFailed _) = Nothing
    relBound (RelErrorBound r)  = Just r

    relFailure RelErrorOff          = Nothing
    relFailure (RelErrorFailed msg) = Just msg
    relFailure (RelErrorBound _)    = Nothing

    printFieldName ResValue = ""
    printFieldName (ResRecordField recField) = recField
    printFieldName (ResTupleIndex tupleIdx) = show tupleIdx

toAnalysisResult :: [FunSummary] -> String -> String -> AnalysisResult
toAnalysisResult res certFileName numCertFileName =
  AnalysisResult {
    results = map toAnalysisResultFun res,
    certFile = certFileName,
    numCertFile = numCertFileName
  }

toJSONAnalysisResults :: [FunSummary]
                               -> String -> String -> Data.ByteString.Lazy.Internal.ByteString
toJSONAnalysisResults res certFileName numCertFileName  = encode $ toAnalysisResult res certFileName numCertFileName