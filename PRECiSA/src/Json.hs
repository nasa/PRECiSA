-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Json where

import GHC.Generics
import Data.Aeson

data AnalysisResultFun = AnalysisResultFun {
    function :: String,
    stableError :: Double,
    unstableError :: Maybe Double
} deriving (Generic, Show)

data AnalysisResult = AnalysisResult {
    results :: [AnalysisResultFun],
    certFile :: String,
    numCertFile :: String
} deriving (Generic, Show)


instance ToJSON AnalysisResultFun
instance FromJSON AnalysisResultFun

instance ToJSON AnalysisResult
  where
    toEncoding (AnalysisResult results certFile numCertFile) =
      pairs ("results" .= results <> "certFile" .= certFile <> "numCertFile" .= numCertFile)

instance FromJSON AnalysisResult



toAnalysisResultFun :: (String, Double, Maybe Double) -> AnalysisResultFun
toAnalysisResultFun (funName, stableErr, unstableErr) =
  AnalysisResultFun { function = funName,
                      stableError = stableErr,
                      unstableError = unstableErr
                    }

toAnalysisResult :: [(String, Double, Maybe Double)] -> String -> String -> AnalysisResult
toAnalysisResult results certFile certFileName =
  AnalysisResult {
    results = map toAnalysisResultFun results,
    certFile = certFile,
    numCertFile = certFileName
  }
-- toJSONAnalysisResults :: [(String, Double, Maybe Double)] -> String -> String ->
toJSONAnalysisResults results certFile certFileName  = encode $ toAnalysisResult results certFile certFileName