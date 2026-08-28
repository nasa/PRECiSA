-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Kodiak.Paver where

import qualified Foreign.C as C
import Data.Maybe (fromMaybe)
import Control.Exception (Exception,throwIO)
import Control.Monad (foldM)
import Control.Monad.Except (runExcept)

import AbsSpecLang
import AbsPVSLang (BExpr(..), FBExpr(..),ResultField)
import AbstractDomain
import Kodiak.Kodiak
import Kodiak.Runnable
import Kodiak.Generator
import qualified Kodiak.Runnable   as KR
import qualified Kodiak.Runner     as KR
import qualified Kodiak.Expression as K

data SearchParameters = SP { maximumDepth :: C.CUInt, minimumPrecision :: C.CUInt }

data Input = Input { name       :: String
                   , expression :: K.BExpr
                   , bindings   :: [VarBind]
                   , maxDepth   :: C.CUInt
                   , precision  :: C.CUInt
                   }

newtype Output = Output { filename :: FilePath } deriving (Eq,Show)

type FunName = String

-- | Kodiak could not PAVE the box: an evaluation-time failure, thrown from
--   inside the paver's branch-and-bound loop.
--
--   The paver's counterpart of 'Kodiak.Runner.KodiakMaximizeFailed', and a
--   separate type for the same reason that the min-max failure is: this is a
--   different evaluator answering a different question -- which subregions of
--   the input box are unstable, not how large the error is -- so its failures
--   need their own message to be diagnosable.
--
--   Kodiak signals such failures by throwing a C++ exception, which escapes the
--   FFI and kills the process (SIGABRT); going through 'paveGuarded' and
--   re-throwing the decoded status turns that abort into a Haskell exception
--   callers can catch and the top level can print.
newtype KodiakPaveFailed = KodiakPaveFailed KodiakStatus

-- | Written out rather than derived because this string is what PRECiSA prints
--   when the exception reaches the top level: GHC's default handler SHOWS the
--   exception, so 'show' is the user-facing message and has to read like one.
instance Show KodiakPaveFailed where
  show (KodiakPaveFailed status) =
    "Kodiak failed while paving the regions of unstability: " ++ describeStatus status

instance Exception KodiakPaveFailed

-- | The paving could not be WRITTEN to its file.
--
--   Deliberately distinct from 'KodiakPaveFailed': the paving itself succeeded
--   and the failure is in saving it, so the two point at completely different
--   causes -- an unwritable path or an allocation failure while formatting the
--   boxes, versus a formula that could not be evaluated over the box. Reporting
--   one as the other would send the reader looking in the wrong place.
data KodiakSavePavingFailed = KodiakSavePavingFailed FilePath KodiakStatus

-- | Hand-written for the same reason as 'Show' 'KodiakPaveFailed'. Names the
--   file, because that is the first thing to check when a write fails.
instance Show KodiakSavePavingFailed where
  show (KodiakSavePavingFailed file status) =
    "Kodiak failed while writing the paving to " ++ file ++ ": " ++ describeStatus status

instance Exception KodiakSavePavingFailed

-- | Shared by both 'Show' instances above, so that a paving failure and a save
--   failure describe the same status identically.
describeStatus :: KodiakStatus -> String
describeStatus KodiakOk          = "reported success (should not happen)"
describeStatus KodiakDivByZero   = "division by an interval that contains zero"
describeStatus (KodiakError msg) = msg

instance KR.KodiakRunnable Input () Output where
  run Input { name, expression, bindings, maxDepth, precision } _ = do
    let variableMap   = KR.variableMapFromBinds bindings
    cName <- C.newCString name
    pSys <- paver_create cName
    paver_set_maxdepth pSys maxDepth
    paver_set_precision pSys (negate (fromInteger $ toInteger precision))
    pExpr <- KR.run expression variableMap
    mapM_ (`KR.run` pSys) bindings
    -- A failed pave leaves a paving of only the part of the box explored
    -- before Kodiak threw, so throwing here is also what stops the save below
    -- from writing that partial paving out as if it were the real one.
    paveGuarded pSys pExpr >>= either (throwIO . KodiakPaveFailed) return
    let outputFile = name
    cFilename <- C.newCString outputFile
    savePavingGuarded pSys cFilename
      >>= either (throwIO . KodiakSavePavingFailed outputFile) return
    return $ Output outputFile

paveUnstabilityConditions :: [(FunName,ResultField,K.BExpr)] -> Spec -> SearchParameters -> (String -> String) -> IO [(String,ResultField,FilePath)]
paveUnstabilityConditions condMap (Spec bindings) searchParams nameGen = mapM paveFunction condMap
  where
    bindingsMap = map (\(SpecBind f b) -> (f,b)) bindings
    paveFunction (name,field,bexpr) = labelWith name field . filename <$> run kodiakInput ()
      where
        labelWith name' field' x = (name',field',x)
        kodiakInput = Input { name = nameGen name
                            , expression = bexpr
                            , bindings = fromMaybe (error $ "runFunction: var " ++ show name ++ " not found.") (lookup name bindingsMap)
                            , maxDepth = maximumDepth searchParams
                            , precision = minimumPrecision searchParams
                            }

conds2Kodiak' :: [Conditions] -> Maybe K.BExpr
conds2Kodiak' = foldM (\b a -> conds2Kodiak a >>= (Just . K.Or b)) K.False
  where
    conds2Kodiak :: Conditions -> Maybe K.BExpr
    conds2Kodiak (Conds cs) = foldM (\b a -> cond2Kodiak a >>= (Just . K.Or b)) K.False cs
      where
        cond2Kodiak :: Condition -> Maybe K.BExpr
        cond2Kodiak cond =
          case runExcept (kodiakize (And (realPathCond cond) (realCond cond))) of
            Left _ -> Nothing
            Right kr -> case runExcept (kodiakize (FAnd (fpPathCond cond) (fpCond cond))) of
              Left _ -> Nothing
              Right kf -> Just (K.And kr kf)