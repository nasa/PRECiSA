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
import Control.Monad (foldM)
import Control.Monad.Except (runExcept)

import AbsSpecLang
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

data Output = Output { filename :: FilePath } deriving (Eq,Show)

type FunName = String

instance KR.KodiakRunnable Input () Output where
  run Input { name, expression, bindings, maxDepth, precision } _ = do
    let variableMap   = KR.variableMapFromBinds bindings
    cName <- C.newCString name
    pSys <- paver_create cName
    paver_set_maxdepth pSys maxDepth
    paver_set_precision pSys (negate (fromInteger $ toInteger precision))
    pExpr <- KR.run expression variableMap
    mapM_ (flip KR.run pSys) bindings
    paver_pave pSys pExpr
    let outputFile = name
    cFilename <- C.newCString outputFile
    paver_save_paving pSys cFilename
    return $ Output outputFile

paveUnstabilityConditions :: [(FunName,K.BExpr)] -> Spec -> SearchParameters -> (String -> String) -> IO [(String,FilePath)]
paveUnstabilityConditions condMap (Spec bindings) searchParams nameGen = mapM paveFunction condMap
  where
    bindingsMap = map (\(SpecBind f b) -> (f,b)) bindings
    paveFunction (name,bexpr) = labelWith name . (filename) <$> run kodiakInput ()
      where
        labelWith name' x = (name',x)
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
    conds2Kodiak (Cond cs) = foldM (\b a -> cond2Kodiak a >>= (Just . K.Or b)) K.False cs
      where
        cond2Kodiak :: Condition -> Maybe K.BExpr
        cond2Kodiak (r,f) =
          case runExcept (kodiakize r) of
            Left _ -> Nothing
            Right kr -> case runExcept (kodiakize f) of
              Left _ -> Nothing
              Right kf -> Just (K.And kr kf)