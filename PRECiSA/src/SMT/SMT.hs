-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
 
-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
 
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
  
  
module SMT.SMT where

import AbstractDomain
import AbstractSemantics
import AbsPVSLang
import AbsSpecLang
import PPExt
import PVSTypes
import SMT.PrettyPrinter
import SMT.GenerateSMTModel
import Text.Printf (printf)
import Control.Monad
import System.IO
import System.FilePath
import System.Directory
import System.Process
import Data.Char (toUpper)
import qualified Data.List as List
import Data.List (isInfixOf)
import Data.Maybe (catMaybes, fromMaybe)
import Prelude hiding ((<>))
import Common.TypesUtils (VarName)

filterUnsatCebs :: FilePath -> Interpretation -> Spec -> IO Interpretation
filterUnsatCebs path interp spec = mapM (checkSat path spec) interp

checkSat :: FilePath -> Spec -> FunctionInterpretation -> IO FunctionInterpretation
checkSat path (Spec spec) (fun, (isTrans,a,b,acebs)) =
  do satUnstableACEBs <- acebsM'
     return (fun, (isTrans,a,b,stableACEBs ++ satUnstableACEBs))
  where
    acebsM' = catMaybes <$> zipWithM (checkCebSat path fun varBinds) unstableACEBs [0,1..]
    (stableACEBs,unstableACEBs) = List.partition isStable acebs 
    varBinds = fromMaybe (error $ "checkSat: function " ++ show fun ++ " not found.") (findInSpec fun spec)

fpRockCMD :: String -> String -> String
fpRockCMD executor file = executor ++ " 5 rnd-to-zero " ++ file

checkCebSat :: FilePath -> FunName -> [VarBind] -> ACeb -> Int -> IO (Maybe ACeb)
checkCebSat path fun binds ceb n =
    do let tempDir = combine path $ show n
       createDirectoryIfMissing True tempDir
       maybeExecutor <- findExecutable "fprock" >>= mapM canonicalizePath
       let resultsPath = tempDir ++ "/results"
       createDirectoryIfMissing True resultsPath
       case maybeExecutor of
           Nothing       ->
               do putStrLn "FPRock cannot be found!"
                  return $ Just ceb
           Just executor ->
               do smtFilePath     <- generateSMTFile tempDir fun binds ceb
                  printf "smtFilePath for %s: %s\n" fun smtFilePath
                  putStrLn $ fpRockCMD executor smtFilePath
                  (_,_,_,pHandle) <- createProcess (shell $ fpRockCMD executor smtFilePath) { cwd = Just tempDir }
                  exitCode        <- waitForProcess pHandle
                  printf "SMT for %s ceb %d returned code %s\n" fun n (show exitCode)
                  printf "running in %s\n" tempDir
                  isSat <- parseResults tempDir
                  if isSat
                    then return $ Just ceb
                    else
                        do printf "SMT for %s ceb %d is unsat\n" fun n
                           return Nothing

parseResults :: FilePath -> IO Bool
parseResults basePath =
    do rawFilePaths <- listDirectory resultsPath
       let filePaths = map (combine resultsPath) rawFilePaths
       printf "Output files are %s\n" (show filePaths)
       results <- mapM parseResultFile filePaths
       if all (Nothing==) results
         then return True
         else return $ elem (Just True) results
    where
        resultsPath = basePath ++ "/results"

parseResultFile :: FilePath -> IO (Maybe Bool)
parseResultFile filePath =
    do handle   <- openFile filePath ReadMode
       contents <- hGetContents handle
       if null contents
         then return Nothing
         else return $ Just (not $ "UNSAT" `isInfixOf` map toUpper contents)


generateSMTFile :: FilePath -> FunName -> [VarBind] -> ACeb -> IO FilePath
generateSMTFile filePath f varBinds aceb = do 
  let fileName = combine filePath "config.txt"
  (newCond, be, newRVar, newFPVar, newErrVar) <- genSMTConstraints (conds aceb) varBinds
  if isUnstable aceb
    then writeFile fileName (render $ generateSMTFileFromUnstable f varBinds newFPVar newErrVar newRVar newCond be)
    else writeFile fileName (render $ generateSMTFileFromStable   f varBinds aceb)
  return fileName

generateSMTFileFromStable :: FunName -> [VarBind] -> ACeb -> Doc
generateSMTFileFromStable f varBinds aceb =
  text "% This file is automatically generated by PRECiSA \n"
  $$
  text ("% Function " ++ show f ++ "\n")
  $$
  text ("% Real result: " ++ show (rExprs  aceb))
  $$
  text ("% Floating-point result: " ++ show (fpExprs aceb))
  $$
  text (show $ cFlow aceb)
  $$
  text "Float: "
    <+> prettySMTVarBinds varBinds
  $$
  text ("Real: ")
    <+> prettySMTVarBindsReal varBinds
  $$
  prettySMT (conds aceb)

generateSMTFileFromUnstable :: FunName -> [VarBind] -> [(VarName, PVSType)] -> [VarName] -> [VarName] -> Conditions -> BExpr -> Doc
generateSMTFileFromUnstable f varBinds newFPVar newErrVar newRVar newCond be =
  -- (newCond, be, newRVar, newFPVar, newErrVar) <- genSMTConstraints (conds aceb) varBinds
  -- let fileName = combine filePath "config.txt"
  text "% This file is automatically generated by PRECiSA \n"
  $$
  text ("% Function " ++ show f ++ "\n")
  $$
  text "Float: "
    <+> prettySMTVarBinds varBinds
    <+> (if newFPVar /= [] then comma <+> prettySMTFVarIds newFPVar else emptyDoc)
  $$
  text ("\n"++"Real: ")
    <+> prettySMTVarBindsReal varBinds
    <+> (if newErrVar /= [] then comma <> hsep (punctuate comma $ map prettySMTVarId newErrVar) else emptyDoc)
    <+> (if newRVar  /= []  then comma <> prettySMTVarIds newRVar else emptyDoc)
  $$
  prettySMT newCond
  $$
  prettySMTbexpr be




