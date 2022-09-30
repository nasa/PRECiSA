-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
 
-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
 
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
  

module Parser.Parser where

import ErrM
import Common.DecisionPath
import Parser.ParDecisionPaths
import MapPVSLangAST
import MapRealPVSLangAST
import MapSpecLangAST
import AbsPVSLang
import AbsSpecLang
import System.FilePath (takeBaseName)

parseFileToProgram :: FilePath -> IO (Err Program)
parseFileToProgram src_filename = fmap (parseProgram src_filename) (readFile src_filename)

parseProgram :: FilePath -> String -> Err Program
parseProgram src_filename str =
  do
    rawParsedProg <- rawparserPVS str
    let thName = namePVSTheory rawParsedProg
    let fileName = takeBaseName src_filename
    let parsedProg = raw2Prog rawParsedProg
    return parsedProg

parseFileToTargetDPs :: FilePath -> IO (Err TargetDPs)
parseFileToTargetDPs src_filename = fmap (parseTargetDPs) (readFile src_filename)

parseTargetDPs :: String -> Err TargetDPs
parseTargetDPs str =
  do
    parsedTargetDPs <- rawparserTargetDPs str
    return parsedTargetDPs

parseFileToSpec :: [Decl] ->  FilePath -> IO (Err Spec)
parseFileToSpec decls src_filename = fmap (parseSpec decls) (readFile src_filename)

parseSpec :: [Decl] -> String -> Err Spec
parseSpec decls str =
  do
    rawParsedSpec <- rawparserSpec str
    let parsedSpec = raw2Spec decls rawParsedSpec
    return parsedSpec

parseFileToRealProgram :: FilePath -> IO (Err RProgram)
parseFileToRealProgram src_filename = fmap (parseRealProgram src_filename) (readFile src_filename) 

parseRealProgram :: FilePath -> String -> Err RProgram
parseRealProgram src_filename str =
  do
    rawParsedRealProg <- rawparserRealPVS str
    let thName = namePVSRealTheory rawParsedRealProg
    let fileName = takeBaseName src_filename
    if  fileName /= thName
      then error $ "The PVS theory name " ++ show fileName ++" does not match the file name " ++ show thName ++ "."
      else do
        let parsedRealProg = raw2RealProg rawParsedRealProg
        return parsedRealProg



--parsePavingVars :: String -> Err AbsRawPavingVars.VarTupleListProg
--parsePavingVars str =
--  do
--    pavingVars <- rawparserPavingVars str
--    return pavingVars

--parseFileToPavingVars :: FilePath -> IO (Err AbsRawPavingVars.VarTupleListProg)
--parseFileToPavingVars src_filename = fmap (parsePavingVars) (readFile src_filename)