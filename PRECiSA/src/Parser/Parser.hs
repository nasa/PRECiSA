-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.

module Parser.Parser
  ( parseFileToProgram,
    parseFileToRealProgram,
    parseFileToSpec,
    parseFileToTargetDPs,
    parseFileToFPCoreProgram,
    parseFPCoreProgram,
    parseFPCoreFAExpr,
    parseFileToFPCoreSpec,
  )
where

import AbsPVSLang
import AbsSpecLang
import Common.DecisionPath
import ErrM
import MapPVSLangAST
import MapRealPVSLangAST
import MapFPCoreLangAST
import MapFPCoreSpecLangAST
import MapSpecLangAST
import Parser.ParDecisionPaths

parseFileToProgram :: FilePath -> IO (Err Program)
parseFileToProgram src_filename = fmap parseProgram (readFile src_filename)

parseProgram :: String -> Err Program
parseProgram str =
  do
    rawParsedProg <- rawparserPVS str
    return $ raw2Prog rawParsedProg

parseFileToTargetDPs :: FilePath -> IO (Err TargetDPs)
parseFileToTargetDPs src_filename = fmap parseTargetDPs (readFile src_filename)

parseTargetDPs :: String -> Err TargetDPs
parseTargetDPs = rawparserTargetDPs

parseFileToSpec :: [Decl] -> FilePath -> IO (Err Spec)
parseFileToSpec decls src_filename = fmap (parseSpec decls) (readFile src_filename)

parseSpec :: [Decl] -> String -> Err Spec
parseSpec decls str =
  do
    rawParsedSpec <- rawparserSpec str
    let parsedSpec = raw2Spec decls rawParsedSpec
    return parsedSpec

parseFileToRealProgram :: FilePath -> IO (Err RProgram)
parseFileToRealProgram src_filename = fmap parseRealProgram (readFile src_filename)

parseRealProgram :: String -> Err RProgram
parseRealProgram str =
  do
    rawParsedRealProg <- rawparserRealPVS str
    return $ raw2RealProg rawParsedRealProg

parseFileToFPCoreProgram :: FilePath -> IO (Err Program)
parseFileToFPCoreProgram src_filename = fmap parseFPCoreProgram (readFile src_filename)

parseFPCoreProgram :: String -> Err Program
parseFPCoreProgram str =
  do
    rawParsedFPCoreProg <- fpcoreparserPVS str
    return $ fpcore2Prog rawParsedFPCoreProg


parseFPCoreFAExpr :: String -> Err FAExpr
parseFPCoreFAExpr str =
  do
    rawParsedFPCoreProg <- fpcoreparserPVS str
    let prog = fpcore2Prog rawParsedFPCoreProg
    let extractfae [Decl _ _ _ _ fae] = fae
        extractfae _ = error "program not supported"
    return $ extractfae prog

parseFileToFPCoreSpec :: FilePath -> IO Spec
parseFileToFPCoreSpec src_filename = fmap parseFPCoreSpec (readFile src_filename)

parseFPCoreSpec :: String -> Spec
parseFPCoreSpec str = unerr res
  where
    res = do
      rawParsedFPCoreProg <- fpcoreparserPVS str
      return $ fpcore2Spec rawParsedFPCoreProg

--parsePavingVars :: String -> Err AbsRawPavingVars.VarTupleListProg
--parsePavingVars str =
--  do
--    pavingVars <- rawparserPavingVars str
--    return pavingVars

--parseFileToPavingVars :: FilePath -> IO (Err AbsRawPavingVars.VarTupleListProg)
--parseFileToPavingVars src_filename = fmap (parsePavingVars) (readFile src_filename)
