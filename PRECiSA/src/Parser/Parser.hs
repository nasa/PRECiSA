module Parser.Parser where

import ErrM
import Common.DecisionPath
import Parser.ParDecisionPaths
import MapPVSLangAST
import MapRealPVSLangAST
import MapSpecLangAST
--import MapPavingVarsAST
import AbsPVSLang
import AbsSpecLang
--import AbsRawPavingVars
--import Kodiak.PavingVars

parseFileToProgram :: FilePath -> IO (Err Program)
parseFileToProgram src_filename = fmap (parseProgram) (readFile src_filename)

parseProgram :: String -> Err Program
parseProgram str =
  do
    rawParsedProg <- rawparserPVS str
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
parseFileToRealProgram src_filename = fmap (parseRealProgram) (readFile src_filename)

parseRealProgram :: String -> Err RProgram
parseRealProgram str =
  do
    rawParsedRealProg <- rawparserRealPVS str
    let parsedRealProg = raw2RealProg rawParsedRealProg
    return parsedRealProg

--parsePavingVars :: String -> Err AbsRawPavingVars.VarTupleListProg
--parsePavingVars str =
--  do
--    pavingVars <- rawparserPavingVars str
--    return pavingVars

--parseFileToPavingVars :: FilePath -> IO (Err AbsRawPavingVars.VarTupleListProg)
--parseFileToPavingVars src_filename = fmap (parsePavingVars) (readFile src_filename)