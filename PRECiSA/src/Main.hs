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


{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.List as List
import AbsPVSLang
import AbsSpecLang
import AbstractSemantics
import AbstractDomain
import Common.DecisionPath
import Common.ControlFlow
import Data.Maybe (fromJust)
import Data.Time
import ErrM
import FPrec
import Foreign.C
import FramaC.PrettyPrint
import Options
import NumericalError
import PPExt
import Kodiak.Runner
import Kodiak.Runnable
import Kodiak.Generator
import qualified Kodiak.Expression as K
import Prelude hiding ((<>))
import PVSCert
import Parser.Parser
import SMT.SMT
import System.Directory
import System.FilePath
import Transformation
import TransformationUtils
import Translation.Float2Real
import Translation.Real2Float
import Utils (fst3,snd3,trd3)

main :: IO ()
main = do
  cmd <- parseOptions
  case cmd of
    Options (Analyze  options) -> parseAndAnalyze options
    Options (Generate options) -> generateCProg   options

printVersion :: String -> IO ()
printVersion prgname = do
    putStr " ("
    putStr prgname
    putStr ")"

parseRealProg :: FilePath -> IO RProgram
parseRealProg fileprog = do
  errparseProg <- parseFileToRealProgram fileprog
  errify error errparseProg

generateCProg :: GenerateOptions -> IO ()
generateCProg
  GenerateOptions
    { optRealProgramFile     = prog
    , optRealInputRangeFile  = inputs
    , targetFormat           = fprec }
  = case fprec of
    "double" ->  real2FPC prog inputs FPDouble
    "single" ->  real2FPC prog inputs FPSingle
    _ -> error ""


real2FPC :: FilePath -> FilePath -> FPrec -> IO ()
real2FPC fileprog filespec fp = do
  realProg <- parseRealProg fileprog
  let decls = real2fpProg fp realProg
  errparseSpec <- parseFileToSpec decls filespec
  spec <- errify fail errparseSpec
  let dps = map initDpsToNone decls

  let maxDepth = 7
  let prec = 14

  let tranProgTuples = transformProgramSymb decls
  let tranProg = map fst3 tranProgTuples
  let progSemStable = fixpointSemantics decls (botInterp decls) 3 semConf dps
  let progStableConds = map (stableConditions . semantics) progSemStable
  let progSymbRoundOffErrs = map (maxRoundOffError . semantics) progSemStable
  let searchParams = SP { maximumDepth = fromInteger . toInteger $ maxDepth, minimumPrecision = fromInteger . toInteger $ prec }
  results <- computeAllErrorsInKodiak progSemStable spec searchParams
  let maxErrors = summarizeAllErrors (getKodiakResults results)

  let roErrorsDecl = zip (map fst progSemStable) (zip progSymbRoundOffErrs progStableConds)
  
  funErrEnv <- mapM (computeErrorGuards spec) tranProgTuples

  let tranProgPairs = zip tranProg (map snd3 tranProgTuples)
  let framaCfileContent = genFramaCFile fp realProg spec tranProgPairs roErrorsDecl maxErrors progStableConds funErrEnv
  writeFile framaCfile (render   framaCfileContent)
 
  writeFile pvsProgFile     (render $ genFpProgFile fp fpFileName decls)

  let symbCertificate = render $ genCertFile fpFileName certFileName inputFileName tranProg progSemStable True
  writeFile certFile symbCertificate


  time <- getCurrentTime

  let numCertificate = render $ genNumCertFile certFileName numCertFileName results spec maxDepth prec time True
  writeFile numCertFile numCertificate
  writeFile exprCertFile (render $ genExprCertFile exprCertFileName (vcat (map (printExprFunCert fp) funErrEnv)))
  return ()
    where
      inputFileName = takeBaseName fileprog
      fpFileName = inputFileName ++ "_fp"
      tranFileName = inputFileName ++ "_tran"
      filePath = dropFileName fileprog
      framaCfile = filePath ++ inputFileName ++ ".c"
      pvsProgFile = filePath ++ fpFileName ++ ".pvs"
      certFileName = "cert_" ++ inputFileName
      numCertFileName = "num_cert_" ++ inputFileName
      exprCertFileName = "expr_cert_" ++ inputFileName
      exprCertFile = filePath ++ exprCertFileName ++ ".pvs"
      certFile =  filePath ++ certFileName ++ ".pvs"
      numCertFile =  filePath ++ numCertFileName ++ ".pvs"
      semConf = SemConf { assumeTestStability = True, mergeUnstables = True}


initDpsToNone :: Decl -> (FunName, [a])
initDpsToNone (Decl _ f _ _) = (f,[])

parseAndAnalyze :: AnalyzeOptions -> IO ()
parseAndAnalyze
  AnalyzeOptions
          { optProgramFile          = fileprog
          , optInputRangeFile       = filespec
          , optPathFile             = filedps
          , optMaxDepth             = maxDepth
          , optPrecision            = prec
          , optMaxNumLemma          = maxel
          , optAssumeStability      = sta
          , optNoCollapsedUnstables = notMu
          , optSMTOptimization      = useSMT } = do
  errparseProg <- parseFileToProgram fileprog
  decls <- errify error errparseProg
  errparseSpec <- parseFileToSpec decls filespec
  spec <- errify fail errparseSpec
  dps <- if null filedps
    then
      return $ map initDpsToNone decls
    else
      do
        errparseTargetDPs <- parseFileToTargetDPs filedps
        errify fail errparseTargetDPs
  --------------
  time <- getCurrentTime
  -------------
  let progSem = fixpointSemantics decls (botInterp decls) 3 semConf dps
  checkProgSize progSem 0 maxel
  let symbCertificates = render $ genCertFile inputFileName certFileName realProgFileName decls progSem False
  writeFile certFile symbCertificates
  let realProgDoc = genRealProgFile inputFileName  realProgFileName (fp2realProg decls)
  writeFile realProgFile (render realProgDoc)
  -------------- just for batch mode
  putStrLn "********************************************************************"
  putStrLn "****************************** PRECiSA *****************************"
  putStrLn ""
  let searchParams = SP { maximumDepth = fromInteger . toInteger $ maxDepth, minimumPrecision = fromInteger . toInteger $ prec }
  let rawPgmSemUlp = initErrVars progSem
  let pgmSemUlp = removeInfiniteCebS rawPgmSemUlp
  createDirectoryIfMissing True filePathSMT
  -- generateSMTFiles filePathSMT pgmSemUlp spec >>= printf "Generated SMT files: %s\n" . show
  filteredPgmSemUlp <- if useSMT then filterUnsatCebs filePathSMT pgmSemUlp spec else return pgmSemUlp
  -- if pgmSemUlp /= filteredPgmSemUlp then putStrLn "Semantics filtered!" else return ()
  -- printInfiteErrorInterp $ filterInfiniteCebS rawPgmSemUlp
  results <- computeAllErrorsInKodiak filteredPgmSemUlp spec searchParams
--   let maxErrors = summarizeAllErrors results
  printAllErrors results
  let numCertificate = render $ genNumCertFile certFileName numCertFileName results spec maxDepth prec time False
  writeFile numCertFile numCertificate
  putStrLn ""
  -------------- just for batch mode
  putStrLn "********************************************************************"
  putStrLn "***** Files generated successfully *****"
  putStrLn ("Symbolic lemmas and proofs in: " ++ certFile)
  putStrLn ("Numeric lemmas and proofs in: " ++ numCertFile)
  -------------
  return ()
    where
      mu = not notMu
      semConf = SemConf { assumeTestStability = sta, mergeUnstables = mu}
      inputFileName = takeBaseName fileprog
      filePath = dropFileName fileprog
      filePathSMT = filePath ++ inputFileName ++ "_SMT/"
      certFile =  filePath ++ certFileName ++ ".pvs"
      numCertFile = filePath ++ numCertFileName ++ ".pvs"
      realProgFile = filePath ++ inputFileName ++ "_real.pvs"
      certFileName = "cert_" ++ inputFileName
      numCertFileName = "num_cert_" ++ inputFileName
      realProgFileName = inputFileName ++ "_real"


getKodiakResults :: [(String,FPrec,[Arg],[(Conditions, LDecisionPath,ControlFlow,KodiakResult,AExpr,[FAExpr],[AExpr])])] -> [(String, [KodiakResult])]
getKodiakResults = map getKodiakResult
  where
     getKodiakResult (f,_,_,errors) = (f, map getKodiakError errors)
     getKodiakError (_, _,_,err,_,_,_) = err

summarizeAllErrors :: [(String, [KodiakResult])] -> [(String, Double)]
summarizeAllErrors errorMap = [ (f,maximum $ map maximumUpperBound results) | (f, results) <- errorMap]

printAllErrors :: [(String,FPrec,[Arg],[(Conditions,LDecisionPath,ControlFlow,KodiakResult,AExpr,[FAExpr],[AExpr])])] -> IO ()
printAllErrors = mapM_ printFunction
  where
    stabilityOrder (_,p1,Stable,_,_,_,_)  (_,p2,Stable,_,_,_,_) = compare p1 p2
    stabilityOrder (_,_,Stable,_,_,_,_)   (_,_,_,_,_,_,_) = LT
    stabilityOrder (_,_,Unstable,_,_,_,_) (_,_,_,_,_,_,_) = GT

    printFunction (f,_,_,results) = do
      putStrLn $ "Function " ++ f
      mapM_ printResult $ zip ([0,1..] :: [Integer]) $ List.sortBy stabilityOrder results
        where
          printResult (i,(_,path,flow,result,_,_,_)) =
            if flow == Stable
            then putStrLn $ show i ++ "- path " ++ show path ++ ": " ++ render (prettyNumError $ maximumUpperBound result)
            else putStrLn $ show i ++ "- unstable paths ++ " ++ show path ++ ": " ++ render (prettyNumError $ maximumUpperBound result)

data SearchParameters = SP { maximumDepth :: CUInt, minimumPrecision :: CUInt }

computeAllErrorsInKodiak :: Interpretation -> Spec -> SearchParameters -> IO [(String,FPrec,[Arg],[(Conditions, LDecisionPath,ControlFlow,KodiakResult,AExpr,[FAExpr],[AExpr])])]
computeAllErrorsInKodiak interp (Spec bindings) searchParams = mapM runFunction functionNames
  where
    functionNames = map fst interp
    functionBindingsMap = map (\(SpecBind f b) -> (f,b)) bindings
    functionErrorExpressionsMap = map toPathFlowErrorTuple interp
      where
        toPathFlowErrorTuple (f,(fp,args,acebs)) = (f,map (\x -> (fp,args,aceb2PathFlowErrorTuple x)) acebs)
          where
            aceb2PathFlowErrorTuple aceb = (conds aceb, decisionPath aceb, cFlow aceb, eExpr aceb, fpExprs aceb, rExprs aceb)

    runFunction name = do results <- mapM runErrorExpression $ fromJust $ lookup name functionErrorExpressionsMap
                          let (fprec,args,_):_ = results
                          let results' = map (\(_,_,a) -> a) results
                          return (name, fprec, args, results')
      where
        runErrorExpression (fprec :: FPrec,args :: [Arg],(conds :: Conditions,path :: LDecisionPath, flow, err, fpes, res)) = do
          result <- run kodiakInput ()
          return (fprec, args, (conds, path, flow, result, initAExpr err, fpes, res))
            where
              kodiakInput = KI { name = name,
                                 expression = initAExpr err,
                                 bindings = fromJust $ lookup name functionBindingsMap,
                                 maxDepth = maximumDepth searchParams,
                                 precision = minimumPrecision searchParams
                               }



