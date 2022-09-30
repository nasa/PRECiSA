-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Precisa where

import qualified Data.List as List
import AbsPVSLang
import AbsSpecLang
import AbstractSemantics
import AbstractDomain
import Common.DecisionPath
import Common.ControlFlow
import Control.Monad.Except
import Data.Maybe (fromMaybe)
import ErrM
import PVSTypes
import FramaC.PrettyPrint
import FramaC.PrecisaPrelude
import Options
import PPExt
import Kodiak.Runner
import Kodiak.Runnable
import qualified Kodiak.Paver as KP
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


real2FPC :: FilePath -> FilePath -> PVSType -> IO ()
real2FPC fileprog filespec fp = do
  realProg <- parseRealProg fileprog

  -- fp progam
  let decls = real2fpProg fp realProg
  errparseSpec <- parseFileToSpec decls filespec
  spec <- errify fail errparseSpec
  let dpsNone = map initDpsToNone decls

  -- transfromed program --
  let tranProgTuples = transformProgramSymb realProg decls

  -- program semantics
  let progSemStable = fixpointSemantics decls (botInterp decls) 3 semConf dpsNone
  let progStableConds = map (stableConditions . semantics) progSemStable
  let progSymbRoundOffErrs = map (maxRoundOffError . semantics) progSemStable

  let maxBBDepth = 7
  let prec = 14
  let searchParams = KP.SP { maximumDepth = fromInteger . toInteger $ maxBBDepth
                           , minimumPrecision = fromInteger . toInteger $ prec }
  results <- computeAllErrorsInKodiak progSemStable spec searchParams
  -- numerical round-off errors declarations
  let numROErrorsDecl = summarizeAllErrors (getKodiakResults results)

  -- symbolic round-off errors error vars
  let roErrorsDecl = zip (map fst progSemStable) (zip progSymbRoundOffErrs progStableConds)

  -- numerical round-off errors error vars
  funErrEnv <- mapM (computeErrorGuards spec progSemStable) tranProgTuples

  let framaCfileContent = genFramaCFile fp spec realProg decls tranProgTuples roErrorsDecl numROErrorsDecl
                                        funErrEnv progSemStable
  writeFile framaCfile (render framaCfileContent)

  writeFile precisaPreludeFile (render precisaPreludeContent)

  writeFile pvsProgFile (render $ genFpProgFile fp fpFileName decls)

  let symbCertificate = render $ genCertFile fpFileName certFileName inputFileName decls progSemStable
  writeFile certFile symbCertificate

  let numCertificate = render $ genNumCertFile certFileName numCertFileName results decls spec maxBBDepth prec True
  writeFile numCertFile numCertificate

  let guardExpressionsCert = render $ genExprCertFile inputFileName fpFileName exprCertFileName (vcat (map (printExprFunCert fp) funErrEnv))
  writeFile exprCertFile guardExpressionsCert
  putStrLn $ "PRECiSA: intrumented C code and PVS certificate generated in " ++ filePath ++ "."

  return ()
    where
      precisaPreludeFile = filePath ++ "precisa_prelude.c"
      inputFileName = takeBaseName fileprog
      fpFileName = inputFileName ++ "_fp"
      filePath = dropFileName fileprog
      framaCfile = filePath ++ inputFileName ++ ".c"
      pvsProgFile = filePath ++ fpFileName ++ ".pvs"
      certFileName = "cert_" ++ inputFileName
      numCertFileName = inputFileName ++ "_num_cert"
      exprCertFileName = inputFileName ++ "_expr_cert"
      exprCertFile = filePath ++ exprCertFileName ++ ".pvs"
      certFile =  filePath ++ certFileName ++ ".pvs"
      numCertFile =  filePath ++ numCertFileName ++ ".pvs"
      semConf = SemConf { assumeTestStability = True, mergeUnstables = True}

initDpsToNone :: Decl -> (FunName, [LDecisionPath])
initDpsToNone (Decl _ _ f _ _) = (f,[])
initDpsToNone (Pred _ _ f _ _) = (f,[])

initDpsToAll :: Decl -> (FunName, [LDecisionPath])
initDpsToAll (Decl _ _ f _ _) = (f,[root])
initDpsToAll (Pred _ _ f _ _) = (f,[root])

parseAndAnalyze :: AnalyzeOptions -> IO ()
parseAndAnalyze
  AnalyzeOptions
          { optProgramFile          = fileprog
          , optInputRangeFile       = filespec
          , optPathFile             = filedps
          , optWithPaving           = withPaving
          , optMaxDepth             = maxBBDepth
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
  -------------
  let progSem = fixpointSemantics decls (botInterp decls) 3 semConf dps
  checkProgSize progSem 0 maxel
  let symbCertificates = render $ genCertFile inputFileName certFileName realProgFileName decls progSem
  writeFile certFile symbCertificates
  let realProgDoc = genRealProgFile inputFileName  realProgFileName (fp2realProg decls)
  writeFile realProgFile (render realProgDoc)
  -------------- just for batch mode
  putStrLn "********************************************************************"
  putStrLn "****************************** PRECiSA *****************************"
  putStrLn ""
  let searchParams = KP.SP { maximumDepth = fromInteger . toInteger $ maxBBDepth, minimumPrecision = fromInteger . toInteger $ prec }
  let rawPgmSemUlp = initErrVars progSem
  let pgmSemUlp = removeInfiniteCebS rawPgmSemUlp

  filteredPgmSemUlp <- if useSMT
    then do createDirectoryIfMissing True filePathSMT
            filterUnsatCebs filePathSMT pgmSemUlp spec
    else return pgmSemUlp

  let unfoldedPgmSem = unfoldSemantics filteredPgmSemUlp
  results <- computeAllErrorsInKodiak unfoldedPgmSem spec searchParams
  printAllErrors results
  let numCertificate = render $ genNumCertFile certFileName numCertFileName results decls spec maxBBDepth prec False
  writeFile numCertFile numCertificate
  putStrLn ""
  -------------- just for batch mode
  putStrLn "********************************************************************"
  putStrLn "***** Files generated successfully *****"
  putStrLn ("Symbolic lemmas and proofs in: " ++ certFile)
  putStrLn ("Numeric lemmas and proofs in: " ++ numCertFile)
  pavingFiles <- if withPaving
  then do
    let unstableCondInterp = map (\(fun, (_,_,_,sem)) -> (fun, map conds (filter isUnstable sem))) unfoldedPgmSem
    let kodiakFunConds = map (\(f,conditions) -> (f,fromMaybe (error "kodiakFunConds") (KP.conds2Kodiak' conditions))) unstableCondInterp
    KP.paveUnstabilityConditions kodiakFunConds spec searchParams (generatePavingFilename (filePath++inputFileName))
  else return []
  when withPaving $
      mapM_ (\(fun,file) -> putStrLn $ "Paving for function " ++ fun ++ " generated in: " ++ file) pavingFiles
  -------------
  -- return ()
    where
      mu = not notMu
      semConf = SemConf { assumeTestStability = sta, mergeUnstables = mu}
      inputFileName = takeBaseName fileprog
      filePath = dropFileName fileprog
      filePathSMT = filePath ++ inputFileName ++ "_SMT/"
      certFile =  filePath ++ certFileName ++ ".pvs"
      numCertFile = filePath ++ numCertFileName ++ ".pvs"
      realProgFile = filePath ++ inputFileName ++ "_real.pvs"
      certFileName = inputFileName ++ "_cert"
      numCertFileName = inputFileName ++ "_num_cert"
      realProgFileName = inputFileName ++ "_real"
      generatePavingFilename pvsFilename functionName = pvsFilename ++ "." ++ functionName ++ ".paving"

getKodiakResults :: [(String,PVSType,[Arg],[(Conditions, LDecisionPath,ControlFlow,KodiakResult,AExpr,[FAExpr],[AExpr])])] -> [(String, [KodiakResult])]
getKodiakResults = map getKodiakResult
  where
     getKodiakResult (f,_,_,errors) = (f, map getKodiakError errors)
     getKodiakError (_, _,_,err,_,_,_) = err

summarizeAllErrors :: [(String, [KodiakResult])] -> [(String, Double)]
summarizeAllErrors errorMap = [ (f,maximum $ map maximumUpperBound results) | (f, results) <- errorMap]

printAllErrors :: [(String,PVSType,[Arg],[(Conditions,LDecisionPath,ControlFlow,KodiakResult,AExpr,[FAExpr],[AExpr])])] -> IO ()
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


computeAllErrorsInKodiak :: Interpretation
                         -> Spec
                         -> KP.SearchParameters
                         -> IO [(String,PVSType,[Arg],[(Conditions, LDecisionPath,ControlFlow,KodiakResult,AExpr,[FAExpr],[AExpr])])]
computeAllErrorsInKodiak  interp (Spec specBinds) searchParams = mapM runFunction functionNames
  where
    functionNames = map fst functionBindingsMap
    functionBindingsMap = map (\(SpecBind f b) -> (f,b)) specBinds
    functionErrorExpressionsMap = map toPathFlowErrorTuple interp
      where
        toPathFlowErrorTuple (f,(_,fp,args,acebs)) = (f,map (\x -> (fp,args,aceb2PathFlowErrorTuple x)) acebs)
          where
            aceb2PathFlowErrorTuple aceb = (conds aceb, decisionPath aceb, cFlow aceb, eExpr aceb, fpExprs aceb, rExprs aceb)

    runFunction fname = do
      results <- mapM runErrorExpression $
                  fromMaybe (error $ "runFunction: function " ++ show fname ++ " not found in input bound specification.")
                            (lookup fname functionErrorExpressionsMap)
      let (fprec,args,_):_ = results
      let results' = map (\(_,_,a) -> a) results
      return (fname, fprec, args, results')
      where
        runErrorExpression (fprec :: PVSType,args :: [Arg],(conditions :: Conditions,path :: LDecisionPath, flow, err, fpes, res)) = do
          result <- run kodiakInput ()
          return (fprec, args, (conditions, path, flow, result, initAExpr err, fpes, res))
            where
              kodiakInput = KI { kiName = fname,
                                 kiExpression = simplAExpr $ initAExpr err,
                                 kiBindings = fromMaybe (error $ "runFunction: function " ++ show fname ++ " not found.")
                                                      (lookup fname functionBindingsMap),
                                 kiMaxDepth  = KP.maximumDepth searchParams,
                                 kiPrecision = KP.minimumPrecision searchParams
                               }
