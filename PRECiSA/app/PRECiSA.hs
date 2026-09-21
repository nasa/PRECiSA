-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module PRECiSA
  ( main
  )
where

import AbsPVSLang
import AbstractSemantics
import AbstractDomain
import AnalysisResult
import Common.DecisionPath
import Common.ControlFlow
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import ErrM
import RelativeError (RelError(..))
import FPCore.FPCorePrinter
import Frontend.PVS.MapPVSLangAST (parseFileToProgram)
import Options
import PPExt
import Kodiak.Runner
import qualified Kodiak.Paver as KP
import Kodiak.ErrorComputation (computeAllErrorsInKodiakMap)
import qualified PVSio.Runner as PVSio
import qualified PVSio.ErrorComputation as PVSioEC
import ErrorResult ()
import Prelude hiding ((<>))
import Certificate.Numerical (genNumCertFile, genNumCertFileTuples, prettyNumError)
import Certificate.Real (genRealTheory)
import Certificate.Symbolic (genCertFile)
import Parser.Parser
import SMT.SMT
import System.Directory
import System.FilePath
import qualified JSON
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = parseOptions >>= parseAndAnalyze

initDpsToNone :: Decl -> (FunName, [LDecisionPath])
initDpsToNone (Decl _ _ f _ _) = (f,[])
initDpsToNone (Pred _ _ f _ _) = (f,[])
initDpsToNone (CollDecl _ _ f _ _) = (f,[])

initDpsToAll :: Decl -> (FunName, [LDecisionPath])
initDpsToAll (Decl _ _ f _ _) = (f,[root])
initDpsToAll (Pred _ _ f _ _) = (f,[root])
initDpsToAll (CollDecl _ _ f _ _) = (f,[root])

renderPVS :: Doc -> String
renderPVS = renderStyle Style{mode = LeftMode, lineLength = 80, ribbonsPerLine = 2.0}

parseAndAnalyze :: Options -> IO ()
parseAndAnalyze
  Options
          { optProgramFile          = fileprog
          , optInputRangeFile       = filespec
          , optPathFile             = filedps
          , optParseFPCore          = parsefpcore
          , optParseFPCoreSpec      = parsefpcorespec
          , optPrintFPCore          = printfpcore
          , optFunctionCalls        = optUnfoldFuns
          , optWithPaving           = withPaving
          , optMaxDepth             = maxBBDepth
          , optPrecision            = prec
          , optAssumeStability      = sta
          , jsonOutput              = jsonOut
          , optSMTOptimization      = useSMT
          , optRelativeError        = relErr
          , optUsePVSio             = usePVSio } = do

  let noCollapsedStables = False
  errparseProg <- if parsefpcore
                  then
                    parseFileToFPCoreProgram fileprog
                  else
                    parseFileToProgram fileprog
  pgm@(Program _imps decls) <- errify error errparseProg
  spec <- if parsefpcorespec
          then do
            if parsefpcore
            then
              parseFileToFPCoreSpec fileprog
            else
              error "Cannot parse FPCore as spec unless also parsed as program"
          else do
            errparseSpec <- parseFileToSpec decls filespec
            errify fail errparseSpec
  dps <- if noCollapsedStables
         then return $ map initDpsToAll decls
         else if null filedps
         then return $ map initDpsToNone decls
         else do
           errparseTargetDPs <- parseFileToTargetDPs filedps
           errify fail errparseTargetDPs

  -------------
  let progSem = fixpointSemantics pgm (botInterp decls) 3 semConf dps
  let symbCertificates = renderPVS $ genCertFile inputFileName certFileName realTheoryName pgm progSem relErr
  writeFile certFile symbCertificates
  let realProgDoc = genRealTheory realTheoryName pgm
  writeFile realProgFile (renderPVS realProgDoc)

  let searchParams = KP.SP { maximumDepth = fromInteger . toInteger $ maxBBDepth
                           , minimumPrecision = fromInteger . toInteger $ prec }
  let pgmSemUlp = removeInfiniteCebS progSem

  filteredPgmSemUlp <- if useSMT
    then do createDirectoryIfMissing True filePathSMT
            filterUnsatCebs (KP.maximumDepth searchParams) (KP.minimumPrecision searchParams) filePathSMT pgmSemUlp spec
    else return pgmSemUlp

  let unfoldedPgmSem = unfoldSemantics filteredPgmSemUlp

  (resultSummary, numCertificate) <- if usePVSio
    then do
      pvsioResults <- PVSioEC.computeAllErrorsInPVSioMap optUnfoldFuns decls semConf unfoldedPgmSem spec searchParams
      let summary = summarizeAllErrorsPVSio (getPVSioResults pvsioResults)
      let cert = renderPVS $ genNumCertFileTuples certFileName numCertFileName pvsioResults decls spec maxBBDepth prec False
      return (summary, cert)
    else do
      kodiakResults <- computeAllErrorsInKodiakMap optUnfoldFuns relErr decls semConf unfoldedPgmSem spec searchParams
      let summary = summarizeAllErrors kodiakResults
      let cert = renderPVS $ genNumCertFile certFileName numCertFileName kodiakResults decls spec maxBBDepth prec False
      return (summary, cert)

  writeFile numCertFile numCertificate

  when printfpcore $ do
    putStrLn $ renderPVS $ fpcprintProgram pgm spec

  pavingFiles <- if withPaving
    then do
      let unstableCondInterp = concat $ Map.elems $ Map.mapWithKey (\ fun (_,_,_,sem) -> Map.elems $ Map.mapWithKey (\ field semField -> (fun, field, map conds (filter isUnstable semField))) sem ) unfoldedPgmSem
      let kodiakFunConds = map (\(f,field,conditions) -> (f, field, fromMaybe (error "kodiakFunConds") (KP.conds2Kodiak' conditions))) unstableCondInterp
      KP.paveUnstabilityConditions kodiakFunConds spec searchParams (generatePavingFilename (filePath++inputFileName))
    else
      return []

  if jsonOut
  then do
    let jsonRes = JSON.toJSONAnalysisResults resultSummary certFile numCertFile
    BS.putStr jsonRes
  else do
    putStrLn "**************************************************************************"
    putStrLn "********************************* PRECiSA ********************************"
    putStrLn ""
    printAllErrors resultSummary
    putStrLn ("Symbolic lemmas and proofs in: " ++ certFile)
    putStrLn ("Numeric lemmas and proofs in: " ++ numCertFile)

    when withPaving $
        mapM_ (\(fun,field,file) -> putStrLn $ "Paving for function " ++ fun ++ " and field "
                                               ++ show field ++ " generated in: " ++ file) pavingFiles

    putStrLn ""
    putStrLn "**************************************************************************"
    where
      mu = True
      impErr = False
      semConf = SemConf {improveError = impErr
                        ,assumeTestStability = sta
                        ,mergeUnstables = mu
                        ,unfoldFunCalls = optUnfoldFuns}
      inputFileName = takeBaseName fileprog
      filePath = dropFileName fileprog
      filePathSMT = filePath ++ inputFileName ++ "_SMT/"
      certFile =  filePath ++ certFileName ++ ".pvs"
      numCertFile = filePath ++ numCertFileName ++ ".pvs"
      realProgFile = filePath ++ inputFileName ++ "_real.pvs"
      certFileName = inputFileName ++ "_cert"
      numCertFileName = inputFileName ++ "_num_cert"
      realTheoryName = inputFileName ++ "_real"
      generatePavingFilename pvsFilename functionName = pvsFilename ++ "." ++ functionName ++ ".paving"

getPVSioResults :: [(String,PVSType,[Arg],[(ResultField,[(Conditions, LDecisionPath,ControlFlow,PVSio.PVSioResult,AExpr,[FAExpr],[AExpr])])])] -> [(String, ResultField, [(ControlFlow,PVSio.PVSioResult)])]
getPVSioResults = concatMap getPVSioResult

getPVSioResult :: (String,PVSType,[Arg],[(ResultField, [(Conditions, LDecisionPath,ControlFlow,PVSio.PVSioResult,AExpr,[FAExpr],[AExpr])])]) -> [(String, ResultField, [(ControlFlow,PVSio.PVSioResult)])]
getPVSioResult (f,_,_,funSem) = map getPVSioErrorField funSem
  where
    getPVSioErrorField (field, fieldSem) = (f, field, map getPVSioError fieldSem)
    getPVSioError (_,_,cf,err,_,_,_) = (cf,err)

summarizeAllErrors :: [FunResult] -> [FunSummary]
summarizeAllErrors = concatMap summarizeFun
  where
    summarizeFun fr = map summarizeField (frFields fr)
      where
        summarizeField (field, prs) =
          let stables   = filter ((== Stable)   . prFlow) prs
              unstables = filter ((== Unstable) . prFlow) prs
          in FunSummary
               { fsName        = frName fr
               , fsField       = field
               , fsStable      = maximum $ map (maximumUpperBound . prKodiak) stables
               , fsUnstable    = if null unstables then Nothing
                                 else Just $ maximum $ map (maximumUpperBound . prKodiak) unstables
               , fsRelStable   = worstRelError stables
               , fsRelUnstable = worstRelError unstables
               }

summarizeAllErrorsPVSio :: [(String, ResultField, [(ControlFlow, PVSio.PVSioResult)])] -> [FunSummary]
summarizeAllErrorsPVSio = map summarizeFunErrorPVSio
  where
    summarizeFunErrorPVSio (f, field, results) =
      let stableCases = filter ((== Stable) . fst) results in
      let unstableCases = filter ((== Unstable) . fst) results in
      FunSummary
        { fsName = f
        , fsField = field
        , fsStable = fromRational $ maximum $ map (PVSio.maximumValue . snd) stableCases
        , fsUnstable = if null unstableCases then Nothing
                       else Just $ fromRational $ maximum $ map (PVSio.maximumValue . snd) unstableCases
        , fsRelStable = RelErrorOff      -- PVSio backend does not support relative error
        , fsRelUnstable = RelErrorOff
        }

-- | Print errors for all backends (unified via FunSummary)
printAllErrors :: [FunSummary] -> IO ()
printAllErrors = mapM_ printFunction
  where
    printFunction fs = do
      putStrLn $ "Function " ++ fsName fs ++ printField (fsField fs)
      putStrLn $ "|real - floating-point| <= " ++ render (prettyNumError (fsStable fs))
      printRel (fsRelStable fs)
      case fsUnstable fs of
        Nothing -> return ()
        Just divergence -> do
          putStrLn ""
          putStrLn "There are unstable conditionals leading to divergent real and floating-point control-flows."
          putStrLn $ "divergence <= " ++ render (prettyNumError divergence)
          printRel (fsRelUnstable fs)
      putStrLn ""
      putStrLn "**************************************************************************"

    printRel RelErrorOff = return ()
    printRel (RelErrorBound (RelFinite ub)) =
      putStrLn $ "|real - floating-point| / |real| <= " ++ render (prettyNumError ub)
    printRel (RelErrorBound RelInfinite) =
      putStrLn "|real - floating-point| / |real| <= +infinity (the real result could not be shown to be bounded away from zero)"
    printRel (RelErrorFailed msg) =
      putStrLn $ "relative error could not be computed: " ++ msg

    printField ResValue = ""
    printField (ResRecordField recField) = " field " ++ recField
    printField (ResTupleIndex tupleIdx) = " index " ++ show tupleIdx
