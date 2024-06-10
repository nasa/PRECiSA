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
  ( main,
    -- computeAllErrorsInKodiak,
    computeAllErrorsInKodiakMap
  )
where

import AbsPVSLang
import AbsSpecLang
import AbstractSemantics
import AbstractDomain
import Common.DecisionPath
import Common.ControlFlow
import Control.Monad.Except
import Data.Maybe (fromMaybe,fromJust)
import qualified Data.Map as Map
import ErrM
import FPCore.FPCorePrinter
import FunctionCallErrorAbstraction
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
import Translation.Float2Real
import qualified Json as JSON
import Data.ByteString.Lazy (hPutStr)
import System.IO (stdout)
import Utils(snd4,trd4,frt4)

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
          , optSMTOptimization      = useSMT } = do

  let noCollapsedStables = False
  errparseProg <- if parsefpcore
                  then do
                    parseFileToFPCoreProgram fileprog
                  else do
                    parseFileToProgram fileprog
  decls <- errify error errparseProg
  spec <- if parsefpcorespec
          then do
            if parsefpcore
            then
              parseFileToFPCoreSpec fileprog
            else
              error $ "Cannot parse FPCore as spec unless also parsed as program"
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
  let progSem = fixpointSemantics decls (botInterp decls) 3 semConf dps
  let symbCertificates = renderPVS $ genCertFile inputFileName certFileName realProgFileName decls progSem
  writeFile certFile symbCertificates
  let realProgDoc = genRealProgFile inputFileName  realProgFileName (fp2realProg decls)
  writeFile realProgFile (renderPVS realProgDoc)

  let searchParams = KP.SP { maximumDepth = fromInteger . toInteger $ maxBBDepth
                           , minimumPrecision = fromInteger . toInteger $ prec }
  let pgmSemUlp = removeInfiniteCebS progSem

  filteredPgmSemUlp <- if useSMT
    then do createDirectoryIfMissing True filePathSMT
            filterUnsatCebs (KP.maximumDepth searchParams) (KP.minimumPrecision searchParams) filePathSMT pgmSemUlp spec
    else return pgmSemUlp

  let unfoldedPgmSem = unfoldSemantics filteredPgmSemUlp

  results <- computeAllErrorsInKodiakMap optUnfoldFuns decls semConf unfoldedPgmSem spec searchParams

  -- results <- if optUnfoldFuns
  --             then computeAllErrorsInKodiakMap unfoldedPgmSem spec searchParams
  --             else computeAllErrorsInKodiak sta unfoldedPgmSem spec searchParams

  let resultSummary = summarizeAllErrors (getKodiakResults results)

  let numCertificate = renderPVS $ genNumCertFile certFileName numCertFileName results decls spec maxBBDepth prec False
  writeFile numCertFile numCertificate

  if printfpcore
  then do putStrLn $ renderPVS $ fpcprintProgram decls spec
  else return ()

  pavingFiles <- if withPaving
    then do
      let unstableCondInterp = concat $ Map.elems $ Map.mapWithKey (\ fun (_,_,_,sem) -> Map.elems $ Map.mapWithKey (\ field semField -> (fun, field, map conds semField)) sem ) unfoldedPgmSem
      let kodiakFunConds = map (\(f,field,conditions) -> (f, field, fromMaybe (error "kodiakFunConds") (KP.conds2Kodiak' conditions))) unstableCondInterp
      KP.paveUnstabilityConditions kodiakFunConds spec searchParams (generatePavingFilename (filePath++inputFileName))
    else
      return []

  if jsonOut
  then do
    let jsonRes = JSON.toJSONAnalysisResults resultSummary certFile numCertFile
    hPutStr stdout jsonRes
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
      realProgFileName = inputFileName ++ "_real"
      generatePavingFilename pvsFilename functionName = pvsFilename ++ "." ++ functionName ++ ".paving"

getKodiakResults :: [(String,PVSType,[Arg],[(ResultField,[(Conditions, LDecisionPath,ControlFlow,KodiakResult,AExpr,[FAExpr],[AExpr])])])] -> [(String, ResultField, [(ControlFlow,KodiakResult)])]
getKodiakResults = concatMap getKodiakResult

getKodiakResult :: (String,PVSType,[Arg],[(ResultField, [(Conditions, LDecisionPath,ControlFlow,KodiakResult,AExpr,[FAExpr],[AExpr])])]) -> [(String, ResultField, [(ControlFlow,KodiakResult)])]
getKodiakResult (f,_,_,funSem) = map getKodiakErrorField funSem
  where
    getKodiakErrorField (field, fieldSem) = (f, field, map getKodiakError fieldSem)
    getKodiakError (_,_,cf,err,_,_,_) = (cf,err)

summarizeAllErrors :: [(String, ResultField, [(ControlFlow, KodiakResult)])] -> [(String,ResultField, Double, Maybe Double)]
summarizeAllErrors errorMap = map summarizeFunError errorMap

summarizeFunError :: (String, ResultField, [(ControlFlow, KodiakResult)]) -> (String, ResultField, Double, Maybe Double)
summarizeFunError (f, field, results) =
  let stableCases = filter ((== Stable) . fst) results in
  let unstableCases = filter ((== Unstable) . fst) results in
    (f, field, maximum $ map (maximumUpperBound . snd) stableCases
      , if null unstableCases then Nothing
        else Just $ maximum $ map (maximumUpperBound . snd) unstableCases)

printAllErrors :: [(String,ResultField,Double,Maybe Double)] -> IO ()
printAllErrors = mapM_ printFunction
  where
    printFunction (f,field,stableErr, unstableErr) = do
      putStrLn $ "Function " ++ f ++ printField field
      putStrLn $ "|real - floating-point| <= " ++ render (prettyNumError $ stableErr)
      case unstableErr of
        Nothing -> return ()
        Just divergence -> do
          putStrLn ""
          putStrLn "There are unstable conditionals leading to divergent real and floating-point control-flows."
          putStrLn $ "divergence <= " ++ render (prettyNumError $ divergence)
      putStrLn ""
      putStrLn "**************************************************************************"

    printField ResValue = ""
    printField (ResRecordField recField) = " field " ++ recField
    printField (ResTupleIndex tupleIdx) = " index " ++ show tupleIdx



computeAllErrorsInKodiakMap ::
  Bool
  -> [Decl]
  -> SemanticConfiguration
  -> Interpretation
  -> Spec
  -> KP.SearchParameters
  -> IO [(String
         ,PVSType
         ,[Arg]
         ,[(ResultField, [(Conditions
                          ,LDecisionPath
                          ,ControlFlow
                          ,KodiakResult
                          ,AExpr
                          ,[FAExpr]
                          ,[AExpr])])])]
computeAllErrorsInKodiakMap unfoldFunCalls decls config interp spec@(Spec specBinds) searchParams = mapM runFunction functionNames
  where
    declInterps = Map.filter isNumericalInterp interp
    functionNames = Map.keys declInterps
    functionBindingsMap = map (\(SpecBind f b) -> (f,b)) specBinds

    runFunction fname = do
      let funInfo = fromMaybe errorMsg $ Map.lookup fname interp
      let fprec = snd4 $ funInfo
      let args = trd4 $ funInfo
      let fSem = frt4 funInfo
      let fields = Map.keys fSem
      results <- mapM (runFunField fname fSem) fields
      return (fname, fprec, args, results)
      where
        errorMsg = error $ "computeAllErrorsInKodiakMap: function " ++ fname ++ " not found."


    runFunField fname sem field = do
      let funErrExprs = fromMaybe errorMsgField (Map.lookup field sem)
      let functionErrorExpressionsMap = map aceb2PathFlowErrorTuple funErrExprs
      fieldResults <- mapM runErrorExpression functionErrorExpressionsMap
      return (field, fieldResults)
        where
          errorMsgField = error $ "runFunction: function " ++ show fname ++ " not found in input bound specification."
          aceb2PathFlowErrorTuple aceb = (conds aceb, decisionPath aceb, cFlow aceb,
                 fromJust $ eExpr aceb, fDeclRes $ fpExprs aceb, rDeclRes $ rExprs aceb)

          runErrorExpression (conditions :: Conditions,path :: LDecisionPath, flow, err, fpes, res) = do
            ki <- kodiakInput
            result <- run ki ()
            return (conditions, path, flow, result, initAExpr err, fpes, res)
              where
                kodiakInput = do
                  let binds = fromJust $ findInSpec fname specBinds
                  errExpr <- if unfoldFunCalls
                             then return $ simplAExpr $ initAExpr err
                             else do
                                let (_,_,(AExprBody funBody)) = fromMaybe (error $ "Function " ++ fname ++ " not found.") $ findInDecls fname decls
                                let locVars = localVarsWithType funBody
                                (replaceFunCallErr True config interp emptyEnv locVars binds) $ simplAExpr $ initAExpr err
                  return $ KI { kiName = fname,
                       kiExpression = errExpr,
                       kiBindings = fromMaybe (error $ "runFunction: function " ++ show fname ++ " not found.")
                                            (lookup fname functionBindingsMap),
                       kiMaxDepth  = KP.maximumDepth searchParams,
                       kiPrecision = KP.minimumPrecision searchParams
                     }