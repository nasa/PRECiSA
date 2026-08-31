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
import AnalysisResult
import Common.DecisionPath
import Common.ControlFlow
import Control.Monad (when)
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
import RelativeError (RelError(..), computeRelError)
import SMT.SMT
import System.Directory
import System.FilePath
import Translation.Float2Real
import qualified Json as JSON
import qualified Data.ByteString.Lazy as BS
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
          , optSMTOptimization      = useSMT
          , optRelativeError        = relErr } = do

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
  let progSem = fixpointSemantics decls (botInterp decls) 3 semConf dps
  let symbCertificates = renderPVS $ genCertFile inputFileName certFileName realProgFileName decls progSem relErr
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

  results <- computeAllErrorsInKodiakMap optUnfoldFuns relErr decls semConf unfoldedPgmSem spec searchParams

  -- results <- if optUnfoldFuns
  --             then computeAllErrorsInKodiakMap unfoldedPgmSem spec searchParams
  --             else computeAllErrorsInKodiak sta unfoldedPgmSem spec searchParams

  let resultSummary = summarizeAllErrors results

  let numCertificate = renderPVS $ genNumCertFile certFileName numCertFileName results decls spec maxBBDepth prec False
  writeFile numCertFile numCertificate

  when printfpcore $ do
    putStrLn $ renderPVS $ fpcprintProgram decls spec

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
      realProgFileName = inputFileName ++ "_real"
      generatePavingFilename pvsFilename functionName = pvsFilename ++ "." ++ functionName ++ ".paving"

summarizeAllErrors :: [FunResult] -> [FunSummary]
summarizeAllErrors = concatMap summarizeFun

summarizeFun :: FunResult -> [FunSummary]
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



computeAllErrorsInKodiakMap ::
  Bool
  -> Bool
  -> [Decl]
  -> SemanticConfiguration
  -> Interpretation
  -> Spec
  -> KP.SearchParameters
  -> IO [FunResult]
computeAllErrorsInKodiakMap unfoldFunCalls' relErrEnabled decls config interp (Spec specBinds) searchParams = mapM runFunction functionNames
  where
    declInterps = Map.filter isNumericalInterp interp
    functionNames = Map.keys declInterps
    functionBindingsMap = map (\(SpecBind f b) -> (f,b)) specBinds

    runFunction fname = do
      let funInfo = fromMaybe errorMsg $ Map.lookup fname interp
      let fprec = snd4 funInfo
      let args = trd4 funInfo
      let fSem = frt4 funInfo
      let fields = Map.keys fSem
      results <- mapM (runFunField fname fSem) fields
      return FunResult { frName = fname, frType = fprec, frArgs = args, frFields = results }
      where
        errorMsg = error $ "computeAllErrorsInKodiakMap: function " ++ fname ++ " not found."


    runFunField fname sem field = do
      let funErrExprs = fromMaybe errorMsgField (Map.lookup field sem)
      let functionErrorExpressionsMap = map aceb2PathInput funErrExprs
      fieldResults <- mapM runErrorExpression functionErrorExpressionsMap
      return (field, fieldResults)
        where
          errorMsgField = error $ "runFunction: function " ++ show fname ++ " not found in input bound specification."
          aceb2PathInput aceb = PathInput
            { piConds = conds aceb, piPath = decisionPath aceb
            , piFlow = cFlow aceb, piErrExpr = fromJust $ eExpr aceb
            , piFpExprs = fDeclRes $ fpExprs aceb
            , piRealExprs = rDeclRes $ rExprs aceb }

          runErrorExpression pathInput = do
            errExpr <- processedErrExpr
            result  <- run (kodiakInput errExpr) ()
            -- The absolute bound is handed to the relative run rather than
            -- recomputed: it is read off the very 'KodiakResult' whose
            -- 'maximumUpperBound' becomes the absolute error in the report and
            -- in the certificate, so when the relative run falls back to
            -- dividing it by a floor on the exact result, the two bounds in the
            -- output are about the same number.
            relError <- if relErrEnabled
                        then toRelErrorResult <$> computeRelError searchParams fname
                               binds errExpr (maximumUpperBound result)
                               (piRealExprs pathInput)
                        else return RelErrorOff
            return PathResult { prConds     = piConds pathInput
                              , prPath      = piPath pathInput
                              , prFlow      = piFlow pathInput
                              , prKodiak    = result
                              , prErrExpr   = initAExpr err
                              , prFpExprs   = piFpExprs pathInput
                              , prRealExprs = piRealExprs pathInput
                              , prRelError  = relError }
              where
                err = piErrExpr pathInput
                binds = fromMaybe (error $ "runFunction: function " ++ show fname ++ " not found.")
                                  (lookup fname functionBindingsMap)
                -- The processed error expression, computed once and shared by
                -- the absolute and the relative Kodiak runs. The relative error
                -- must be the ratio of exactly the quantity the absolute
                -- certificate bounds, so both runs must see the same expression.
                processedErrExpr =
                  if unfoldFunCalls'
                  then return $ simplAExpr $ initAExpr err
                  else case findInDecls fname decls of
                    Just (_,_,AExprBody funBody) -> do
                      let locVars = localVarsWithType funBody
                      replaceFunCallErr True config interp emptyEnv locVars binds $ simplAExpr $ initAExpr err
                    _ -> error $ "[computeAllErrorsInKodiakMap.runFunField] Function " ++ fname ++ " not found."
                kodiakInput errExpr =
                  KI { kiName = fname,
                       kiExpression = errExpr,
                       kiBindings = binds,
                       kiMaxDepth  = KP.maximumDepth searchParams,
                       kiPrecision = KP.minimumPrecision searchParams
                     }