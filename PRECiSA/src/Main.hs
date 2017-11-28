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


{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifndef DUMP_LOG
#define MB_DUMP_LOG Nothing
#else
#ifdef OVERRIDE_DUMP_LOG
#define MB_DUMP_LOG (Just OVERRIDE_DUMP_LOG)
#else
#define MB_DUMP_LOG (Just DUMP_LOG)
#endif
#endif

#ifndef SHOW_LOG
#define SHOW_LOG False
#endif

module Main where

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess,exitFailure)
import Control.Monad
import Data.List (sortBy)
import Data.Maybe (fromJust)
import System.FilePath.Windows
import ErrM
import Parser.ParPVSLang
import Parser.ParSpecLang
import Parser.ParDecisionPaths
import AbsPVSLang
import AbsSpecLang
import Common.ControlFlow
import AbstractSemantics
import AbstractDomain
import Kodiak.Kodiak
import Kodiak.KodiakRunner
import Kodiak.KodiakRunnable
import Foreign.C
import PPExt
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Time
import Debug.Trace
import FPrec
import Common.DecisionPath
import Foreign.C.Types (CDouble, CFloat)
import Data.Bits.Floating
import Data.Ratio
import Numeric


--{- COMMAND LINE OPTIONS AND ARGUMENTS -}

data Options = Options
   { optHelp         :: Bool
   , optDumpFile     :: Maybe FilePath
   , optShowLog      :: Bool
   } deriving Show

defaultOptions = Options
   { optHelp         = False
   , optDumpFile     = MB_DUMP_LOG
   , optShowLog      = SHOW_LOG
   }

optionDescrs :: [OptDescr (Options -> Options)]
optionDescrs =
  [ Option ['h'] ["help"]
    (NoArg (\ opts -> opts { optHelp = True }))
    "display this help"
  , Option ['d'] ["dumpfile"]
    (ReqArg (\ d opts -> opts { optDumpFile = Just d }) "FILE")
    "sets the dump file"
  , Option ['l'] ["showlog"]
    (NoArg (\ opts -> opts { optShowLog = True }))
    "shows the log of the tool"
  ]

processOpts :: [String] -> (Options, [String],[String])
processOpts argv = (foldl (flip id) defaultOptions o, n, errs)
  where
    (o,n,errs) = getOpt Permute optionDescrs argv

printVersion prgname = do
    putStr " ("
    putStr prgname
    putStr ")"

printHelp prgname = do
  let header = unlines [
        "",
        "Usage: ",
        "\t" ++ prgname ++ " [opt] (program-to-analyse)",
        "",
        "FPEAnalysis takes a let program file and compute the associated overapproximation of the floatting-point error produced."
        ]
  putStrLn $ usageInfo header optionDescrs


--{- PARSERS -}

parseFileToProgram :: FilePath -> IO (Err Program)
parseFileToProgram src_filename = fmap (parseProgram) (readFile src_filename)

parseProgram :: FilePath -> Err Program
parseProgram str =
  do
    parsedProg <- rawparserLet str
    return parsedProg

parseFileToTargetDPs :: FilePath -> IO (Err TargetDPs)
parseFileToTargetDPs src_filename = fmap (parseTargetDPs) (readFile src_filename)

parseTargetDPs :: FilePath -> Err TargetDPs
parseTargetDPs str =
  do
    parseTargetDPs <- rawparserTargetDPs str
    return parseTargetDPs

parseFileToSpec :: FilePath -> IO (Err Spec)
parseFileToSpec src_filename = fmap (parseSpec) (readFile src_filename)

parseSpec :: FilePath -> Err Spec
parseSpec str =
  do
    parsedSpec <- rawparserSpec str
    return parsedSpec

ok :: a -> IO a
ok x = 
    return x


--{- MAIN -}

main :: IO ()
main = do

  prgname <- getProgName
  cmdargs <- getArgs

--  printVersion prgname

  let (options,args,errs) = processOpts cmdargs

  when (not $ null errs) $ do
    printHelp prgname
    putStrLn $ "Problems parsing arguments: " ++ concat errs ++ "\n"
    exitFailure

  when (optHelp options) $ do
    printHelp prgname
    exitSuccess

  case args of
    [fileprog,filespec,filedps,md,prec,dp,sta,maxelem] -> do
      parseAndAnalysis fileprog filespec filedps (read md::Int) (read prec::Int) (read dp::Int) (read sta::Bool) (read maxelem::Int) 
--      putStrLn "Analysis done."
--      putStrLn "done."
    _ -> putStrLn "wrong number of arguments"


checkProgSize :: [(String, (FPrec,[VarId], ACebS))] -> Int -> Int -> IO ()
checkProgSize [] n maxel | n >= maxel = error "The generated file is too big! Try to run the analysis with the stable tests assumption."
                         | otherwise  = return ()
checkProgSize ((_,(_,_,cebs)):xs) n maxel | (length cebs) + n >= maxel = error "The generated file is too big! Try to run the analysis with the stable tests assumption."
                                          | otherwise = checkProgSize xs ((length cebs) + n) maxel

parseAndAnalysis :: FilePath -- program to analyze
                 -> FilePath -- specification with initial ranges
                 -> FilePath -- paths of interest
                 -> Int      -- max depth
                 -> Int      -- precision
                 -> Int      -- display precision
                 -> Bool     -- stable test assumption?
                 -> Int      -- max number of lemmas
                 -> IO ()
parseAndAnalysis fileprog filespec filedps maxDepth prec displayPrec sta maxel = do
  errparseProg <- parseFileToProgram fileprog
  prog <- errify error errparseProg
  errparseSpec <- parseFileToSpec filespec
  spec <- errify fail errparseSpec
  errparseTargetDPs <- parseFileToTargetDPs filedps
  dps <- errify fail errparseTargetDPs
  -------------- 
  time <- getCurrentTime
  -------------
  genCertFile  inputFileName certFile certFileName prog time maxel (sem prog dps)
  genRealProgFile inputFileName realProgFile realProgFileName (fp2realProg prog)
  -------------- just for batch mode
  putStrLn "********************************************************************"
  putStrLn "****************************** PRECiSA *****************************"
  putStrLn ""
  let searchParams = SP { maximumDepth = fromInteger . toInteger $ maxDepth, minimumPrecision = fromInteger . toInteger $ prec }
  let pgmInterpretation = initErrVars (sem prog dps)
  printInfiteErrorInterp (filterInfiniteCebS pgmInterpretation)
  results <- computeAllErrorsInKodiak' (removeInfiniteCebS pgmInterpretation) spec searchParams
--   let maxErrors = summarizeAllErrors results
  printAllErrors' results
  genNumCertFile certFileName numCertFile numCertFileName results spec maxDepth prec displayPrec time
  putStrLn ""
  putStrLn ""
  -------------- just for batch mode
  putStrLn "********************************************************************"
  putStrLn "***** Files generated successfully *****" 
  putStrLn ("Symbolic lemmas and proofs in: " ++ certFile)
  putStrLn ("Numeric lemmas and proofs in: " ++ numCertFile)
  -------------
  return ()
    where
      sem p@(Prog decls) d = fixpointSemantics p (botInterp decls) 3 semConf d
      semConf = SemConf { assumeTestStability = sta}
      inputFileName = takeBaseName fileprog
      certFile =  (dropFileName fileprog) ++ certFileName ++ ".pvs"
      numCertFile = (dropFileName fileprog) ++ numCertFileName ++ ".pvs"
      realProgFile = (dropFileName fileprog) ++ inputFileName ++ "_real.pvs"
      certFileName = "cert_" ++ inputFileName
      numCertFileName = "num_cert_" ++ inputFileName
      realProgFileName = inputFileName ++ "_real"

printInfiteErrorInterp :: Interpretation -> IO [()]
printInfiteErrorInterp interp = mapM printInfiteError interp
  where
    printInfiteError (f, _) = putStrLn $ "Function " ++ f ++ " has an infinite error."

filterInfiniteCebS :: Interpretation -> Interpretation
filterInfiniteCebS [] = []
filterInfiniteCebS ((f,(fp,args,cebs)):is) =
  if filteredCebS /= []
  then (f,(fp,args,filteredCebS)):filterInfiniteCebS is
  else filterInfiniteCebS is
  where
    filteredCebS = filter hasInfiniteError cebs
    hasInfiniteError ceb@(ACeb { eExpr = ee}) = ee == Infinity

removeInfiniteCebS :: Interpretation -> Interpretation
removeInfiniteCebS [] = []
removeInfiniteCebS ((f,(fp,args,cebs)):is) =
  if filteredCebS /= []
  then (f,(fp,args,filteredCebS)):removeInfiniteCebS is
  else removeInfiniteCebS is
  where
    filteredCebS = filter (not . hasInfiniteError) cebs
    hasInfiniteError ceb@(ACeb { eExpr = ee}) = ee == Infinity

summarizeAllErrors :: [(String, [KodiakResult])] -> [(String, Double)]
summarizeAllErrors errorMap = [ (f,maximum $ map maximumUpperBound results) | (f, results) <- errorMap]

printAllErrors' :: [(String,FPrec,[VarId],[(Conditions,LDecisionPath,ControlFlow,KodiakResult)])] -> IO ()
printAllErrors' = mapM_ printFunction
  where
    stabilityOrder (_,p1,Stable,_)  (_,p2,Stable,_) = compare p1 p2
    stabilityOrder (_,_,Stable,_)   (_,_,_,_) = LT
    stabilityOrder (_,_,Unstable,_) (_,_,_,_) = GT

    printFunction (f,_,_,results) = do
      putStrLn $ "Function " ++ f
      mapM_ printResult $ sortBy stabilityOrder results
        where
          printResult (_,path,flow,result) =
            if flow == Stable
            then putStrLn $ "path " ++ show path ++ ": " ++ (showFFloat Nothing (nextUp (maximumUpperBound result)) "")
            else putStrLn $ "unstable paths: " ++ (showFFloat Nothing (nextUp (maximumUpperBound result)) "")

data SearchParameters = SP { maximumDepth :: CUInt, minimumPrecision :: CUInt }

computeAllErrorsInKodiak' :: Interpretation -> Spec -> SearchParameters -> IO [(String,FPrec,[VarId],[(Conditions, LDecisionPath,ControlFlow,KodiakResult)])]
computeAllErrorsInKodiak' interp spec searchParams = mapM runFunction functionNames
  where
    functionNames = map fst interp
    functionBindingsMap = map (\(SpecBind f _ b) -> (f,b)) bindings
      where Spec bindings = spec
    functionVariableMapMap = map (\(f,bs) -> (f,createVariableMap bs)) functionBindingsMap
      where createVariableMap bs = VMap $ zip allVariables [(0::CUInt)..]
              where allVariables = map (\(VarBind v _ _) -> v) bs
    functionErrorExpressionsMap = map toPathFlowErrorTuple interp
      where
        toPathFlowErrorTuple (f,(fp,args,acebs)) = (f,map (\x -> (fp,args,aceb2PathFlowErrorTuple x)) acebs)
          where
            aceb2PathFlowErrorTuple aceb = (conds aceb, decisionPath aceb, cFlow aceb, eExpr aceb)

    runFunction name = do results <- mapM runErrorExpression $ fromJust $ lookup name functionErrorExpressionsMap
                          let (fprec,args,_):_ = results
                          let results' = map (\(_,_,a) -> a) results
                          return (name, fprec, args, results')
      where
        runErrorExpression (fprec :: FPrec,args :: [VarId],(cond :: Conditions,path :: LDecisionPath, flow, error)) = do
          result <- run kodiakInput ()
          return (fprec, args, (cond, path, flow, result))
            where
              kodiakInput = KI { name = name,
                                 expression = error,
                                 bindings = fromJust $ lookup name functionBindingsMap,
                                 maxDepth = maximumDepth searchParams,
                                 precision = minimumPrecision searchParams
                               }


--{- OUTPUT FILE GENERATION -}

genCertFile :: (Show t) => [Char] -> FilePath -> [Char] -> Program -> t -> Int -> [(String, (FPrec, [VarId], ACebS))] -> IO ()
genCertFile inputFileName certFile certFileName prog date maxel sem = do
  checkProgSize sem 0 maxel
  writeFile  certFile ("% This file is automatically generated by PRECiSA \n")
  appendFile certFile (certFileName ++ ": THEORY\n")
  appendFile certFile ("BEGIN\n")
  appendFile certFile ("IMPORTING PRECiSA@strategies, " ++ inputFileName ++ "," ++ (inputFileName ++ "_real")  ++ "\n\n")
  appendFile certFile ("%|- *_TCC* : PROOF\n")
  appendFile certFile ("%|- (precisa-gen-cert-tcc)\n")
  appendFile certFile ("%|- QED\n\n")
  appendFile certFile $ printCerts sem prog
  appendFile certFile ("\n")
  appendFile certFile ("END " ++ certFileName)
  return ()

genRealProgFile inputFileName realProgFile realProgFileName prog = do
  writeFile  realProgFile ("% This file is automatically generated by PRECiSA \n")
  appendFile realProgFile (realProgFileName ++ ": THEORY\n")
  appendFile realProgFile ("BEGIN\n")
  appendFile realProgFile ("IMPORTING " ++ inputFileName ++ "\n\n")
  appendFile realProgFile $ render $ prettyDoc prog
  appendFile realProgFile ("\n")
  appendFile realProgFile ("END " ++ realProgFileName)
  return ()


genNumCertFile :: Show a => [Char] -> FilePath -> [Char] -> [(String,FPrec,[VarId],[(Conditions,LDecisionPath,ControlFlow,KodiakResult)])] -> Spec -> Int -> Int -> Int -> a -> IO ()
-- generates the file for generating concrete bounds given initial ranges for the variables
genNumCertFile certFileName numCertFile numCertFileName kodiakResult spec@(Spec specBinds) maxDepth prec displayPrec date = do
  writeFile  numCertFile ("% This file is automatically generated by PRECiSA \n")
  appendFile numCertFile ("% " ++ (show date) ++"\n")
  appendFile numCertFile ("% maxDepth: " ++ (show maxDepth) ++ " , prec: 10^-" ++ (show prec) ++ " , displayPrec: " ++ (show displayPrec) ++ "\n\n")
  appendFile numCertFile (numCertFileName ++ ": THEORY\n")
  appendFile numCertFile ("BEGIN\n")
  appendFile numCertFile ("IMPORTING " ++ certFileName ++ ", PRECiSA@bbiasp, PRECiSA@bbiadp, PRECiSA@strategies \n\n")
  appendFile numCertFile ("%|- *_TCC* : PROOF\n")
  appendFile numCertFile ("%|- (precisa-gen-cert-tcc)\n")
  appendFile numCertFile ("%|- QED\n\n")
  appendFile numCertFile $ render $ printNumCerts kodiakResult specBinds maxDepth prec
  appendFile numCertFile ("\n")
  appendFile numCertFile ("END " ++ numCertFileName)
  return ()

ppVarWithProg prog x = prog++"_r_"++x

bindList :: [SpecBind] -> [VarBind]
-- list of variable ranges in a program specification
bindList binds = Set.toList . Set.fromList $ aux binds
  where
    aux [] = []
    aux ((SpecBind _ _ b):bs) = b ++ (aux bs)


printCerts :: [(String, (FPrec, [VarId], ACebS))] -> Program -> String
printCerts [] _ = []
printCerts ((f,(fp, args, cset)):interp) prog@(Prog decls)
  = (printLemmasAndProofs f args stm cset fp 0)
    ++ (printCerts interp prog)
  where
    stm = findInDecls f decls 
    findInDecls f [] = error ("findInDecls: function " ++ f ++ " not found")
    findInDecls f ((Decl _ (NonVarId g) _ stm):ds) | f==g = stm
                                                   | otherwise = findInDecls f ds 

printLemmasAndProofs :: String -> [VarId] -> Stm -> ACebS -> FPrec -> Int -> String
-- prints the lemmas and proofs for each conditional error bound
printLemmasAndProofs _ _ _ [] _ _ = []
printLemmasAndProofs f args stm (aceb:acebs) fp n
  = (render $ prPvsLemma f args aceb fp n) ++ (render $ prPvsProof f fp n)
    ++ (printLemmasAndProofs f args stm acebs fp (n+1))

prPvsProof :: String -> FPrec -> Int -> Doc    
-- print the proof corresponding to a conditional error bound
prPvsProof f fp n =
    text "%|- " <> text f <> text "_" <> int n <> text ": PROOF"
    $$ text "%|- (precisa)"
    $$ text "%|- QED"
    $$ text "\n"

prPvsLemma :: String -> [VarId] -> ACeb -> FPrec -> Int -> Doc
prPvsLemma f args aceb fp n =
  text f <> text "_" <> int n <+> text ": LEMMA"
  $$ text "FORALL(" <>  (hsep $ punctuate comma $ map prErrorInt args)
                    <>  text ": nonneg_real" <> comma
                    <+> (hsep $ punctuate comma $ map prRealInt args)
                    <>  text ": real" <> comma
                    <+> (hsep $ punctuate comma $ map prettyDoc args)
                    <> text ": " <>  pvsType fp <> text ")" <> text ":"
  $$ prPvsArgs args
  $$ text "AND" <+> parens (prettyDocWith fp c)
  $$ text "IMPLIES"
  $$ text "abs(" <> s2f fp <>
    text "(" <> text f <> text "(" <>
    (hsep $ punctuate comma $ map prettyDoc args) <> text ")" <> text ")"
     <+> text "-"
     <+> text f <> text "_real" <> text "("
     <> (hsep $ punctuate comma $ map (prettyDocWith fp) (map realVar args)) <> text ")"
     <> text ")"
     <> text "<=" <> prettyDocWith fp err
  $$ text "\n"
  where
    prPvsArgs args = hsep $ punctuate (text " AND") $ map prPVSVarId args
    
    prRealInt (VarId x) = text "r_" <> text x
    prErrorInt (VarId x) = text "e_" <> text x
    
    prPVSVarId (VarId x) = text "abs(" <> s2f fp <> text "(" <> text x <> text ")"
                           <+> text "-" <+> text "r_" <> text x <> text ")"
                           <> text "<=" <> text "e_" <> text x
    ACeb {conds = c, eExpr = err} = aceb

    realVar (VarId x) = RealMark x


printNumCerts :: [(String,FPrec,[VarId],[(Conditions,LDecisionPath,ControlFlow,KodiakResult)])] -> [SpecBind] -> Int -> Int -> Doc
printNumCerts [] _ _ _ = emptyDoc
printNumCerts ((f,fp,args,result):res') spec maxDepth prec =
  (printNumCertsFun f args result ranges 0 fp prec maxDepth)
    $$ (printNumCerts res' spec maxDepth prec)
  where
    ranges = findInSpec f spec
    findInSpec fun [] = error ("findInDecls: function " ++ f ++ " not found")
    findInSpec fun ((SpecBind g _ varBind):rest) | fun == g = varBind
                                                 | otherwise = findInSpec fun rest

-- do it with a map in the previous function directly
printNumCertsFun :: String -> [VarId] -> [(Conditions,LDecisionPath,ControlFlow,KodiakResult)] -> [VarBind] -> Int -> FPrec -> Int -> Int -> Doc
printNumCertsFun f args [] ranges n fp prec maxDepth = emptyDoc
printNumCertsFun f args ((cond, _, _, res):result') ranges n fp prec maxDepth =
  prPvsLemmaSpec f args cond roundOffError ranges n fp prec maxDepth
  $$
  printNumCertsFun f args result' ranges (n+1) fp prec maxDepth
  where
    roundOffError = maximumUpperBound res


prPvsLemmaSpec :: String -> [VarId] -> Conditions -> Double -> [VarBind] -> Int -> FPrec -> Int -> Int -> Doc
-- prints the lemma corresponding to a conditional error bound
prPvsLemmaSpec f args cond roundOffError ranges n fp prec maxDepth =
  text f <> text "_c_" <> int n <+> text ": LEMMA"
  $$ text "FORALL(" <> (hsep $ punctuate comma $ map ((prettyDocWith fp) . realVar) args)
                    <>  text ": real" <> comma
                    <+> (hsep $ punctuate comma $ map prettyDoc args) <> text ": " <>  pvsType fp <> text ")" <> text ":"
  $$ prPvsArgs fp args 
  $$ text "AND" <+> parens (prettyDocWith fp cond)
  $$ text "AND" <+> (hsep $ punctuate (text " AND ") $ map printVarRange ranges)
  $$ text "IMPLIES"
  $$ text "abs(" <> s2f fp
  <> text "(" <> text f <> text "("
  <> (hsep $ punctuate comma $ map prettyDoc args) <> text ")" <> text ")"
  <+> text "-"
  <+> text f <> text "_real" <> text "("
  <> (hsep $ punctuate comma $ map (prettyDocWith fp) (map realVar args)) <> text ")"
  <> text ")"
  <> text "<="
  <> text (showFFloat Nothing (nextUp roundOffError) "")  <> text "\n"
  $$ text "%|-" <+> text f <> text "_c_" <> int n <+> text ": PROOF"
  $$ text "%|-" <+> parens (text "prove-concrete-lemma" <+> text f <> text "_" <> int n <+> int prec <+> int maxDepth)
  $$ text "%|- QED\n" 
  where
    prPvsArgs fp args = hsep $ punctuate (text " AND") $ map (prPVSVarId fp) args 
    
    realVar (VarId x) = RealMark x
    
    prPVSVarId fp (VarId x) = text "abs(" <> s2f fp <> text "(" <> text x <> text ")" <+> text "-" <+> text "r_" <> text x <> text ")"
                           <> text "<=" <> prettyDocWith fp (HalfUlp (RealMark x))

printVarRange :: VarBind -> Doc
-- print the ranges of the variables as an hypothesis of a lemma
printVarRange (VarBind x lb ub) =
  text "r_" <> text x <+> text "##" <+> text "[|" <> prettyDoc lb <> comma <> prettyDoc ub <> text "|]"



prKodiakSem :: Interpretation -> Interpretation -> Doc
prKodiakSem [] _ = emptyDoc
prKodiakSem ((f,(fp, args, cset)):rest) interp = 
  text ""
  $$ text "Semantics of " <> text f <> colon
  $$ text ""
  $$ prKodiakAcebS cset interp fp
  $$ text ""
  $$ prKodiakSem rest interp


prKodiakAcebS :: ACebS -> Interpretation -> FPrec -> Doc
prKodiakAcebS [] _ _ = emptyDoc
prKodiakAcebS (aceb:acebs) interp fp =
  prettyKodiakAceb aceb interp fp 
  $$ text ""
  $$ (prKodiakAcebS acebs interp fp)

prettyKodiakAceb :: ACeb -> Interpretation -> FPrec -> Doc
prettyKodiakAceb aceb interp fp =
  text (show cflow) 
  $$ prettyKodiakCond cs interp fp
  $$ text "=>"
  $$ prettyKodiakWith fp err
    where
      ACeb { eExpr = err, conds = Cond cs, rExprs = rExpr, cFlow = cflow} = aceb

prettyKodiakCond :: [Condition] -> Interpretation -> FPrec -> Doc
prettyKodiakCond [] _ _ = text "True"

prettyKodiakCond [(be,fbe)] interp fp =
  prettyKodiakBExpr be interp fp
  $$ text "And"
  $$ prettyKodiakFBExpr fbe interp fp

prettyKodiakCond ((be,fbe):cs) interp fp =
  prettyKodiakBExpr be interp fp
  $$ text "And"
  $$ prettyKodiakFBExpr fbe interp fp
  $$ text "And"
  $$ prettyKodiakCond cs interp fp 

prettyKodiakBExpr :: BExpr -> Interpretation -> FPrec -> Doc
prettyKodiakBExpr (Or  b1 b2) interp fp = parens $ (prettyKodiakBExpr b1 interp fp) <> text "Or"  <> (prettyKodiakBExpr b1 interp fp)
prettyKodiakBExpr (And b1 b2) interp fp = parens $ (prettyKodiakBExpr b1 interp fp) <> text "And" <> (prettyKodiakBExpr b1 interp fp)
prettyKodiakBExpr (Not b)     interp fp = parens $ text "Not" <> (prettyKodiakBExpr b interp fp)
prettyKodiakBExpr (Eq  a1 a2) interp fp = parens $ (prettyKodiakAExpr a1 interp fp) <> text "==" <> (prettyKodiakAExpr a2 interp fp)
prettyKodiakBExpr (Neq a1 a2) interp fp = parens $ (prettyKodiakAExpr a1 interp fp) <> text "!=" <> (prettyKodiakAExpr a2 interp fp)
prettyKodiakBExpr (Lt  a1 a2) interp fp = parens $ (prettyKodiakAExpr a1 interp fp) <> text "<"  <> (prettyKodiakAExpr a2 interp fp)
prettyKodiakBExpr (LtE a1 a2) interp fp = parens $ (prettyKodiakAExpr a1 interp fp) <> text "<=" <> (prettyKodiakAExpr a2 interp fp)
prettyKodiakBExpr (Gt  a1 a2) interp fp = parens $ (prettyKodiakAExpr a2 interp fp) <> text "<"  <> (prettyKodiakAExpr a1 interp fp)
prettyKodiakBExpr (GtE a1 a2) interp fp = parens $ (prettyKodiakAExpr a2 interp fp) <> text "<=" <> (prettyKodiakAExpr a1 interp fp)
prettyKodiakBExpr BTrue  _ _ = text "True"
prettyKodiakBExpr BFalse _ _ = text "False"


prettyKodiakFBExpr :: FBExpr -> Interpretation -> FPrec -> Doc
prettyKodiakFBExpr (FOr  fb1 fb2) interp fp = parens $ (prettyKodiakFBExpr fb1 interp fp) <> text "Or"  <> (prettyKodiakFBExpr fb1 interp fp)
prettyKodiakFBExpr (FAnd fb1 fb2) interp fp = parens $ (prettyKodiakFBExpr fb1 interp fp) <> text "And" <> (prettyKodiakFBExpr fb1 interp fp)
prettyKodiakFBExpr (FNot fb)      interp fp = parens $ text "Not" <> (prettyKodiakFBExpr fb interp fp)
prettyKodiakFBExpr (FEq  fa1 fa2) interp fp = parens $ (prettyKodiakFAExpr fa1 interp fp) <> text "==" <> (prettyKodiakFAExpr fa2 interp fp)
prettyKodiakFBExpr (FNeq fa1 fa2) interp fp = parens $ (prettyKodiakFAExpr fa1 interp fp) <> text "!=" <> (prettyKodiakFAExpr fa2 interp fp)
prettyKodiakFBExpr (FLt  fa1 fa2) interp fp = parens $ (prettyKodiakFAExpr fa1 interp fp) <> text "<"  <> (prettyKodiakFAExpr fa2 interp fp)
prettyKodiakFBExpr (FLtE fa1 fa2) interp fp = parens $ (prettyKodiakFAExpr fa1 interp fp) <> text "<=" <> (prettyKodiakFAExpr fa2 interp fp)
prettyKodiakFBExpr (FGt  fa1 fa2) interp fp = parens $ (prettyKodiakFAExpr fa2 interp fp) <> text "<"  <> (prettyKodiakFAExpr fa1 interp fp)
prettyKodiakFBExpr (FGtE fa1 fa2) interp fp = parens $ (prettyKodiakFAExpr fa2 interp fp) <> text "<=" <> (prettyKodiakFAExpr fa1 interp fp)
prettyKodiakFBExpr FBTrue  _ _ = text "True"
prettyKodiakFBExpr FBFalse _ _ = text "False"


prettyKodiakAExpr :: AExpr -> Interpretation -> FPrec -> Doc
prettyKodiakAExpr Pi _ _ = text "Pi"
prettyKodiakAExpr (Int i) _ _ =
    if length (show i) > 9
        then text "val" <> (parens $ text "Interval" <> (parens $ (text (showFullPrecision lb) <> comma <> text (showFullPrecision ub))))
        else text "val" <> (parens $ integer i)
    where
        (lb,ub) = rat2interval (toRational i) 
prettyKodiakAExpr (Double r) _ _ =
    if (length (show num) > 9) || (length (show den) > 9)
        then text "val" <> (parens $ text "Interval" <> (parens $ (text (showFullPrecision lb) <> comma <> text (showFullPrecision ub))))
        else parens $ text "val" <> (parens $ text "rat" <> (parens $ (integer num) <> comma <> (integer den)))
    where
        (lb,ub) = rat2interval r
        num = (numerator r)
        den = (denominator r)
prettyKodiakAExpr (Var x)     _ _ = text x
prettyKodiakAExpr (EFun f []) _ _ = text f
prettyKodiakAExpr (Add a1 a2) interp fp = parens $ (prettyKodiakAExpr a1 interp fp) <+> text "+" <+> (prettyKodiakAExpr a2 interp fp)
prettyKodiakAExpr (Sub a1 a2) interp fp = parens $ (prettyKodiakAExpr a1 interp fp) <+> text "-" <+> (prettyKodiakAExpr a2 interp fp)
prettyKodiakAExpr (Mul a1 a2) interp fp = parens $ (prettyKodiakAExpr a1 interp fp) <+> text "*" <+> (prettyKodiakAExpr a2 interp fp)
prettyKodiakAExpr (Div a1 a2) interp fp = parens $ (prettyKodiakAExpr a1 interp fp) <+> text "/" <+> (prettyKodiakAExpr a2 interp fp)
prettyKodiakAExpr (Mod a1 a2) _ _ = error "prettyKodiakAExpr Mod niy"
prettyKodiakAExpr (Pow a (Int n)) interp fp = (parens $ prettyKodiakAExpr a interp fp) <> text "^" <> integer n
prettyKodiakAExpr (Pow a1 a2) _ fp = error "prettyKodiakWith niy"
prettyKodiakAExpr (Neg   a) interp fp = parens $ text "-" <> prettyKodiakAExpr a interp fp
prettyKodiakAExpr (Floor a) interp fp = text "Floor" <> lparen <> prettyKodiakAExpr a interp fp <> rparen 
prettyKodiakAExpr (Sqrt  a) interp fp = text "Sqrt"  <> lparen <> prettyKodiakAExpr a interp fp <> rparen
prettyKodiakAExpr (Abs   a) interp fp = text "Abs"   <> lparen <> prettyKodiakAExpr a interp fp <> rparen
prettyKodiakAExpr (Sin   a) interp fp = text "Sin"   <> lparen <> prettyKodiakAExpr a interp fp <> rparen
prettyKodiakAExpr (Cos   a) interp fp = text "Cos"   <> lparen <> prettyKodiakAExpr a interp fp <> rparen
prettyKodiakAExpr (Tan   a) interp fp = text "Tan"   <> lparen <> prettyKodiakAExpr a interp fp <> rparen
prettyKodiakAExpr (ASin  a) interp fp = text "Asin"  <> lparen <> prettyKodiakAExpr a interp fp <> rparen
prettyKodiakAExpr (ACos  a) interp fp = text "Acos"  <> lparen <> prettyKodiakAExpr a interp fp <> rparen
prettyKodiakAExpr (ATan  a) interp fp = text "Atan"  <> lparen <> prettyKodiakAExpr a interp fp <> rparen
prettyKodiakAExpr (EFun f args) interp fp = text f <> (parens $ hsep $ punctuate comma $ map (\x -> prettyKodiakAExpr x interp fp) args)
prettyKodiakAExpr (SUlp a) interp fp = text "SUlp" <> lparen <> prettyKodiakAExpr a interp fp <> rparen
prettyKodiakAExpr (DUlp a) interp fp = text "DUlp" <> lparen <> prettyKodiakAExpr a interp fp <> rparen
prettyKodiakAExpr (StoR a) interp fp = prettyKodiakFAExpr a interp fp
prettyKodiakAExpr (DtoR a) interp fp = prettyKodiakFAExpr a interp fp
prettyKodiakAExpr (EE e)   interp fp = prettyKodiakWith fp e
prettyKodiakAExpr (FPrec)  interp fp= text "val(24)"
prettyKodiakAExpr (FPrec)  interp fp = text "val(52)"
prettyKodiakAExpr (FExp fa) interp fp = text "FExp" <> (parens $ prettyKodiakFAExpr fa interp fp)
prettyKodiakAExpr (RealMark x) interp fp = text x
prettyKodiakAExpr (Max as) interp fp = text "Max" <> parens (lbrace <> (hsep $ punctuate comma $ (map (\a -> prettyKodiakAExpr a interp fp ) as)) <> rbrace)
prettyKodiakAExpr (Min as) interp fp = text "Min" <> parens (lbrace <> (hsep $ punctuate comma $ (map (\a -> prettyKodiakAExpr a interp fp ) as)) <> rbrace)
prettyKodiakAExpr        p _ _ = error $ "prettyKodiakWith " ++ show p

prettyKodiakFAExpr :: FAExpr -> Interpretation -> FPrec -> Doc

prettyKodiakFAExp fa@(FInt i)    fp _ = prettyKodiakWith fp (Int i)
prettyKodiakFAExp fa@(FDouble rat) fp _ = text "MM" <> (parens $ prettyKodiakWith fp (Double rat)
                                                  <> comma
                                                  <> prettyKodiakWith fp (ErrRat $ abs $ toRational ((fromRat rat) :: CDouble) - (rat :: Rational))) 
prettyKodiakFAExp FPi            fp _ = text "Pi"
prettyKodiakFAExp fa@(RtoS a)    fp _ = prettyKodiakWith FPSingle a
prettyKodiakFAExp fa@(RtoD a)    fp _ = prettyKodiakWith FPDouble a 

prettyKodiakFAExpr (FVar x) _ fp = text "MM" <> (parens $ prettyKodiakWith fp (RealMark x) <> comma <> prettyKodiakWith fp (Div (ulp (RealMark x)) (Int 2)))
  where
    ulp = case fp of 
        FPSingle -> SUlp
        FPDouble -> DUlp

prettyKodiakFAExpr fa interp fp = text "MM" <> (parens $ prettyKodiakWith fp (fae2real fa) <> comma <> prettyKodiakWith fp (fpError fa interp fp))

fpError :: FAExpr -> Interpretation -> FPrec -> EExpr
fpError (FInt i)       _  _ = ErrRat 0
fpError (FDouble n)    _  _ = ErrRat $ abs $ toRational ((fromRat n) :: CDouble) - (n :: Rational)
fpError (FVar x)       _ fp = AE $ Div (ulp (RealMark x)) (Int 2)
  where
    ulp = case fp of 
        FPSingle -> SUlp
        FPDouble -> DUlp
fpError (FPi)          _ _ = error "fpError: FPi niy"
fpError (RtoS (Int n))       _ _ = ErrRat $ 0
fpError (RtoD (Int n))       _ _ = ErrRat $ 0
fpError (RtoS (Double n))       _ _ = ErrRat $ abs $ toRational ((fromRat n) :: CFloat) - (n :: Rational)
fpError (RtoD (Double n))       _ _ = ErrRat $ abs $ toRational ((fromRat n) :: CDouble) - (n :: Rational)
fpError (RtoS _)       _ _ = error "fpError: RtoS niy"
fpError (RtoD _)       _ _ = error "fpError: RtoD niy"
fpError (FAdd fa1 fa2) fp interp = ErrAdd (fae2real fa1) (fpError fa1 fp interp) (fae2real fa2) (fpError fa2 fp interp)
fpError (FSub fa1 fa2) fp interp = ErrSub (fae2real fa1) (fpError fa1 fp interp) (fae2real fa2) (fpError fa2 fp interp)
fpError (FMul fa1 fa2) fp interp = ErrMul (fae2real fa1) (fpError fa1 fp interp) (fae2real fa2) (fpError fa2 fp interp)
fpError (FDiv fa1 fa2) fp interp = ErrDiv (fae2real fa1) (fpError fa1 fp interp) (fae2real fa2) (fpError fa2 fp interp)
fpError (FPow fa1 fa2) _ _ = error "fpError: FPow niy"
fpError (FMod fa1 fa2) _ _ = error "fpError: FMod niy"
fpError (FNeg   fa) fp interp = ErrNeg   (fae2real fa) (fpError fa fp interp) 
fpError (FFloor fa) fp interp = ErrFloor (fae2real fa) (fpError fa fp interp) 
fpError (FSqrt  fa) fp interp = ErrSqrt  (fae2real fa) (fpError fa fp interp) 
fpError (FAbs   fa) fp interp = ErrAbs   (fae2real fa) (fpError fa fp interp) 
fpError (FSin   fa) fp interp = ErrSin   (fae2real fa) (fpError fa fp interp) 
fpError (FCos   fa) fp interp = ErrCos   (fae2real fa) (fpError fa fp interp) 
fpError (FTan   fa) fp interp = ErrTan   (fae2real fa) (fpError fa fp interp) 
fpError (FAcos  fa) fp interp = ErrAcos  (fae2real fa) (fpError fa fp interp) 
fpError (FAsin  fa) fp interp = ErrAsin  (fae2real fa) (fpError fa fp interp) 
fpError (FAtan  fa) fp interp = ErrAtan  (fae2real fa) (fpError fa fp interp) 
fpError (FMin fas)  fp interp = MaxErr $ map (\fa -> fpError fa fp interp) fas
fpError (FMax fas)  fp interp = MaxErr $ map (\fa -> fpError fa fp interp) fas
fpError (FEFun fun fas) fp interp = error ""

initErrVars :: [(String, (FPrec, [VarId], ACebS))] -> [(String, (FPrec, [VarId], ACebS))]
initErrVars sem = map initErrVarsSem sem
  where 
    initErrVarsSem (funName, (fp, vars, acebs)) = (funName, (fp, vars, map initErrAceb acebs)) 



s2f :: FPrec -> Doc
-- prints the conversion function to fp to real
s2f fp = case fp of 
  FPSingle -> text "StoR"
  FPDouble -> text "DtoR"



pvsType :: FPrec -> Doc
pvsType FPSingle = text "unb_single"
pvsType FPDouble = text "unb_double" 




