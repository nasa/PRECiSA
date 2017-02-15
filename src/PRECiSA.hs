-- Copyright 2016 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
--
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS,
-- ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS
-- AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT
-- AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
-- 
-- Waiver and Indemnity: RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS
-- IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT,
-- ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.



{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}


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

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess,exitFailure)
import Control.Monad
import System.FilePath.Windows
import ErrM
import ParPVSLang
import ParSpecLang
import AbsPVSLang
import AbsSpecLang
import AbstractSemantics
import AbstractDomain
import PPExt
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Time

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
    parsedLet <- rawparserLet str
    return parsedLet

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
  --- * First argument is the "program" to be analyzed
  --- * Second argument is the "specification" (bound for the function arguments)
  prgname <- getProgName
  cmdargs <- getArgs

  printVersion prgname

  let (options,args,errs) = processOpts cmdargs

  when (not $ null errs) $ do
    printHelp prgname
    putStrLn $ "Problems parsing arguments: " ++ concat errs ++ "\n"
    exitFailure

  when (optHelp options) $ do
    printHelp prgname
    exitSuccess

  case args of
    [fileprog,filespec,md,prec,dp,sta,maxif] -> do
      parseAndAnalysis fileprog filespec (read md::Int) (read prec::Int) (read dp::Int) (read sta::Bool) (read maxif::Int)
      putStrLn "Analysis done."
      putStrLn "done."
    _ -> putStrLn "wrong number of arguments"

checkProgSize :: [(String, (FPrec, [VarId], CebS))] -> Int -> Int -> IO ()
checkProgSize [] n maxel | n >= maxel = error "The generated file is too big! Try to run the analysis with the stable tests assumption."
                         | otherwise  = return ()
checkProgSize ((_,(_,_,cebs)):xs) n maxel | (length cebs) + n >= maxel = error "The generated file is too big! Try to run the analysis with the stable tests assumption."
                                          | otherwise = checkProgSize xs ((length cebs) + n) maxel

parseAndAnalysis :: FilePath -- program to analyze
                 -> FilePath -- specification with initial ranges
                 -> Int      -- max depth
                 -> Int      -- precision
                 -> Int      -- display precision
                 -> Bool     -- stable test assumption?
                 -> Int      -- max number of lemmas
                 -> IO ()
-- generates two files
-- the first one contains lemmas and proof stating the overapprox of the round-off error of the program
-- the second one contains the concrete bounds generator for the round-off error symbolic expressions
parseAndAnalysis fileprog filespec maxDepth prec displayPrec sta maxel = do
  errparseProg <- parseFileToProgram fileprog
  Prog decls <- errify error errparseProg
  errparseSpec <- parseFileToSpec filespec
  Spec spec <- errify fail errparseSpec
  -------------- 
  time <- getCurrentTime
  -------------
  genCertFile  inputFileName certFile  certFileName  decls time sta maxel
  genClgenFile inputFileName clgenFile clgenFileName decls spec maxDepth prec displayPrec time sta
  --------------
  putStrLn "***** Files generated successfully *****" 
  putStrLn ("Lemmas and proofs in: " ++ certFile)
  putStrLn ("Concrete bounds in: " ++ clgenFile)
  -------------
  return ()
  where
    inputFileName = takeBaseName fileprog
    certFile = "./results/" ++ certFileName ++ ".pvs"
    clgenFile = "./results/" ++ clgenFileName ++ ".pvs"
    certFileName = "cert_" ++ inputFileName
    clgenFileName = "clgen_" ++ inputFileName

genCertFile :: Show a => [Char] -> FilePath -> [Char] -> [Decl] -> a -> Bool -> Int -> IO ()
-- generates a file with PVS lemmas and proof for the computed symbolic error expression
genCertFile inputFileName certFile certFileName decls date sta maxel = do
  checkProgSize sem 0 maxel
  writeFile  certFile ("% This file is automatically generated by PRECiSA \n")
  appendFile certFile ("% " ++ (show date) ++"\n\n")
  appendFile certFile (certFileName ++ ": THEORY\n")
  appendFile certFile ("BEGIN\n")
  appendFile certFile ("IMPORTING PRECiSA@strategies, " ++ inputFileName ++ "\n\n")
  appendFile certFile ("%|- *_TCC* : PROOF\n")
  appendFile certFile ("%|- (try (then (grind-reals) (fail)) (skip) (subtype-tcc))\n")
  appendFile certFile ("%|- QED\n\n")
  printCerts certFile sem decls
  appendFile certFile ("\n")
  appendFile certFile ("END " ++ certFileName)
  return ()
    where
      sem = declsSemK decls botInterp 1 sta

genClgenFile :: Show a => [Char] -> FilePath -> [Char] -> [Decl] -> [SpecBind] -> Int -> Int -> Int -> a -> Bool-> IO ()
-- generates the file for generating concrete bounds given initial ranges for the variables
genClgenFile inputFileName clgenFile clgenFileName decls spec maxDepth prec displayPrec date sta = do
  writeFile  clgenFile ("% This file is automatically generated by PRECiSA \n")
  appendFile clgenFile ("% " ++ (show date) ++"\n\n")
  appendFile clgenFile ("% maxDepth: " ++ (show maxDepth) ++ " , prec: " ++ (show prec) ++ " , displayPrec: " ++ (show displayPrec) ++ "\n\n")
  appendFile clgenFile (clgenFileName ++ ": THEORY\n")
  appendFile clgenFile ("BEGIN\n\n")
  appendFile clgenFile ("IMPORTING PRECiSA@bbiasp, PRECiSA@bbiadp, PRECiSA@precIOsa,  PRECiSA@strategies  \n\n")
  appendFile clgenFile (render $ (hsep $ punctuate comma $ map prettyDoc (varsList decls)) <> text ": real")
  appendFile clgenFile ("\n\n")
  printEFuns clgenFile sem spec
  appendFile clgenFile ("gen(sout: OStream, ranges:list[[string,ProperInterval]], maxdepth:nat, prec:posreal, displayprec:nat): void =\n")
  appendFile clgenFile ("LET\n")
  appendFile clgenFile ("max  :real = 0,\n")
  
  appendFile clgenFile ("dummy:void = fprintf (sout, \"% This file is automatically generated by PRECiSA ~%\"),\n")
  appendFile clgenFile ("dummy:void = fprintf (sout, \"" ++ "clemmas_" ++ inputFileName ++ ": THEORY ~%\"),\n")
  appendFile clgenFile ("dummy:void = fprintf (sout, \"BEGIN ~%\"),\n")
  appendFile clgenFile ("dummy:void = fprintf (sout, \"IMPORTING cert_" ++ inputFileName ++ ", PRECiSA@bbiasp, PRECiSA@bbiadp ~%\"),\n")
  appendFile clgenFile ("dummy:void = printf (\"Concrete bounds: ~%\"),")
  printGenFun clgenFile sem
  appendFile clgenFile ("\ndummy:void = fprintf (sout, \"END " ++ "clemmas_" ++ inputFileName ++ " ~%\")IN\n")
  appendFile clgenFile ("printf(\"Overall concrete bound: ~-20/pvs:d/~%\",max)")
  appendFile clgenFile ("\nmain : void = \n")
  appendFile clgenFile ("LET sout = fopenout(create, \"" ++ "clemmas_" ++ inputFileName ++ ".pvs\")")
  printInputIntervals clgenFile spec
  appendFile clgenFile (" IN \n")
  appendFile clgenFile ("gen (sout, ")
  printMainFun clgenFile spec maxDepth prec displayPrec
  appendFile clgenFile (") & fclose(sout) & printf(\"File succesfully generated.\")\n\n")
  appendFile clgenFile ("END " ++ clgenFileName)
  return ()
    where
      sem = declsSemK decls botInterp 1 sta


--{- OUTPUT FILE GENERATION -}

varsList :: [Decl] -> [VarId]
-- list of variables in a program declaration
varsList decls = map realVar $ Set.toList $ Set.fromList (foldl aux [] decls)
  where
    aux varList (Decl _ _ vars _) = varList++vars
    realVar (VarId x) = VarId ("r_"++x)

bindList :: [SpecBind] -> [VarBind]
-- list of variable ranges in a program specification
bindList binds = Set.toList . Set.fromList $ aux binds
  where
    aux [] = []
    aux ((SpecBind _ _ b):bs) = b ++ (aux bs)

printInputIntervals :: FilePath -> [SpecBind] -> IO()
-- print the main function call of the generator with the actual parameters
printInputIntervals file [] = do
  return ()
printInputIntervals file spec@(_:_) = do
  appendFile file (",\n")
  appendFile file (render $ printListInterval 1 $ bindList spec)
  return ()

printListInterval :: Int -> [VarBind] -> Doc
printListInterval _ [] = emptyDoc
printListInterval i [(VarBind _ lb ub)] =  text "i" <> int i <> text ":ProperInterval =" <+> text "[|" <> prettyDoc lb <> comma <> prettyDoc ub <> text "|]"
printListInterval i ((VarBind _ lb ub):vs) =  text "i" <> int i <> text ":ProperInterval ="
                                              <+> text "[|" <> prettyDoc lb <> comma <> prettyDoc ub <> text "|]" <> comma $$ printListInterval (i+1) vs

printMainFun :: FilePath -> [SpecBind] -> Int -> Int -> Int -> IO()
-- print the main function call of the generator with the actual parameters
printMainFun file spec md prec dprec = do
  appendFile file "(: " 
  appendFile file (render $ printVarRanges 1 $ bindList spec)
  appendFile file " :)"
  appendFile file (render $ comma <+> int md <> comma <+> int prec <> comma <+> int dprec)
  return ()

printVarRange :: VarBind -> Doc
-- print the ranges of the variables as an hypothesis of a lemma
printVarRange (VarBind x lb ub) =
  text "r_" <> text x <+> text "##" <+> text "[|" <> prettyDoc lb <> comma <> prettyDoc ub <> text "|]"

printVarRanges :: Int -> [VarBind] -> Doc
-- print the ranges of the variables as an input to the main function
printVarRanges _ [] = emptyDoc
printVarRanges i [(VarBind x lb ub)] = text "(\"r_" <> text x <> text "\"" <> comma <> text "i" <> int i <> text ")"
printVarRanges i ((VarBind x lb ub):binds) = text "(\"r_" <> text x <> text "\"" 
                                             <> comma <> text "i" <> int i <>text ")"
                                             <> comma <+> printVarRanges (i+1) binds

printEFuns :: FilePath -> [(String, (FPrec, [VarId], CebS))] -> [SpecBind] -> IO ()
-- prints the error expressions and lemmas in the concrete bounds generator for each function
printEFuns _ [] _ = return ()
printEFuns file ((fun, (fp,args,cebs)):interp) spec = do
  appendFile file (render $ printEFun fun args fp cebs 0 spec)
  printEFuns file interp spec
  return ()

printEFun :: String -> [VarId] -> FPrec -> CebS -> Int -> [SpecBind] -> Doc
-- prints the error expressions and lemmas in the concrete bounds generator for each conditional error bound
printEFun fun _ _ [] n _ = emptyDoc
printEFun fun args fp (ceb@(condR, condFP, expr, r, e, pvsTree):cebs) n spec
  = text fun <> text "_" <> int n <> text "_aeexpr : string = \""
    <> prEExpr (zeroErrEE $ exprErr ceb) fp
    <> text "\"\n"
    $$ text fun <> text "_" <> int n <> text "_lemma_str : string = \""
    <> prPvsLemmaSpec (render $ text fun) args expr r e condR condFP fp n ranges (text " ~a~%\"")
    $$ printEFun fun args fp cebs (n+1) spec
  where
    ranges = findRangesInSpec fun spec
    findRangesInSpec fun [] = error ("findRangesInSpec: something went wrong, function "++ fun ++ " not found.")
    findRangesInSpec fun ((SpecBind fun' _ r):rs) | (fun == fun') = r
                                                  | otherwise = findRangesInSpec fun rs


printGenFun :: FilePath -> [(String, (FPrec, [VarId], CebS))] -> IO ()
-- prints the body of the generation function 
printGenFun _ [] = return ()
printGenFun file [(fun, (_,_,cebs))] = do
  appendFile file (render $ printPrLemma fun cebs 0)
  return ()
printGenFun file ((fun, (_,_,cebs)):interp) = do
  appendFile file (render $ printPrLemma fun cebs 0)
  appendFile file ("\n")
  printGenFun file interp
  return ()

printPrLemma :: String -> CebS -> Int -> Doc
-- prints the function print_lemma for each conditional error bound
printPrLemma fun [] n = emptyDoc 
printPrLemma fun (_:cebs) n =
  text "new  :real = fprint_concrete_lemma (sout)("
  <> text fun <> text "_" <> int n <> text "_lemma_str" <> comma
  <+> text fun <> text "_" <> int n <> text "_aeexpr" <> comma
  <+> text "ranges, maxdepth, prec)"
  <+> text ","
  $$  text "max  :real = IF(new>max) THEN new ELSE max ENDIF,"
  $$  text "dummy:void = fprint_proof (sout)("
  <>  text "\"" <> text fun <> text "_"  <> int n <> text "\"" <> comma
  <+> text "\"" <> text fun <> text "_c_" <> int n <> text "\"" <> text ")"
  <+> text ","
  $$  text "dummy:void = printf(\" - lemma ~a: ~-20/pvs:d/~%\",(\"" <> text fun <> text "_"  <> int n <> text "\",new)),"
--  $$ printProofGen fun n
  $$ printPrLemma fun cebs (n+1)

--printProofGen :: String -> Int -> Doc
--printProofGen fun n =
--    text "printf (\"%|- " <> text fun <> text "_c" <> int n <> text ": PROOF ~%\") &"
--    $$ text "printf (\"%|- (then (skeep) (use \\\"" <> text fun <> text "_" <> int n <> text "\\\")) ~%\") &"
--    $$ text "printf (\"%|- (spread (split -1)~%\") &"
--    $$ text "printf (\"%|- ((spread (invoke (case \\\"%1 <= %2\\\") (! -1 2) (! 1 2)) ~%\") &"
--    $$ text "printf (\"%|- ((then (invoke (name-raplace \\\"expr\\\" \\\"%1\\\") (! -1 1)) (assert)) ~%\") &"
--    $$ text "printf (\"%|- (interval))) ~%\") &" 
--    $$ text "printf (\"%|- (propax) (propax) (propax)))) ~%\") &" 
--    $$ text "printf (\"%|-  QED ~%\") &"


printCerts :: FilePath -> [(String, (FPrec, [VarId], CebS))] -> [Decl] -> IO ()
-- produces the certificate file
printCerts _ [] _ = return ()
printCerts fileConjs ((f,(fp, args, cset)):interp) decls = do
  printLemmasAndProofs fileConjs f args stm cset fp 0
  printCerts fileConjs interp decls
  return ()
  where
    stm = findInDecls f decls 
    findInDecls f [] = error ("findInDecls: function " ++ f ++ " not found")
    findInDecls f ((Decl _ (NonVarId g) _ stm):ds) | f==g = stm
                                                   | otherwise = findInDecls f ds 

printLemmasAndProofs :: FilePath -> String -> [VarId] -> Stm -> CebS -> FPrec -> Int -> IO ()
-- prints the lemmas and proofs for each conditional error bound
printLemmasAndProofs _ _ _ _ [] _ _ = return ()
printLemmasAndProofs fileConjs f args stm ((condR, condFP, expr, r, e, pvsTree):cset) fp n = do
  appendFile fileConjs (render $ prPvsLemma f args expr r e condR condFP fp n (prEExpr e fp))
  appendFile fileConjs (render $ prPvsProof f pvsTree n)
  printLemmasAndProofs fileConjs f args stm cset fp (n+1)
  return ()

prPvsProof :: String -> PVStree -> Int -> Doc    
-- print the proof corresponding to a conditional error bound
prPvsProof f pvsTree n =
    text "%|- " <> text f <> text "_" <> int n <> text ": PROOF"
    $$ text "%|- (then"
    $$ text "%|- (skeep)"
    $$ text "%|- (expand \"" <> text f <> text "\" :assert? none)"
    $$ text "%|- (try-simp-ite)"
    $$ prettyDoc pvsTree
    $$ text "%|- )"
    $$ text "%|- QED"
    $$ text "\n"

prPvsLemma :: String -> [VarId] -> FAExpr -> AExpr -> EExpr -> BExpr -> FBExpr -> FPrec -> Int -> Doc -> Doc
-- prints the lemma corresponding to a conditional error bound
prPvsLemma f args expr r e condR condFP fp n errDoc =
  text f <> text "_" <> int n <+> text ": LEMMA"
  $$ text "FORALL(" <>  (hsep $ punctuate comma $ map prErrorInt args)
                    <>  text ": nonneg_real" <> comma
                    <+> (hsep $ punctuate comma $ map prRealInt args)
                    <>  text ": real" <> comma
                    <+> (hsep $ punctuate comma $ map prettyDoc args) <> text ": " <>  pvsType fp <> text ")" <> text ":"
  $$ prPvsArgs args
  $$ text "AND" <+>  prBExpr condR fp
  $$ text "AND" <+>  prFCond condFP fp
  $$ text "IMPLIES"
  $$ text "abs(" <> s2f fp <> text "(" <> text f <> text "(" <>
    (hsep $ punctuate comma $ map prettyDoc args) <> text ")" <> text ")" <+> text "-" <+> prAExpr r fp <> text ")" <> text "<=" <> errDoc
  $$ text "\n"
  where
    prPvsArgs args = hsep $ punctuate (text " AND") $ map prPVSVarId args
    
    prRealInt (VarId x) = text "r_" <> text x
    prErrorInt (VarId x) = text "e_" <> text x
    
    prPVSVarId (VarId x) = text "abs(" <> s2f fp <> text "(" <> text x <> text ")" <+> text "-" <+> text "r_" <> text x <> text ")"
                           <> text "<=" <> text "e_" <> text x

prPvsLemmaSpec :: String -> [VarId] -> FAExpr -> AExpr -> EExpr -> BExpr -> FBExpr -> FPrec -> Int -> [VarBind] -> Doc -> Doc
-- prints the lemma corresponding to a conditional error bound
prPvsLemmaSpec f args expr r e condR condFP fp n ranges errDoc =
  text f <> text "_c_" <> int n <+> text ": LEMMA"
  <> text   "~%" <> text "FORALL(" <> (hsep $ punctuate comma $ map prRealInt args)
                    <>  text ": real" <> comma
                    <+> (hsep $ punctuate comma $ map prettyDoc args) <> text ": " <>  pvsType fp <> text ")" <> text ":"
  <> text   "~%" <> prPvsArgs args
  <> text   "~%" <> text "AND" <+> prBExpr (zeroErrBE condR) fp
  <> text   "~%" <> text "AND" <+> prFCond (zeroErrFBE condFP) fp
  <> text   "~%" <> text "AND" <+> (hsep $ punctuate (text " AND ") $ map printVarRange ranges)
  <> text   "~%" <> text "IMPLIES"
  <> text   "~%" <> text "abs(" <> s2f fp <> text "(" <> text f <> text "(" <>
    (hsep $ punctuate comma $ map prettyDoc args) <> text ")" <> text ")" <+> text "-" <+> prAExpr r fp <> text ")" <> text "<=" <> errDoc
  $$ text "\n"
  where
    prPvsArgs args = hsep $ punctuate (text " AND") $ map prPVSVarId args
    
    prRealInt (VarId x) = text "r_" <> text x
    prErrorInt (VarId x) = text "e_" <> text x
    
    prPVSVarId (VarId x) = text "abs(" <> s2f fp <> text "(" <> text x <> text ")" <+> text "-" <+> text "r_" <> text x <> text ")"
                           <> text "<=" <> text "0"

s2f :: FPrec -> Doc
-- prints the conversion function to fp to real
s2f fp = case fp of 
  FPSingle -> text "StoR"
  FPDouble -> text "DtoR"

















