module FramaC.PrettyPrint where

import AbsPVSLang
import AbsSpecLang
import AbstractDomain (Condition)
import Control.Monad.State
import FPrec
import Data.List (init,last)
import qualified Common.ShowRational as Rat
import Translation.Float2Real
import Debug.Trace
import Data.Set (fromList, toList)
import Data.Maybe (fromJust)
import Prelude hiding ((<>))
--import Data.Set (toList, fromList)
import Transformation (listErrEnvVars)
import Utils
import Kodiak.Runnable
import Kodiak.Runner
import NumericalError
import Data.Numbers.FloatingHex (showHFloat)
import PPExt

genFramaCFile :: FPrec -> RProgram -> [(Decl,[(VarName,FAExpr,FBExpr)])] -> Spec -> [(FunName, (EExpr, [Condition]))] -> IO Doc
genFramaCFile fp realDecls tauDecls (Spec specBinds) errs = do
  program <- printProgram fp (mapM aux realDecls)
  return $(
    printHeader
    $$
    text (vspace 1)
    $$
    program
    $$
    text (vspace 1)
    $$
    printMain)
  where
    aux realDecl@(RDecl _ f _ _ ) = do
      numROErr <- numROError f
      return (realDecl, tauDecl f, errVars f, initValues f, symbROError f, stableConds f,numROErr)
    findInSymbDecl fun [] = error $ "findInProg: function "++ show fun ++ " not found."
    findInSymbDecl fun (declPair@(Decl _ g _ _,_):ds) | fun==g = declPair
                                                        | otherwise = findInSymbDecl fun ds
    tauDeclPair f = findInSymbDecl f tauDecls
    tauDecl f = fst $ tauDeclPair f
    errVars f = snd $ tauDeclPair f
    initValues  f = findInSpec f specBinds
    symbROError f = fst $ fromJust $ lookup f errs
    stableConds f = snd $ fromJust $ lookup f errs
    numROError  f = maximumUpperBound <$> run kodiakInput ()
      where kodiakInput = KI { name = "error",
                                 expression = initAExpr $ symbROError f,
                                 bindings = initValues  f,
                                 maxDepth = 7,
                                 precision = 14
                                }


printProgram :: FPrec -> IO [(RDecl, Decl,[(VarName,FAExpr,FBExpr)], [VarBind], EExpr, [Condition], Double)] -> IO Doc
printProgram fpTarget tuples = do
  tuples' <- tuples
  declsDoc <- mapM (printDeclWithACSL fpTarget) tuples'
  return $ vcat declsDoc

printDeclWithACSL :: FPrec -> (RDecl,Decl,[(VarName,FAExpr,FBExpr)],[VarBind], EExpr,[Condition],Double) -> IO Doc
printDeclWithACSL fp (realDecl@(RDecl _ f realArgs _ ), taudecl@(Decl _ _ _ stm), errVars, varBinds, symbROError, stableConds,numROErr) = do
  numDecl <- printNumDeclWithACSL fp f realArgs (forIndexes stm) taudecl errVars varBinds numROErr
  return
    (printSymbDeclWithACSL fp (realDecl,(taudecl, errVars),symbROError,stableConds)
        $$
        text (vspace 1)
        $$
        numDecl)

prettyErrorHex :: Double -> Doc
prettyErrorHex roErr = text (showHFloat (roErr :: Double) "")

printNumDeclWithACSL :: FPrec -> String -> [Arg] -> [(VarName, FAExpr, FAExpr)] -> Decl -> [(VarName,FAExpr,FBExpr)] -> [VarBind] -> Double -> IO Doc
printNumDeclWithACSL fp f realArgs forIdx (Decl fpFun _ args _) errVars varBinds roErr = do
  numErrArgs <- computeNumErrArgs
  return
    (text "/*@"
    $$ text "ensures" <+> (text "(" <> printQuantIdx forIdx) <+> printVarBinds varBinds <> text ")" <+> text "&&"
    $$ text "\\result.isValid"
    $$ text "==> \\abs(\\result.value - " <> text f <> parens (printRealArgs fp realArgs) <> text ") <=" <+> prettyErrorHex roErr <> text ";"
    $$ text "*/"
    $$ maybeRetType fpFun <+> text fNum <+> parens (printRealArgs fp realArgs) <+> text "{"
    $$ text "return"
    <+> text f <> text "_" <> prettyFPrec fpFun
    <+> parens (printRealArgs fp realArgs <> printErrArgs numErrArgs)
    <> text ";"
    $$ text "}")
  where
    printErrArgs numErrArgs = if null numErrArgs then emptyDoc else comma <> docListComma (map prettyErrorHex numErrArgs)
    fNum = f ++ "_num"
    errArgs = drop (length realArgs) args
    computeNumErrArgs = mapM (roError varBinds [] . fst . aux) errArgs
    aux (Arg var _) = lookup3 var errVars

maybeRetType :: FPrec -> Doc
maybeRetType TInt     = text "struct maybeInt"
maybeRetType FPSingle = text "struct maybeFloat"
maybeRetType FPDouble = text "struct maybeDouble"
maybeRetType fp  = error ("maybeRetType: return type " ++ show fp ++ " not supported.")


genFramaCSymbFile :: FPrec -> RProgram -> [(Decl,[(VarName,FAExpr,FBExpr)])] -> [(FunName, (EExpr, [Condition]))] -> Doc
genFramaCSymbFile fp realDecls tauDecls errs =
  printHeader
  $$
  text (vspace 1)
  $$
  printSymbProgram fp (map aux realDecls)
  $$
  text (vspace 1)
  $$
  printMain
    where
      aux realDecl@(RDecl _ f _ _ ) = (realDecl, findInSymbDecl f tauDecls, symbROError f, stableConds f)
      findInSymbDecl fun [] = error $ "findInProg: function "++ show fun ++ " not found."
      findInSymbDecl fun (tauDecl@(Decl _ g _ _,_):ds) | fun==g = tauDecl
                                                         | otherwise = findInSymbDecl fun ds
      symbROError f = fst $ fromJust $ lookup f errs
      stableConds f = snd $ fromJust $ lookup f errs

printSymbProgram :: FPrec -> [(RDecl, (Decl, [(VarName, FAExpr, FBExpr)]), EExpr, [Condition])] -> Doc
printSymbProgram fp tuples = vcat $ map (printSymbDeclWithACSL fp) tuples

printSymbDeclWithACSL :: FPrec -> (RDecl, (Decl, [(VarName, FAExpr, FBExpr)]), EExpr, [Condition]) -> Doc
printSymbDeclWithACSL fp (realDecl@(RDecl _ f realArgs _ ), (decl@(Decl _ _ tauargs taustm),errVars), symbROError, listStableCond)  =
  vcat (map (printACSLlogicDecl fp) realDeclForList)
  $$
  text (vspace 1)
  $$
  printACSLlogicDecl fp realDeclMain
  $$
  text (vspace 1)
  $$
  printAxiomaticTranDecl fp f taustm tauargs realArgs listStableCond
  $$
  text (vspace 1)
  $$
  printFPSymbPrecond fp f realArgs tauargs errVars (localVars taustm) (forIndexes taustm) symbROError
  $$
  printFPDecl decl
    where
      (realDeclMain,realDeclForList) =  makeDeclRecursive realDecl

printAxiomaticTranDecl :: FPrec -> String -> Stm -> [Arg] -> [Arg] -> [Condition] -> Doc
printAxiomaticTranDecl fp f stm tauargs realArgs listStableCond =
     text "/*@ axiomatic " <> text (f ++ "_trans") <+> text "{"
  $$ printPredTransValue fp f stm tauargs
  <> text (vspace 1)
  $$ printPredStablePaths fp f realArgs listStableCond
  $$ text "}"
  $$ text "*/"

printPredTransValue :: FPrec -> String -> Stm -> [Arg] -> Doc
printPredTransValue fp f stm tauargs =
     text "predicate" <+> text (f ++ "_trans_value")
     <> parens (prettyACSLFormalArgs fp tauargs
     <> comma <+> text "double result")
  $$ text "="
  <+> (if null funCalls then stmDoc else resValQuant $$ stmDoc) <+> text ";"
    where
      resValQuant = text "\\exists" <+> listVarRes <> text ";" <+> listFuns <+> text "&&"
      listVarRes = docListComma (map ((\x -> prettyACSLPrec fp <+> x) . text . fst) funCalls)
      listFuns   = docListAnd (map (printACSLFAexpr . snd) funCalls)
      (stmDoc,funCalls) = runState (prettyStmValue stm) []

printPredStablePaths :: FPrec -> String -> [Arg] -> [Condition] -> Doc
printPredStablePaths fp f realArgs listStableCond =
      text "predicate"
  <+> text (f ++ "_stable_paths")
  <> parens (prettyACSLFormalArgs fp (realArgs++fpArgs))
  $$  text "="
  <+> docListOr (map printACSLCond listStableCond)
  <+> text ";"
  where
    fpArgs = map (argCast fp) realArgs


prettyStmValue :: Stm -> State [(VarName, FAExpr)] Doc
prettyStmValue (Let x _ ae stm) = do
    stmDoc <- prettyStmValue stm
    aeDoc  <- prettyFAExprValue ae
    return  $ text "\\let"
          <+> text x
          <+> text "="
          <+> aeDoc
          <+> text ";"
          $$  parens stmDoc

prettyStmValue (Ite (IsValid _) stmThen UnstWarning) = do
  stmDoc <- prettyStmValue stmThen
  return (parens stmDoc)

prettyStmValue (Ite be stmThen UnstWarning) = do
  stmDoc <- prettyStmValue stmThen
  beDoc  <- prettyFBExprValue be
  return $  beDoc
        <+> text "&&"
        <+> parens stmDoc

prettyStmValue (Ite be stmThen stmElse) = do
  beDoc  <- prettyFBExprValue be
  stmThenDoc <- prettyStmValue stmThen
  stmElseDoc <- prettyStmValue stmElse
  return $  beDoc
        $$  text "?"
        <+> parens stmThenDoc
        $$  text ":"
        <+> parens stmElseDoc

prettyStmValue (ListIte [] _) = error "prettyDoc RListIte: empty stmThen list"

prettyStmValue (ListIte ((beThen,stmThen):thenList) UnstWarning) = do
  beThenDoc <- prettyFBExprValue beThen
  stmThenDoc <- prettyStmValue stmThen
  stmThenListDoc <- mapM (prettyStmValue . snd) (init thenList)
  stmElseDoc <- prettyStmValue $ (snd . last) thenList
  beListDoc <-  mapM (prettyFBExprValue . fst) (init thenList)
  return $  parens beThenDoc
        $$  text "?"
        <+> stmThenDoc <+> text ":"
        $$  vcat (map (\(stmDoc,beDoc) -> beDoc
                                        $$  text "?"
                                        <+> stmDoc <+> text ":") (zip stmThenListDoc beListDoc))
        $$ stmElseDoc

prettyStmValue (ListIte ((beThen,stmThen):thenList) stmElse) = do
  beThenDoc <- prettyFBExprValue beThen
  stmThenDoc <- prettyStmValue stmThen
  stmElseDoc <- prettyStmValue stmElse
  stmThenListDoc <- mapM (prettyStmValue . snd) thenList
  beListDoc <-  mapM (prettyFBExprValue . fst) thenList
  return $  parens beThenDoc
        $$  text "?"
        <+> stmThenDoc <+> text ":"
        $$  vcat (map (\(stmDoc,beDoc) -> beDoc
                                        $$  text "?"
                                        <+> stmDoc <+> text ":") (zip stmThenListDoc beListDoc))
        $$  stmElseDoc

prettyStmValue (StmExpr ae) = do
  aeDoc <- prettyFAExprValue ae
  return $ text "result == " <+> aeDoc

prettyStmValue ForLoop{} = error "prettyACSLstm: Something went wrong, for loops not allowed in logic assertions."
prettyStmValue UnstWarning = error "prettyStmValue: value WARNING not expected"

prettyUnaryOpValue :: Doc -> FAExpr -> State [(VarName, FAExpr)] Doc
prettyUnaryOpValue op a = do
  aDoc <- prettyFAExprValue a
  return $ op <> parens aDoc

prettyBinaryOpValue :: Doc -> FAExpr -> FAExpr -> State [(VarName, FAExpr)] Doc
prettyBinaryOpValue op a1 a2 = do
  a1Doc <- prettyFAExprValue a1
  a2Doc <- prettyFAExprValue a2
  return $ op <> parens (a1Doc <> comma <+> a2Doc)

prettyBinFormulaValue :: Doc -> FAExpr -> FAExpr -> State [(VarName, FAExpr)] Doc
prettyBinFormulaValue symb a1 a2 = do
  a1Doc <- prettyFAExprValue a1
  a2Doc <- prettyFAExprValue a2
  return $ parens $ a1Doc <+> symb <+> a2Doc

prettyFBExprValue :: FBExpr -> State [(VarName, FAExpr)] Doc
prettyFBExprValue  (FOr e1 e2) = do
  b1Doc <- prettyFBExprValue e1
  b2Doc <- prettyFBExprValue e2
  return $ parens $ b1Doc <+> text "||"  <+> b2Doc
prettyFBExprValue (FAnd e1 e2) = do
  b1Doc <- prettyFBExprValue e1
  b2Doc <- prettyFBExprValue e2
  return $ parens $ b1Doc <+> text "&&"  <+> b2Doc
prettyFBExprValue     (FNot e) = do
  bDoc <- prettyFBExprValue e
  return $ text "!" <> bDoc
prettyFBExprValue  (FEq a1 a2) = prettyBinFormulaValue (text "=" ) a1 a2
prettyFBExprValue (FNeq a1 a2) = prettyBinFormulaValue (text "!=") a1 a2
prettyFBExprValue  (FLt a1 a2) = prettyBinFormulaValue (text "<" ) a1 a2
prettyFBExprValue (FLtE a1 a2) = prettyBinFormulaValue (text "<=") a1 a2
prettyFBExprValue  (FGt a1 a2) = prettyBinFormulaValue (text "<" ) a2 a1
prettyFBExprValue (FGtE a1 a2) = prettyBinFormulaValue (text "<=") a2 a1
prettyFBExprValue       FBTrue = return $ text "\\true"
prettyFBExprValue      FBFalse = return $ text "\\false"
prettyFBExprValue (IsValid ae) = return $ text "isValid" <> parens (prettyDoc ae)


prettyFAExprValue :: FAExpr -> State [(VarName, FAExpr)] Doc
prettyFAExprValue (FVar FPSingle x)           = return $ text $ "single_" ++ x
prettyFAExprValue (FVar FPDouble x)           = return $ text $ "double_" ++ x
prettyFAExprValue (RtoS (Int i))       = return $ integer i <> text ".0"
prettyFAExprValue (RtoD (Int i))       = return $ integer i <> text ".0"
prettyFAExprValue (RtoS (Rat d))       = return $ parens $ text $ showRational d
prettyFAExprValue (RtoD (Rat d))       = return $ parens $ text $ showRational d
prettyFAExprValue (FInt i)             = return $ integer i
prettyFAExprValue (FCnst _ d)          = return $ parens $ text $ showRational d
prettyFAExprValue (FNeg _ (FInt i))    = return $ text "-" <> integer i
prettyFAExprValue (FNeg _ (FCnst _ d)) = return $ text "-" <> parens (text $ showRational d)
prettyFAExprValue (FEFun f fp args)    = do
  currentState <- get
  let n = length currentState
  let res = "res" ++ show n
  put ((res, FEFun (f ++ "_trans_value") fp (args++[FVar Real res])):currentState)
  return $ text res

prettyFAExprValue (FINeg           a) = prettyUnaryOpValue (text "Ineg"  ) a
prettyFAExprValue (FIAbs           a) = prettyUnaryOpValue (text "Iabs"  ) a
prettyFAExprValue (FNeg   FPSingle a) = prettyUnaryOpValue (text "Sneg"  ) a
prettyFAExprValue (FFloor FPSingle a) = prettyUnaryOpValue (text "Sfloor") a
prettyFAExprValue (FSqrt  FPSingle a) = prettyUnaryOpValue (text "Ssqrt" ) a
prettyFAExprValue (FAbs   FPSingle a) = prettyUnaryOpValue (text "Sabs"  ) a
prettyFAExprValue (FSin   FPSingle a) = prettyUnaryOpValue (text "Ssin"  ) a
prettyFAExprValue (FCos   FPSingle a) = prettyUnaryOpValue (text "Scos"  ) a
prettyFAExprValue (FTan   FPSingle a) = prettyUnaryOpValue (text "Stan"  ) a
prettyFAExprValue (FAsin  FPSingle a) = prettyUnaryOpValue (text "Sasin" ) a
prettyFAExprValue (FAcos  FPSingle a) = prettyUnaryOpValue (text "Sacos" ) a
prettyFAExprValue (FAtan  FPSingle a) = prettyUnaryOpValue (text "Satan" ) a
prettyFAExprValue (FLn    FPSingle a) = prettyUnaryOpValue (text "Sln"   ) a
prettyFAExprValue (FExpo  FPSingle a) = prettyUnaryOpValue (text "Sexp"  ) a
prettyFAExprValue (FNeg   FPDouble a) = prettyUnaryOpValue (text "Dneg"  ) a
prettyFAExprValue (FFloor FPDouble a) = prettyUnaryOpValue (text "Dfloor") a
prettyFAExprValue (FSqrt  FPDouble a) = prettyUnaryOpValue (text "Dsqrt" ) a
prettyFAExprValue (FAbs   FPDouble a) = prettyUnaryOpValue (text "Dabs"  ) a
prettyFAExprValue (FSin   FPDouble a) = prettyUnaryOpValue (text "Dsin"  ) a
prettyFAExprValue (FCos   FPDouble a) = prettyUnaryOpValue (text "Dcos"  ) a
prettyFAExprValue (FTan   FPDouble a) = prettyUnaryOpValue (text "Dtan"  ) a
prettyFAExprValue (FAsin  FPDouble a) = prettyUnaryOpValue (text "Dasin" ) a
prettyFAExprValue (FAcos  FPDouble a) = prettyUnaryOpValue (text "Dacos" ) a
prettyFAExprValue (FAtan  FPDouble a) = prettyUnaryOpValue (text "Datan" ) a
prettyFAExprValue (FLn    FPDouble a) = prettyUnaryOpValue (text "Dln"   ) a
prettyFAExprValue (FExpo  FPDouble a) = prettyUnaryOpValue (text "Dexp"  ) a

prettyFAExprValue (FIAdd           a1 a2) = prettyBinaryOpValue (text "Iadd" ) a1 a2
prettyFAExprValue (FISub           a1 a2) = prettyBinaryOpValue (text "Isub" ) a1 a2
prettyFAExprValue (FIMul           a1 a2) = prettyBinaryOpValue (text "Imul" ) a1 a2
prettyFAExprValue (FIDiv           a1 a2) = prettyBinaryOpValue (text "Idiv" ) a1 a2
prettyFAExprValue (FItDiv          a1 a2) = prettyBinaryOpValue (text "Itdiv") a1 a2
prettyFAExprValue (FIMod           a1 a2) = prettyBinaryOpValue (text "Imod" ) a1 a2
prettyFAExprValue (FItMod          a1 a2) = prettyBinaryOpValue (text "Itmod") a1 a2
prettyFAExprValue (FIPow           a1 a2) = prettyBinaryOpValue (text "Ipow" ) a1 a2
prettyFAExprValue (FAdd   FPSingle a1 a2) = prettyBinaryOpValue (text "Sadd" ) a1 a2
prettyFAExprValue (FSub   FPSingle a1 a2) = prettyBinaryOpValue (text "Ssub" ) a1 a2
prettyFAExprValue (FMul   FPSingle a1 a2) = prettyBinaryOpValue (text "Smul" ) a1 a2
prettyFAExprValue (FDiv   FPSingle a1 a2) = prettyBinaryOpValue (text "Sdiv" ) a1 a2
prettyFAExprValue (FMod   FPSingle a1 a2) = prettyBinaryOpValue (text "Smod" ) a1 a2
prettyFAExprValue (FAdd   FPDouble a1 a2) = prettyBinaryOpValue (text "Dadd" ) a1 a2
prettyFAExprValue (FSub   FPDouble a1 a2) = prettyBinaryOpValue (text "Dsub" ) a1 a2
prettyFAExprValue (FMul   FPDouble a1 a2) = prettyBinaryOpValue (text "Dmul" ) a1 a2
prettyFAExprValue (FDiv   FPDouble a1 a2) = prettyBinaryOpValue (text "Ddiv" ) a1 a2
prettyFAExprValue (FMod   FPDouble a1 a2) = prettyBinaryOpValue (text "Dmod" ) a1 a2

prettyFAExprValue (FPow   _ a1 a2) = do
  a1Doc <- prettyFAExprValue a1
  a2Doc <- prettyFAExprValue a2
  return $ a1Doc <+> text "^" <+> lparen <> a2Doc <> rparen

prettyFAExprValue (FFma fp a1 a2 a3) = do
  a1Doc <- prettyFAExprValue a1
  a2Doc <- prettyFAExprValue a2
  a3Doc <- prettyFAExprValue a3
  return $ fpDoc <> parens (a1Doc <> comma <+> a2Doc <> comma <+> a3Doc)
  where
    fpDoc = case fp of
      FPSingle -> text "Sfma"
      FPDouble -> text "Dfma"
      _ -> error "fpDoc: unexpected value."

prettyFAExprValue (FMin as) = do
  asDoc <- mapM prettyFAExprValue as
  return $ text "min" <> parens (hsep $ punctuate comma asDoc)

prettyFAExprValue (FMax as) = do
  asDoc <- mapM prettyFAExprValue as
  return $ text "max" <> parens (hsep $ punctuate comma asDoc)

prettyFAExprValue ee = error $ "printACSLFAexpr for " ++ show ee ++ "not implemented yet."




printFPSymbPrecond :: FPrec -> FunName -> [Arg] -> [Arg] -> [(VarName,FAExpr,FBExpr)] -> [(VarName,FAExpr)] -> [(VarName, FAExpr, FAExpr)] -> EExpr -> Doc
printFPSymbPrecond fp f realArgs fpArgs errVars locVars forIdx symbROError =
  text "/*@"
  <> posErrorPrecond
  $$ text "assigns \\nothing;"
  $$ text ""
  $$ behaviorStructure
  $$ text ""
  $$ behaviorStablePaths
  $$ text ""
  $$ behaviorSymbolic
  $$ text "*/"
  where
    posErrorPrecond = if null errVars then (emptyDoc) else
      (emptyDoc $$ text "requires" <+> printPosErrReq (listErrEnvVars errVars) <> text ";")
    realVarArgs = filter (\a -> not (isArgArray a || isArgInt a)) realArgs
    inputVars = map argName realVarArgs
    inputRealVars = map (prettyACSLaexpr . Var fp) inputVars
    inputFPVars   = map (printACSLFAexpr . FVar fp) inputVars
    inputVarErrs  = map (prettyACSLaexpr . (`ErrorMark` fp)) inputVars
    behaviorStructure = text "behavior structure:"
                        $$
                        text "ensures \\result.isValid ==>"
                        <+>
                        text (f ++ "_trans_value") <> parens (docListComma (map (printFPVar fp . argName) fpArgs) <> comma <+> text "\\result.value") <> text ";"
    behaviorStablePaths = text "behavior stable_paths:"
                       $$ text "ensures \\result.isValid"
                       $$ nest 2 (text "==> \\forall" <+> prettyACSLFormalArgs fp realVarArgs <> text ";"
                                              $$ printLocalAndErrVars (text (f ++ "_stable_paths")
                                                     <>  parens (docListComma inputRealVars
                                                     <>  comma
                                                     <+> docListComma inputFPVars)
                                                     <>  text ";"))

    printLocalAndErrVars doc = if null errVars && null locVars
              then doc
              else (printDefLocalVars errVars locVars
               $$ (text "(" <> printQuantIdx forIdx)
               $$ vcat (punctuate (text " &&") (map (printErrVar fp) errVars)) <> text ")"
               $$ nest 2 (text "==>" <>  doc))

    behaviorSymbolic = text "behavior symbolic:"
                    $$ text "ensures \\forall" <+> prettyACSLFormalArgs fp realVarArgs <+> comma <+> docListComma inputVarErrs <+> text ";"
                    $$ docListAnd (map (printErrInputVar fp) inputVars) <+> text "&&"
                    $$ printDefLocalVars errVars locVars
                    $$ (text "(" <> printQuantIdx forIdx)
                    $$ vcat (punctuate (text " &&") (map (printErrVar fp) errVars)) <> text ")" <+> text "&&"
                    $$ text "\\result.isValid"
                    $$ text "==> \\abs(\\result.value -" <+> text f <> parens (printRealArgs fp realArgs) <> text ") <=" <+> prettyACSLaexpr symbROError <+> text ";"

printDefLocalVars :: [(VarName,FAExpr,FBExpr)] -> [(VarName,FAExpr)] -> Doc
printDefLocalVars errVars letVars = vcat $ map printLocalVar vars
  where
    vars = reverse . elimDuplicates $ concatMap (aux . snd3) errVars
    aux var@(FVar _ x) = case lookup x letVars of
      Just ae -> (var,ae):concatMap aux (varList ae)
      Nothing -> []
    aux _ = []
    printLocalVar (FVar fp x,ae) =
      text "\\let" <+> prettyFPrec fp <> text "_" <> text x <+> text "=" <+> prettyDoc ae <> text ";"
      $$
      text "\\let" <+> text x <+> text "=" <+> prettyACSLaexpr (fae2real ae) <> text ";"
    printLocalVar ae = error $ "printLocalVar: case " ++ show ae ++ " niy."

printPosErrReq :: [VarName] -> Doc
printPosErrReq errVars = docListAnd $ map aux errVars
  where
    aux x = text "0 <=" <+> text "double_" <> text x

isArg :: VarName -> [Arg] -> Bool
isArg y (Arg x _:args) = (x == y) || isArg y args
isArg y args = foldl (\ b (Arg x _) -> b || x == y) False args

printQuantIdx :: [(VarName, FAExpr, FAExpr)] -> Doc
printQuantIdx [] = emptyDoc
printQuantIdx indexList =
  text "\\forall integer"
  <+> docListComma (map (text . idxName) indexList) <+> text ";"
  $$  docListAnd  (map aux indexList)
  $$ text "==>"
  where
    idxName (x, _, _) = x
    aux (idx, idxStart, idxEnd) = printFPaexpr idxStart <+> text "<=" <+> text idx <+> text "<=" <+> printFPaexpr idxEnd

printErrInputVar :: FPrec -> VarName -> Doc
printErrInputVar fp x =
  text "\\abs" <> parens (text x
  <+> text "-"
  <+> prettyFPrec fp <> text "_" <> text x)
  <+> text "<="
  <+> prettyACSLaexpr (ErrorMark x fp)

printErrVar :: FPrec -> (VarName,FAExpr,FBExpr) -> Doc
printErrVar fp (ev, ae, FBTrue) =
      text "\\abs" <> parens (printACSLFAexpr ae
  <+> text "-"
  <+> prettyACSLaexpr (fae2real ae))
  <+> text "<="
  <+> prettyFPrec fp <> text "_" <> text ev
printErrVar fp (ev, ae, be) = parens (printFPbexpr be
  $$ text "==>"
  $$ text "\\abs" <> parens (printACSLFAexpr ae
  <+> text "-"
  <+> prettyACSLaexpr (fae2real ae))
  <+> text "<="
  <+> prettyFPrec fp <> text "_" <> text ev)

printIdxBounds :: (VarName, Maybe ArraySize) -> Doc
printIdxBounds (idx, Nothing) = text "0 <=" <+> text idx
printIdxBounds (idx, Just (ArraySizeInt n)) = text "0 <=" <+> text idx <+> text "<" <+> integer n
printIdxBounds (idx, Just (ArraySizeVar x)) = text "0 <=" <+> text idx <+> text "<" <+> text x

idxList :: [FAExpr] -> [VarName]
idxList listArrays = toList $ fromList (map idxArray listArrays)
  where
     idxArray (FArrayElem _ _ _ (FVar _ idx)) = idx
     idxArray fae = error "idxArray: something went wrong, " ++ show fae ++ " is not an array."

idxListWithSize :: [FAExpr] -> [(VarName, Maybe ArraySize)]
idxListWithSize listArrays = toList $ fromList (map idxArray listArrays)
  where
     idxArray (FArrayElem _ size _ (FVar _ idx)) = (idx,size)
     idxArray fae = error $ "idxArray: something went wrong, " ++ show fae ++ " is not an array."

nameArrayIdx :: VarName -> String
nameArrayIdx v = "idx_" ++ v

makeDeclRecursive :: RDecl -> (RDecl,[RDecl])
makeDeclRecursive decl@(RDecl _ f args _) = (newMainDecl, zipWith (makeForFun f args) (reverse forList) [1, 2 ..])
  where
    (newMainDecl, forList) = replaceForWithCallDecl decl

replaceForWithCallDecl :: RDecl -> (RDecl, [RStm])
replaceForWithCallDecl (RDecl fp f args stm) = (RDecl fp f args recStm, forList)
  where
    (recStm, forList) = replaceForWithCallStm f args [] stm

forFunName :: String -> Int -> String
forFunName f n = "for_"++ f ++ show n

replaceForWithCallStm :: FunName -> [Arg] -> [RStm] -> RStm -> (RStm, [RStm])
replaceForWithCallStm f args forList forStm@(RForLoop fp _ idxEnd _ _ _ _) =
   (RStmExpr $ EFun (forFunName f (1 + length forList)) fp (idxEnd : map arg2AExpr args),forStm:forList)
replaceForWithCallStm f args forList (RLet x fp ae stm) = (RLet x fp ae recStm, forListStm)
  where
    (recStm, forListStm) = replaceForWithCallStm f args forList stm
replaceForWithCallStm f args forList (RIte be thenStm elseStm) = (RIte be recStmThen recStmElse, forListStmThen ++ forListStmElse)
  where
    (recStmThen, forListStmThen) = replaceForWithCallStm f args forList thenStm
    (recStmElse, forListStmElse) = replaceForWithCallStm f args forList elseStm
replaceForWithCallStm f args forList (RListIte listThen elseStm) = (RListIte recListThen recStmElse, forListThen ++ forListElse)
  where
    forListThen = concatMap (snd . replaceForWithCallStm f args forList . snd) listThen
    recListThen = zip (map fst listThen) (map (fst . replaceForWithCallStm f args forList . snd) listThen)
    (recStmElse,forListElse) = replaceForWithCallStm f args forList elseStm
replaceForWithCallStm _ _ forList stm@(RStmExpr _) = (stm, forList)
replaceForWithCallStm _ _ forList RUnstWarning      = (RUnstWarning, forList)

makeForFun :: String -> [Arg] -> RStm -> Int -> RDecl
makeForFun f args (RForLoop fp idxStart _ initAcc _ _ forBody) n =
  RDecl fp (forFunName f n) (Arg "I" TInt:args) declBody
  where
    declBody = RIte (Eq (Var TInt "I") idxStart) forBodyThen forBodyElse
    nextCall = EFun (forFunName f n) fp (Sub (Var TInt "I") (Int 1) : map arg2AExpr args)
    forBodyThen = substituteInRStm [("Acc", initAcc),("I", idxStart)] forBody
    forBodyElse = substituteInRStm [("Acc", nextCall)] forBody
makeForFun _ _ _ _ = error "makeForFun: for loop stm expected"

printACSLlogicDecl :: FPrec -> RDecl -> Doc
printACSLlogicDecl fpTarget (RDecl fp f args stm) =
     text "/*@"
  $$ text "axiomatic real_function_" <> text f <+> text "{"
  $$ text "logic"
  <+> prettyACSLPrec fp
  <+> text f
  <+> parens (prettyACSLFormalArgs fpTarget args)
  <+> text "="
  $$ prettyACSLstm stm <> text ";"
  $$ text "}"
  $$ text "*/"

printVarBinds :: [VarBind] -> Doc
printVarBinds varBinds = docListAnd $ map printVarBind varBinds
  where
    printVarBind (VarBind _ _ LInf UInf) = empty
    printVarBind (VarBind x _         LInf   (UBInt    n)) = text x <+> text "<=" <+> integer n
    printVarBind (VarBind x _         LInf   (UBDouble r)) = text x <+> text "<=" <+> text (Rat.showRational Nothing r)
    printVarBind (VarBind x _ (LBInt     n)         UInf)  = integer n  <+> text "<=" <+> text x
    printVarBind (VarBind x _ (LBDouble  r)         UInf)  = text (Rat.showRational Nothing r) <+> text "<=" <+> text x
    printVarBind (VarBind x _ (LBInt    lb) (UBInt    ub)) = integer lb <+> text "<=" <+> text x <+> text "<=" <+> integer ub
    printVarBind (VarBind x _ (LBDouble lb) (UBDouble ub)) = text (Rat.showRational Nothing lb) <+> text x <+> text "<=" <+> text (Rat.showRational Nothing ub)
    printVarBind (VarBind x _ (LBInt    lb) (UBDouble ub)) = integer lb <+> text "<=" <+> text x <+> text "<=" <+> text (Rat.showRational Nothing ub)
    printVarBind (VarBind x _ (LBDouble lb) (UBInt    ub)) = text (Rat.showRational Nothing lb) <+> text "<=" <+> text x <+> text "<=" <+> integer ub

prettyACSLstm :: RStm -> Doc
prettyACSLstm (RLet x _ ae stm) =
      text "\\let"
  <+> text x
  <+> text "="
  <+> prettyACSLaexpr ae
  <+> text ";"
  $$ parens (prettyACSLstm stm)
prettyACSLstm (RIte be stmThen stmElse) =
      prettyACSLbexpr be
  <+> text "?"
  <+> parens (prettyACSLstm stmThen)
  <+> text ":"
  <+> parens (prettyACSLstm stmElse)
prettyACSLstm (RListIte [] _) = error "prettyDoc RListIte: empty stmThen list"
prettyACSLstm (RListIte ((beThen,stmThen):thenList) stmElse) =
    parens (prettyACSLbexpr beThen)
  $$ text "?"
  <+> prettyACSLstm stmThen <+> text ":"
  $$ vcat (map (\(be,stm) -> parens (prettyACSLbexpr be)
  $$ text "?" <+> prettyACSLstm stm <+> text ":") thenList)
  $$ prettyACSLstm stmElse
prettyACSLstm (RStmExpr ae) = prettyACSLaexpr ae
prettyACSLstm RForLoop{} = error "prettyACSLstm: Something went wrong, for loops not allowed in logic assertions."
prettyACSLstm RUnstWarning = error "prettyACSLstm: WARNING not defined in real programs" -- text "return WARNING"

prettyACSLbexpr :: BExpr -> Doc
prettyACSLbexpr (Or  be1 be2) = parens (prettyACSLbexpr be1) <+> text "||" <+> parens (prettyACSLbexpr be2)
prettyACSLbexpr (And be1 be2) = parens (prettyACSLbexpr be1) <+> text "&&" <+> parens (prettyACSLbexpr be2)
prettyACSLbexpr (Not be)      = text "!" <+> parens (prettyACSLbexpr be)
prettyACSLbexpr (Eq  ae1 ae2) = prettyACSLaexpr ae1 <+> text "==" <+> prettyACSLaexpr ae2
prettyACSLbexpr (Neq ae1 ae2) = prettyACSLaexpr ae1 <+> text "!=" <+> prettyACSLaexpr ae2
prettyACSLbexpr (Lt  ae1 ae2) = prettyACSLaexpr ae1 <+> text "<"  <+> prettyACSLaexpr ae2
prettyACSLbexpr (LtE ae1 ae2) = prettyACSLaexpr ae1 <+> text "<=" <+> prettyACSLaexpr ae2
prettyACSLbexpr (Gt  ae1 ae2) = prettyACSLaexpr ae2 <+> text "<"  <+> prettyACSLaexpr ae1
prettyACSLbexpr (GtE ae1 ae2) = prettyACSLaexpr ae2 <+> text "<=" <+> prettyACSLaexpr ae1
prettyACSLbexpr BTrue  = text "\\true"
prettyACSLbexpr BFalse = text "\\false"

prettyACSLaexpr :: AExpr -> Doc
prettyACSLaexpr (Add ae1 ae2) = parens $ prettyACSLaexpr ae1 <+> text "+" <+> prettyACSLaexpr ae2
prettyACSLaexpr (Sub ae1 ae2) = parens $ prettyACSLaexpr ae1 <+> text "-" <+> prettyACSLaexpr ae2
prettyACSLaexpr (Mul ae1 ae2) = parens $ prettyACSLaexpr ae1 <+> text "*" <+> prettyACSLaexpr ae2
prettyACSLaexpr (Div ae1 ae2) = parens $ prettyACSLaexpr ae1 <+> text "/" <+> prettyACSLaexpr ae2
prettyACSLaexpr (Mod ae1 ae2) = parens $ prettyACSLaexpr ae1 <+> text "%" <+> prettyACSLaexpr ae2
prettyACSLaexpr (Pow ae1 ae2) = text "\\pow" <+> parens (prettyACSLaexpr ae1 <+> comma <+> prettyACSLaexpr ae2)
prettyACSLaexpr   (Neg ae) = text "-"       <> parens (prettyACSLaexpr ae)
prettyACSLaexpr (Floor ae) = text "\\floor" <> parens (prettyACSLaexpr ae)
prettyACSLaexpr  (Sqrt ae) = text "\\sqrt"  <> parens (prettyACSLaexpr ae)
prettyACSLaexpr  (Abs  ae) = text "\\abs"   <> parens (prettyACSLaexpr ae)
prettyACSLaexpr  (Sin  ae) = text "\\sin"   <> parens (prettyACSLaexpr ae)
prettyACSLaexpr  (Cos  ae) = text "\\cos"   <> parens (prettyACSLaexpr ae)
prettyACSLaexpr  (Tan  ae) = text "\\tan"   <> parens (prettyACSLaexpr ae)
prettyACSLaexpr  (ASin ae) = text "\\asin"  <> parens (prettyACSLaexpr ae)
prettyACSLaexpr  (ACos ae) = text "\\acos"  <> parens (prettyACSLaexpr ae)
prettyACSLaexpr  (ATan ae) = text "\\atan"  <> parens (prettyACSLaexpr ae)
prettyACSLaexpr  (Ln   ae) = text "\\log"   <> parens (prettyACSLaexpr ae)
prettyACSLaexpr  (Expo ae) = text "\\exp"   <> parens (prettyACSLaexpr ae)
prettyACSLaexpr  (Int n) = integer n
prettyACSLaexpr  (Rat r) = text $ showRational r
--prettyACSLaexpr  (Var TInt x) = text "int_" <> text x
prettyACSLaexpr  (Var _ x) = text x
prettyACSLaexpr  (RealMark x) = text x
prettyACSLaexpr  (EFun f _ args) = text f <> parens (docListComma $ map prettyACSLaexpr args)
prettyACSLaexpr  (ArrayElem (Array _ _) _ v idx) = text v <> text "[" <> prettyACSLaexpr idx <> text "]"
prettyACSLaexpr  (ArrayElem _ _ v idx) = text v <> text "[" <> prettyACSLaexpr idx <> text "]"
prettyACSLaexpr (Min [ae]) = prettyACSLaexpr ae
prettyACSLaexpr (Min [ae1,ae2]) = text "\\min" <> parens (prettyACSLaexpr ae1 <+> comma <+> prettyACSLaexpr ae2)
prettyACSLaexpr (Min (ae1:aes)) = text "\\min" <> parens (prettyACSLaexpr ae1 <+> comma <+> prettyACSLaexpr (Min aes))
prettyACSLaexpr (Max [ae]) = prettyACSLaexpr ae
prettyACSLaexpr (Max [ae1,ae2]) = text "\\max" <> parens (prettyACSLaexpr ae1 <+> comma <+> prettyACSLaexpr ae2)
prettyACSLaexpr (Max (ae1:aes)) = text "\\max" <> parens (prettyACSLaexpr ae1 <+> comma <+> prettyACSLaexpr (Max aes))
-----------------------------------------------------------------------------------------------------------------------
prettyACSLaexpr (MaxErr [ae]) = prettyACSLaexpr ae
prettyACSLaexpr (MaxErr [ae1,ae2]) = text "\\max" <> parens (prettyACSLaexpr ae1 <+> comma <+> prettyACSLaexpr ae2)
prettyACSLaexpr (MaxErr (ae1:aes)) = text "\\max" <> parens (prettyACSLaexpr ae1 <+> comma <+> prettyACSLaexpr (MaxErr aes))
prettyACSLaexpr (ErrAdd   FPSingle ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errAdd_sp"   ae1 ee1 ae2 ee2
prettyACSLaexpr (ErrAdd   FPDouble ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errAdd_dp"   ae1 ee1 ae2 ee2
prettyACSLaexpr (ErrSub   FPSingle ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errSub_sp"   ae1 ee1 ae2 ee2
prettyACSLaexpr (ErrSub   FPDouble ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errSub_dp"   ae1 ee1 ae2 ee2
prettyACSLaexpr (ErrMul   FPSingle ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errMul_sp"   ae1 ee1 ae2 ee2
prettyACSLaexpr (ErrMul   FPDouble ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errMul_dp"   ae1 ee1 ae2 ee2
prettyACSLaexpr (ErrDiv   FPSingle ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errDiv_sp"   ae1 ee1 ae2 ee2
prettyACSLaexpr (ErrDiv   FPDouble ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errDiv_dp"   ae1 ee1 ae2 ee2
prettyACSLaexpr (ErrItDiv FPSingle ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errItDiv_sp" ae1 ee1 ae2 ee2
prettyACSLaexpr (ErrItDiv FPDouble ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errItDiv_dp" ae1 ee1 ae2 ee2
prettyACSLaexpr (ErrMod   FPSingle ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errMod_sp"   ae1 ee1 ae2 ee2
prettyACSLaexpr (ErrMod   FPDouble ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errMod_dp"   ae1 ee1 ae2 ee2
prettyACSLaexpr (ErrItMod FPSingle ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errItMod_sp" ae1 ee1 ae2 ee2
prettyACSLaexpr (ErrItMod FPDouble ae1 ee1 ae2 ee2) = printBinOpErrorACSL "errItMod_dp" ae1 ee1 ae2 ee2
prettyACSLaexpr (ErrFloor  FPSingle ae ee) = printUnaryOpErrorACSL "errFloor_sp"  ae ee
prettyACSLaexpr (ErrFloor  FPDouble ae ee) = printUnaryOpErrorACSL "errFloor_dp"  ae ee
prettyACSLaexpr (ErrFloor0 FPSingle ae ee) = printUnaryOpErrorACSL "errFloor0_sp" ae ee
prettyACSLaexpr (ErrFloor0 FPDouble ae ee) = printUnaryOpErrorACSL "errFloor0_dp" ae ee
prettyACSLaexpr (ErrSqrt   FPSingle ae ee) = printUnaryOpErrorACSL "errSqrt_sp"   ae ee
prettyACSLaexpr (ErrSqrt   FPDouble ae ee) = printUnaryOpErrorACSL "errSqrt_dp"   ae ee
prettyACSLaexpr (ErrSin    FPSingle ae ee) = printUnaryOpErrorACSL "errSin_sp"    ae ee
prettyACSLaexpr (ErrSin    FPDouble ae ee) = printUnaryOpErrorACSL "errSin_dp"    ae ee
prettyACSLaexpr (ErrCos    FPSingle ae ee) = printUnaryOpErrorACSL "errCos_sp"    ae ee
prettyACSLaexpr (ErrCos    FPDouble ae ee) = printUnaryOpErrorACSL "errCos_dp"    ae ee
prettyACSLaexpr (ErrTan    FPSingle ae ee) = printUnaryOpErrorACSL "errTan_sp"    ae ee
prettyACSLaexpr (ErrTan    FPDouble ae ee) = printUnaryOpErrorACSL "errTan_dp"    ae ee
prettyACSLaexpr (ErrAsin   FPSingle ae ee) = printUnaryOpErrorACSL "errAsin_sp"   ae ee
prettyACSLaexpr (ErrAsin   FPDouble ae ee) = printUnaryOpErrorACSL "errAsin_dp"   ae ee
prettyACSLaexpr (ErrAcos   FPSingle ae ee) = printUnaryOpErrorACSL "errAcos_sp"   ae ee
prettyACSLaexpr (ErrAcos   FPDouble ae ee) = printUnaryOpErrorACSL "errAcos_dp"   ae ee
prettyACSLaexpr (ErrAtan   FPSingle ae ee) = printUnaryOpErrorACSL "errAtan_sp"   ae ee
prettyACSLaexpr (ErrAtan   FPDouble ae ee) = printUnaryOpErrorACSL "errAtan_dp"   ae ee
prettyACSLaexpr (ErrAtanT  FPSingle ae ee) = printUnaryOpErrorACSL "errAtanT_sp"  ae ee
prettyACSLaexpr (ErrAtanT  FPDouble ae ee) = printUnaryOpErrorACSL "errAtanT_dp"  ae ee
prettyACSLaexpr (ErrAbs    FPSingle ae ee) = printUnaryOpErrorACSL "errAbs_sp"    ae ee
prettyACSLaexpr (ErrAbs    FPDouble ae ee) = printUnaryOpErrorACSL "errAbs_dp"    ae ee
prettyACSLaexpr (ErrLn     FPSingle ae ee) = printUnaryOpErrorACSL "errLn_sp"     ae ee
prettyACSLaexpr (ErrLn     FPDouble ae ee) = printUnaryOpErrorACSL "errLn_sp"     ae ee
prettyACSLaexpr (ErrExpo   FPSingle ae ee) = printUnaryOpErrorACSL "errExpo_sp"   ae ee
prettyACSLaexpr (ErrExpo   FPDouble ae ee) = printUnaryOpErrorACSL "errExpo_dp"   ae ee
prettyACSLaexpr (ErrStoD            ae ee) = printUnaryOpErrorACSL "errStoD"   ae ee
prettyACSLaexpr (ErrDtoS            ae ee) = printUnaryOpErrorACSL "errDtoS"   ae ee
prettyACSLaexpr (ErrItoS            ae ee) = printUnaryOpErrorACSL "errItoS"   ae ee
prettyACSLaexpr (ErrItoD            ae ee) = printUnaryOpErrorACSL "errItoD"   ae ee
prettyACSLaexpr (ErrMulPow2R FPSingle n ee) = text "ErrMulPow2R_sp" <> (text "(" <> integer n <> comma <+> prettyDoc ee <> text ")")
prettyACSLaexpr (ErrMulPow2R FPDouble n ee) = text "ErrMulPow2R_dp" <> (text "(" <> integer n <> comma <+> prettyDoc ee <> text ")")
prettyACSLaexpr (ErrMulPow2L FPSingle n ee) = text "ErrMulPow2L_sp" <> (text "(" <> integer n <> comma <+> prettyDoc ee <> text ")")
prettyACSLaexpr (ErrMulPow2L FPDouble n ee) = text "ErrMulPow2L_dp" <> (text "(" <> integer n <> comma <+> prettyDoc ee <> text ")")
prettyACSLaexpr (HalfUlp ae FPSingle) = text "ulp_sp" <> (text "(" <> prettyACSLaexpr ae <> text ")/2")
prettyACSLaexpr (HalfUlp ae FPDouble) = text "ulp_dp" <> (text "(" <> prettyACSLaexpr ae <> text ")/2")
prettyACSLaexpr (ErrNeg _ _ ee) = prettyACSLaexpr ee
prettyACSLaexpr (ErrRat rat) = text $ show rat
prettyACSLaexpr (ErrorMark x _) = text "E_" <> text x
prettyACSLaexpr (StoR (FVar _ x)) = text x
prettyACSLaexpr (DtoR (FVar _ x)) = text x
prettyACSLaexpr  ae = error $ "prettyACSLaexpr not defined for " ++ show ae


printBinOpErrorACSL :: String -> AExpr -> AExpr -> AExpr -> AExpr -> Doc
printBinOpErrorACSL nameErrFun r1 e1 r2 e2 =
    text nameErrFun <> (text "(" <>  prettyACSLaexpr r1 <> comma <+> prettyACSLaexpr e1 <> comma
                                 <+> prettyACSLaexpr r2 <> comma <+> prettyACSLaexpr e2 <> text ")")

printUnaryOpErrorACSL :: String -> AExpr -> AExpr -> Doc
printUnaryOpErrorACSL nameErrFun r e = text nameErrFun <> (text "(" <>  prettyACSLaexpr r <> comma
                                                                    <+> prettyACSLaexpr e <> text ")")

prettyACSLFormalArgs :: FPrec -> [Arg] -> Doc
prettyACSLFormalArgs fp args = docListComma $ map (prettyACSLFormalArg fp) args

prettyACSLFormalArg :: FPrec -> Arg -> Doc
prettyACSLFormalArg fp (Arg x (Array _ _)) = prettyACSLPrec fp <+> text "*" <> text x
prettyACSLFormalArg _  (Arg x     Real) = text  "real" <+> text x
prettyACSLFormalArg _  (Arg x     TInt) = text "int" <+> text x
prettyACSLFormalArg _  (Arg x FPSingle) = text "single" <+> text ("single_" ++ x)
prettyACSLFormalArg _  (Arg x FPDouble) = text "double" <+> text ("double_" ++ x)

prettyFPArgs :: [Arg] -> Doc
prettyFPArgs args = docListComma $ map prettyFPArg args

prettyFPArg :: Arg -> Doc
prettyFPArg (Arg x (Array fp _)) = prettyFPrec fp <+> text x <> text "[]"
prettyFPArg (Arg x TInt) = text "int" <+> text x
prettyFPArg (Arg x fp) = prettyACSLPrec fp <+> prettyFPrec fp <> text "_" <> text x

printRealArgs :: FPrec -> [Arg] -> Doc
printRealArgs fp args = docListComma $ map (aux fp) args
  where
    --aux fp (Arg (VarId x) (Array _ _)) = prettyFPrec fp <> text "_" <> text x
    aux _ (Arg x _) = text x

prettyACSLPrec :: FPrec -> Doc
prettyACSLPrec TInt = text "integer"
prettyACSLPrec fp = prettyFPrec fp

prettyFPrec :: FPrec -> Doc
prettyFPrec Real = text "real"
prettyFPrec TInt = text "int"
prettyFPrec FPSingle = text "single"
prettyFPrec FPDouble = text "double"
prettyFPrec fp = error $ "prettyFPrec: case " ++ show fp ++ " niy."

printFPDecl :: Decl -> Doc
printFPDecl (Decl fp f args stm) =
  maybeRetType fp
  <+> text f <> text "_" <> prettyFPrec fp
  <+> parens (prettyFPArgs args)
  <+> text "{"
  $$ nest 2 (retType <+> text "res;")
  $$ nest 2 stmDoc
  $$ text "return res;"
  $$ text "}"
  where
    (stmDoc,_) = runState (printFPstm fp stm Nothing) 0
    retType = maybeRetType fp

printFPVar :: FPrec -> String -> Doc
printFPVar TInt x = text x
printFPVar fp x = prettyFPrec fp <> text "_" <> text x

validityCheckListBinOp :: FBExpr -> FBExpr -> State Int [(FAExpr, Int)]
validityCheckListBinOp be1 be2 = do
  listBe1 <- validityCheckList be1
  listBe2 <- validityCheckList be2
  return $ listBe1 ++ listBe2

validityCheckList :: FBExpr -> State Int [(FAExpr,Int)]
validityCheckList (FOr  be1 be2) = validityCheckListBinOp be1 be2
validityCheckList (FAnd be1 be2) = validityCheckListBinOp be1 be2
validityCheckList (FNot be) = validityCheckList be
validityCheckList (IsValid ae) = do
  currentState <- get
  put (currentState + 1)
  return [(ae,currentState)]
validityCheckList            _ = return []

printStructVars :: FPrec -> [(FAExpr,Int)] -> Doc
printStructVars fp list = vcat (map printStructVar list)
  where
    printStructVar (funCall@FEFun{}, n) = maybeRetType fp <+> text "aux_" <> int n <+> text "=" <+> printFPaexpr funCall <+> text ";"
    printStructVar (Value (StructVar v), n) = maybeRetType fp <+> text "aux_" <> int n <+> text "=" <+> text v <+> text ";"
    printStructVar (ae,_) = error $ "printStructVars: unexpected value " ++ show ae ++ "."

printFPstm :: FPrec -> Stm -> Maybe VarName -> State Int Doc
printFPstm fp (Let x fpLet ae stm) isForBody = do
  stmDoc <- printFPstm fp stm isForBody
  return (prettyFPrec fpLet
             <+> printFPVar fpLet x
             <+> text "="
             <+> printFPaexpr ae
             <+> text ";"
             $$ stmDoc)

printFPstm fp (Ite be stmThen stmElse) isForBody = do
  list <- validityCheckList be
  case list of
    [] -> do
      stmThenDoc <- printFPstm fp stmThen isForBody
      stmElseDoc <- printFPstm fp stmElse isForBody
      return (text "if"
                <+> parens (printFPbexpr be)
                $$  text "{"
                <+> stmThenDoc
                $$  text "} else {"
                <+> stmElseDoc
                $$  text "}")
    callList -> do
      stmThenDoc <- printFPstm fp (replaceCallsInStm callList stmThen) isForBody
      stmElseDoc <- printFPstm fp (replaceCallsInStm callList stmElse) isForBody
      return (printStructVars fp callList
                $$  text "if"
                <+> parens (printFPbexpr (replaceCallsInBExpr callList be))
                $$  text "{"
                <+> stmThenDoc
                $$  text "} else {"
                <+> stmElseDoc
                $$  text "}")

printFPstm _ (ListIte [] _) _ = error "printFPstm: ListIte cannot have an empty list."
printFPstm fp (ListIte ((beThen,stmThen):listThen) stmElse) isForBody = do
  stmThenDoc <- printFPstm fp stmThen isForBody
  stmElseDoc <- printFPstm fp stmElse isForBody
  listThenDoc <- printListThen
  return (text "if"
            <+> parens (printFPbexpr beThen)
            $$ text "{"
            <+> stmThenDoc
            $$ text "}"
            <+> listThenDoc
            <+> text "else {"
            <+> stmElseDoc
            $$  text "}" )
  where
    printListThen = if null listThen
      then return empty
      else vcat <$> mapM (\(be,stm) -> do
        stmDoc <- printFPstm fp stm isForBody
        return (text "else if" <+> parens (printFPbexpr be)
                   $$ text "{" <+> stmDoc <+> text "}")) listThen

printFPstm fp (StmExpr ae) Nothing =
  return (text "res = "
            <+> printSome fp <> parens (printFPaexpr ae)
            <>  text ";")

printFPstm fp (StmExpr ae) (Just acc) =
  return (text acc <+> text "="
              <+> printFPaexpr ae
               <>  text ";"
            $$ text "res =" <+> printSome fp <+> text " (" <> text acc <> text ") ;")

printFPstm fp (ForLoop fpFor idxStart idxEnd initAcc idx acc forBody) _ = do
  forBodyDoc <- printFPstm fp forBody (Just acc)
  return $
    text "int" <+> text idx <> text ";"
    $$
    prettyFPrec fpFor <+> (if fpFor == TInt then empty else prettyFPrec fpFor <> text "_") <+> text acc <+> text "=" <+> printFPaexpr initAcc <> text ";"
    $$
    text "res =" <+> printSome fp <> parens (text acc) <> text ";"
    $$
    printACSLForLoop [idx,acc,"res"] (FISub idxEnd (FVar TInt idx))
    $$
    text "for" <> parens (   text idx <> text "="  <> printFPaexpr idxStart <> text ";"
                         <+> text idx <> text "<=" <> printFPaexpr idxEnd   <> text ";"
                         <+> text idx <> text "++" )
    <> text "{"
    $$
    text "if (res.isValid) {"
    $$
    forBodyDoc
    $$
    text "} else {"
    $$
    text "res =" <+> printNone fp <+> text ";"
    $$
    text "}"
    $$
    text "}"

printFPstm fp UnstWarning _ = return $ text "res =" <+> printNone fp <+> text ";"

printSome :: FPrec -> Doc
printSome TInt = text "some"
printSome FPSingle = text "someFloat"
printSome FPDouble = text "someDouble"
printSome fp = error $ "printSome: case " ++ show fp ++ " niy."

printNone :: FPrec -> Doc
printNone TInt = text "none()"
printNone FPSingle = text "noneFloat()"
printNone FPDouble = text "noneDouble()"
printNone fp = error $ "printNone: case " ++ show fp ++ " niy."

printACSLForLoop :: [VarName] -> FAExpr -> Doc
printACSLForLoop assignVars variant = text"/*@ loop invariant"
     $$ text "loop assigns" <+> docListComma (map text assignVars) <+> text ";"
     $$ text "loop variant" <+> printFPaexpr variant <+> text ";"
     $$ text "*/"


printFPaexpr :: FAExpr -> Doc
printFPaexpr (FIAdd     ae1 ae2) = parens $ printFPaexpr ae1 <+> text "+" <+> printFPaexpr ae2
printFPaexpr (FISub     ae1 ae2) = parens $ printFPaexpr ae1 <+> text "-" <+> printFPaexpr ae2
printFPaexpr (FIMul     ae1 ae2) = parens $ printFPaexpr ae1 <+> text "*" <+> printFPaexpr ae2
printFPaexpr (FIDiv     ae1 ae2) = parens $ printFPaexpr ae1 <+> text "/" <+> printFPaexpr ae2
printFPaexpr (FItDiv    ae1 ae2) = parens $ printFPaexpr ae1 <+> text "/" <+> printFPaexpr ae2
printFPaexpr (FIMod     ae1 ae2) = parens $ printFPaexpr ae1 <+> text "%" <+> printFPaexpr ae2
printFPaexpr (FItMod    ae1 ae2) = parens $ printFPaexpr ae1 <+> text "%" <+> printFPaexpr ae2
printFPaexpr (FAdd   _  ae1 ae2) = parens $ printFPaexpr ae1 <+> text "+" <+> printFPaexpr ae2
printFPaexpr (FSub   _  ae1 ae2) = parens $ printFPaexpr ae1 <+> text "-" <+> printFPaexpr ae2
printFPaexpr (FMul   _  ae1 ae2) = parens $ printFPaexpr ae1 <+> text "*" <+> printFPaexpr ae2
printFPaexpr (FDiv   _  ae1 ae2) = parens $ printFPaexpr ae1 <+> text "/" <+> printFPaexpr ae2
printFPaexpr (FMod   _  ae1 ae2) = parens $ printFPaexpr ae1 <+> text "%" <+> printFPaexpr ae2
printFPaexpr (FPow   _  ae1 ae2) = text "pow" <> parens (printFPaexpr ae1 <+> comma <+> printFPaexpr ae2)
printFPaexpr (FIPow     ae1 ae2) = text "pow" <> parens (printFPaexpr ae1 <+> comma <+> printFPaexpr ae2)
printFPaexpr (FINeg     ae) = text "-"   <+>  parens (printFPaexpr ae)
printFPaexpr (FNeg   _  ae) = text "-"   <+>  parens (printFPaexpr ae)
printFPaexpr (FIAbs     ae) = text "abs"  <>  parens (printFPaexpr ae)
printFPaexpr (FFloor _ ae) = text "floor" <>  parens (printFPaexpr ae)
printFPaexpr (FSqrt  _ ae) = text "sqrt"  <>  parens (printFPaexpr ae)
printFPaexpr (FAbs   _ ae) = text "abs"   <>  parens (printFPaexpr ae)
printFPaexpr (FSin   _ ae) = text "sin"   <>  parens (printFPaexpr ae)
printFPaexpr (FCos   _ ae) = text "cos"   <>  parens (printFPaexpr ae)
printFPaexpr (FTan   _ ae) = text "floor" <>  parens (printFPaexpr ae)
printFPaexpr (FAcos  _ ae) = text "asin"  <>  parens (printFPaexpr ae)
printFPaexpr (FAsin  _ ae) = text "acos"  <>  parens (printFPaexpr ae)
printFPaexpr (FAtan  _ ae) = text "acos"  <>  parens (printFPaexpr ae)
printFPaexpr (FLn    _ ae) = text "log"   <>  parens (printFPaexpr ae)
printFPaexpr (FExpo  _ ae) = text "exp"   <>  parens (printFPaexpr ae)
printFPaexpr (FInt  i) = integer i
printFPaexpr (FCnst _ rat) = text $ Rat.showRational Nothing rat
printFPaexpr (FEFun f fp args) = text f <> text "_" <> prettyFPrec fp <+> parens (docListComma $ map printFPaexpr args)
printFPaexpr (Value (FEFun f fp args)) = text f <> text "_" <> prettyFPrec fp <+> parens (docListComma $ map printFPaexpr args) <> text ".value"
printFPaexpr (Value (StructVar x)) = text x <> text ".value"
printFPaexpr (Value ae) = error $ "printFPaexpr: function Value applied to " ++ show ae ++ "."
printFPaexpr (FVar  TInt x) = text x
printFPaexpr (FVar  fp x) = prettyFPrec fp <> text "_" <> text x
printFPaexpr (StructVar x) = text x
printFPaexpr (FArrayElem _ _ v idxExpr) = text v <> text "[" <> printFPaexpr idxExpr <> text "]"
printFPaexpr (FMin _) = error "printFPaexpr: min not defined"
printFPaexpr (FMax _) = error "printFPaexpr: max not defined"
printFPaexpr (RtoD (Int i)) = integer i
printFPaexpr (RtoS (Int i)) = integer i
printFPaexpr (RtoD (Rat rat)) = text $ Rat.showRational Nothing rat
printFPaexpr (RtoS (Rat rat)) = text $ Rat.showRational Nothing rat
printFPaexpr (StoD ae) = text "(double)" <> parens (printFPaexpr ae)
printFPaexpr (DtoS ae) = text "(single)" <> parens (printFPaexpr ae)
printFPaexpr (ItoD ae) = text "(double)" <> parens (printFPaexpr ae)
printFPaexpr (ItoS ae) = text "(single)" <> parens (printFPaexpr ae)
printFPaexpr FFma{} = error "printFPaexpr: fused multiply add not defined"
printFPaexpr ae = error $ "printFPaexpr: case " ++ show ae ++ " niy."

printFPbexpr :: FBExpr -> Doc
printFPbexpr (FOr  be1 be2) = parens (printFPbexpr be1) <+> text "||" <+> parens (printFPbexpr be2)
printFPbexpr (FAnd be1 be2) = parens (printFPbexpr be1) <+> text "&&" <+> parens (printFPbexpr be2)
printFPbexpr (FNot be)      = text "!" <> parens (printFPbexpr be)
printFPbexpr (FEq  ae1 ae2) = printFPaexpr ae1 <+> text "==" <+> printFPaexpr ae2
printFPbexpr (FNeq ae1 ae2) = printFPaexpr ae1 <+> text "!=" <+> printFPaexpr ae2
printFPbexpr (FLt  ae1 ae2) = printFPaexpr ae1 <+> text "<"  <+> printFPaexpr ae2
printFPbexpr (FLtE ae1 ae2) = printFPaexpr ae1 <+> text "<=" <+> printFPaexpr ae2
printFPbexpr (FGt  ae1 ae2) = printFPaexpr ae2 <+> text "<"  <+> printFPaexpr ae1
printFPbexpr (FGtE ae1 ae2) = printFPaexpr ae2 <+> text "<=" <+> printFPaexpr ae1
printFPbexpr (IsValid (Value ae))   = printFPaexpr ae  <> text ".isValid"
printFPbexpr (IsValid ae)   = printFPaexpr ae  <> text ".isValid"
printFPbexpr FBTrue  = text "true"
printFPbexpr FBFalse = text "false"




printACSLCond :: Condition -> Doc
printACSLCond (be, fbe) = parens (prettyACSLbexpr be <+> text "&&" <+> printACSLFBexpr fbe)



printACSLFBexpr :: FBExpr -> Doc
printACSLFBexpr  (FOr e1 e2) = parens $ printACSLFBexpr e1 <+> text "||"  <+> printACSLFBexpr e2
printACSLFBexpr (FAnd e1 e2) = parens $ printACSLFBexpr e1 <+> text "&&" <+> printACSLFBexpr e2
printACSLFBexpr     (FNot e) = text "!" <> parens (printACSLFBexpr e)
printACSLFBexpr  (FEq a1 a2) = parens $ printACSLFAexpr a1 <+> text "="  <+> printACSLFAexpr a2
printACSLFBexpr (FNeq a1 a2) = parens $ printACSLFAexpr a1 <+> text "!=" <+> printACSLFAexpr a2
printACSLFBexpr  (FLt a1 a2) = parens $ printACSLFAexpr a1 <+> text "<"  <+> printACSLFAexpr a2
printACSLFBexpr (FLtE a1 a2) = parens $ printACSLFAexpr a1 <+> text "<=" <+> printACSLFAexpr a2
printACSLFBexpr  (FGt a1 a2) = parens $ printACSLFAexpr a2 <+> text "<"  <+> printACSLFAexpr a1
printACSLFBexpr (FGtE a1 a2) = parens $ printACSLFAexpr a2 <+> text "<=" <+> printACSLFAexpr a1
printACSLFBexpr       FBTrue = text "\\true"
printACSLFBexpr      FBFalse = text "\\false"
printACSLFBexpr (IsValid ae) = text "isValid" <> parens (prettyDoc ae)


printACSLFAexpr :: FAExpr -> Doc
printACSLFAexpr (RtoS (Int i)) = integer i <> text ".0"
printACSLFAexpr (RtoD (Int i)) = integer i <> text ".0"
printACSLFAexpr (RtoS (Rat d)) = parens $ text $ showRational d
printACSLFAexpr (RtoD (Rat d)) = parens $ text $ showRational d
printACSLFAexpr (RtoS _) = error "printACSLFaexpr no defined for RtoS."
printACSLFAexpr (RtoD _) = error "printACSLFaexpr no defined for RtoD."
printACSLFAexpr (StoD _) = error "printACSLFaexpr no defined for StoD."
printACSLFAexpr (DtoS _) = error "printACSLFaexpr no defined for StoD."
printACSLFAexpr (ItoS _) = error "printACSLFaexpr no defined for ItoS."
printACSLFAexpr (ItoD _) = error "printACSLFaexpr no defined for ItoD."
printACSLFAexpr (FInt i) = integer i
printACSLFAexpr (FCnst _ d) = parens $ text $ showRational d
printACSLFAexpr (FNeg _ (FInt i)) = text "-" <> integer i
printACSLFAexpr (FNeg _ (FCnst _ d)) = text "-" <> parens (text $ showRational d)
printACSLFAexpr (FVar FPSingle x) = text ("single_" ++ x)
printACSLFAexpr (FVar FPDouble x) = text ("double_" ++ x)
printACSLFAexpr (FVar _ x) = text x
printACSLFAexpr (FEFun f _ []) = text f
printACSLFAexpr (FEFun f _ args) = text f <> parens (hsep $ punctuate comma $ map printACSLFAexpr args)
printACSLFAexpr (FIAdd a1 a2)  = text "Iadd"  <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2)
printACSLFAexpr (FISub a1 a2)  = text "Isub"  <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2)
printACSLFAexpr (FIMul a1 a2)  = text "Imul"  <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2)
printACSLFAexpr (FIDiv a1 a2)  = text "Idiv"  <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2)
printACSLFAexpr (FItDiv a1 a2) = text "Itdiv" <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2)
printACSLFAexpr (FIMod a1 a2)  = text "Imod"  <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2)
printACSLFAexpr (FItMod a1 a2) = text "Itmod" <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2)
printACSLFAexpr (FIPow a1 a2)  = text "Ipow"  <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2)
printACSLFAexpr (FINeg a)      = text "Ineg"  <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FIAbs a)      = text "Iabs"  <> lparen <> printACSLFAexpr a <> rparen
    --
printACSLFAexpr (FAdd   FPSingle a1 a2) = text "Sadd" <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2)
printACSLFAexpr (FSub   FPSingle a1 a2) = text "Ssub" <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2)
printACSLFAexpr (FMul   FPSingle a1 a2) = text "Smul" <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2)
printACSLFAexpr (FDiv   FPSingle a1 a2) = text "Sdiv" <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2)
printACSLFAexpr (FMod   FPSingle a1 a2) = text "Smod" <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2)
printACSLFAexpr (FFma   FPSingle a1 a2 a3) = text "Sfma" <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2 <> comma <+> printACSLFAexpr a3)
printACSLFAexpr (FPow   FPSingle a1 a2) = printACSLFAexpr a1 <+> text "^" <+> lparen <> printACSLFAexpr a2 <> rparen
printACSLFAexpr (FNeg   FPSingle a)     = text "Sneg"   <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FFloor FPSingle a)     = text "Sfloor" <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FSqrt  FPSingle a)     = text "Ssqrt"  <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FAbs   FPSingle a)     = text "Sabs"   <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FSin   FPSingle a)     = text "Ssin"   <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FCos   FPSingle a)     = text "Scos"   <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FTan   FPSingle a)     = text "Stan"   <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FAsin  FPSingle a)     = text "Sasin"  <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FAcos  FPSingle a)     = text "Sacos"  <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FAtan  FPSingle a)     = text "Satan"  <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FLn    FPSingle a)     = text "Sln"    <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FExpo  FPSingle a)     = text "Sexp"   <> lparen <> printACSLFAexpr a <> rparen
    --
printACSLFAexpr (FAdd   FPDouble a1 a2) = text "Dadd" <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2)
printACSLFAexpr (FSub   FPDouble a1 a2) = text "Dsub" <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2)
printACSLFAexpr (FMul   FPDouble a1 a2) = text "Dmul" <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2)
printACSLFAexpr (FDiv   FPDouble a1 a2) = text "Ddiv" <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2)
printACSLFAexpr (FMod   FPDouble a1 a2) = text "Dmod" <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2)
printACSLFAexpr (FFma   FPDouble a1 a2 a3) = text "Dfma" <> parens (printACSLFAexpr a1 <> comma <+> printACSLFAexpr a2 <> comma <+> printACSLFAexpr a3)
printACSLFAexpr (FPow   FPDouble a1 a2) = printACSLFAexpr a1 <+> text "^" <+> lparen <> printACSLFAexpr a2 <> rparen
printACSLFAexpr (FNeg   FPDouble a)     = text "Dneg"   <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FFloor FPDouble a)     = text "Dfloor" <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FSqrt  FPDouble a)     = text "Dsqrt"  <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FAbs   FPDouble a)     = text "Dabs"   <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FSin   FPDouble a)     = text "Dsin"   <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FCos   FPDouble a)     = text "Dcos"   <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FTan   FPDouble a)     = text "Dtan"   <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FAsin  FPDouble a)     = text "Dasin"  <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FAcos  FPDouble a)     = text "Dacos"  <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FAtan  FPDouble a)     = text "Datan"  <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FLn    FPDouble a)     = text "Dln"    <> lparen <> printACSLFAexpr a <> rparen
printACSLFAexpr (FExpo  FPDouble a)     = text "Dexp"   <> lparen <> printACSLFAexpr a <> rparen
    --
printACSLFAexpr (FMin as) = text "min" <> parens (hsep (punctuate comma $ map printACSLFAexpr as))
printACSLFAexpr (FMax as) = text "min" <> parens (hsep (punctuate comma $ map printACSLFAexpr as))
    --
printACSLFAexpr (FAdd   t _ _) = error $ "printACSLFAexpr FAdd: unexpeced " ++ show t ++ " value"
printACSLFAexpr (FSub   t _ _) = error $ "printACSLFAexpr FSub: unexpeced " ++ show t ++ " value"
printACSLFAexpr (FMul   t _ _) = error $ "printACSLFAexpr FMul: unexpeced " ++ show t ++ " value"
printACSLFAexpr (FDiv   t _ _) = error $ "printACSLFAexpr FDiv: unexpeced " ++ show t ++ " value"
printACSLFAexpr (FMod   t _ _) = error $ "printACSLFAexpr FMod: unexpeced " ++ show t ++ " value"
printACSLFAexpr (FPow   t _ _) = error $ "printACSLFAexpr FPow: unexpeced " ++ show t ++ " value"
printACSLFAexpr (FFma   t _ _ _)   = error $ "printACSLFAexpr FFma: unexpeced " ++ show t ++ " value"
printACSLFAexpr (FNeg   t _)   = error $ "printACSLFAexpr FNeg: unexpeced " ++ show t ++ " value"
printACSLFAexpr (FFloor t _)   = error $ "printACSLFAexpr FFloor: unexpeced " ++ show t ++ " value"
printACSLFAexpr (FSqrt  t _)   = error $ "printACSLFAexpr FSqrt: unexpeced " ++ show t ++ " value"
printACSLFAexpr (FAbs   t _)   = error $ "printACSLFAexpr FAbs: unexpeced " ++ show t ++ " value"
printACSLFAexpr (FSin   t _)   = error $ "printACSLFAexpr FSin: unexpeced " ++ show t ++ " value"
printACSLFAexpr (FCos   t _)   = error $ "printACSLFAexpr FCos: unexpeced " ++ show t ++ " value"
printACSLFAexpr (FTan   t _)   = error $ "printACSLFAexpr FTan: unexpeced " ++ show t ++ " value"
printACSLFAexpr (FAsin  t _)   = error $ "printACSLFAexpr FAsin: unexpeced " ++ show t ++ " value"
printACSLFAexpr (FAcos  t _)   = error $ "printACSLFAexpr FAcos: unexpeced " ++ show t ++ " value"
printACSLFAexpr (FAtan  t _)   = error $ "printACSLFAexpr FAtan: unexpeced " ++ show t ++ " value"
printACSLFAexpr (FLn    t _)   = error $ "printACSLFAexpr FLn: unexpeced " ++ show t ++ " value"
printACSLFAexpr (FExpo  t _)   = error $ "printACSLFAexpr FExpo: unexpeced " ++ show t ++ " value"
printACSLFAexpr ee = error $ "printACSLFAexpr for " ++ show ee ++ "not implemented yet."


vspace :: Int -> String
vspace n = concat $ replicate n "\n"

printHeader :: Doc
printHeader = text "// This file is automatically generated by PRECiSA \n"
      $$ text "#include<stdio.h>"
      $$ text "#include<stdlib.h>"
      $$ text "#include<math.h>"
      $$ text "#include<string.h>"
      $$ text "#include<stdbool.h>"
      $$ text "#include\"precisa_prelude.c\""

printMain :: Doc
printMain = text "int main () { return 0; }"

replaceCallsInAExpr :: [(FAExpr,Int)] -> FAExpr -> FAExpr
replaceCallsInAExpr callList call@FEFun{} = case lookup call callList of
  Just n  -> Value $ StructVar $ "aux_" ++ show n
  Nothing -> call
replaceCallsInAExpr callList (FFma fp ae1 ae2 ae3) = FFma fp (replaceCallsInAExpr callList ae1)
                                                             (replaceCallsInAExpr callList ae2)
                                                             (replaceCallsInAExpr callList ae3)
replaceCallsInAExpr callList (FIAdd     ae1 ae2) = FIAdd     (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInAExpr callList (FISub     ae1 ae2) = FISub     (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInAExpr callList (FIMul     ae1 ae2) = FIMul     (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInAExpr callList (FIDiv     ae1 ae2) = FIDiv     (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInAExpr callList (FItDiv    ae1 ae2) = FItDiv    (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInAExpr callList (FIMod     ae1 ae2) = FIMod     (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInAExpr callList (FItMod    ae1 ae2) = FItMod    (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInAExpr callList (FIPow     ae1 ae2) = FIPow     (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInAExpr callList (FAdd   fp ae1 ae2) = FAdd   fp (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInAExpr callList (FSub   fp ae1 ae2) = FSub   fp (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInAExpr callList (FMul   fp ae1 ae2) = FMul   fp (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInAExpr callList (FDiv   fp ae1 ae2) = FDiv   fp (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInAExpr callList (FPow   fp ae1 ae2) = FPow   fp (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInAExpr callList (FMod   fp ae1 ae2) = FMod   fp (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInAExpr callList (FIExp     ae1 ae2) = FIExp     (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInAExpr callList (FINeg     ae)      = FINeg     (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (FIAbs     ae)      = FIAbs     (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (Value     ae)      = Value     (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (StoD      ae)      = StoD      (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (DtoS      ae)      = DtoS      (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (ItoD      ae)      = ItoD      (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (ItoS      ae)      = ItoS      (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (FNeg   fp ae)      = FNeg   fp (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (FFloor fp ae)      = FFloor fp (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (FSqrt  fp ae)      = FSqrt  fp (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (FAbs   fp ae)      = FAbs   fp (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (FSin   fp ae)      = FSin   fp (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (FCos   fp ae)      = FCos   fp (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (FTan   fp ae)      = FTan   fp (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (FAcos  fp ae)      = FAcos  fp (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (FAsin  fp ae)      = FAsin  fp (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (FAtan  fp ae)      = FAtan  fp (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (FLn    fp ae)      = FLn    fp (replaceCallsInAExpr callList ae)
replaceCallsInAExpr callList (FExpo  fp ae)      = FExpo  fp (replaceCallsInAExpr callList ae)

replaceCallsInAExpr _ ae@(FInt  _)      = ae
replaceCallsInAExpr _ ae@(FCnst _ _)    = ae
replaceCallsInAExpr _ ae@(FVar  _ _)    = ae
replaceCallsInAExpr _ ae@(StructVar  _) = ae
replaceCallsInAExpr callList (FMin aes) = FMin (map (replaceCallsInAExpr callList) aes)
replaceCallsInAExpr callList (FMax aes) = FMax (map (replaceCallsInAExpr callList) aes)
replaceCallsInAExpr _ ae = error $ "replaceCallsInAExpr not defined for " ++ show ae

replaceCallsInBExpr :: [(FAExpr,Int)] -> FBExpr -> FBExpr
replaceCallsInBExpr callList (FOr  be1 be2) = FOr  (replaceCallsInBExpr callList be1) (replaceCallsInBExpr callList be2)
replaceCallsInBExpr callList (FAnd be1 be2) = FAnd (replaceCallsInBExpr callList be1) (replaceCallsInBExpr callList be2)
replaceCallsInBExpr callList (FNot be)      = FNot (replaceCallsInBExpr callList be)
replaceCallsInBExpr callList (FEq  ae1 ae2) = FEq  (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInBExpr callList (FNeq ae1 ae2) = FNeq (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInBExpr callList (FLt  ae1 ae2) = FLt  (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInBExpr callList (FLtE ae1 ae2) = FLtE (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInBExpr callList (FGt  ae1 ae2) = FGt  (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInBExpr callList (FGtE ae1 ae2) = FGtE (replaceCallsInAExpr callList ae1) (replaceCallsInAExpr callList ae2)
replaceCallsInBExpr callList (IsValid ae)   = IsValid (replaceCallsInAExpr callList ae)
replaceCallsInBExpr _ FBTrue  = FBTrue
replaceCallsInBExpr _ FBFalse = FBFalse

replaceCallsInStm :: [(FAExpr,Int)] -> Stm -> Stm
replaceCallsInStm callList (Let x fp ae stm) = Let x fp (replaceCallsInAExpr callList ae) (replaceCallsInStm callList stm)
replaceCallsInStm callList (Ite be stmThen stmElse) = Ite (replaceCallsInBExpr callList be) (replaceCallsInStm callList stmThen) (replaceCallsInStm callList stmElse)
replaceCallsInStm callList (ListIte listThen stmElse) = ListIte (map aux listThen) (replaceCallsInStm callList stmElse)
  where
    aux (be,stm) = (replaceCallsInBExpr callList be, replaceCallsInStm callList stm)
replaceCallsInStm callList (StmExpr ae) = StmExpr (replaceCallsInAExpr callList ae)
replaceCallsInStm callList (ForLoop fp idxStart idxEnd initAcc idx acc forBody) = ForLoop fp idxStart idxEnd initAcc idx acc (replaceCallsInStm callList forBody)
replaceCallsInStm _ UnstWarning = UnstWarning





