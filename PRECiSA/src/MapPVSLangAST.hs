-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}

module MapPVSLangAST
  ( raw2FAExpr,
    raw2FBExprStm,
    raw2CollExpr,
    isBExpr,
    isRecordType,
    isTupleType,
    raw2FPType,
    toErrM,
    retTypeFun,
    raw2Args,
    TypeContext,
    rawparserPVS,
    raw2Prog,
  )
where

import AbsRawPVSLang
import qualified AbsRawPVSLang as Raw
import AbsPVSLang
import qualified AbsPVSLang as PVS
import Common.TypesUtils
import Data.Functor.Identity
import Control.Monad.Reader
import Control.Monad.Extra
import ErrM
import Numeric
import qualified Operators as Op
import Parser.ParRawPVSLang
import Parser.LexRawPVSLang
import Data.Maybe (fromMaybe)

--------------------------------------------------------------------------------
--  ReaderT refactoring for input parameters and maintanability

newtype M a = M { unM :: ReaderT Env Identity a }
  deriving (Functor, Applicative, Monad, MonadReader Env)

data Env = Env
  { varTypeEnv :: VarTypeEnv,
    funTypeEnv :: FunTypeEnv,
    typeContext :: TypeContext
  }
  deriving (Eq, Ord, Show)

updateVarTypeEnv :: (VarTypeEnv -> VarTypeEnv) -> Env -> Env
updateVarTypeEnv f (Env venv fenv tyenv) = Env (f venv) fenv tyenv

runM :: Env -> M a -> a
runM env m = runIdentity $ runReaderT (unM m) env

askTypeContext :: M TypeContext
askTypeContext = asks typeContext

askVarTypeEnv :: M VarTypeEnv
askVarTypeEnv = asks varTypeEnv

askFunTypeEnv :: M FunTypeEnv
askFunTypeEnv = asks funTypeEnv

--------------------------------------------------------------------------------

type VarTypeEnv = [(String, PVSType)]
type FunTypeEnv = [(String, PVSType)]
type TypeContext = [(String, PVSType)]

isNumType :: AbsRawPVSLang.Type -> Bool
isNumType (TypeSimple (Id "int"))                  = True
isNumType (TypeSimple (Id "integer"))              = True
isNumType (TypeSimple (Id "single"))               = True
isNumType (TypeSimple (Id "unb_single"))           = True
isNumType (TypeSimple (Id "unb_pos_single"))       = True
isNumType (TypeSimple (Id "unb_nz_single"))        = True
isNumType (TypeSimple (Id "double"))               = True
isNumType (TypeSimple (Id "unb_double"))           = True
isNumType (TypeSimple (Id "unb_pos_double"))       = True
isNumType (TypeSimple (Id "unb_nz_double"))        = True
isNumType (TypeBelow _)                            = True
isNumType (ParametricTypeBi (Id "fixed_point") _ _)= True
isNumType _ = False

isIntType :: AbsRawPVSLang.Type -> Bool
isIntType (TypeSimple (Id "int"))     = True
isIntType (TypeSimple (Id "integer")) = True
isIntType (TypeBelow _)               = True
isIntType _                           = False

isRecordType :: AbsRawPVSLang.Type -> Bool
isRecordType (TypeRecord _) = True
isRecordType _ = False

isTupleType :: AbsRawPVSLang.Type -> Bool
isTupleType (TypeTuple _) = True
isTupleType _ = False

isBExpr :: AbsRawPVSLang.Expr -> Bool
isBExpr (AbsRawPVSLang.Or  _fbe1 _fbe2) = True
isBExpr (AbsRawPVSLang.And _fbe1 _fbe2) = True
isBExpr (AbsRawPVSLang.Not       _fbe) = True
isBExpr (AbsRawPVSLang.Eq  _fae1 _fae2) = True
isBExpr (AbsRawPVSLang.Neq _fae1 _fae2) = True
isBExpr (AbsRawPVSLang.Lt  _fae1 _fae2) = True
isBExpr (AbsRawPVSLang.LtE _fae1 _fae2) = True
isBExpr (AbsRawPVSLang.Gt  _fae1 _fae2) = True
isBExpr (AbsRawPVSLang.GtE _fae1 _fae2) = True
isBExpr AbsRawPVSLang.BTrue  = True
isBExpr AbsRawPVSLang.BFalse = True
isBExpr _ = False

getTypeContext :: [AbsRawPVSLang.Decl] -> TypeContext
getTypeContext = getTypeContext' []

getTypeContext' :: TypeContext -> [AbsRawPVSLang.Decl] -> TypeContext
getTypeContext' tc [] = tc
getTypeContext' tc (DeclTypeAlias (Id name) ty:ds) = getTypeContext' tc' ds
  where tc' = (name,raw2FPType tc ty):tc
getTypeContext' tc (_:ds) = getTypeContext' tc ds

raw2Prog :: AbsRawPVSLang.Program -> AbsPVSLang.Program
raw2Prog pgm = runM defaultEnv (mapM raw2Decl decls')
  where
    decls = getDeclList pgm
      where
        getDeclList :: AbsRawPVSLang.Program -> [AbsRawPVSLang.Decl]
        getDeclList (AbsRawPVSLang.Prog     _   ds _) = ds
        getDeclList (AbsRawPVSLang.ProgImp  _ _ ds _) = ds

    defaultEnv =
      Env
        { varTypeEnv = [],
          funTypeEnv = fenv,
          typeContext = tc
        }
      where
        fenv = map (retTypeFun tc) decls'
        tc = getTypeContext decls

    decls' = nonTypeDeclarations decls
      where
        nonTypeDeclarations :: [AbsRawPVSLang.Decl] -> [AbsRawPVSLang.Decl]
        nonTypeDeclarations = filter (not . isTypeSynonym)
          where
            isTypeSynonym :: AbsRawPVSLang.Decl -> Bool
            isTypeSynonym (DeclTypeAlias _ _) = True
            isTypeSynonym _ = False

retTypeFun :: TypeContext -> AbsRawPVSLang.Decl -> (String, PVSType)
retTypeFun tc (DeclConstant (AbsRawPVSLang.Id f)   fp _) = (f, raw2FPType tc fp)
retTypeFun tc (DeclFunction (AbsRawPVSLang.Id f) _ fp _) = (f, raw2FPType tc fp)
retTypeFun _ decl = error $ "[retTypeFun] unexpected argument: " ++ show decl

rationalizeFP :: Show a => a -> Rational
rationalizeFP d = (fst . head $ readSigned readFloat $ show d) :: Rational

raw2Decl :: AbsRawPVSLang.Decl -> M AbsPVSLang.Decl
raw2Decl e
  | DeclConstant f (TypeSimple (Id "bool")) e1 <- e = Pred False Original (raw2Id f) [] <$> raw2FBExprStmM e1
  | DeclFunction f rawArgs (TypeSimple (Id "bool")) e1 <- e
    = do
        tc <- askTypeContext
        let args = raw2Args tc rawArgs
        let venv  = map mapArg2Pair args
        e1' <- local (updateVarTypeEnv (const venv)) $ raw2FBExprStmM e1
        pure (Pred False Original (raw2Id f) args e1')
  | DeclConstant f fptype@(TypeArray _ _) stm <- e
    = do
        fenv <- askFunTypeEnv
        tc   <- askTypeContext
        pure (CollDecl False (raw2FPType tc fptype) (raw2Id f) [] (raw2CollExpr tc [] fenv stm))
  | DeclFunction f rawArgs fptype@(TypeArray _ _) stm <- e
    = do
        fenv <- askFunTypeEnv
        tc   <- askTypeContext
        let args = raw2Args tc rawArgs
        let env  = map mapArg2Pair args
        pure (CollDecl False (raw2FPType tc fptype) (raw2Id f) args (raw2CollExpr tc env fenv stm))
  | DeclConstant f fptype@(TypeRecord _) stm <- e
    = do
        fenv <- askFunTypeEnv
        tc   <- askTypeContext
        pure (CollDecl False (raw2FPType tc fptype) (raw2Id f) [] (raw2CollExpr tc [] fenv stm))
  | DeclFunction f rawArgs fptype@(TypeRecord _) stm <- e
    = do
        fenv <- askFunTypeEnv
        tc   <- askTypeContext
        let args = raw2Args tc rawArgs
        let env  = map mapArg2Pair args
        pure (CollDecl False (raw2FPType tc fptype) (raw2Id f) args (raw2CollExpr tc env fenv stm))
  | DeclConstant f fptype@(TypeTuple _) stm <- e
    = do
        fenv <- askFunTypeEnv
        tc   <- askTypeContext
        pure (CollDecl False (raw2FPType tc fptype) (raw2Id f) [] (raw2CollExpr tc [] fenv stm))
  | DeclFunction f rawArgs fptype@(TypeTuple _) stm <- e
    = do
        fenv <- askFunTypeEnv
        tc   <- askTypeContext
        let args = raw2Args tc rawArgs
        let env  = map mapArg2Pair args
        let ty   = raw2FPType tc fptype
        let stm' = raw2CollExpr tc env fenv stm
        pure (CollDecl False ty (raw2Id f) args stm')
  | DeclConstant f fptype stm <- e
    = do
        ty   <- raw2FPTypeM fptype
        stm' <- raw2FAExprM stm
        pure (Decl False ty (raw2Id f) [] stm')
  | DeclFunction f rawArgs fptype stm <- e
    = do
        ty   <- raw2FPTypeM fptype
        args <- raw2ArgsM rawArgs
        let venv  = map mapArg2Pair args
        stm' <- local (updateVarTypeEnv (const venv)) $ raw2FAExprM stm
        pure (Decl False ty (raw2Id f) args stm')
  | otherwise = error $ "[raw2Decl] unexpected argument: " ++ show e

raw2Elsif :: AbsRawPVSLang.ElsIf -> M (AbsPVSLang.FBExpr, AbsPVSLang.FAExpr)
raw2Elsif (ElsIf be stm) = (,) <$> raw2FBExprM be <*> raw2FAExprM stm

raw2BinOp :: AbsRawPVSLang.Expr -> AbsRawPVSLang.Expr -> Op.BinOp -> M AbsPVSLang.FAExpr
raw2BinOp e1 e2 op = do
  e1' <- raw2FAExprM e1
  e2' <- raw2FAExprM e2
  let fp = lubPVSType (getPVSType e1') (getPVSType e2')
  pure (PVS.BinaryFPOp op fp e1' e2')

raw2BinOpWithType :: AbsRawPVSLang.Expr -> AbsRawPVSLang.Expr -> Op.BinOp -> PVSType -> M AbsPVSLang.FAExpr
raw2BinOpWithType e1 e2 op ty = do
  e1' <- raw2FAExprM e1
  e2' <- raw2FAExprM e2
  pure (PVS.BinaryFPOp op ty e1' e2')

realToFP :: PVSType -> AbsRawPVSLang.Expr -> AbsPVSLang.FAExpr
realToFP _  (AbsRawPVSLang.Int i) = AbsPVSLang.FInt i
realToFP fp (AbsRawPVSLang.Rat d) = AbsPVSLang.ToFloat fp $ AbsPVSLang.Rat $ rationalizeFP d
realToFP _ fae = error $ "realToFP: " ++ show fae ++ "is not of type rational."

intToFP :: PVSType -> AbsRawPVSLang.Expr -> AbsPVSLang.FAExpr
intToFP _ (AbsRawPVSLang.Int i) = AbsPVSLang.FInt i
intToFP _ fae = error $ "intToFP: " ++ show fae ++ "is not of type int."

raw2CollExpr :: TypeContext -> VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> AbsPVSLang.CollFAExpr
raw2CollExpr tc env fenv x = runM defaultEnv (raw2CollExprM x)
  where
    defaultEnv =
      Env
        { varTypeEnv = env,
          funTypeEnv = fenv,
          typeContext = tc
        }

raw2CollExprM :: AbsRawPVSLang.Expr -> M AbsPVSLang.CollFAExpr
raw2CollExprM e
  | Raw.With array idx newValue <- e = ArrayUpdate <$> raw2CollExprM array <*> raw2FAExprM idx <*> raw2FAExprM newValue
  | Raw.Let letElems stm <- e
    = do
        allEnv <- ask
        let f (accEnv,ls) x =
              local (const accEnv) $
                do
                  l@(v,x',_) <- raw2LetElemM x
                  let accEnv' = updateVarTypeEnv ((v,x'):) accEnv
                  pure (accEnv',ls++[l])
        (allEnv',ls') <- foldM f (allEnv,[]) letElems
        stm' <- local (const allEnv') $ raw2CollExprM stm
        pure (PVS.CLet ls' stm')
  | Raw.If be thenSmt elseStm <- e = CIte <$> raw2FBExprM be <*> raw2CollExprM thenSmt <*> raw2CollExprM elseStm
  | Raw.ListIf be stmThen listElsif elseStm <- e
    = do
        be'        <- raw2FBExprM be
        stmThen'   <- raw2CollExprM stmThen
        listElsif' <- mapM raw2CollElsif listElsif
        elseStm'   <- raw2CollExprM elseStm
        pure (CListIte ((be',stmThen') : listElsif') elseStm')
  | Raw.RecordExpr es <- e = PVS.RecordExpr <$> mapM raw2RecordElem es
  | Raw.TupleExpr  es <- e = PVS.TupleExpr  <$> mapM raw2TupleExpr  es
  | Raw.ExprId (Raw.Id i) <- e
    = do
        fenv <- askFunTypeEnv
        env  <- askVarTypeEnv
        pure $ case lookup i fenv of
          Just t@(Tuple _)   -> CollFun False i t []
          Just t@(Record _)  -> CollFun False i t []
          Just t@(Array _ _) -> CollFun False i t []
          Just _  -> error $ "Identifier " ++ show i ++ "is not of data collection type."
          Nothing -> case lookup i env of
                        Just t@(Tuple  _)  -> PVS.CollVar t i
                        Just t@(Record _)  -> PVS.CollVar t i
                        Just t@(Array _ _) -> PVS.CollVar t i
                        Just _  -> error $ "Identifier " ++ show i ++ "is not of data collection type."
                        Nothing -> error $ "Identifier " ++ show i ++ "not found." ++ " in env: " ++ show env
  | Raw.Call (Raw.Id f) args <- e
    = do
        fenv <- askFunTypeEnv
        let fp = fromMaybe
                  (error $ "raw2FAExpr: function " ++ show f ++ " not found.")
                  (lookup f fenv)
        CollFun False f fp <$> mapM raw2FAExprM args
  | otherwise = error $ "raw2CollExpr: " ++ show e ++ "is not of type data collection expression."
  where
    raw2CollElsif :: Raw.ElsIf -> M (PVS.FBExpr, PVS.CollFAExpr)
    raw2CollElsif (ElsIf be stm) = (,) <$> raw2FBExprM be <*> raw2CollExprM stm

    raw2RecordElem :: Raw.RecordElem -> M (RecordField, Either PVS.FAExpr PVS.FBExpr)
    raw2RecordElem (AbsRawPVSLang.RecordElem (AbsRawPVSLang.Id field) expr)
      = (field,) <$>
          if isBExpr expr
            then Right <$> raw2FBExprM expr
            else Left  <$> raw2FAExprM expr

    raw2TupleExpr :: Raw.Expr -> M (Either PVS.FAExpr PVS.FBExpr)
    raw2TupleExpr expr
      = if isBExpr expr
          then Right <$> raw2FBExprM expr
          else Left  <$> raw2FAExprM expr

raw2FBExprStm :: TypeContext -> VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> AbsPVSLang.FBExprStm
raw2FBExprStm tc env fenv x = runM defaultEnv (raw2FBExprStmM x)
  where
    defaultEnv =
      Env
        { varTypeEnv = env,
          funTypeEnv = fenv,
          typeContext = tc
        }

raw2FBExprStmM :: AbsRawPVSLang.Expr -> M AbsPVSLang.FBExprStm
raw2FBExprStmM e
  | Raw.Let letElems stm <- e
    = do
        let f (accEnv,ls) x =
              local (const accEnv) $
                do
                  l@(v,x',_) <- raw2LetElemM x
                  let accEnv' = updateVarTypeEnv ((v,x'):) accEnv
                  pure (accEnv',ls++[l])
        allEnv <- ask
        (allEnv',ls') <- foldM f (allEnv,[]) letElems
        stm' <- local (const allEnv') $ raw2FBExprStmM stm
        pure (PVS.BLet ls' stm')
  | Raw.If be thenSmt elseStm <- e = PVS.BIte <$> raw2FBExprM be <*> raw2FBExprStmM thenSmt <*> raw2FBExprStmM elseStm
  | Raw.ListIf be stmThen listElsif elseStm <- e
    = do
        be'        <- raw2FBExprM be
        stmThen'   <- raw2FBExprStmM stmThen
        listElsif' <- mapM raw2BElsif listElsif
        elseStm'   <- raw2FBExprStmM elseStm
        pure (BListIte ((be',stmThen') : listElsif') elseStm')
  | otherwise = BExpr <$> raw2FBExprM e

raw2BElsif :: AbsRawPVSLang.ElsIf -> M (AbsPVSLang.FBExpr, AbsPVSLang.FBExprStm)
raw2BElsif (ElsIf be stm) = (,) <$> raw2FBExprM be <*> raw2FBExprStmM stm

raw2FBExpr :: TypeContext -> VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> AbsPVSLang.FBExpr
raw2FBExpr tc env fenv x = runM defaultEnv (raw2FBExprM x)
  where
    defaultEnv =
      Env
        { varTypeEnv = env,
          funTypeEnv = fenv,
          typeContext = tc
        }

raw2FBExprM :: AbsRawPVSLang.Expr -> M AbsPVSLang.FBExpr
raw2FBExprM e
  | Raw.BTrue  <- e = pure PVS.FBTrue
  | Raw.BFalse <- e = pure PVS.FBFalse
  | Raw.Not e1 <- e = PVS.FNot <$> raw2FBExprM e1
  | Raw.Call (Raw.Id f) as <- e = PVS.FEPred False Original f <$> mapM raw2FAExprM as
  | Raw.Or  e1 e2 <- e = PVS.FOr  <$> raw2FBExprM e1 <*> raw2FBExprM e2
  | Raw.And e1 e2 <- e = PVS.FAnd <$> raw2FBExprM e1 <*> raw2FBExprM e2
  | Raw.Eq  e1 e2 <- e = PVS.FRel Op.Eq  <$> raw2FAExprM e1 <*> raw2FAExprM e2
  | Raw.Neq e1 e2 <- e = PVS.FRel Op.Neq <$> raw2FAExprM e1 <*> raw2FAExprM e2
  | Raw.Lt  e1 e2 <- e = PVS.FRel Op.Lt  <$> raw2FAExprM e1 <*> raw2FAExprM e2
  | Raw.LtE e1 e2 <- e = PVS.FRel Op.LtE <$> raw2FAExprM e1 <*> raw2FAExprM e2
  | Raw.Gt  e1 e2 <- e = PVS.FRel Op.Gt  <$> raw2FAExprM e1 <*> raw2FAExprM e2
  | Raw.GtE e1 e2 <- e = PVS.FRel Op.GtE <$> raw2FAExprM e1 <*> raw2FAExprM e2
  | otherwise = error $ "raw2FBExpr: Boolean expression expected but got " ++ show e ++ "."

raw2FAExpr :: TypeContext -> VarTypeEnv -> FunTypeEnv -> AbsRawPVSLang.Expr -> AbsPVSLang.FAExpr
raw2FAExpr tc env fenv x = runM defaultEnv (raw2FAExprM x)
  where
    defaultEnv =
      Env
        { varTypeEnv = env,
          funTypeEnv = fenv,
          typeContext = tc
        }

raw2FAExprM :: Expr -> M FAExpr
raw2FAExprM e
  | Raw.Int n     <- e = pure (PVS.FInt n)
  | Raw.Rat d     <- e = pure (PVS.ToFloat FPDouble $ PVS.Rat $ rationalizeFP d)
  | Raw.ExprNeg (Raw.Int i) <- e = pure (PVS.FInt (-i))
  | Raw.ExprNeg (Raw.Rat d) <- e = pure (PVS.ToFloat FPDouble $ PVS.Rat $ rationalizeFP (-d))
  | Raw.ExprNeg (Raw.Call (Raw.Id "ItoD") [Raw.Int i]) <- e = pure (PVS.ToFloat FPDouble $ PVS.Int (-i))
  | Raw.ExprNeg (Raw.Call (Raw.Id "ItoS") [Raw.Int i]) <- e = pure (PVS.ToFloat FPSingle $ PVS.Int (-i))
  | Raw.ExprNeg (Raw.Call (Raw.Id "RtoD") [Raw.Rat d]) <- e = pure (PVS.ToFloat FPDouble $ PVS.Rat $ rationalizeFP (-d))
  | Raw.ExprNeg (Raw.Call (Raw.Id "RtoS") [Raw.Rat d]) <- e = pure (PVS.ToFloat FPSingle $ PVS.Rat $ rationalizeFP (-d))
  | Raw.ExprNeg e1 <- e = do
      e1' <- raw2FAExprM e1
      let ty = getPVSType e1'
      pure (PVS.UnaryFPOp Op.NegOp ty e1')
  | Raw.ExprId (Raw.Id i) <- e = do
      env  <- askVarTypeEnv
      fenv <- askFunTypeEnv
      case lookup i fenv of
        Just (Tuple _) -> error $ "Identifier " ++ show i ++ "is a tuple."
        Just (Record _) -> error $ "Identifier " ++ show i ++ "is a record."
        Just fp -> pure (PVS.FEFun False i ResValue fp [])
        Nothing ->
          case lookup i env of
            Just fp -> pure (PVS.FVar fp i)
            Nothing -> error $ "Identifier " ++ show i ++ "not found." ++ " in env: " ++ show env ++ " ,nor in fenv: " ++ show fenv
  | Raw.Call (Raw.Id f) [fae] <- e = do
    fae' <- raw2FAExprM fae
    let fp = getPVSType fae'
    pure $ case f of
      "floor"  -> AbsPVSLang.UnaryFPOp Op.FloorOp fp       fae'
      "Sfloor" -> AbsPVSLang.UnaryFPOp Op.FloorOp FPSingle fae'
      "Dfloor" -> AbsPVSLang.UnaryFPOp Op.FloorOp FPDouble fae'
      "abs"    -> AbsPVSLang.UnaryFPOp Op.AbsOp   fp       fae'
      "Sabs"   -> AbsPVSLang.UnaryFPOp Op.AbsOp   FPSingle fae'
      "Dabs"   -> AbsPVSLang.UnaryFPOp Op.AbsOp   FPDouble fae'
      "sqrt"   -> AbsPVSLang.UnaryFPOp Op.SqrtOp  fp       fae'
      "Ssqrt"  -> AbsPVSLang.UnaryFPOp Op.SqrtOp  FPSingle fae'
      "Dsqrt"  -> AbsPVSLang.UnaryFPOp Op.SqrtOp  FPDouble fae'
      "sin"    -> AbsPVSLang.UnaryFPOp Op.SinOp   fp       fae'
      "Ssin"   -> AbsPVSLang.UnaryFPOp Op.SinOp   FPSingle fae'
      "Dsin"   -> AbsPVSLang.UnaryFPOp Op.SinOp   FPDouble fae'
      "cos"    -> AbsPVSLang.UnaryFPOp Op.CosOp   fp       fae'
      "Scos"   -> AbsPVSLang.UnaryFPOp Op.CosOp   FPSingle fae'
      "Dcos"   -> AbsPVSLang.UnaryFPOp Op.CosOp   FPDouble fae'
      "tan"    -> AbsPVSLang.UnaryFPOp Op.TanOp   fp       fae'
      "Stan"   -> AbsPVSLang.UnaryFPOp Op.TanOp   FPSingle fae'
      "Dtan"   -> AbsPVSLang.UnaryFPOp Op.TanOp   FPDouble fae'
      "asin"   -> AbsPVSLang.UnaryFPOp Op.AsinOp  fp       fae'
      "Sasin"  -> AbsPVSLang.UnaryFPOp Op.AsinOp  FPSingle fae'
      "Dasin"  -> AbsPVSLang.UnaryFPOp Op.AsinOp  FPDouble fae'
      "acos"   -> AbsPVSLang.UnaryFPOp Op.AcosOp  fp       fae'
      "Sacos"  -> AbsPVSLang.UnaryFPOp Op.AcosOp  FPSingle fae'
      "Dacos"  -> AbsPVSLang.UnaryFPOp Op.AcosOp  FPDouble fae'
      "atan"   -> AbsPVSLang.UnaryFPOp Op.AtanOp  fp       fae'
      "Satan"  -> AbsPVSLang.UnaryFPOp Op.AtanOp  FPSingle fae'
      "Datan"  -> AbsPVSLang.UnaryFPOp Op.AtanOp  FPDouble fae'
      "ln"     -> AbsPVSLang.UnaryFPOp Op.LnOp    fp       fae'
      "Sln"    -> AbsPVSLang.UnaryFPOp Op.LnOp    FPSingle fae'
      "Dln"    -> AbsPVSLang.UnaryFPOp Op.LnOp    FPDouble fae'
      "exp"    -> AbsPVSLang.UnaryFPOp Op.ExpoOp  fp       fae'
      "Sexp"   -> AbsPVSLang.UnaryFPOp Op.ExpoOp  FPSingle fae'
      "Dexp"   -> AbsPVSLang.UnaryFPOp Op.ExpoOp  FPDouble fae'
      "Ineg"   -> AbsPVSLang.UnaryFPOp Op.NegOp   TInt     fae'
      "Sneg"   -> AbsPVSLang.UnaryFPOp Op.NegOp   FPSingle fae'
      "Dneg"   -> AbsPVSLang.UnaryFPOp Op.NegOp   FPDouble fae'
      "RtoS"   -> realToFP FPSingle fae
      "RtoD"   -> realToFP FPDouble fae
      "ItoS"   -> intToFP  FPSingle fae
      "ItoD"   -> intToFP  FPDouble fae
      _        -> error $ "[raw2FAExpr] unexpected function identifier: " ++ show f
  | Raw.Call (Raw.Id "nth") [Raw.ExprId (Raw.Id name),idx] <- e
    = do
        idx' <- raw2FAExprM idx
        env  <- askVarTypeEnv
        let ty = getElementType env name
        pure (PVS.FListElem ty name idx')
  | Raw.Call (Raw.Id "map") [Raw.ExprId (Raw.Id funName),Raw.ExprId (Raw.Id name)] <- e
    = do
        env  <- askVarTypeEnv
        let ty = getElementType env name
        pure (PVS.FMap ty funName name)
  | Raw.Call (AbsRawPVSLang.Id "fold")
      [Raw.ExprId (Raw.Id funName)
      ,AbsRawPVSLang.ExprId (AbsRawPVSLang.Id name)
      ,AbsRawPVSLang.Int n
      ,baseCase] <- e
    = do
        env  <- askVarTypeEnv
        let ty = getElementType env name
        baseCase' <- raw2FAExprM baseCase
        pure (PVS.FFold ty funName name n baseCase')
  | Raw.Call (Raw.Id  "add") [e1,e2] <- e = raw2BinOp         e1 e2 Op.AddOp
  | Raw.Call (Raw.Id "Iadd") [e1,e2] <- e = raw2BinOpWithType e1 e2 Op.AddOp TInt
  | Raw.Call (Raw.Id "Sadd") [e1,e2] <- e = raw2BinOpWithType e1 e2 Op.AddOp FPSingle
  | Raw.Call (Raw.Id "Dadd") [e1,e2] <- e = raw2BinOpWithType e1 e2 Op.AddOp FPDouble
  | Raw.Call (Raw.Id  "sub") [e1,e2] <- e = raw2BinOp         e1 e2 Op.SubOp
  | Raw.Call (Raw.Id "Isub") [e1,e2] <- e = raw2BinOpWithType e1 e2 Op.SubOp TInt
  | Raw.Call (Raw.Id "Ssub") [e1,e2] <- e = raw2BinOpWithType e1 e2 Op.SubOp FPSingle
  | Raw.Call (Raw.Id "Dsub") [e1,e2] <- e = raw2BinOpWithType e1 e2 Op.SubOp FPDouble
  | Raw.Call (Raw.Id  "mul") [e1,e2] <- e = raw2BinOp         e1 e2 Op.MulOp
  | Raw.Call (Raw.Id "Imul") [e1,e2] <- e = raw2BinOpWithType e1 e2 Op.MulOp TInt
  | Raw.Call (Raw.Id "Smul") [e1,e2] <- e = raw2BinOpWithType e1 e2 Op.MulOp FPSingle
  | Raw.Call (Raw.Id "Dmul") [e1,e2] <- e = raw2BinOpWithType e1 e2 Op.MulOp FPDouble
  | Raw.Call (Raw.Id  "div") [e1,e2] <- e = raw2BinOp         e1 e2 Op.DivOp
  | Raw.Call (Raw.Id "Idiv") [e1,e2] <- e = raw2BinOpWithType e1 e2 Op.DivOp TInt
  | Raw.Call (Raw.Id "Sdiv") [e1,e2] <- e = raw2BinOpWithType e1 e2 Op.DivOp FPSingle
  | Raw.Call (Raw.Id "Ddiv") [e1,e2] <- e = raw2BinOpWithType e1 e2 Op.DivOp FPDouble
  | Raw.Call (Raw.Id  "mod") [e1,e2] <- e = raw2BinOp         e1 e2 Op.ModOp
  | Raw.Call (Raw.Id "Imod") [e1,e2] <- e = raw2BinOpWithType e1 e2 Op.ModOp TInt
  | Raw.Call (Raw.Id "Smod") [e1,e2] <- e = raw2BinOpWithType e1 e2 Op.ModOp FPSingle
  | Raw.Call (Raw.Id "Dmod") [e1,e2] <- e = raw2BinOpWithType e1 e2 Op.ModOp FPDouble
  | Raw.Call (Raw.Id f) args <- e
    = do
        env  <- askVarTypeEnv
        fenv <- askFunTypeEnv
        case lookup f fenv of
          Just fp ->
            do
              args' <- mapM raw2FAExprM args
              pure (PVS.FEFun False f ResValue fp args')
          Nothing ->
            case lookup f env of
              Just (Array _argsType t) ->
                do
                  args' <- mapM raw2FAExprM args
                  pure (FArrayElem t f args')
              _ -> error $ "[raw2FAExpr] identifier " ++ show f ++ " not found in env: " ++ show env
  | Raw.ExprAdd e1 e2 <- e = raw2BinOp e1 e2 Op.AddOp
  | Raw.ExprSub e1 e2 <- e = raw2BinOp e1 e2 Op.SubOp
  | Raw.ExprMul e1 e2 <- e = raw2BinOp e1 e2 Op.MulOp
  | Raw.ExprDiv e1 e2 <- e = raw2BinOp e1 e2 Op.DivOp
  | Raw.ExprPow e1 e2 <- e = raw2BinOp e1 e2 Op.PowOp
  | Raw.Let letElems stm <- e
    = do
        let f (accEnv,ls) x =
              local (const accEnv) $
                do
                  l@(v,x',_) <- raw2LetElemM x
                  let accEnv' = updateVarTypeEnv ((v,x'):) accEnv
                  pure (accEnv',ls++[l])
        allEnv <- ask
        (allEnv',ls') <- foldM f (allEnv,[]) letElems
        stm' <- local (const allEnv') $ raw2FAExprM stm
        pure (PVS.Let ls' stm')
  | Raw.If bexpr stmThen stmElse <- e
    = do
        env  <- askVarTypeEnv
        fenv <- askFunTypeEnv
        tc   <- askTypeContext
        let bexpr'   = raw2FBExpr tc env fenv bexpr
        stmThen' <- raw2FAExprM stmThen
        stmElse' <- raw2FAExprM stmElse
        pure (PVS.Ite bexpr' stmThen' stmElse')
  | ListIf be stmThen elsifs stmElse <- e
    = do
        be'      <- raw2FBExprM be
        stmThen' <- raw2FAExprM stmThen
        stmElse' <- raw2FAExprM stmElse
        elsifs'  <- mapM raw2Elsif elsifs
        pure (ListIte ((be',stmThen') : elsifs') stmElse')
  | TupleIndex (Id tuple) idx <- e
    = do
        env  <- askVarTypeEnv
        let t = fromMaybe (error $ "raw2FAExpr: tuple " ++ show tuple ++ " not found.")
                  (lookup tuple env)
        let fp = case t of
              Tuple idxTypes -> idxTypes !! fromInteger (idx - 1)
              _ -> error $ "raw2FAExpr: " ++ show t ++ "is not a tuple type."
        pure (FTupleElem fp tuple idx)
  | TupleFunIndex (Id f) args idx <- e
    = do
        fenv <- askFunTypeEnv
        let t = fromMaybe (error $ "raw2FAExpr: function " ++ show f ++ " not found.")
                      (lookup f fenv)
        let fp = case t of
              Tuple idxTypes -> idxTypes !! fromInteger (idx - 1)
              _ -> error $ "raw2FAExpr: " ++ show t ++ "is not a tuple type."
        args' <- mapM raw2FAExprM args
        pure (FEFun False f (ResTupleIndex idx) fp args')
  | RecordField (Id record) (Id field) <- e
    = do
        env  <- askVarTypeEnv
        let t = fromMaybe (error $ "raw2FAExpr: record " ++ show record ++ " not found.")
                      (lookup record env)
        let fp = case t of
              Record fieldTypes -> fromMaybe (error $ "raw2FAExpr: record field " ++ show field ++ " not found.")
                                              (lookup field fieldTypes)
              _ -> error $ "raw2FAExpr: " ++ show t ++ "is not a record type."
        pure (FRecordElem fp record field)
  | RecordFunField (Id f) args (Id field) <- e
    = do
        fenv <- askFunTypeEnv
        let t = fromMaybe (error $ "raw2FAExpr: function " ++ show f ++ " not found.") (lookup f fenv)
        let fp = case t of
              Record fieldTypes -> fromMaybe (error $ "raw2FAExpr: record field " ++ show field ++ " not found.")
                                              (lookup field fieldTypes)
              _ -> error $ "raw2FAExpr: " ++ show t ++ "is not a record type."
        args' <- mapM raw2FAExprM args
        pure (FEFun False f (ResRecordField field) fp args')
  | otherwise = error $ "raw2FAExpr: artihmetic expression expected but got " ++ show e ++ "."
  where
    getElementType :: VarTypeEnv -> String -> PVSType
    getElementType env name = case t of
      List t' -> t'
      _       -> error errorMsg
      where
        t = fromMaybe (error errorMsg) (lookup name env)
        errorMsg = "raw2FAExpr: list " ++ show name ++ " not found."

raw2LetElemM :: AbsRawPVSLang.LetElem -> M AbsPVSLang.FLetElem
raw2LetElemM (AbsRawPVSLang.LetElem x ae) = (raw2Id x, FPDouble,) <$> raw2FAExprM ae
raw2LetElemM (LetElemType x t ae) = (raw2Id x,,) <$> raw2FPTypeM t <*> raw2FAExprM ae

raw2Args :: TypeContext -> AbsRawPVSLang.Args -> [AbsPVSLang.Arg]
raw2Args tc a = runM (defaultTypeContextEnv tc) (raw2ArgsM a)

raw2ArgsM :: AbsRawPVSLang.Args -> M [AbsPVSLang.Arg]
raw2ArgsM (FArgs args) = concatMapM raw2Arg args
raw2ArgsM args@(FArgsNoType _) = error $ "[raw2Args] arguments have no type: " ++ show args

raw2Arg :: AbsRawPVSLang.Arg -> M [AbsPVSLang.Arg]
raw2Arg arg = mapM (raw2ArgWithType t') xs'
  where
    (xs',t') = getIdsAndType arg
      where
        getIdsAndType (FArg xs t)        = (xs,t)
        getIdsAndType (FArgGuard xs t _) = (xs,t)

raw2ArgWithType :: AbsRawPVSLang.Type -> AbsRawPVSLang.Id -> M AbsPVSLang.Arg
raw2ArgWithType t x =
  do
    ty <- raw2FPTypeM t
    pure $ AbsPVSLang.Arg (raw2Id x) ty

raw2FieldDecls :: TypeContext -> AbsRawPVSLang.FieldDecls -> (RecordField,PVSType)
raw2FieldDecls tc fd = runM (defaultTypeContextEnv tc) (raw2FieldDeclsM fd)

raw2FieldDeclsM :: AbsRawPVSLang.FieldDecls -> M (RecordField, PVSType)
raw2FieldDeclsM (FieldDecls (Id field) t) =
  do
    ty <- raw2FPTypeM t
    pure (field, ty)

raw2FPType :: TypeContext -> AbsRawPVSLang.Type -> PVSType
raw2FPType tc x = runM (defaultTypeContextEnv tc) (raw2FPTypeM x)

defaultTypeContextEnv :: TypeContext -> Env
defaultTypeContextEnv = Env [] []

raw2FPTypeM :: AbsRawPVSLang.Type -> M PVSType
raw2FPTypeM ty'
  | TypeBelow e <- ty' = Below <$> raw2FAExprM e
  | otherwise
    = do
        tc <- askTypeContext
        pure $ raw2FPType' tc ty'
  where
    raw2FPType' tc ty
      | (TypeSimple (Id "int"))            <- ty = TInt
      | (TypeSimple (Id "integer"))        <- ty = TInt
      | (TypeSimple (Id "single"))         <- ty = FPSingle
      | (TypeSimple (Id "unb_single"))     <- ty = FPSingle
      | (TypeSimple (Id "unb_pos_single")) <- ty = FPDouble
      | (TypeSimple (Id "unb_nz_single"))  <- ty = FPDouble
      | (TypeSimple (Id "double"))         <- ty = FPDouble
      | (TypeSimple (Id "unb_double"))     <- ty = FPDouble
      | (TypeSimple (Id "unb_pos_double")) <- ty = FPDouble
      | (TypeSimple (Id "unb_nz_double"))  <- ty = FPDouble
      | (TypeSimple (Id "bool"))           <- ty = Boolean
      | (TypeSimple (Id "real"))           <- ty = Real
      | TypeSimple (Id "nnreal")         <- ty = Real
      | TypeSimple (Id i)                <- ty =
        case lookup i tc of
          Just ty'' -> ty''
          Nothing -> error $ "[raw2FPType.raw2FPType'] simple type: " ++ show ty ++ " not found in type context: " ++ show tc
      | ParametricTypeBi _t _a _b <- ty = error "Fixed-point numbers not supported yet."
      | TypeRecord fieldDecls <- ty = Record (map (raw2FieldDecls tc) fieldDecls)
      | TypeTuple ts <- ty = Tuple (map (raw2FPType' tc) ts)
      | TypeArray ts t <- ty , all isIntType ts && isNumType t = Array (map (raw2FPType' tc) ts) (raw2FPType' tc t)
      | TypeArray _ _ <- ty = error $ "[raw2FPType.raw2FPType'] generic type of array is not supported: " ++ show ty
      | AbsRawPVSLang.TypeFun typeList retType <- ty  = AbsPVSLang.TypeFun (map (raw2FPType' tc) typeList) (raw2FPType' tc retType)
      | TypeFun2 typeList retType <- ty = AbsPVSLang.TypeFun (map (raw2FPType' tc) typeList) (raw2FPType' tc retType)
      | TypeList t <- ty = List (raw2FPType' tc t)
      | otherwise = error $ "[raw2FPType.raw2FPType'] unsupported type: " ++ show ty

toErrM :: Either String a -> Err a
toErrM (Left e) = Bad e
toErrM (Right a) = Ok a

rawparserPVS :: String -> Err AbsRawPVSLang.Program
rawparserPVS = toErrM . pProgram . tokens

raw2Id :: AbsRawPVSLang.Id -> VarName
raw2Id (AbsRawPVSLang.Id x) = x
