-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module AbsPVSLang where

import Data.Bits.Floating
import Data.Maybe(fromMaybe)
import PVSTypes
import Utils
import PPExt
import Prelude hiding ((<>))
import Operators
import Data.Set (fromList, toList)
import Data.Bifunctor (bimap,second)
import Common.TypesUtils
import Control.Monad.State
import Data.Either (fromLeft, fromRight)

type FunName = String
type EExpr = AExpr
type TightErr = Bool

data LetElem  = LetElem {letVar  :: VarName,
                         letType :: PVSType,
                         letExpr :: AExpr}
  deriving (Eq, Ord, Read, Show)

type FLetElem = (VarName,PVSType,FAExpr)

type IsTrans = Bool

data AExpr
 -- real arithmetic expressions
    = BinaryOp BinOp AExpr AExpr
    | UnaryOp UnOp  AExpr
    | FromFloat PVSType FAExpr
    | Int Integer
    | Rat Rational
    | EFun FunName PVSType [AExpr]
    | Var PVSType VarName
    | ArrayElem PVSType (Maybe ArraySize) VarName AExpr
    | Prec PVSType
    | FExp FAExpr
    | RealMark VarName
    | ErrorMark VarName PVSType
    | Min [AExpr]
    | Max [AExpr]
    | RLet [LetElem] AExpr
    | RIte BExpr AExpr AExpr
    | RListIte [(BExpr,AExpr)] AExpr
    | RForLoop PVSType AExpr AExpr AExpr VarName VarName AExpr
    | RUnstWarning
    | ErrFma   PVSType AExpr EExpr AExpr EExpr AExpr EExpr
    | ErrBinOp BinOp PVSType AExpr EExpr AExpr EExpr
    | ErrUnOp  UnOp  TightErr PVSType AExpr EExpr
    | ErrCast  PVSType PVSType AExpr EExpr
    | ErrMulPow2R PVSType Integer EExpr
    | ErrMulPow2L PVSType Integer EExpr
    | HalfUlp AExpr PVSType
    | ErrRat Rational
    | MaxErr [EExpr]
    | Infinity
    deriving (Eq, Ord, Read, Show)

data FAExpr
-- fp arithmetic expressions
    = FInt  Integer
    | FCnst PVSType Rational
    | FEFun IsTrans String PVSType [FAExpr]
    | FVar  PVSType VarName
    | StructVar PVSType String
    | FArrayElem PVSType (Maybe ArraySize) VarName FAExpr
    | TypeCast PVSType PVSType FAExpr
    | ToFloat  PVSType AExpr
    | Value FAExpr
    | BinaryFPOp BinOp PVSType FAExpr FAExpr
    | UnaryFPOp  UnOp  PVSType FAExpr
    | FFma PVSType FAExpr FAExpr FAExpr
    | FMin [FAExpr]
    | FMax [FAExpr]
    --------
    | Let [FLetElem] FAExpr
    | Ite FBExpr FAExpr FAExpr
    | ListIte [(FBExpr, FAExpr)] FAExpr
    | ForLoop PVSType FAExpr FAExpr FAExpr VarName VarName FAExpr
    -- ForLoop fp idxStart idxEnd initAcc idx acc forBody
    | UnstWarning
    deriving (Eq, Ord, Read, Show)

data BExpr
-- real valued boolean expressions
  = Or  BExpr BExpr
  | And BExpr BExpr
  | Not BExpr
  | Rel RelOp AExpr AExpr
  | BTrue
  | BFalse
  | EPred String [AExpr]
  deriving (Eq, Ord, Read, Show)

data BExprStm
  = RBLet [LetElem] BExprStm
  | RBIte BExpr BExprStm BExprStm
  | RBListIte [(BExpr,BExprStm)] BExprStm
  | RBExpr BExpr
  deriving (Eq, Ord, Read, Show)

data FBExpr
-- fp valued boolean expressions
  = FBTrue
  | FBFalse
  | FOr  FBExpr FBExpr
  | FAnd FBExpr FBExpr
  | FNot FBExpr
  | FRel RelOp FAExpr FAExpr
  | IsValid  FAExpr
  | BIsValid  FBExpr
  | BValue FBExpr
  | BStructVar VarName
  | FEPred IsTrans PredAbs String [FAExpr]
  deriving (Eq, Ord, Read, Show)

data FBExprStm
  = BLet [FLetElem] FBExprStm
  | BIte FBExpr FBExprStm FBExprStm
  | BListIte [(FBExpr,FBExprStm)] FBExprStm
  | BExpr FBExpr
  | BUnstWarning
  deriving (Eq, Ord, Read, Show)

-- progam
type Program = [Decl]

data Arg = Arg VarName PVSType
    deriving (Eq, Ord, Show, Read)

-- set of declarations
data Decl = Decl IsTrans PVSType  FunName [Arg] FAExpr
          | Pred IsTrans PredAbs FunName [Arg] FBExprStm
    deriving (Eq, Ord, Show, Read)

-- real valued progam
type RProgram = [RDecl]

-- real valued set of declarations
data RDecl = RDecl PVSType FunName [Arg] AExpr
           | RPred         FunName [Arg] BExprStm
    deriving (Eq, Ord, Show, Read)

varFLetElem :: FLetElem -> VarName
varFLetElem = fst3

typeFLetElem :: FLetElem -> PVSType
typeFLetElem = snd3

exprFLetElem :: FLetElem -> FAExpr
exprFLetElem = trd3

lookupFLetElem :: VarName -> [FLetElem] -> Maybe FAExpr
lookupFLetElem _ [] = Nothing
lookupFLetElem x ((y,_,expr):letelems) | x == y    = Just expr
                                       | otherwise = lookupFLetElem x letelems

isPred :: Decl -> Bool
isPred Pred{} = True
isPred _ = False

isDecl :: Decl -> Bool
isDecl Decl{} = True
isDecl _ = False

predAbstraction :: Decl -> Maybe PredAbs
predAbstraction Decl{} = Nothing
predAbstraction (Pred _ predAbs _ _ _) = Just predAbs

varName :: AExpr -> VarName
varName (Var _ x) = x
varName (RealMark x) = x
varName a = error $ "varName: not defined for " ++ show a

declName :: Decl -> FunName
declName (Decl _ _ f _ _) = f
declName (Pred _ _ f _ _) = f

declType :: Decl -> PVSType
declType (Decl _ fp _ _ _) = fp
declType Pred{} = Boolean

realDeclName :: RDecl -> FunName
realDeclName (RDecl _ f _ _) = f
realDeclName (RPred   f _ _) = f

fvarName :: FAExpr -> VarName
fvarName (FVar _ x) = x
fvarName a = error $ "fvarName: not defined for " ++ show a

declArgs :: Decl -> [Arg]
declArgs (Decl _ _ _ args _) = args
declArgs (Pred _ _ _ args _) = args

declBody :: Decl -> Either FAExpr FBExprStm
declBody (Decl _ _ _ _ body) = Left  body
declBody (Pred _ _ _ _ body) = Right body

realDeclBody :: RDecl -> Either AExpr BExprStm
realDeclBody (RDecl _ _ _ body) = Left  body
realDeclBody (RPred _ _   body) = Right body

realDeclArgs :: RDecl -> [Arg]
realDeclArgs (RDecl _ _ args _) = args
realDeclArgs (RPred _   args _) = args

applyFunToDeclBody :: (FAExpr -> a) -> (FBExprStm -> a) -> Decl -> a
applyFunToDeclBody fa fb d@(Decl _ _ _ _ aexpr) = fa aexpr
applyFunToDeclBody fa fb d@(Pred _ _ _ _ bexpr) = fb bexpr

applyFunToRDeclBody :: (AExpr -> a) -> (BExprStm -> a) -> RDecl -> a
applyFunToRDeclBody fa fb d@(RDecl _ _ _ aexpr) = fa aexpr
applyFunToRDeclBody fa fb d@(RPred _ _ bexpr) = fb bexpr

var2Arg :: VarName -> PVSType -> Arg
var2Arg = Arg

arg2var :: Arg -> FAExpr
arg2var (Arg x fp) = FVar fp x

arg2varWithType :: PVSType -> Arg -> FAExpr
arg2varWithType _  (Arg x TInt) = FVar TInt x
arg2varWithType fp (Arg x _)    = FVar fp x

arg2rvar :: Arg -> AExpr
arg2rvar (Arg x fp) = Var fp x

mapArg2Pair :: Arg -> (VarName, PVSType)
mapArg2Pair (Arg x fp) = (x,fp)

argName :: Arg -> VarName
argName (Arg x _) = x

nameArrayIdx :: VarName -> String
nameArrayIdx v = "idx_" ++ v

argPrec :: Arg -> PVSType
argPrec (Arg _ fp) = fp

isArgArray :: Arg -> Bool
isArgArray (Arg _ (Array _ _)) = True
isArgArray _ = False

isArgInt :: Arg -> Bool
isArgInt (Arg _ TInt) = True
isArgInt _ = False

isArgFP :: Arg -> Bool
isArgFP (Arg _ FPDouble) = True
isArgFP (Arg _ FPSingle) = True
isArgFP _ = False

isIntAExpr :: AExpr -> Bool
isIntAExpr (Int _)      = True
isIntAExpr (Var TInt _) = True
isIntAExpr (Rat n)      = toRational (floor (fromRational n :: Double) :: Integer) == n
isIntAExpr (UnaryOp  _      ae) = isIntAExpr ae
isIntAExpr (BinaryOp _ ae1 ae2) = isIntAExpr ae1 && isIntAExpr ae2
isIntAExpr (EFun _ TInt _) = True
isIntAExpr (Min aes) = foldl (\acc expr -> acc && isIntAExpr expr) True aes
isIntAExpr (Max aes) = foldl (\acc expr -> acc && isIntAExpr expr) True aes
isIntAExpr (RLet _ ae) = isIntAExpr ae
isIntAExpr (RIte _ thenExpr elseExpr) = isIntAExpr thenExpr && isIntAExpr elseExpr
isIntAExpr (RListIte listThen elseExpr) = foldl aux True (map snd listThen) && isIntAExpr elseExpr
  where
    aux b expr = b && isIntAExpr expr
isIntAExpr (RForLoop _ _ _ _ _ _ body) = isIntAExpr body
isIntAExpr _ = False

isFloatingFAExpr :: FAExpr -> Bool
isFloatingFAExpr expr = (getPVSType expr == FPSingle) || (getPVSType expr == FPDouble)

isIntFAExpr :: FAExpr -> Bool
isIntFAExpr (FInt _)      = True
isIntFAExpr (FVar TInt _) = True
isIntFAExpr  UnstWarning  = True
isIntFAExpr (FCnst _ n)   = toRational (floor (fromRational n :: Double) :: Integer) == n
isIntFAExpr (FArrayElem TInt _ _ _) = True
isIntFAExpr (Value ae) = isIntFAExpr ae
isIntFAExpr (UnaryFPOp  _ _      ae) = isIntFAExpr ae
isIntFAExpr (BinaryFPOp _ _ ae1 ae2) = isIntFAExpr ae1 && isIntFAExpr ae2
isIntFAExpr (FFma _ ae1 ae2 ae3) = isIntFAExpr ae1 && isIntFAExpr ae2 && isIntFAExpr ae3
isIntFAExpr (FMin aes) = foldl (\acc expr -> acc && isIntFAExpr expr) True aes
isIntFAExpr (FMax aes) = foldl (\acc expr -> acc && isIntFAExpr expr) True aes
isIntFAExpr (FEFun _ _ TInt _) = True
isIntFAExpr (Let _ ae) = isIntFAExpr ae
isIntFAExpr (Ite _ thenExpr elseExpr) = isIntFAExpr thenExpr && isIntFAExpr elseExpr
isIntFAExpr (ListIte listThen elseExpr) = foldl aux True (map snd listThen) && isIntFAExpr elseExpr
  where
    aux b expr = b && isIntFAExpr expr
isIntFAExpr (ForLoop _ _ _ _ _ _ body) = isIntFAExpr body
isIntFAExpr _ = False

unaryOpPVSType :: PVSType -> AExpr -> PVSType
unaryOpPVSType fp ae | isIntAExpr ae = TInt
                     | otherwise     = fp

binaryOpPVSType :: PVSType -> AExpr ->  AExpr -> PVSType
binaryOpPVSType fp ae1 ae2 | isIntAExpr ae1 && isIntAExpr ae2 = TInt
                         | otherwise     = fp

ternaryOpPVSType :: PVSType -> AExpr -> AExpr -> AExpr -> PVSType
ternaryOpPVSType fp ae1 ae2 ae3 | isIntAExpr ae1 && isIntAExpr ae2 && isIntAExpr ae3 = TInt
                              | otherwise     = fp

isStm :: FAExpr -> Bool
isStm Let{} = True
isStm Ite{}     = True
isStm ListIte{} = True
isStm ForLoop{} = True
isStm _ = False

isNum :: FAExpr -> Bool
isNum (FInt _)    = True
isNum (FCnst _ _) = True
isNum _ = False

isArithExpr :: FAExpr -> Bool
isArithExpr ae = foldFAExpr const isArithExpr' const ae True
  where
    isArithExpr' b (FInt _)        = b && True
    isArithExpr' b (FCnst _ _)     = b && True
    isArithExpr' b (FEFun _ _ _ _)   = b && True
    isArithExpr' b (FVar _ _)      = b && True
    isArithExpr' b (StructVar _ _) = b && True
    isArithExpr' b (FArrayElem{})  = b && True
    isArithExpr' b (TypeCast{})    = b && True
    isArithExpr' b (ToFloat _ _)   = b && True
    isArithExpr' b (Value _)       = b && True
    isArithExpr' b (BinaryFPOp{})  = b && True
    isArithExpr' b (UnaryFPOp{})   = b &&  True
    isArithExpr' b (FFma{})        = b && True
    isArithExpr' b (FMin _)        = b && True
    isArithExpr' b (FMax _)        = b &&  True
    isArithExpr' _ _ = False

isListArithExprs :: [FAExpr] -> Bool
isListArithExprs = foldr (\a b -> b && isArithExpr a) True

areLetElemsArithExps :: [FLetElem] -> Bool
areLetElemsArithExps letElems = isListArithExprs (map exprFLetElem letElems)

argCast :: PVSType -> Arg -> Arg
argCast _  (Arg x TInt) = Arg x TInt
argCast fp (Arg x _) = Arg x fp

findInDecls :: String -> [Decl] -> Maybe (PVSType,[Arg], Either FAExpr FBExprStm)
findInDecls _ [] = Nothing
findInDecls fun (Decl _ retType g args stm:ds) | fun==g = Just (retType,args,Left stm)
                                               | otherwise = findInDecls fun ds
findInDecls fun (Pred _ _ g args be:ds) | fun==g = Just (Boolean,args,Right be)
                                        | otherwise = findInDecls fun ds

findInProg :: String -> [Decl] -> Maybe Decl
findInProg _ [] = Nothing
findInProg fun (decl@(Decl _ _ g _ _):ds) | fun==g = Just decl
                                          | otherwise = findInProg fun ds
findInProg fun (decl@(Pred _ _ g _ _):ds) | fun==g = Just decl
                                          | otherwise = findInProg fun ds

findInRealProg :: String -> [RDecl] -> Maybe RDecl
findInRealProg _ [] = Nothing
findInRealProg fun (decl@(RDecl _ g _ _):ds) | fun==g = Just decl
                                             | otherwise = findInRealProg fun ds
findInRealProg fun (decl@(RPred g _ _):ds) | fun==g = Just decl
                                           | otherwise = findInRealProg fun ds

makeFPDeclRecursive :: Decl -> (Decl, [Decl])
makeFPDeclRecursive (Decl isTrans fp f args stm) = (Decl isTrans fp f args recStm, forList)
  where
    (recStm, forList) = runState (replaceForWithFPCallStm isTrans f args stm) []
makeFPDeclRecursive decl@Pred{} = (decl,[])

replaceForWithFPCallStm :: IsTrans -> FunName -> [Arg] -> FAExpr -> State [Decl] FAExpr
replaceForWithFPCallStm isTrans f args (ForLoop fp idxInit idxEnd accInit idx acc body) = do
   currentState <- get
   let n = 1 + length currentState
   let fRec = forFunName f n
   bodyRec <- replaceForWithFPCallStm isTrans f (Arg idx TInt: Arg acc fp:args) body
 --  let funCallIth = FEFun fRec fp (BinaryFPOp SubOp (FVar TInt idx) (Int 1) : (FVar fp acc) : map arg2rvar args)
   put (makeForFPRecFun isTrans fRec fp idx idxEnd acc bodyRec args :currentState)
   return $ FEFun isTrans fRec fp (idxInit : accInit : map arg2var args)

replaceForWithFPCallStm isTrans f args (Let letElems stm) = do
  recStm <- replaceForWithFPCallStm isTrans f args stm
  return $ Let letElems recStm

replaceForWithFPCallStm isTrans f args (Ite be thenStm elseStm) = do
  recStmThen <- replaceForWithFPCallStm isTrans f args thenStm
  recStmElse <- replaceForWithFPCallStm isTrans f args elseStm
  return $ Ite be recStmThen recStmElse

replaceForWithFPCallStm isTrans f args (ListIte listThen elseStm) = do
  recListThen <- mapM (replaceForWithFPCallStm isTrans f args . snd) listThen
  let listRecThen = zip (map fst listThen) recListThen
  recStmElse <- replaceForWithFPCallStm isTrans f args elseStm
  return $ ListIte listRecThen recStmElse

replaceForWithFPCallStm _ _ _ a = return a

makeForFPRecFun :: IsTrans -> FunName -> PVSType -> VarName -> FAExpr -> VarName -> FAExpr -> [Arg] -> Decl
makeForFPRecFun isTrans fRec fp idx idxEnd acc body args = Decl isTrans fp fRec (Arg idx TInt: Arg acc fp :args) recBody
  where
    nextIdx  = BinaryFPOp AddOp fp (FVar TInt idx) (FInt 1)
    recBody = Ite (FRel Eq (FVar TInt idx) idxEnd)
                   (FVar fp acc)
                   (FEFun isTrans fRec fp (nextIdx : body : map arg2var args))

makeRealDeclRecursive :: RDecl -> (RDecl, ([RDecl],[(AExpr,AExpr)]))
makeRealDeclRecursive (RDecl fp f args stm) = (RDecl fp f args recStm, forList)
  where
    (recStm, forList) = runState (replaceForWithRealCallStm f args stm) ([],[])
makeRealDeclRecursive decl@RPred{} = (decl,([],[]))

replaceForWithRealCallStm :: FunName -> [Arg] -> AExpr -> State ([RDecl],[(AExpr,AExpr)]) AExpr
replaceForWithRealCallStm f args forStm@(RForLoop fp idxInit idxEnd accInit idx acc body) = do
   (currentState, forListExpr) <- get
   let n = 1 + length currentState
   let fRec = forFunName f n
   bodyRec <- replaceForWithRealCallStm f (Arg idx TInt: Arg acc fp:args) body
   let funCallIth = EFun fRec fp (BinaryOp SubOp (Var TInt idx) (Int 1) : Var Real acc : map arg2rvar args)
   put (makeForRealRecFun fRec fp idx idxEnd acc bodyRec args :currentState,
        (forStm, funCallIth):forListExpr)
   return $ EFun fRec fp (idxInit : accInit : map arg2rvar args)

replaceForWithRealCallStm f args (RLet letElems stm) = do
  recStm <- replaceForWithRealCallStm f args stm
  return $ RLet letElems recStm

replaceForWithRealCallStm f args (RIte be thenStm elseStm) = do
  recStmThen <- replaceForWithRealCallStm f args thenStm
  recStmElse <- replaceForWithRealCallStm f args elseStm
  return $ RIte be recStmThen recStmElse

replaceForWithRealCallStm f args (RListIte listThen elseStm) = do
  recListThen <- mapM (replaceForWithRealCallStm f args . snd) listThen
  let listThen' = zip (map fst listThen) recListThen
  recStmElse <- replaceForWithRealCallStm f args elseStm
  return $ RListIte listThen' recStmElse

replaceForWithRealCallStm _ _ a = return a

makeForRealRecFun :: FunName -> PVSType -> VarName -> AExpr -> VarName -> AExpr -> [Arg] -> RDecl
makeForRealRecFun fRec fp idx idxEnd acc body args = RDecl fp fRec (Arg idx TInt: Arg acc fp :args) recBody
  where
    nextIdx  = BinaryOp AddOp (Var TInt idx) (Int 1)
    recBody = RIte (Rel Eq (Var TInt idx) idxEnd)
                    (Var fp acc)
                    (EFun fRec fp (nextIdx : body : map arg2rvar args))

forFunName :: String -> Int -> String
forFunName f n = "for_"++ f ++ show n

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

isBExprEquivFalse :: BExpr -> Bool
isBExprEquivFalse BFalse = True
isBExprEquivFalse (And b1 b2) = isBExprEquivFalse b1 || isBExprEquivFalse b2
isBExprEquivFalse (Or  b1 b2) = isBExprEquivFalse b1 && isBExprEquivFalse b2
isBExprEquivFalse b = any (flip elem bs . Not) bs
    where
        bs = flatAnd b

isFBExprEquivFalse :: FBExpr -> Bool
isFBExprEquivFalse FBFalse = True
isFBExprEquivFalse (FAnd b1 b2) = isFBExprEquivFalse b1 || isFBExprEquivFalse b2
isFBExprEquivFalse (FOr  b1 b2) = isFBExprEquivFalse b1 && isFBExprEquivFalse b2
isFBExprEquivFalse b = any (flip elem bs . FNot) bs
    where
        bs = flatFAnd b

flatAnd :: BExpr -> [BExpr]
flatAnd (And b1 b2) = flatAnd b1 ++ flatAnd b2
flatAnd b = [b]

flatFAnd :: FBExpr -> [FBExpr]
flatFAnd (FAnd b1 b2) = flatFAnd b1 ++ flatFAnd b2
flatFAnd b = [b]

simplFAnd :: FBExpr -> FBExpr -> FBExpr
simplFAnd FBTrue  be      = be
simplFAnd be      FBTrue  = be
simplFAnd FBFalse _       = FBFalse
simplFAnd _       FBFalse = FBFalse
simplFAnd be1     be2     = FAnd be1 be2

listFAnd :: [FBExpr] -> FBExpr
listFAnd [] = FBTrue
listFAnd [be] = be
listFAnd bes  = foldl1 FAnd bes

listFOr :: [FBExpr] -> FBExpr
listFOr [] = FBFalse
listFOr [be] = be
listFOr bes  = foldl1 FOr bes

listAnd :: [BExpr] -> BExpr
listAnd [] = BTrue
listAnd [be] = be
listAnd bes  = foldl1 And bes

listOr :: [BExpr] -> BExpr
listOr [] = BFalse
listOr [be] = be
listOr bes  = foldl1 Or bes


getPVSType :: FAExpr -> PVSType
getPVSType (FInt  _)             = TInt
getPVSType (FCnst     fp _)      = fp
getPVSType (FVar      fp _)      = fp
getPVSType (StructVar fp _)      = fp
getPVSType (FEFun _ _ fp _)      = fp
getPVSType (UnaryFPOp  _ fp _  ) = fp
getPVSType (BinaryFPOp _ fp _ _) = fp
getPVSType (FFma fp _ _ _)       = fp
getPVSType (TypeCast _ fp _  )   = fp
getPVSType (ToFloat      fp _  ) = fp
getPVSType (Let _ stm)           = getPVSType stm
getPVSType (Ite _ thenExpr elseExpr)   = getPVSType thenExpr `lubPVSType` getPVSType elseExpr
getPVSType (ListIte [] _) =  error "getPVSType: empty list of then branches."
getPVSType (ListIte listThen elseExpr) =  foldl1 lubPVSType (map (getPVSType . snd) listThen)
                                          `lubPVSType`
                                          getPVSType elseExpr
getPVSType (ForLoop fp _ _ _ _ _ _) = fp
getPVSType UnstWarning = error "getPVSType undefined for UnstWarning."
getPVSType ae = error $ "getPVSType niy for "++ show ae

--------------------------------------------
-- semantic equivalence error expressions --
--------------------------------------------

rewriteEquivEExpr :: EExpr -> EExpr
rewriteEquivEExpr = replaceInAExpr rewriteEquivEExpr' (const Nothing)
  where
    rewriteEquivEExpr' (ErrMulPow2L _ _ ee) = Just $ rewriteEquivEExpr ee
    rewriteEquivEExpr' (ErrMulPow2R _ _ ee) = Just $ rewriteEquivEExpr ee
    rewriteEquivEExpr' (ErrUnOp AbsOp _ _ _ ee) = Just $ rewriteEquivEExpr ee
    rewriteEquivEExpr' (ErrUnOp NegOp _ _ _ ee) = Just $ rewriteEquivEExpr ee
    rewriteEquivEExpr' (ErrUnOp  tight op fp ae ee)      = Just $ ErrUnOp tight op fp ae (rewriteEquivEExpr ee)
    rewriteEquivEExpr' (ErrBinOp op fp ae1 ee1 ae2 ee2)  = Just $ ErrBinOp op fp ae1 (rewriteEquivEExpr ee1) ae2 (rewriteEquivEExpr ee2)
    rewriteEquivEExpr' (MaxErr       ees)
      | and $ zipWith (==) ees' $ tail ees' = Just $ head ees'
      | otherwise = Just $ MaxErr ees'
       where
         ees' = map rewriteEquivEExpr ees
    rewriteEquivEExpr' _ = Nothing

equivEExpr :: EExpr -> EExpr -> Bool
equivEExpr ee ee' = rewriteEquivEExpr ee == rewriteEquivEExpr ee'

---------------------------------
-- fp bool expr simplification --
---------------------------------

simplFBExprFix :: FBExpr -> FBExpr
simplFBExprFix be =
  if be' == be
  then be
  else simplFBExprFix be'
  where
    be' = simplFBExpr be

simplFBExpr :: FBExpr -> FBExpr
simplFBExpr (FAnd FBTrue c)  = simplFBExpr c
simplFBExpr (FAnd c FBTrue)  = simplFBExpr c
simplFBExpr (FAnd FBFalse _) = FBFalse
simplFBExpr (FAnd _ FBFalse) = FBFalse
simplFBExpr (FAnd c1 c2)     = FAnd (simplFBExpr c1) (simplFBExpr c2)
simplFBExpr (FOr FBTrue _)   = FBTrue
simplFBExpr (FOr _ FBTrue)   = FBTrue
simplFBExpr (FOr FBFalse c)  = simplFBExpr c
simplFBExpr (FOr c FBFalse)  = simplFBExpr c
simplFBExpr (FOr c1 c2)      = FOr (simplFBExpr c1) (simplFBExpr c2)
simplFBExpr (FNot (FNot c))  = simplFBExpr c
simplFBExpr (FNot FBTrue)    = FBFalse
simplFBExpr (FNot FBFalse)   = FBTrue
simplFBExpr (FNot c)         = FNot $ simplFBExpr c
simplFBExpr rel@FRel{}       = simplFRel rel
simplFBExpr p@FEPred{}       = p
simplFBExpr FBTrue           = FBTrue
simplFBExpr FBFalse          = FBFalse
simplFBExpr be@(IsValid _)   = be
simplFBExpr (BIsValid be)    = BIsValid $ simplFBExpr be
simplFBExpr (BValue be)      = BValue   $ simplFBExpr be
simplFBExpr v@(BStructVar _) = v


simplFRel :: FBExpr -> FBExpr
simplFRel (FRel Eq a1 a2) = case a1 of
    (FInt n)    -> case a2 of
                    (FInt m) ->  if n == m then FBTrue else FBFalse
                    (FCnst _ m) ->  if fromIntegral n == m then FBTrue else FBFalse
                    _ -> FRel Eq a1 a2 -- (simplIAExpr a2)
    (FCnst _ n) -> case a2 of
                    (FInt m) ->  if n == fromIntegral m then FBTrue else FBFalse
                    (FCnst _ m) ->  if n == m then FBTrue else FBFalse
                    _ -> FRel Eq a1 a2 -- (simplIAExpr a2)
    _           -> FRel Eq a1 a2 -- (simplFAExpr a1) (simplFAExpr a2)
simplFRel (FRel Neq a1 a2) = case a1 of
    (FInt n)    -> case a2 of
                    (FInt m) ->  if n /= m then FBTrue else FBFalse
                    (FCnst _ m) ->  if fromIntegral n /= m then FBTrue else FBFalse
                    _ -> FRel Neq a1 (simplFAExpr a2)
    (FCnst _ n) -> case a2 of
                    (FInt m) ->  if n /= fromIntegral m then FBTrue else FBFalse
                    (FCnst _ m) ->  if n /= m then FBTrue else FBFalse
                    _ -> FRel Neq a1 (simplFAExpr a2)
    _           ->  FRel Neq (simplFAExpr a1) (simplFAExpr a2)
simplFRel (FRel Lt a1 a2) = case a1 of
    (FInt n)    -> case a2 of
                    (FInt m) ->  if n < m then FBTrue else FBFalse
                    (FCnst _ m) ->  if fromIntegral n < m then FBTrue else FBFalse
                    _ -> FRel Lt a1 (simplFAExpr a2)
    (FCnst _ n)  -> case a2 of
                    (FInt m) ->  if n < fromIntegral m then FBTrue else FBFalse
                    (FCnst _ m) ->  if n < m then FBTrue else FBFalse
                    _ -> FRel Lt a1 (simplFAExpr a2)
    _           -> FRel Lt (simplFAExpr a1) (simplFAExpr a2)
simplFRel (FRel LtE a1 a2) = case a1 of
    (FInt n)    -> case a2 of
                    (FInt m) ->  if n <= m then FBTrue else FBFalse
                    (FCnst _ m) ->  if fromIntegral n <= m then FBTrue else FBFalse
                    _ -> FRel LtE (simplFAExpr a1) (simplFAExpr a2)
    (FCnst _ n) -> case a2 of
                    (FInt m) ->  if n <= fromIntegral m then FBTrue else FBFalse
                    (FCnst _ m) ->  if n <= m then FBTrue else FBFalse
                    _ -> FRel LtE a1 (simplFAExpr a2)
    _          -> FRel LtE (simplFAExpr a1) (simplFAExpr a2)
simplFRel (FRel Gt a1 a2) = case a1 of
    (FInt n)    -> case a2 of
                    (FInt m) ->  if n > m then FBTrue else FBFalse
                    (FCnst _ m) ->  if fromIntegral n > m then FBTrue else FBFalse
                    _ -> FRel Gt a1 (simplFAExpr a2)
    (FCnst _ n) -> case a2 of
                    (FInt m) ->  if n > fromIntegral m then FBTrue else FBFalse
                    (FCnst _ m) ->  if n > m then FBTrue else FBFalse
                    _ -> FRel Gt a1 (simplFAExpr a2)
    _          -> FRel Gt (simplFAExpr a1) (simplFAExpr a2)
simplFRel (FRel GtE a1 a2) = case a1 of
    (FInt n)    -> case a2 of
                    (FInt m) ->  if n >= m then FBTrue else FBFalse
                    (FCnst _ m) ->  if fromIntegral n >= m then FBTrue else FBFalse
                    _ -> FRel GtE a1 (simplFAExpr a2)
    (FCnst _ n) -> case a2 of
                    (FInt m) ->  if n >= fromIntegral m then FBTrue else FBFalse
                    (FCnst _ m) ->  if n >= m then FBTrue else FBFalse
                    _ -> FRel GtE a1 (simplFAExpr a2)
    _          -> FRel GtE (simplFAExpr a1) (simplFAExpr a2)

simplFRel be = error $ "simplFRel: unexpected value: "++ show be

-----------------------------------
-- real bool expr simplification --
-----------------------------------

simplBExprFix :: BExpr -> BExpr
simplBExprFix be =
  if be' == be
  then be
  else simplBExprFix be'
  where
    be' = simplBExpr be

simplBExpr :: BExpr -> BExpr
simplBExpr (And BTrue c)  = simplBExpr c
simplBExpr (And c BTrue)  = simplBExpr c
simplBExpr (And BFalse _) = BFalse
simplBExpr (And _ BFalse) = BFalse
simplBExpr (And c1 c2)    = And (simplBExpr c1) (simplBExpr c2)
simplBExpr (Or BTrue _)   = BTrue
simplBExpr (Or _ BTrue)   = BTrue
simplBExpr (Or BFalse c)  = simplBExpr c
simplBExpr (Or c BFalse)  = simplBExpr c
simplBExpr (Or c1 c2)     = Or (simplBExpr c1) (simplBExpr c2)
simplBExpr (Not (Not c))  = simplBExpr c
simplBExpr (Not BTrue)    = BFalse
simplBExpr (Not BFalse)   = BTrue
simplBExpr (Not c)        = Not $ simplBExpr c
simplBExpr rel@Rel{}      = simplRel rel
simplBExpr p@(EPred _ _)  = p
simplBExpr BTrue          = BTrue
simplBExpr BFalse         = BFalse

simplRel :: BExpr -> BExpr
simplRel (Rel Eq a1 a2) = case a1 of
    (Int n)    -> case a2 of
                    (Int m) ->  if n == m then BTrue else BFalse
                    (Rat m) ->  if fromIntegral n == m then BTrue else BFalse
                    _ -> Rel Eq a1 (simplAExpr a2)
    (Rat n) -> case a2 of
                    (Int m) ->  if n == fromIntegral m then BTrue else BFalse
                    (Rat m) ->  if n == m then BTrue else BFalse
                    _ -> Rel Eq a1 (simplAExpr a2)
    _          -> Rel Eq (simplAExpr a1) (simplAExpr a2)
simplRel (Rel Neq a1 a2) = case a1 of
    (Int n)    -> case a2 of
                    (Int m) ->  if n /= m then BTrue else BFalse
                    (Rat m) ->  if fromIntegral n /= m then BTrue else BFalse
                    _ -> Rel Neq a1 (simplAExpr a2)
    (Rat n) -> case a2 of
                    (Int m) ->  if n /= fromIntegral m then BTrue else BFalse
                    (Rat m) ->  if n /= m then BTrue else BFalse
                    _ -> Rel Neq a1 (simplAExpr a2)
    _          -> Rel Neq (simplAExpr a1) (simplAExpr a2)
simplRel (Rel Lt a1 a2) = case a1 of
    (Int n)    -> case a2 of
                    (Int m) ->  if n < m then BTrue else BFalse
                    (Rat m) ->  if fromIntegral n < m then BTrue else BFalse
                    _ -> Rel Lt a1 (simplAExpr a2)
    (Rat n)  -> case a2 of
                    (Int m) ->  if n < fromIntegral m then BTrue else BFalse
                    (Rat m) ->  if n < m then BTrue else BFalse
                    _ -> Rel Lt a1 (simplAExpr a2)
    _           -> Rel Lt (simplAExpr a1) (simplAExpr a2)
simplRel (Rel LtE a1 a2) = case a1 of
    (Int n)    -> case a2 of
                    (Int m) ->  if n <= m then BTrue else BFalse
                    (Rat m) ->  if fromIntegral n <= m then BTrue else BFalse
                    _ -> Rel LtE a1 (simplAExpr a2)
    (Rat n) -> case a2 of
                    (Int m) ->  if n <= fromIntegral m then BTrue else BFalse
                    (Rat m) ->  if n <= m then BTrue else BFalse
                    _ -> Rel LtE a1 (simplAExpr a2)
    _          -> Rel LtE (simplAExpr a1) (simplAExpr a2)
simplRel (Rel Gt a1 a2) = case a1 of
    (Int n)    -> case a2 of
                    (Int m) ->  if n > m then BTrue else BFalse
                    (Rat m) ->  if fromIntegral n > m then BTrue else BFalse
                    _ -> Rel Gt a1 (simplAExpr a2)
    (Rat n) -> case a2 of
                    (Int m) ->  if n > fromIntegral m then BTrue else BFalse
                    (Rat m) ->  if n > m then BTrue else BFalse
                    _ -> Rel Gt a1 (simplAExpr a2)
    _          -> Rel Gt (simplAExpr a1) (simplAExpr a2)
simplRel (Rel GtE a1 a2) = case a1 of
    (Int n)    -> case a2 of
                    (Int m) ->  if n >= m then BTrue else BFalse
                    (Rat m) ->  if fromIntegral n >= m then BTrue else BFalse
                    _ -> Rel GtE a1 (simplAExpr a2)
    (Rat n) -> case a2 of
                    (Int m) ->  if n >= fromIntegral m then BTrue else BFalse
                    (Rat m) ->  if n >= m then BTrue else BFalse
                    _ -> Rel GtE a1 (simplAExpr a2)
    _          -> Rel GtE (simplAExpr a1) (simplAExpr a2)

simplRel be = error $ "simplRel: unexpected value: "++ show be

------------------------------------
-- real arith expr simplification --
------------------------------------

simplAExpr :: AExpr -> AExpr
simplAExpr ae =
  if ae' == ae
  then ae
  else simplAExpr ae'
  where
    ae' = simplAExprAux ae

simplAExprAux :: AExpr -> AExpr
simplAExprAux = replaceInAExpr simplAExprAux' simplFAExprAux'

simplAExprAux' :: AExpr -> Maybe AExpr
simplAExprAux' (BinaryOp AddOp a       (Int 0)) = Just $ simplAExprAux a
simplAExprAux' (BinaryOp AddOp (Int 0)       a) = Just $ simplAExprAux a
simplAExprAux' (BinaryOp AddOp a       (Rat 0)) = Just $ simplAExprAux a
simplAExprAux' (BinaryOp AddOp (Rat 0)       a) = Just $ simplAExprAux a
simplAExprAux' (BinaryOp AddOp (Int n) (Int m)) = Just $ Int (n+m)
simplAExprAux' (BinaryOp AddOp (Int n) (Rat m)) = Just $ Rat (fromIntegral n + m)
simplAExprAux' (BinaryOp AddOp (Rat n) (Int m)) = Just $ Rat (n + fromIntegral m)
simplAExprAux' (BinaryOp AddOp (Rat n) (Rat m)) = Just $ Rat (n+m)
simplAExprAux' (BinaryOp SubOp a       (Int 0)) = Just $ simplAExprAux a
simplAExprAux' (BinaryOp SubOp (Int 0)       a) = Just $ UnaryOp NegOp (simplAExprAux a)
simplAExprAux' (BinaryOp SubOp a       (Rat 0)) = Just $ simplAExprAux a
simplAExprAux' (BinaryOp SubOp (Rat 0)       a) = Just $ UnaryOp NegOp (simplAExprAux a)
simplAExprAux' (BinaryOp SubOp (Int n) (Int m)) = Just $ Int (n-m)
simplAExprAux' (BinaryOp SubOp (Int n) (Rat m)) = Just $ Rat (fromIntegral n - m)
simplAExprAux' (BinaryOp SubOp (Rat n) (Int m)) = Just $ Rat (n - fromIntegral m)
simplAExprAux' (BinaryOp SubOp (Rat n) (Rat m)) = Just $ Rat (n-m)
simplAExprAux' (BinaryOp MulOp      a  (Int 1)) = Just $ simplAExprAux a
simplAExprAux' (BinaryOp MulOp (Int 1)       a) = Just $ simplAExprAux a
simplAExprAux' (BinaryOp MulOp      a  (Rat 1)) = Just $ simplAExprAux a
simplAExprAux' (BinaryOp MulOp (Rat 1)       a) = Just $ simplAExprAux a
simplAExprAux' (BinaryOp MulOp (Int 0)      _ ) = Just $ Int 0
simplAExprAux' (BinaryOp MulOp      _  (Int 0)) = Just $ Int 0
simplAExprAux' (BinaryOp MulOp (Rat 0)      _ ) = Just $ Rat 0
simplAExprAux' (BinaryOp MulOp      _  (Rat 0)) = Just $ Rat 0
simplAExprAux' (BinaryOp MulOp (Int n) (Int m)) = Just $ Int (n*m)
simplAExprAux' (BinaryOp MulOp (Int n) (Rat m)) = Just $ Rat (fromIntegral n * m)
simplAExprAux' (BinaryOp MulOp (Rat n) (Int m)) = Just $ Rat (n * fromIntegral m)
simplAExprAux' (BinaryOp MulOp (Rat n) (Rat m)) = Just $ Rat (n*m)
simplAExprAux' (UnaryOp  NegOp (Int n))         = Just $ Int (-n)
simplAExprAux' (MaxErr aes) = if null res then Just $ Int 0 else Just $ MaxErr res
  where
    res = filter (not . isZeroAExpr) aes
simplAExprAux' _ = Nothing

----------------------------------
-- fp arith expr simplification --
----------------------------------

simplFAExpr :: FAExpr -> FAExpr
simplFAExpr ae =
  if ae' == ae
  then ae
  else simplFAExpr ae'
  where
    ae' = simplFAExprAux ae

simplFAExprAux :: FAExpr -> FAExpr
simplFAExprAux = replaceInFAExpr simplAExprAux' simplFAExprAux'

simplFAExprAux' :: FAExpr -> Maybe FAExpr
simplFAExprAux' (BinaryFPOp AddOp  _          a  (FInt    0)) = Just $ simplFAExprAux a
simplFAExprAux' (BinaryFPOp AddOp  _ (FInt    0)          a ) = Just $ simplFAExprAux a
simplFAExprAux' (BinaryFPOp AddOp  _          a  (FCnst _ 0)) = Just $ simplFAExprAux a
simplFAExprAux' (BinaryFPOp AddOp  _ (FCnst _ 0)          a ) = Just $ simplFAExprAux a
simplFAExprAux' (BinaryFPOp AddOp  _ (FInt    n) (FInt    m)) = Just $ FInt (n+m)
simplFAExprAux' (BinaryFPOp AddOp fp (FInt    n) (FCnst _ m)) = Just $ FCnst fp (fromIntegral n + m)
simplFAExprAux' (BinaryFPOp AddOp fp (FCnst _ n) (FInt    m)) = Just $ FCnst fp (n + fromIntegral m)
simplFAExprAux' (BinaryFPOp AddOp fp (FCnst _ n) (FCnst _ m)) = Just $ FCnst fp (n+m)
simplFAExprAux' (BinaryFPOp SubOp  _          a  (FInt    0)) = Just $ simplFAExprAux a
simplFAExprAux' (BinaryFPOp SubOp fp (FInt    0)          a ) = Just $ UnaryFPOp NegOp fp $ simplFAExprAux a
simplFAExprAux' (BinaryFPOp SubOp  _          a  (FCnst _ 0)) = Just $ simplFAExprAux a
simplFAExprAux' (BinaryFPOp SubOp fp (FCnst _ 0)          a ) = Just $ UnaryFPOp NegOp fp $ simplFAExprAux a
simplFAExprAux' (BinaryFPOp SubOp  _ (FInt    n) (FInt    m)) = Just $ FInt (n-m)
simplFAExprAux' (BinaryFPOp SubOp fp (FInt    n) (FCnst _ m)) = Just $ FCnst fp (fromIntegral n - m)
simplFAExprAux' (BinaryFPOp SubOp fp (FCnst _ n) (FInt    m)) = Just $ FCnst fp (n - fromIntegral m)
simplFAExprAux' (BinaryFPOp SubOp fp (FCnst _ n) (FCnst _ m)) = Just $ FCnst fp (n-m)
simplFAExprAux' (BinaryFPOp MulOp  _          a  (FInt    1)) = Just $ simplFAExprAux a
simplFAExprAux' (BinaryFPOp MulOp  _ (FInt    1)          a ) = Just $ simplFAExprAux a
simplFAExprAux' (BinaryFPOp MulOp  _          a  (FCnst _ 1)) = Just $ simplFAExprAux a
simplFAExprAux' (BinaryFPOp MulOp  _ (FCnst _ 1)          a ) = Just $ simplFAExprAux a
simplFAExprAux' (BinaryFPOp MulOp  _ (FInt    0)          _ ) = Just $ FInt 0
simplFAExprAux' (BinaryFPOp MulOp  _          _  (FInt    0)) = Just $ FInt 0
simplFAExprAux' (BinaryFPOp MulOp fp (FCnst _ 0)          _ ) = Just $ FCnst fp 0
simplFAExprAux' (BinaryFPOp MulOp fp          _  (FCnst _ 0)) = Just $ FCnst fp 0
simplFAExprAux' (BinaryFPOp MulOp  _ (FInt    n) (FInt    m)) = Just $ FInt (n*m)
simplFAExprAux' (BinaryFPOp MulOp fp (FInt    n) (FCnst _ m)) = Just $ FCnst fp (fromIntegral n * m)
simplFAExprAux' (BinaryFPOp MulOp fp (FCnst _ n) (FInt    m)) = Just $ FCnst fp (n * fromIntegral m)
simplFAExprAux' (BinaryFPOp MulOp fp (FCnst _ n) (FCnst _ m)) = Just $ FCnst fp (n*m)
simplFAExprAux' (UnaryFPOp  NegOp  _ (FInt    n))             = Just $ FInt (-n)
simplFAExprAux' (UnaryFPOp  NegOp  _ (FCnst fp n))            = Just $ FCnst fp (-n)
simplFAExprAux' (UnaryFPOp  AbsOp  _ (FInt    n))             = Just $ if n>=0 then FInt n else FInt (-n)
simplFAExprAux' (UnaryFPOp  AbsOp fp (FCnst _ n))             = Just $ if n>=0 then FCnst fp n else FCnst fp (-n)
simplFAExprAux' _ = Nothing

initBExpr :: BExpr -> BExpr
initBExpr = replaceInBExpr initErrorMark (const Nothing)

initAExpr :: AExpr -> AExpr
initAExpr = replaceInAExpr initErrorMark (const Nothing)

initFAExpr :: FAExpr -> FAExpr
initFAExpr = replaceInFAExpr initErrorMark (const Nothing)

initFBExpr :: FBExpr -> FBExpr
initFBExpr = replaceInFBExpr initErrorMark (const Nothing)

initErrorMark :: AExpr -> Maybe AExpr
initErrorMark (ErrorMark _ TInt) = Just (Int 0)
initErrorMark (ErrorMark x   fp) = Just (HalfUlp (RealMark x) fp)
initErrorMark (FromFloat fp fae) = Just (FromFloat fp (initFAExpr fae))
initErrorMark _ = Nothing

substituteInBExpr :: [(VarName, AExpr)] -> BExpr -> BExpr
substituteInBExpr subs = replaceInBExpr (replaceVarWithAExpr subs) (const Nothing)

substituteInAExpr :: [(VarName, AExpr)] -> AExpr -> AExpr
substituteInAExpr subs = replaceInAExpr (replaceVarWithAExpr subs) (const Nothing)

isExactlyRepresentable :: Rational -> Bool
isExactlyRepresentable n = toRational(fromRational n :: Double) == n

nextDouble :: Data.Bits.Floating.FloatingBits a w => Rational -> a
nextDouble f = if isExactlyRepresentable f then fromRational f else
    nextUp $ fromRational f

prevDouble :: Data.Bits.Floating.FloatingBits a w => Rational -> a
prevDouble f =  if isExactlyRepresentable f then fromRational f else
   nextDown $ fromRational f

nextUp' :: (RealFrac a, Data.Bits.Floating.FloatingBits a w) => a -> a
nextUp' f = if isInt f then f else nextUp f

nextDown' :: (RealFrac a, Data.Bits.Floating.FloatingBits a w) => a -> a
nextDown' f = if isInt f then f else nextDown f

localVars :: FAExpr -> [(VarName, FAExpr)]
localVars fae = elimDuplicates (foldFAExpr const varList' varListAExpr' fae [])
  where
    varList' :: [(VarName, FAExpr)] -> FAExpr -> [(VarName, FAExpr)]
    varList' acc (Let letElems _) = map (\(x,_,ae) -> (x,ae)) letElems ++ acc
    varList' acc _                = acc

    varListAExpr' :: [(VarName, FAExpr)] -> AExpr -> [(VarName, FAExpr)]
    varListAExpr' acc _ = acc

forIndexes :: FAExpr -> [(VarName, FAExpr, FAExpr)]
forIndexes fae = elimDuplicates (foldFAExpr const varList' varListAExpr' fae [])
  where
    varList' :: [(VarName, FAExpr, FAExpr)] -> FAExpr -> [(VarName, FAExpr, FAExpr)]
    varList' acc (ForLoop _ idxStart idxEnd _ idx _ _) = acc++[(idx, idxStart, idxEnd)]
    varList' acc _                = acc

    varListAExpr' :: [(VarName, FAExpr, FAExpr)] -> AExpr -> [(VarName, FAExpr, FAExpr)]
    varListAExpr' acc _ = acc

varList :: FAExpr -> [FAExpr]
varList fae = elimDuplicates (foldFAExpr const varList' varListAExpr' fae [])
  where
    varList' :: [FAExpr] -> FAExpr -> [FAExpr]
    varList' acc var@(FVar _ _) = acc++[var]
    varList' acc _              = acc

    varListAExpr' :: [FAExpr] -> AExpr -> [FAExpr]
    varListAExpr' acc _ = acc

varNameList :: FAExpr -> [VarName]
varNameList expr= map fvarName $ varList expr

funCallListBExpr :: BExpr -> [AExpr]
funCallListBExpr be = elimDuplicates $ foldBExpr const funCallListAcc be []

funCallListAExpr :: AExpr -> [AExpr]
funCallListAExpr ae = elimDuplicates $ foldAExpr const funCallListAcc ae []

funCallListAcc :: [AExpr] -> AExpr -> [AExpr]
funCallListAcc acc fc@EFun{} = acc++[fc]
funCallListAcc acc _         = acc

funCallListFBExprStm :: FBExprStm -> [FAExpr]
funCallListFBExprStm be = elimDuplicates $ foldFBExprStm const funFCallListAcc const be []

funCallListFBExprStmWithConds :: RProgram -> FBExprStm -> [FAExpr]
funCallListFBExprStmWithConds rprog expr = filter hasConds list
   where
      hasConds (FEFun _ name _ _) = hasConditionals rprog (fromLeft (notFound name)
            $ realDeclBody (fromMaybe (notFound name) $ findInRealProg name rprog))
      hasConds aexpr = error $ "funCallListFBExprStmWithConds: Expression " ++ show aexpr ++ " is not a function call."
      list = funCallListFBExprStm expr
      notFound     name = error $ "Function " ++ show name ++ " not found in program."

funCallListFBExpr :: FBExpr -> [FAExpr]
funCallListFBExpr be = elimDuplicates $ foldFBExpr const funFCallListAcc const be []

funCallListFAExpr :: FAExpr -> [FAExpr]
funCallListFAExpr ae = elimDuplicates $ foldFAExpr (\acc be -> acc++(funCallListFBExpr be)) funFCallListAcc const ae []

funCallListFAExprWithConds :: RProgram -> FAExpr -> [FAExpr]
funCallListFAExprWithConds rprog expr = filter hasConds list
   where
      hasConds (FEFun _ name _ _) = hasConditionals rprog (fromLeft (notFound name)
            $ realDeclBody (fromMaybe (notFound name) $ findInRealProg name rprog))
      hasConds aexpr = error $ "funCallListFBExprStmWithConds: Expression " ++ show aexpr ++ " is not a function call."
      list = funCallListFAExpr expr
      notFound     name = error $ "Function " ++ show name ++ " not found in program."

funFCallListAcc :: [FAExpr] -> FAExpr -> [FAExpr]
funFCallListAcc acc fc@FEFun{}          = acc++[fc]
funFCallListAcc acc (Value fc@FEFun {}) = acc++[fc]
funFCallListAcc acc (ToFloat _ (Int _)) = acc
funFCallListAcc acc (ToFloat _ (Rat _)) = acc
funFCallListAcc _   (ToFloat _ _)       = error "funCallList: case ToFloat niy"
funFCallListAcc acc _                   = acc

predCallListFBExprStm :: FBExprStm -> [FBExpr]
predCallListFBExprStm be = elimDuplicates $ foldFBExprStm predFCallListAcc const const be []

predCallListFBExprStmWithCond :: RProgram -> FBExprStm -> [FBExpr]
predCallListFBExprStmWithCond rprog expr = filter hasConds list
   where
      hasConds (FEPred _ _ name _) = hasConditionalsBExpr rprog (fromRight (notFound name)
            $ realDeclBody (fromMaybe (notFound name) $ findInRealProg name rprog))
      list = predCallListFBExprStm expr
      notFound     name = error $ "Function " ++ show name ++ " not found in program."

predCallListFBExpr :: FBExpr -> [FBExpr]
predCallListFBExpr be = elimDuplicates $ foldFBExpr predFCallListAcc const const be []

predCallListFAExpr :: FAExpr -> [FBExpr]
predCallListFAExpr ae = elimDuplicates $ foldFAExpr predFCallListAcc const const  ae []

predCallListFAExprWithConds :: RProgram -> FAExpr -> [FBExpr]
predCallListFAExprWithConds rprog expr = filter hasConds list
   where
      hasConds (FEPred _ _ name _) = hasConditionalsBExpr rprog (fromRight (notFound name) $ realDeclBody (fromMaybe (notFound name) $ findInRealProg name rprog))
      list = predCallListFAExpr expr
      notFound     name = error $ "Function " ++ show name ++ " not found in program."

predFCallListAcc :: [FBExpr] -> FBExpr -> [FBExpr]
predFCallListAcc acc fc@FEPred{} = acc++[fc]
predFCallListAcc acc _           = acc

funHasConds :: RProgram -> FunName -> Bool
funHasConds rprog name = either (hasConditionals rprog) (hasConditionalsBExpr rprog)
    $ realDeclBody (fromMaybe (notFound name) $ findInRealProg name rprog)
      where
        notFound name = error $ "Function " ++ show name ++ " not found in program."

noRoundOffErrorIn :: FBExpr -> Bool
noRoundOffErrorIn FEPred{} = False
noRoundOffErrorIn be = foldFBExpr const noRoundOffErrorInAExpr' const be True

noRoundOffErrorInAExpr :: FAExpr -> Bool
noRoundOffErrorInAExpr (FEFun _ _ TInt _) = True
noRoundOffErrorInAExpr ae = foldFAExpr const noRoundOffErrorInAExpr' const ae True

noRoundOffErrorInAExpr' :: Bool -> FAExpr -> Bool
noRoundOffErrorInAExpr' acc (FCnst _ n)    = acc && toRational (floor (fromRational n :: Double) :: Integer) == n
noRoundOffErrorInAExpr' acc (FVar  TInt _) = acc
noRoundOffErrorInAExpr' _   (FVar  _    _) = False
noRoundOffErrorInAExpr' acc (ToFloat _ (Int _)) = acc
noRoundOffErrorInAExpr' _   (ToFloat _ _) = False
noRoundOffErrorInAExpr' acc (TypeCast _ _ (FInt    _)) = acc
noRoundOffErrorInAExpr' acc (TypeCast _ _ (FCnst _ n)) = acc && toRational (floor (fromRational n :: Double) :: Integer) == n
noRoundOffErrorInAExpr' acc  TypeCast{} = acc
noRoundOffErrorInAExpr' acc (FArrayElem TInt _ _ _) = acc
noRoundOffErrorInAExpr' _    FArrayElem{} = False
noRoundOffErrorInAExpr' acc (FEFun _ _ TInt _) = acc
noRoundOffErrorInAExpr' acc  _              = acc

equivModuloIndex :: FAExpr -> FAExpr -> Bool
equivModuloIndex (FInt n) (FInt m) = n==m
equivModuloIndex (FCnst fp1 r1) (FCnst fp2 r2) = fp1==fp2 && r1==r2
equivModuloIndex (FVar  fp1 x1) (FVar  fp2 x2) = fp1==fp2 && x1==x2
equivModuloIndex (FEFun isTrans1 f1 fp1 args1)
                 (FEFun isTrans2 f2 fp2 args2) = isTrans1 == isTrans2
                                                 && f1 == f2 && fp1 == fp2
                                                 && foldl (\b (arg1,arg2) -> b
                                                 && equivModuloIndex arg1 arg2) True (zip args1 args2)
equivModuloIndex (FArrayElem fp1 size1 v1 _) (FArrayElem fp2 size2 v2 _) = fp1==fp2 && size1 == size2 && v1==v2
equivModuloIndex (ToFloat fp1  (Int n))  (ToFloat fp2 (Int m))  = fp1==fp2 && n==m
equivModuloIndex (ToFloat fp1  (Rat r1)) (ToFloat fp2 (Rat r2)) = fp1==fp2 && r1==r2
equivModuloIndex (BinaryFPOp op  fp  ae1  ae2 )
                 (BinaryFPOp op' fp' ae1' ae2') | op == op' && fp == fp' = equivModuloIndex ae1 ae1'
                                                                        && equivModuloIndex ae2 ae2'
equivModuloIndex (UnaryFPOp op  fp  ae )
                 (UnaryFPOp op' fp' ae')  | op == op'   && fp == fp'   = equivModuloIndex ae ae'
equivModuloIndex (TypeCast fp1  fp2  ae )
                 (TypeCast fp1' fp2' ae') | fp1 == fp1' && fp2 == fp2' = equivModuloIndex ae ae'
equivModuloIndex (ToFloat _  _ )
                 (ToFloat _ _) = error "equivModuloIndex niy for ToFloat."
equivModuloIndex (FFma   _ ae1 ae2 ae3) (FFma   _ ae1' ae2' ae3') = equivModuloIndex ae1 ae1'
                                                                 && equivModuloIndex ae2 ae2'
                                                                 && equivModuloIndex ae3 ae3'
equivModuloIndex (Value ae) (Value ae') = equivModuloIndex ae ae'
equivModuloIndex (FMin aes1) (FMin aes2) = foldl (\b (ae1,ae2) -> b && equivModuloIndex ae1 ae2) True (zip aes1 aes2)
equivModuloIndex (FMax aes1) (FMax aes2) = foldl (\b (ae1,ae2) -> b && equivModuloIndex ae1 ae2) True (zip aes1 aes2)
equivModuloIndex _ _ = False

foldUnaryFAExpr :: (a -> FBExpr -> a) -> (a -> FAExpr -> a) -> (a -> AExpr -> a) -> FAExpr -> FAExpr -> a -> a
foldUnaryFAExpr bExprF faExprF aExprF ae ae1 a = foldFAExpr bExprF faExprF aExprF ae1
                                               $ faExprF a ae

foldBinaryFAExpr :: (a -> FBExpr -> a) -> (a -> FAExpr -> a) -> (a -> AExpr -> a) -> FAExpr -> FAExpr -> FAExpr -> a -> a
foldBinaryFAExpr bExprF faExprF aExprF ae ae1 ae2 a = foldFAExpr bExprF faExprF aExprF ae2
                                                    $ foldFAExpr bExprF faExprF aExprF ae1
                                                    $ faExprF a ae

foldUnaryAExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> AExpr -> AExpr -> a -> a
foldUnaryAExpr faExprF aExprF ae ae1 a = foldAExpr faExprF aExprF ae1
                                       $ aExprF a ae

foldBinaryAExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> AExpr -> AExpr -> AExpr -> a -> a
foldBinaryAExpr faExprF aExprF ae ae1 ae2 a = foldAExpr faExprF aExprF ae2
                                            $ foldAExpr faExprF aExprF ae1
                                            $ aExprF a ae

foldQuadAExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> AExpr -> AExpr -> AExpr -> AExpr -> AExpr -> a -> a
foldQuadAExpr faExprF aExprF ae ae1 ae2 ae3 ae4 a = foldAExpr faExprF aExprF ae4
                                                    $ foldAExpr faExprF aExprF ae3
                                                    $ foldAExpr faExprF aExprF ae2
                                                    $ foldAExpr faExprF aExprF ae1
                                                    $ aExprF a ae

foldListFBExpr :: (a -> FBExpr -> a) -> (a -> FAExpr -> a) -> (a -> AExpr -> a) -> [FBExpr] -> a -> a
foldListFBExpr bExprF faExprF aExprF aeList a = foldl (flip (foldFBExpr bExprF faExprF aExprF)) a aeList

foldListFBExprStm :: (a -> FBExpr -> a) -> (a -> FAExpr -> a) -> (a -> AExpr -> a) -> [FBExprStm] -> a -> a
foldListFBExprStm bExprF faExprF aExprF aeList a = foldl (flip (foldFBExprStm bExprF faExprF aExprF)) a aeList

foldListBExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> [BExpr] -> a -> a
foldListBExpr faExprF aExprF aeList a = foldl (flip (foldBExpr faExprF aExprF)) a aeList

foldListFAExpr :: (a -> FBExpr -> a) -> (a -> FAExpr -> a) -> (a -> AExpr -> a) -> [FAExpr] -> a -> a
foldListFAExpr bExprF faExprF aExprF aeList a = foldl (flip (foldFAExpr bExprF faExprF aExprF)) a aeList

foldListAExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> [AExpr] -> a -> a
foldListAExpr faExprF aExprF aeList a = foldl (flip (foldAExpr faExprF aExprF)) a aeList

foldFBExpr :: (a -> FBExpr -> a) -> (a -> FAExpr -> a) -> (a -> AExpr -> a) -> FBExpr -> a -> a
foldFBExpr bExprF faExprF aExprF be@(FOr  be1 be2) a = foldFBExpr bExprF faExprF aExprF be2
                                                     $ foldFBExpr bExprF faExprF aExprF be1
                                                     $ bExprF a be
foldFBExpr bExprF faExprF aExprF be@(FAnd be1 be2) a = foldFBExpr bExprF faExprF aExprF be2
                                                     $ foldFBExpr bExprF faExprF aExprF be1
                                                     $ bExprF a be
foldFBExpr bExprF faExprF aExprF be@(FNot be1)     a = foldFBExpr bExprF faExprF aExprF be1
                                                     $ bExprF a be
foldFBExpr bExprF faExprF aExprF be@(FRel _ ae1 ae2) a = foldFAExpr bExprF faExprF aExprF ae2
                                                       $ foldFAExpr bExprF faExprF aExprF ae1
                                                       $ bExprF a be
foldFBExpr bExprF faExprF aExprF be@(IsValid  ae1) a = foldFAExpr bExprF faExprF aExprF ae1
                                                     $ bExprF a be
foldFBExpr bExprF faExprF aExprF be@(FEPred _ _ _ args)  a = foldListFAExpr bExprF faExprF aExprF args
                                                            $ bExprF a be
foldFBExpr bExprF faExprF aExprF be@(BIsValid  be1) a  = foldFBExpr bExprF faExprF aExprF be1
                                                       $ bExprF a be
foldFBExpr bExprF faExprF aExprF be@(BValue    be1) a  = foldFBExpr bExprF faExprF aExprF be1
                                                       $ bExprF a be
foldFBExpr bExprF _ _ v@(BStructVar _) a = bExprF a v
foldFBExpr bExprF _ _ FBTrue  a = bExprF a FBTrue
foldFBExpr bExprF _ _ FBFalse a = bExprF a FBFalse

foldFBExprStm :: (a -> FBExpr -> a) -> (a -> FAExpr -> a) -> (a -> AExpr -> a) -> FBExprStm -> a -> a
foldFBExprStm bExprF faExprF aExprF (BLet listElems body) a = foldListFAExpr bExprF faExprF aExprF (map trd3 listElems)
                                                            $ foldFBExprStm bExprF faExprF aExprF body a
foldFBExprStm bExprF faExprF aExprF (BIte be stmThen stmElse) a = foldFBExprStm bExprF faExprF aExprF stmElse
                                                                $ foldFBExprStm bExprF faExprF aExprF stmThen
                                                                $ bExprF a be
foldFBExprStm bExprF faExprF aExprF (BListIte listThen stmElse) a = foldListFBExpr bExprF faExprF aExprF (map fst listThen)
                                                                  $ foldListFBExprStm bExprF faExprF aExprF (map snd listThen)
                                                                  $ foldFBExprStm bExprF faExprF aExprF stmElse a
foldFBExprStm bExprF faExprF aExprF (BExpr be) a = foldFBExpr bExprF faExprF aExprF be a
foldFBExprStm _ _ _  BUnstWarning a = a

foldFAExpr :: (a -> FBExpr -> a) -> (a -> FAExpr -> a) -> (a -> AExpr -> a) -> FAExpr -> a -> a
foldFAExpr _ faExprF _ ae@(FCnst _ _)   a = faExprF a ae
foldFAExpr _ faExprF _ ae@(FVar  _ _)   a = faExprF a ae
foldFAExpr _ faExprF _ ae@(StructVar  _ _)   a = faExprF a ae
foldFAExpr _ faExprF _ ae@(FInt _)      a = faExprF a ae
foldFAExpr _ faExprF _ UnstWarning      a = faExprF a UnstWarning

foldFAExpr bExprF faExprF aExprF ae@(BinaryFPOp _ _ ae1 ae2) a = foldBinaryFAExpr bExprF faExprF aExprF ae ae1 ae2 a
foldFAExpr bExprF faExprF aExprF ae@(UnaryFPOp  _ _ ae1    ) a = foldUnaryFAExpr  bExprF faExprF aExprF ae ae1     a
foldFAExpr bExprF faExprF aExprF ae@(TypeCast   _ _ ae1    ) a = foldUnaryFAExpr  bExprF faExprF aExprF ae ae1 a
foldFAExpr _ faExprF aExprF ae@(ToFloat    _   ae1    ) a = foldAExpr faExprF aExprF ae1
                                                            $ faExprF a ae
foldFAExpr bExprF faExprF aExprF ae@(Value            ae1) a = foldUnaryFAExpr bExprF faExprF aExprF ae ae1 a
foldFAExpr bExprF faExprF aExprF ae@(FArrayElem _ _ _ ae1) a = foldUnaryFAExpr bExprF faExprF aExprF ae ae1 a

foldFAExpr bExprF faExprF aExprF ae@(FFma _ ae1 ae2 ae3) a = foldFAExpr bExprF faExprF aExprF ae1
                                                           $ foldFAExpr bExprF faExprF aExprF ae2
                                                           $ foldFAExpr bExprF faExprF aExprF ae3
                                                           $ faExprF a ae
foldFAExpr bExprF faExprF aExprF (FMin      aes) a = foldListFAExpr bExprF faExprF aExprF aes a
foldFAExpr bExprF faExprF aExprF (FMax      aes) a = foldListFAExpr bExprF faExprF aExprF aes a
foldFAExpr bExprF faExprF aExprF ae@(FEFun _ _ _ aes) a = foldListFAExpr bExprF faExprF aExprF aes $ faExprF a ae

foldFAExpr bExprF faExprF aExprF ae@(Let listElems body) a = foldListFAExpr bExprF faExprF aExprF (map trd3 listElems)
                                                              $ foldFAExpr bExprF faExprF aExprF body
                                                              $ faExprF a ae
foldFAExpr bExprF faExprF aExprF ae@(Ite be stmThen stmElse) a = foldBinaryFAExpr bExprF faExprF aExprF ae stmThen stmElse
                                                               $ bExprF a be
foldFAExpr bExprF faExprF aExprF (ListIte listThen stmElse) a = foldListFBExpr bExprF faExprF aExprF (map fst listThen)
                                                                 $ foldListFAExpr bExprF faExprF aExprF (map snd listThen)
                                                                 $ faExprF a stmElse
foldFAExpr bExprF faExprF aExprF ae@(ForLoop _ _ _ _ _ _ body) a = foldUnaryFAExpr bExprF faExprF aExprF ae body a

foldBExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> BExpr -> a -> a
foldBExpr faExprF aExprF (Or  be1 be2)  a = foldBExpr faExprF aExprF be2
                                          $ foldBExpr faExprF aExprF be1 a
foldBExpr faExprF aExprF (And be1 be2)  a = foldBExpr faExprF aExprF be2
                                          $ foldBExpr faExprF aExprF be1 a
foldBExpr faExprF aExprF (Not be1)      a = foldBExpr faExprF aExprF be1 a
foldBExpr faExprF aExprF (Rel _ ae1 ae2) a = foldAExpr faExprF aExprF ae2
                                           $ foldAExpr faExprF aExprF ae1 a
foldBExpr faExprF aExprF (EPred _ args) a = foldListAExpr faExprF aExprF args a
foldBExpr _ _ BTrue          a = a
foldBExpr _ _ BFalse         a = a

foldAExpr :: (a -> FAExpr -> a) -> (a -> AExpr -> a) -> AExpr -> a -> a
foldAExpr _       aExprF ae@(Int _)         a = aExprF a ae
foldAExpr _       aExprF ae@(Rat _)         a = aExprF a ae
foldAExpr _       aExprF ae@(Prec _)        a = aExprF a ae
foldAExpr _       aExprF ae@(Var _ _)       a = aExprF a ae
foldAExpr _       aExprF ae@Infinity        a = aExprF a ae
foldAExpr _       aExprF ae@(RealMark _)    a = aExprF a ae
foldAExpr _       aExprF ae@(ErrRat _)      a = aExprF a ae
foldAExpr _       aExprF ae@(ErrorMark _ _) a = aExprF a ae
foldAExpr _       aExprF ae@RUnstWarning    a = aExprF a ae
foldAExpr faExprF aExprF ae@(FromFloat _ fae) a = foldFAExpr const faExprF aExprF fae
                                                 $ aExprF a ae
foldAExpr faExprF aExprF ae@(UnaryOp  _ ae1)     a = foldUnaryAExpr  faExprF aExprF ae ae1     a
foldAExpr faExprF aExprF ae@(BinaryOp _ ae1 ae2) a = foldBinaryAExpr faExprF aExprF ae ae1 ae2 a
foldAExpr faExprF aExprF ae@(ErrMulPow2R _ _ ee)  a = foldUnaryAExpr  faExprF aExprF ae ee      a
foldAExpr faExprF aExprF ae@(ErrMulPow2L _ _ ee)  a = foldUnaryAExpr  faExprF aExprF ae ee      a
foldAExpr faExprF aExprF ae@(ArrayElem _ _ _ ae1) a = foldUnaryAExpr  faExprF aExprF ae ae1     a
foldAExpr faExprF aExprF ae@(HalfUlp ae1 _)       a = foldUnaryAExpr  faExprF aExprF ae ae1     a
foldAExpr faExprF aExprF ae@(ErrUnOp  _ _ _ ae1 ae2) a = foldBinaryAExpr faExprF aExprF ae ae1 ae2 a
foldAExpr faExprF aExprF ae@(ErrBinOp _ _ ae1 ee1 ae2 ee2) a = foldQuadAExpr faExprF aExprF ae ae1 ee1 ae2 ee2 a
foldAExpr faExprF aExprF ae@(ErrCast  _ _ ae1 ee1) a = foldBinaryAExpr faExprF aExprF ae ae1 ee1 a
foldAExpr faExprF aExprF ae@(EFun _ _ aes) a = foldListAExpr faExprF aExprF aes $ aExprF a ae
foldAExpr faExprF aExprF (Min      aes) a = foldListAExpr faExprF aExprF aes a
foldAExpr faExprF aExprF (Max      aes) a = foldListAExpr faExprF aExprF aes a
foldAExpr faExprF aExprF (MaxErr   aes) a = foldListAExpr faExprF aExprF aes a
foldAExpr faExprF aExprF ae@(FExp fae)        a = foldFAExpr const faExprF aExprF fae
                                                $ aExprF a ae
foldAExpr faExprF aExprF ae@(ErrFma _ ae1 ee1 ae2 ee2 ae3 ee3) a = foldAExpr faExprF aExprF ee3
                                                                 $ foldAExpr faExprF aExprF ae3
                                                                 $ foldAExpr faExprF aExprF ee2
                                                                 $ foldAExpr faExprF aExprF ae2
                                                                 $ foldAExpr faExprF aExprF ee1
                                                                 $ foldAExpr faExprF aExprF ae1
                                                                 $ aExprF a ae
foldAExpr faExprF aExprF ae@(RLet listElems body) a = foldListAExpr faExprF aExprF (map letExpr listElems)
                                                      $ foldAExpr faExprF aExprF body
                                                      $ aExprF a ae
foldAExpr faExprF aExprF ae@(RIte _ stmThen stmElse)    a = foldBinaryAExpr faExprF aExprF ae stmThen stmElse a
foldAExpr faExprF aExprF (RListIte listThen stmElse) a = foldListBExpr faExprF aExprF (map fst listThen)
                                                          $ foldListAExpr faExprF aExprF (map snd listThen)
                                                          $ aExprF a stmElse
foldAExpr faExprF aExprF ae@(RForLoop _ _ _ _ _ _ body) a = foldUnaryAExpr  faExprF aExprF ae body a

replaceInFLetElem :: (AExpr -> Maybe AExpr) -> (FAExpr -> Maybe FAExpr) -> FLetElem -> FLetElem
replaceInFLetElem rf ff (x,t,ae) = (x,t,replaceInFAExpr rf ff ae)

replaceInFAExpr :: (AExpr -> Maybe AExpr) -> (FAExpr -> Maybe FAExpr) -> FAExpr -> FAExpr
replaceInFAExpr rf ff fexpr = fromMaybe (replaceInFAExpr' fexpr) (ff fexpr)
  where
    replaceInFAExpr' :: FAExpr -> FAExpr
    replaceInFAExpr' (BinaryFPOp op fp ae1 ae2) = BinaryFPOp op fp (replaceInFAExpr rf ff ae1) (replaceInFAExpr rf ff ae2)
    replaceInFAExpr' (UnaryFPOp  op fp ae) = UnaryFPOp  op fp (replaceInFAExpr rf ff ae)
    replaceInFAExpr' (TypeCast fpO fpD ae) = TypeCast fpO fpD (replaceInFAExpr rf ff ae)
    replaceInFAExpr' ae@(ToFloat _ (Int _))  = ae
    replaceInFAExpr' ae@(ToFloat _ (Rat _))  = ae
    replaceInFAExpr' (ToFloat  fp ae)      = ToFloat fp (replaceInAExpr rf ff ae)
    replaceInFAExpr' ae@(FInt _)      = ae
    replaceInFAExpr' ae@(FCnst _ _)   = ae
    replaceInFAExpr' ae@(FVar  _ _)   = ae
    replaceInFAExpr' ae@(StructVar _ _) = ae
    replaceInFAExpr' (FArrayElem fp size v idx)  = FArrayElem fp size v (replaceInFAExpr rf ff idx)
    replaceInFAExpr' (FEFun isTrans g fp args)   = FEFun isTrans g fp (map (replaceInFAExpr rf ff) args)
    replaceInFAExpr' (FFma   fp ae1 ae2 ae3) = FFma fp (replaceInFAExpr rf ff ae1)
                                                       (replaceInFAExpr rf ff ae2)
                                                       (replaceInFAExpr rf ff ae3)
    replaceInFAExpr' (FMin aes) = FMin (map (replaceInFAExpr rf ff) aes)
    replaceInFAExpr' (FMax aes) = FMax (map (replaceInFAExpr rf ff) aes)
    replaceInFAExpr' (Value ae) = Value (replaceInFAExpr rf ff ae)
    replaceInFAExpr'  UnstWarning = UnstWarning
    replaceInFAExpr' (Let letElems stm) = Let (map (replaceInFLetElem rf ff) letElems) (replaceInFAExpr rf ff stm)
    replaceInFAExpr' (Ite be thenExpr elseExpr) = Ite (replaceInFBExpr rf ff be) (replaceInFAExpr rf ff thenExpr) (replaceInFAExpr rf ff elseExpr)
    replaceInFAExpr' (ListIte listThen elseExpr) = ListIte (map (bimap (replaceInFBExpr rf ff) (replaceInFAExpr rf ff)) listThen)
                                                           (replaceInFAExpr rf ff elseExpr)
    replaceInFAExpr' (ForLoop fp n0 n acc0 i acc body) = ForLoop fp n0 n (replaceInFAExpr rf ff acc0) i acc (replaceInFAExpr rf ff body)

replaceInFBExpr :: (AExpr -> Maybe AExpr) -> (FAExpr -> Maybe FAExpr) -> FBExpr -> FBExpr
replaceInFBExpr rg fg (FOr  be1 be2) = FOr  (replaceInFBExpr rg fg be1) (replaceInFBExpr rg fg be2)
replaceInFBExpr rg fg (FAnd be1 be2) = FAnd (replaceInFBExpr rg fg be1) (replaceInFBExpr rg fg be2)
replaceInFBExpr rg fg (FNot be)      = FNot (replaceInFBExpr rg fg be)
replaceInFBExpr rg fg (FRel rel ae1 ae2) = FRel rel (replaceInFAExpr rg fg ae1) (replaceInFAExpr rg fg ae2)
replaceInFBExpr rg fg (IsValid ae)   = IsValid (replaceInFAExpr rg fg ae)
replaceInFBExpr rg fg (BIsValid ae)  = BIsValid (replaceInFBExpr rg fg ae)
replaceInFBExpr rg fg (BValue   ae)  = BValue   (replaceInFBExpr rg fg ae)
replaceInFBExpr rg fg (FEPred isTrans predAbs f args) = FEPred isTrans predAbs f (map (replaceInFAExpr rg fg) args)
replaceInFBExpr _  _  v@(BStructVar _) = v
replaceInFBExpr _ _ FBTrue  = FBTrue
replaceInFBExpr _ _ FBFalse = FBFalse

replaceInFBExprStm :: (AExpr -> Maybe AExpr) -> (FAExpr -> Maybe FAExpr) -> FBExprStm -> FBExprStm
replaceInFBExprStm rg fg (BLet letElems stmElse) = BLet (map (replaceInFLetElem rg fg) letElems) (replaceInFBExprStm rg fg stmElse)
replaceInFBExprStm rg fg (BIte be stmThen stmElse) = BIte (replaceInFBExpr rg fg be)
                                                          (replaceInFBExprStm rg fg stmThen)
                                                          (replaceInFBExprStm rg fg stmElse)
replaceInFBExprStm rg fg (BListIte listThen stmElse) = BListIte (map (bimap (replaceInFBExpr rg fg)
                                                                            (replaceInFBExprStm rg fg)) listThen)
                                                                (replaceInFBExprStm rg fg stmElse)
replaceInFBExprStm rg fg (BExpr be) = BExpr $ replaceInFBExpr rg fg be
replaceInFBExprStm _ _ BUnstWarning = BUnstWarning

replaceInAExpr :: (AExpr -> Maybe AExpr) -> (FAExpr -> Maybe FAExpr) -> AExpr -> AExpr
replaceInAExpr rf ff expr = fromMaybe (replaceInAExpr' expr) (rf expr)
  where
    replaceInAExpr' :: AExpr -> AExpr
    replaceInAExpr' (UnaryOp  op ae1)     = UnaryOp  op (replaceInAExpr rf ff ae1)
    replaceInAExpr' (BinaryOp op ae1 ae2) = BinaryOp op (replaceInAExpr rf ff ae1) (replaceInAExpr rf ff ae2)
    replaceInAExpr' (FromFloat fp ae)     = FromFloat fp (replaceInFAExpr rf ff ae)
    replaceInAExpr' (FExp ae) = FExp (replaceInFAExpr rf ff ae)
    replaceInAExpr' ae@(Int _)         = ae
    replaceInAExpr' ae@(Rat _)         = ae
    replaceInAExpr' ae@(Var _ _)       = ae
    replaceInAExpr' ae@ArrayElem{}     = ae
    replaceInAExpr' ae@(RealMark _)    = ae
    replaceInAExpr' ae@(ErrorMark _ _) = ae
    replaceInAExpr' ae@(ErrRat _)      = ae
    replaceInAExpr' (Prec fp) = (Prec fp)
    replaceInAExpr' Infinity     = Infinity
    replaceInAExpr' (EFun g fp args) = EFun g fp (map (replaceInAExpr rf ff) args)
    replaceInAExpr' (Min    aes) = Min    (map (replaceInAExpr rf ff) aes)
    replaceInAExpr' (Max    aes) = Max    (map (replaceInAExpr rf ff) aes)
    replaceInAExpr' (MaxErr aes) = MaxErr (map (replaceInAExpr rf ff) aes)
    -- replaceInAExpr' (Fma   ae1 ae2 ae3) = Fma (replaceInAExpr rf ff ae1)
    --                                           (replaceInAExpr rf ff ae2)
    --                                           (replaceInAExpr rf ff ae3)
    replaceInAExpr' (ErrFma fp ae1 ee1 ae2 ee2 ae3 ee3) = ErrFma fp (replaceInAExpr rf ff ae1) (replaceInAExpr rf ff ee1)
                                                                    (replaceInAExpr rf ff ae2) (replaceInAExpr rf ff ee2)
                                                                    (replaceInAExpr rf ff ae3) (replaceInAExpr rf ff ee3)
    replaceInAExpr' (ErrBinOp op fp ae1 ee1 ae2 ee2) = ErrBinOp op fp (replaceInAExpr rf ff ae1) (replaceInAExpr rf ff ee1)
                                                              (replaceInAExpr rf ff ae2) (replaceInAExpr rf ff ee2)
    replaceInAExpr' (ErrUnOp tight op fp ae1 ee1) = ErrUnOp tight op fp (replaceInAExpr rf ff ae1) (replaceInAExpr rf ff ee1)
    replaceInAExpr' (ErrMulPow2R fp i ee) = ErrMulPow2R fp i (replaceInAExpr rf ff ee)
    replaceInAExpr' (ErrMulPow2L fp i ee) = ErrMulPow2R fp i (replaceInAExpr rf ff ee)
    replaceInAExpr' (HalfUlp ae fp) = HalfUlp (replaceInAExpr rf ff ae) fp
    replaceInAExpr' (ErrCast fp1 fp2 ae ee) = ErrCast fp1 fp2 (replaceInAExpr rf ff ae) (replaceInAExpr rf ff ee)
    ----------------------
    replaceInAExpr' (RLet letElems body)     = RLet (map replaceInLetElem letElems) (replaceInAExpr rf ff body)
      where
        replaceInLetElem letElem = letElem {letExpr = replaceInAExpr rf ff (letExpr letElem)}
    replaceInAExpr' (RIte be  thenExpr elseExpr) = RIte be (replaceInAExpr rf ff thenExpr) (replaceInAExpr rf ff elseExpr)
    replaceInAExpr' (RListIte listThen elseExpr) = RListIte (map (bimap (replaceInBExpr rf ff) (replaceInAExpr rf ff)) listThen)
                                                            (replaceInAExpr rf ff elseExpr)
    replaceInAExpr' (RForLoop fp idxStart idxEnd initAcc idx acc body) = RForLoop fp (replaceInAExpr rf ff idxStart)
                                                                                   (replaceInAExpr rf ff idxEnd)
                                                                                   (replaceInAExpr rf ff initAcc)
                                                                                    idx acc
                                                                                   (replaceInAExpr rf ff body)
    replaceInAExpr' RUnstWarning = RUnstWarning

replaceInBExpr :: (AExpr -> Maybe AExpr) -> (FAExpr -> Maybe FAExpr)  -> BExpr -> BExpr
replaceInBExpr rg fg (Or  be1 be2) = Or  (replaceInBExpr rg fg be1) (replaceInBExpr rg fg be2)
replaceInBExpr rg fg (And be1 be2) = And (replaceInBExpr rg fg be1) (replaceInBExpr rg fg be2)
replaceInBExpr rg fg (Not be)      = Not (replaceInBExpr rg fg be)
replaceInBExpr rg fg (Rel rel  ae1 ae2) = Rel rel  (replaceInAExpr rg fg ae1) (replaceInAExpr rg fg ae2)
replaceInBExpr rg fg (EPred f args) = EPred f (map (replaceInAExpr rg fg) args)
replaceInBExpr  _ _ BTrue  = BTrue
replaceInBExpr  _ _ BFalse = BFalse

replaceVarWithAExpr :: [(VarName, AExpr)] -> AExpr -> Maybe AExpr
replaceVarWithAExpr ((y,ae):subs) var@(Var _ x) | x == y = Just ae
                                                | otherwise = replaceVarWithAExpr subs var
replaceVarWithAExpr ((y,ae):subs) var@(RealMark x) | x == y = Just ae
                                                   | otherwise = replaceVarWithAExpr subs var
replaceVarWithAExpr _ _ = Nothing


isZero :: Rational -> Bool
isZero r = r == toRational (0 :: Integer)

isZeroFAExpr :: FAExpr -> Bool
isZeroFAExpr (FInt 0)            = True
isZeroFAExpr (ToFloat _ (Int 0)) = True
isZeroFAExpr (FCnst _ r)         = isZero r
isZeroFAExpr (TypeCast _ _ ae)   = isZeroFAExpr ae
isZeroFAExpr _ = False

isZeroAExpr :: AExpr -> Bool
isZeroAExpr (Int 0)            = True
isZeroAExpr (FromFloat _ (FInt 0)) = True
isZeroAExpr (Rat r)         = r == toRational (0 :: Integer)
isZeroAExpr _ = False

errVar :: FAExpr -> AExpr
errVar  (FVar fp x) = ErrorMark x fp
errVar ae = error $ "errVar not defined for " ++ show ae ++ "."

realVar :: FAExpr -> AExpr
realVar (FVar _ x) = RealMark  x
realVar ae = error $ "realVar not defined for " ++ show ae ++ "."

nameFVar :: FAExpr -> VarName
nameFVar (FVar _ x) = x
nameFVar ae = error $ "nameFVar: unexpected value " ++ show ae ++ "."

precFVar :: FAExpr -> PVSType
precFVar (FVar fp _) = fp
precFVar ae = error $ "precFVar: unexpected value " ++ show ae ++ "."

unfoldForLoop :: PVSType -> Integer -> Integer -> FAExpr -> VarName -> VarName -> FAExpr -> FAExpr
unfoldForLoop fp n0 = unfoldForLoop' fp n0 n0

unfoldForLoop' :: PVSType -> Integer -> Integer -> Integer -> FAExpr -> VarName -> VarName -> FAExpr -> FAExpr
unfoldForLoop' fp j n0 n acc varI varAcc body | j == n    = acc'
                                              | j >  n    = acc
                                              | j <  n    = unfoldForLoop' fp (j+1) n0 n acc' varI varAcc body
  where
    acc'  = replaceInFAExpr (const Nothing) replaceIdxAndAcc body
    replaceIdxAndAcc (FVar _ var) | var == varI   = Just $ FInt j
                                  | var == varAcc = Just acc
                                  | otherwise     = Nothing
    replaceIdxAndAcc   _          = Nothing

unfoldForLoop' _ _ _ _ acc0 _ _ _ = error $ "unfoldForLoop: " ++ show acc0 ++ ".\n"

listLetElems :: FAExpr -> [FLetElem]
listLetElems ae = foldFAExpr const aux const ae []
  where
    aux acc (Let letElems _) = acc++letElems
    aux acc _ = acc

removeLetInFAExpr :: FAExpr -> FAExpr
removeLetInFAExpr (Let _ bodyLet) = removeLetInFAExpr bodyLet
removeLetInFAExpr (FEFun isTrans f t args) = FEFun isTrans f t (map removeLetInFAExpr args)
removeLetInFAExpr (FArrayElem t size x i) = FArrayElem t size x (removeLetInFAExpr i)
removeLetInFAExpr (TypeCast tFrom tTo fae) = TypeCast tFrom tTo (removeLetInFAExpr fae)
removeLetInFAExpr ae@(ToFloat _ _) = ae
removeLetInFAExpr (Value fae) = Value $ removeLetInFAExpr fae
removeLetInFAExpr (BinaryFPOp op t fae1 fae2) = BinaryFPOp op t (removeLetInFAExpr fae1)
                                                                      (removeLetInFAExpr fae2)
removeLetInFAExpr (UnaryFPOp   op t fae) = UnaryFPOp op t (removeLetInFAExpr fae)
removeLetInFAExpr (FFma t fae1 fae2 fae3) = FFma t (removeLetInFAExpr fae1)
                                                         (removeLetInFAExpr fae2)
                                                         (removeLetInFAExpr fae3)
removeLetInFAExpr (FMin faes) = FMin $ map removeLetInFAExpr faes
removeLetInFAExpr (FMax faes) = FMax $ map removeLetInFAExpr faes
removeLetInFAExpr (Ite be thenExpr elseExpr) = Ite be (removeLetInFAExpr thenExpr) (removeLetInFAExpr elseExpr)
removeLetInFAExpr (ListIte listThen elseExpr) = ListIte (map (second removeLetInFAExpr) listThen)
                                                        (removeLetInFAExpr elseExpr)
removeLetInFAExpr (ForLoop fp idxStart idxEnd initAcc idx acc forBody)
  = ForLoop fp idxStart idxEnd initAcc idx acc (removeLetInFAExpr forBody)
removeLetInFAExpr ae = ae

fpExprsInGuard :: FBExpr -> [FAExpr]
fpExprsInGuard (FOr  be1 be2) = fpExprsInGuard be1 ++ fpExprsInGuard be2
fpExprsInGuard (FAnd be1 be2) = fpExprsInGuard be1 ++ fpExprsInGuard be2
fpExprsInGuard (FNot be) = fpExprsInGuard be
fpExprsInGuard (BIsValid be) = fpExprsInGuard be
fpExprsInGuard (BValue   be) = fpExprsInGuard be
fpExprsInGuard (FRel _ ae1 ae2) = filter isFloatingFAExpr [ae1, ae2]
fpExprsInGuard (IsValid ae) =  filter isFloatingFAExpr [ae]
fpExprsInGuard (FEPred _ _ _ args) = filter isFloatingFAExpr args
fpExprsInGuard _ = []

listFPGuards :: FAExpr -> [FAExpr]
listFPGuards fae = foldFAExpr const fpGuardList' fpGuardListAExpr' fae []
  where
    fpGuardList' :: [FAExpr] -> FAExpr -> [FAExpr]
    fpGuardList' acc (Ite be _ _) = acc ++ fpExprsInGuard be
    fpGuardList' acc (ListIte listThen _) = acc ++ concatMap (fpExprsInGuard . fst) listThen
    fpGuardList' acc _              = acc

    fpGuardListAExpr' :: [FAExpr] -> AExpr -> [FAExpr]
    fpGuardListAExpr' acc _ = acc

listFPGuardsFBExprStm :: FBExprStm -> [FAExpr]
listFPGuardsFBExprStm BUnstWarning = []
listFPGuardsFBExprStm (BLet _ expr) = listFPGuardsFBExprStm expr
listFPGuardsFBExprStm (BIte be thenExpr elseExpr) = fpExprsInGuard be
                                                 ++ listFPGuardsFBExprStm thenExpr
                                                 ++ listFPGuardsFBExprStm elseExpr
listFPGuardsFBExprStm (BListIte listThen elseExpr) = concatMap (fpExprsInGuard . fst) listThen
                                                  ++ concatMap (listFPGuardsFBExprStm . snd) listThen
                                                  ++ listFPGuardsFBExprStm elseExpr
listFPGuardsFBExprStm (BExpr be) = fpExprsInGuard be

buildListIsFiniteCheck :: [Arg] -> Either FAExpr FBExprStm -> [FAExpr]
buildListIsFiniteCheck args expr = elimDuplicates $ map arg2var (filter isArgFP args) ++ listIfGuards
  where
    listIfGuards = case expr of
      Left  ae -> listFPGuards ae
      Right be -> listFPGuardsFBExprStm be


hasConditionals :: RProgram -> AExpr -> Bool
hasConditionals _ (Int _) = False
hasConditionals _ (Rat _) = False
hasConditionals _ (Var _ _) = False
hasConditionals _ (ArrayElem _ _ _ _) = False
hasConditionals prog (BinaryOp _ e1 e2) = hasConditionals prog e1 || hasConditionals prog e2
hasConditionals prog (UnaryOp _ e) = hasConditionals prog e
hasConditionals prog (Min es) = foldl1 (||) (map (hasConditionals prog) es)
hasConditionals prog (Max es) = foldl1 (||) (map (hasConditionals prog) es)
hasConditionals prog (RLet letElems expr) = hasConditionals prog expr
                                            || foldl1 (||) (map ((hasConditionals prog) . letExpr) letElems)
hasConditionals prog (RIte _ _ _) = True
hasConditionals prog (RListIte _ _) = True
hasConditionals prog (RForLoop _ _ _ _ _ _ forBody) = hasConditionals prog forBody
hasConditionals prog (EFun f _ _) = applyFunToRDeclBody (hasConditionals prog)
                                                        (hasConditionalsBExpr prog)
                                                        declBody
   where
      declBody = fromMaybe (error $ "Function " ++ show f ++ " not found.")
                 $ findInRealProg f prog
hasConditionals _ expr = error $ "hasConditionals not defined for expression " ++ show expr ++ "."


hasConditionalsBExpr :: RProgram -> BExprStm -> Bool
hasConditionalsBExpr prog (RBIte _ _ _) = True
hasConditionalsBExpr prog (RBListIte _ _) = True
hasConditionalsBExpr prog (RBLet letElems body) = hasConditionalsBExpr prog body
                                                  || foldl1 (||) (map ((hasConditionals prog) . letExpr) letElems)
hasConditionalsBExpr prog (RBExpr be) = hasConditionalsBExpr' prog be
  where
    hasConditionalsBExpr' _ BTrue  = False
    hasConditionalsBExpr' _ BFalse = False
    hasConditionalsBExpr' prog (Or  be1 be2) = hasConditionalsBExpr' prog be1 ||
                                                hasConditionalsBExpr' prog be2
    hasConditionalsBExpr' prog (And be1 be2) = hasConditionalsBExpr' prog be1 ||
                                                hasConditionalsBExpr' prog be2
    hasConditionalsBExpr' prog (Not be) = hasConditionalsBExpr' prog be
    hasConditionalsBExpr' prog (Rel _ ae1 ae2) = hasConditionals prog ae1 ||
                                                  hasConditionals prog ae2
    hasConditionalsBExpr' prog (EPred f _) = applyFunToRDeclBody (hasConditionals prog)
                                                                      (hasConditionalsBExpr prog)
                                                                       declBody
      where
        declBody = fromMaybe (error $ "Function " ++ show f ++ " not found.")
                 $ findInRealProg f prog
    hasConditionalsBExpr' _ expr = error $ "hasConditionalsBExpr not defined for expression " ++ show expr ++ "."
-----------------------
-- PPExt instances --
-----------------------

prettyVarWithType :: FAExpr -> Doc
prettyVarWithType (FVar fp x) = text x <> text ":" <+> prettyDoc fp
prettyVarWithType ae = error $ "prettyVarWithType: case " ++ show ae ++ " niy."

instance PPExt Arg where
  prettyDoc (Arg x fp) = text x <> text ":" <+> prettyDoc fp

instance PPExt RProgram where
  prettyDoc decls = vcat (map (\d -> prettyDoc d $$ text "") decls)

instance PPExt RDecl where
  prettyDoc (RDecl fp fun args stm)
    = text fun <> text "(" <>
      hsep (punctuate comma $ map prettyDoc args)
      <> text  "):" <+> prettyDoc fp <+> text " =" $$ prettyDoc stm
  prettyDoc (RPred fun args stm)
    = text fun <> text "(" <>
      hsep (punctuate comma $ map prettyDoc args)
      <> text  "):" <+> text "bool" <+> text " =" $$ prettyDoc stm

instance PPExt Program where
  prettyDoc decls = vcat (map prettyDoc decls)

instance PPExt Decl where
  prettyDoc (Decl isTrans fp fun args stm)
    = text fun <> text suffix <> text "(" <>
      hsep (punctuate comma $ map prettyDoc args)
      <> text  "):" <+> prettyDoc fp <+> text " =" <+> prettyDoc stm
    where
      suffix = if isTrans then "_fp" else ""
  prettyDoc (Pred isTrans _ fun args stm)
    = text fun <> text suffix <> text "(" <>
      hsep (punctuate comma $ map prettyDoc args)
      <> text  "):" <+> text "bool" <+> text " =" <+> prettyDoc stm
    where
      suffix = if isTrans then "_fp" else ""

printBinOpError :: (AExpr -> Doc) -> String -> AExpr -> AExpr -> AExpr -> AExpr -> Doc
printBinOpError f nameErrFun r1 e1 r2 e2 =
    text nameErrFun <> (text "(" <>  prettyError' f r1 <> comma <+> prettyError' f e1 <> comma
                                 <+> prettyError' f r2 <> comma <+> prettyError' f e2 <> text ")")

printUnaryOpError :: (AExpr -> Doc) -> String -> AExpr -> AExpr -> Doc
printUnaryOpError f nameErrFun r e = text nameErrFun <> (text "(" <>  prettyError' f r <> comma
                                            <+> prettyError' f e <> text ")")

instance PPExt AExpr where
  prettyDoc = fix prettyAExpr

prettyAExpr :: (AExpr -> Doc) -> AExpr -> Doc
prettyAExpr f Infinity = text "infinity"
prettyAExpr f (Int i) = integer i
prettyAExpr f (Rat d) = parens $ text $ showRational d
prettyAExpr f (FExp fa) = text "Fexp" <> parens (prettyDoc fa)
prettyAExpr f (RealMark x) = text "r_" <> text x
prettyAExpr f (ErrorMark x _)  = text "e_" <> text x
prettyAExpr f (ErrRat r) = parens $ text $ showRational r
prettyAExpr f (Var _ x) = text x
prettyAExpr f (ArrayElem _ _ v idx) = text v <> text "[" <> prettyDoc idx <> text "]"
prettyAExpr f (EFun g _ []) = text g
prettyAExpr f (EFun g _ args) = text g <> parens (hsep $ punctuate comma $ map (prettyAExpr f) args)
prettyAExpr f (UnaryOp  NegOp (Int i)) = text "-" <> integer i
prettyAExpr f (UnaryOp  NegOp (Rat d)) = text "-" <> parens (text $ showRational d)
prettyAExpr f (FromFloat FPSingle a) = text "StoR"   <> lparen <> prettyDoc a <> rparen
prettyAExpr f (FromFloat FPDouble a) = text "StoD"   <> lparen <> prettyDoc a <> rparen

prettyAExpr f (BinaryOp AddOp   a1 a2) = parens $ f a1 <+> text "+" <+> f a2
prettyAExpr f (BinaryOp SubOp   a1 a2) = parens $ f a1 <+> text "-" <+> f a2
prettyAExpr f (BinaryOp MulOp   a1 a2) = parens $ f a1 <+> text "*" <+> f a2
prettyAExpr f (BinaryOp DivOp   a1 a2) = parens $ f a1 <+> text "/" <+> f a2
prettyAExpr f (BinaryOp IDivOp  a1 a2) = text "Idiv" <> parens (f a1 <> comma <+> f a2)
prettyAExpr f (BinaryOp ItDivOp a1 a2) = text "Itdiv" <> parens (f a1 <> comma <+> f a2)
prettyAExpr f (BinaryOp ModOp   a1 a2) = text "mod" <> parens (f a1 <> comma <+> f a2)
prettyAExpr f (BinaryOp ItModOp a1 a2) = text "Itmod" <> parens (f a1 <> comma <+> f a2)
prettyAExpr f (BinaryOp PowOp   a1 a2) = f a1 <> text "^" <> lparen <> f a2 <> rparen

prettyAExpr f (UnaryOp NegOp   a) = text "-"     <> lparen <> f a <> rparen
prettyAExpr f (UnaryOp FloorOp a) = text "floor" <> lparen <> f a <> rparen
prettyAExpr f (UnaryOp SqrtOp  a) = text "sqrt"  <> lparen <> f a <> rparen
prettyAExpr f (UnaryOp AbsOp   a) = text "abs"   <> lparen <> f a <> rparen
prettyAExpr f (UnaryOp SinOp   a) = text "sin"   <> lparen <> f a <> rparen
prettyAExpr f (UnaryOp CosOp   a) = text "cos"   <> lparen <> f a <> rparen
prettyAExpr f (UnaryOp TanOp   a) = text "tan"   <> lparen <> f a <> rparen
prettyAExpr f (UnaryOp AsinOp  a) = text "asin"  <> lparen <> f a <> rparen
prettyAExpr f (UnaryOp AcosOp  a) = text "acos"  <> lparen <> f a <> rparen
prettyAExpr f (UnaryOp AtanOp  a) = text "atan"  <> lparen <> f a <> rparen
prettyAExpr f (UnaryOp LnOp    a) = text "ln"  <> lparen <> f a <> rparen
prettyAExpr f (UnaryOp ExpoOp  a) = text "exp"  <> lparen <> f a <> rparen

prettyAExpr f (MaxErr []) = error "Something went wrong: MaxErr applied to empty list"
prettyAExpr f (MaxErr [es]) = f es
prettyAExpr f (MaxErr ees@(_:(_:_))) =
    text "max" <> parens (hsep $ punctuate comma (map f ees))

prettyAExpr f (ErrBinOp AddOp FPSingle r1 e1 r2 e2) = printBinOpError f "aeboundsp_add" r1 e1 r2 e2
prettyAExpr f (ErrBinOp AddOp FPDouble r1 e1 r2 e2) = printBinOpError f "aebounddp_add" r1 e1 r2 e2
prettyAExpr f (ErrBinOp AddOp TInt     r1 e1 r2 e2) = printBinOpError f  "aeboundi_add" r1 e1 r2 e2
prettyAExpr f (ErrBinOp SubOp FPSingle r1 e1 r2 e2) = printBinOpError f "aeboundsp_sub" r1 e1 r2 e2
prettyAExpr f (ErrBinOp SubOp FPDouble r1 e1 r2 e2) = printBinOpError f "aebounddp_sub" r1 e1 r2 e2
prettyAExpr f (ErrBinOp SubOp TInt     r1 e1 r2 e2) = printBinOpError f  "aeboundi_sub" r1 e1 r2 e2
prettyAExpr f (ErrBinOp MulOp FPSingle r1 e1 r2 e2) = printBinOpError f "aeboundsp_mul" r1 e1 r2 e2
prettyAExpr f (ErrBinOp MulOp FPDouble r1 e1 r2 e2) = printBinOpError f "aebounddp_mul" r1 e1 r2 e2
prettyAExpr f (ErrBinOp MulOp TInt     r1 e1 r2 e2) = printBinOpError f  "aeboundi_mul" r1 e1 r2 e2
prettyAExpr f (ErrBinOp DivOp FPSingle r1 e1 r2 e2) = printBinOpError f "aeboundsp_div" r1 e1 r2 e2
prettyAExpr f (ErrBinOp DivOp FPDouble r1 e1 r2 e2) = printBinOpError f "aebounddp_div" r1 e1 r2 e2
prettyAExpr f (ErrBinOp DivOp TInt     r1 e1 r2 e2) = printBinOpError f  "aeboundi_div" r1 e1 r2 e2
prettyAExpr f (ErrBinOp ModOp TInt     r1 e1 r2 e2) = printBinOpError f "aeboundi_mod" r1 e1 r2 e2
prettyAExpr f (ErrBinOp PowOp FPSingle r1 e1 r2 e2) = printBinOpError f "aeboundsp_pow" r1 e1 r2 e2
prettyAExpr f (ErrBinOp PowOp FPDouble r1 e1 r2 e2) = printBinOpError f "aebounddp_pow" r1 e1 r2 e2
prettyAExpr f (ErrBinOp PowOp TInt     r1 e1 r2 e2) = printBinOpError f  "aeboundi_pow" r1 e1 r2 e2

prettyAExpr f (ErrMulPow2L fp n e) = text nameErrFun <> (text "(" <> integer n <> comma
                                                 <+> prettyAExpr f e <> text ")")
    where
        nameErrFun = case fp of
                        FPSingle -> "aebounddp_mul_p2l"
                        FPDouble -> "aeboundsp_mul_p2l"
                        _ -> error $ "prettyAExpr f ErrMulPow2L: unexpected type " ++ show fp ++ " value."

prettyAExpr f (ErrMulPow2R fp n e) = text nameErrFun <> (text "(" <> integer n <> comma
                                               <+> prettyAExpr f e <> text ")")
  where
      nameErrFun = case fp of
                      FPSingle -> "aebounddp_mul_p2r"
                      FPDouble -> "aeboundsp_mul_p2r"
                      _ -> error $ "prettyAExpr f ErrMulPow2R: unexpected type " ++ show fp ++ " value."

prettyAExpr f (ErrUnOp FloorOp False FPSingle r e) = printUnaryOpError f "aeboundsp_flr"   r e
prettyAExpr f (ErrUnOp FloorOp False FPDouble r e) = printUnaryOpError f "aebounddp_flr"   r e
prettyAExpr f (ErrUnOp FloorOp True  FPSingle r e) = printUnaryOpError f "aeboundsp_flr_t" r e
prettyAExpr f (ErrUnOp FloorOp True  FPDouble r e) = printUnaryOpError f "aebounddp_flr_t" r e
prettyAExpr f (ErrUnOp SqrtOp  False FPSingle r e) = printUnaryOpError f "aeboundsp_sqt" r e
prettyAExpr f (ErrUnOp SqrtOp  False FPDouble r e) = printUnaryOpError f "aebounddp_sqt" r e
prettyAExpr f (ErrUnOp SinOp   False FPSingle r e) = printUnaryOpError f "aeboundsp_sin" r e
prettyAExpr f (ErrUnOp SinOp   False FPDouble r e) = printUnaryOpError f "aebounddp_sin" r e
prettyAExpr f (ErrUnOp CosOp   False FPSingle r e) = printUnaryOpError f "aeboundsp_cos" r e
prettyAExpr f (ErrUnOp CosOp   False FPDouble r e) = printUnaryOpError f "aebounddp_cos" r e
prettyAExpr f (ErrUnOp TanOp   False FPSingle r e) = printUnaryOpError f "aeboundsp_tan" r e
prettyAExpr f (ErrUnOp TanOp   False FPDouble r e) = printUnaryOpError f "aebounddp_tan" r e
prettyAExpr f (ErrUnOp AcosOp  False FPSingle r e) = printUnaryOpError f "aeboundsp_acs" r e
prettyAExpr f (ErrUnOp AcosOp  False FPDouble r e) = printUnaryOpError f "aebounddp_acs" r e
prettyAExpr f (ErrUnOp AsinOp  False FPSingle r e) = printUnaryOpError f "aeboundsp_asn" r e
prettyAExpr f (ErrUnOp AsinOp  False FPDouble r e) = printUnaryOpError f "aebounddp_asn" r e
prettyAExpr f (ErrUnOp AtanOp  False FPSingle r e) = printUnaryOpError f "aeboundsp_atn" r e
prettyAExpr f (ErrUnOp AtanOp  False FPDouble r e) = printUnaryOpError f "aebounddp_atn" r e
prettyAExpr f (ErrUnOp NegOp   False FPSingle r e) = printUnaryOpError f "aeboundsp_neg" r e
prettyAExpr f (ErrUnOp NegOp   False FPDouble r e) = printUnaryOpError f "aebounddp_neg" r e
prettyAExpr f (ErrUnOp NegOp   False TInt     r e) = printUnaryOpError f  "aeboundi_neg" r e
prettyAExpr f (ErrUnOp AbsOp   False FPSingle r e) = printUnaryOpError f  "aeboundsp_abs" r e
prettyAExpr f (ErrUnOp AbsOp   False FPDouble r e) = printUnaryOpError f  "aebounddp_abs" r e
prettyAExpr f (ErrUnOp AbsOp   False TInt     r e) = printUnaryOpError f   "aeboundi_abs" r e
prettyAExpr f (ErrUnOp ExpoOp  False FPSingle r e) = printUnaryOpError f  "aeboundsp_exp" r e
prettyAExpr f (ErrUnOp ExpoOp  False FPDouble r e) = printUnaryOpError f  "aebounddp_exp" r e
prettyAExpr f (ErrUnOp LnOp    False FPSingle r e) = printUnaryOpError f  "aeboundsp_ln"  r e
prettyAExpr f (ErrUnOp LnOp    False FPDouble r e) = printUnaryOpError f  "aebounddp_ln"  r e
prettyAExpr f (HalfUlp r FPSingle)
    = text "ulp_sp" <> (text "(" <> prettyAExpr f r <> text ")/2")
prettyAExpr f (HalfUlp r FPDouble)
    = text "ulp_dp" <> (text "(" <> prettyAExpr f r <> text ")/2")
prettyAExpr f (HalfUlp r (Array FPSingle _))
    = text "ulp_sp" <> (text "(" <> prettyAExpr f r <> text ")/2")

prettyAExpr f (HalfUlp r (Array FPDouble _))
    = text "ulp_dp" <> (text "(" <> prettyAExpr f r <> text ")/2")

prettyAExpr f (ErrCast FPSingle FPDouble r e) = printUnaryOpError f "aebound_StoD"  r e
prettyAExpr f (ErrCast FPDouble FPSingle r e) = printUnaryOpError f "aebound_DtoS"  r e
prettyAExpr f (ErrCast TInt     FPSingle r e) = printUnaryOpError f "aebound_ItoS"  r e
prettyAExpr f (ErrCast TInt     FPDouble r e) = printUnaryOpError f "aebound_ItoD"  r e

prettyAExpr f RUnstWarning = error "Warning should not occur in a real-valued program."-- text "warning"
prettyAExpr f (RLet listElem stm)
  = text "LET" <+> vcat (punctuate comma (map (\letElem -> text (letVar letElem)
        <> text ":" <> prettyDoc (letType letElem) <> text "=" <> prettyAExpr f (letExpr letElem)) listElem))
        $$ text "IN" <+> prettyAExpr f stm
prettyAExpr f (RIte be stm1 stm2)
    = text "IF" <+> prettyDoc be
        $$ text "THEN" <+> prettyAExpr f stm1
        $$ text "ELSE" <+> prettyDoc stm2
        $$ text "ENDIF"
prettyAExpr f (RListIte [] _) = error "prettyDoc RListIte: empty stmThen list"
prettyAExpr f (RListIte ((beThen,stmThen):thenList) stmElse)
    = text "IF" <+> prettyDoc beThen $$ text "THEN" <+> prettyAExpr f stmThen
        $$ vcat (map (\(be,stm) -> text "ELSIF" <+> prettyDoc be
        $$ text "THEN" <+> prettyAExpr f stm) thenList)
        $$ text "ELSE" <+> prettyAExpr f stmElse $$ text "ENDIF"
prettyAExpr f (RForLoop fp idxStart idxEnd initAcc idx acc forBody)
    = text "for[" <> prettyDoc fp <> text "]"
        <> parens (prettyDoc idxStart <> comma
        <> prettyDoc idxEnd   <> comma
        <> prettyDoc initAcc  <> comma
        <> text "LAMBDA"
        <+> parens (text idx <> colon <> text "subrange(" <> prettyDoc idxStart <> comma <> prettyDoc idxEnd
        <> comma <> text acc <> colon <> prettyDoc fp) <> colon
        <+> parens (prettyAExpr f forBody)
        )
prettyAExpr f (Prec FPSingle) = text "ieee754sp_prec"
prettyAExpr f (Prec FPDouble) = text "ieee754dp_prec"
prettyAExpr f (Prec fp)        = error $ "prettyAExpr not defined for Prec " ++ show fp
prettyAExpr f ee = error $ "prettyAExpr f for " ++ show ee ++ "not implemented yet."

prettyError :: AExpr -> Doc
prettyError = fix prettyError'

prettyError' :: (AExpr -> Doc) -> AExpr -> Doc
prettyError' f (BinaryOp DivOp   a1 a2) = text "div_safe" <> parens (f a1 <> comma <+> f a2)
prettyError' f (UnaryOp SqrtOp  a) = text "sqrt_safe"  <> lparen <> f a <> rparen
prettyError' f (UnaryOp AsinOp  a) = text "asin_safe"  <> lparen <> f a <> rparen
prettyError' f (UnaryOp AcosOp  a) = text "acos_safe"  <> lparen <> f a <> rparen
prettyError' f (UnaryOp TanOp  a) = text "tan_safe"  <> lparen <> f a <> rparen
prettyError' f (BinaryOp IDivOp  a1 a2) = text "Idiv" <> parens (f a1 <> comma <+> f a2)
prettyError' f (BinaryOp ItDivOp a1 a2) = text "Itdiv" <> parens (f a1 <> comma <+> f a2)
prettyError' f e = prettyAExpr f e

instance PPExt FAExpr where
  prettyDoc (FInt i) = integer i
  prettyDoc (FCnst _ d) = parens $ text $ showRational d
  prettyDoc (UnaryFPOp NegOp _ (FInt i)) = text "-" <> integer i
  prettyDoc (UnaryFPOp NegOp _ (FCnst _ d)) = text "-" <> parens (text $ showRational d)
  prettyDoc (FVar _ x) = text x
  prettyDoc (FArrayElem _ _ v idx) = text v <> text "[" <> prettyDoc idx <> text "]"
  prettyDoc (FEFun isTrans f _ [])   = text f <> text suffix
    where
      suffix = if isTrans then "_fp" else ""
  prettyDoc (FEFun isTrans f _ args) = text f <> text suffix <> parens (hsep $ punctuate comma $ map prettyDoc args)
    where
      suffix = if isTrans then "_fp" else ""
  --
  prettyDoc (ToFloat  FPSingle          a) = prettyDocUnaryOp "RtoS" a
  prettyDoc (ToFloat  FPDouble          a) = prettyDocUnaryOp "RtoD" a
  --
  prettyDoc (TypeCast FPSingle FPDouble a) = prettyDocUnaryOp "StoD"   a
  prettyDoc (TypeCast FPDouble FPSingle a) = prettyDocUnaryOp "DtoS"   a
  prettyDoc (TypeCast TInt     FPSingle a) = prettyDocUnaryOp "ItoS"   a
  prettyDoc (TypeCast TInt     FPDouble a) = prettyDocUnaryOp "ItoD"   a
  --
  prettyDoc (UnaryFPOp NegOp   TInt     a) = prettyDocUnaryOp "Ineg"   a
  prettyDoc (UnaryFPOp NegOp   FPSingle a) = prettyDocUnaryOp "Sneg"   a
  prettyDoc (UnaryFPOp NegOp   FPDouble a) = prettyDocUnaryOp "Dneg"   a
  prettyDoc (UnaryFPOp AbsOp   TInt     a) = prettyDocUnaryOp "Iabs"   a
  prettyDoc (UnaryFPOp AbsOp   FPSingle a) = prettyDocUnaryOp "Sabs"   a
  prettyDoc (UnaryFPOp AbsOp   FPDouble a) = prettyDocUnaryOp "Dabs"   a
  prettyDoc (UnaryFPOp FloorOp FPSingle a) = prettyDocUnaryOp "Sfloor" a
  prettyDoc (UnaryFPOp FloorOp FPDouble a) = prettyDocUnaryOp "Dfloor" a
  prettyDoc (UnaryFPOp SqrtOp  FPSingle a) = prettyDocUnaryOp "Ssqrt"  a
  prettyDoc (UnaryFPOp SqrtOp  FPDouble a) = prettyDocUnaryOp "Dsqrt"  a
  prettyDoc (UnaryFPOp SinOp   FPSingle a) = prettyDocUnaryOp "Ssin"   a
  prettyDoc (UnaryFPOp SinOp   FPDouble a) = prettyDocUnaryOp "Dsin"   a
  prettyDoc (UnaryFPOp CosOp   FPSingle a) = prettyDocUnaryOp "Scos"   a
  prettyDoc (UnaryFPOp CosOp   FPDouble a) = prettyDocUnaryOp "Dcos"   a
  prettyDoc (UnaryFPOp TanOp   FPSingle a) = prettyDocUnaryOp "Stan"   a
  prettyDoc (UnaryFPOp TanOp   FPDouble a) = prettyDocUnaryOp "Dtan"   a
  prettyDoc (UnaryFPOp AsinOp  FPSingle a) = prettyDocUnaryOp "Sasin"  a
  prettyDoc (UnaryFPOp AsinOp  FPDouble a) = prettyDocUnaryOp "Dasin"  a
  prettyDoc (UnaryFPOp AcosOp  FPSingle a) = prettyDocUnaryOp "Sacos"  a
  prettyDoc (UnaryFPOp AcosOp  FPDouble a) = prettyDocUnaryOp "Dacos"  a
  prettyDoc (UnaryFPOp AtanOp  FPSingle a) = prettyDocUnaryOp "Satan"  a
  prettyDoc (UnaryFPOp AtanOp  FPDouble a) = prettyDocUnaryOp "Datan"  a
  prettyDoc (UnaryFPOp LnOp    FPSingle a) = prettyDocUnaryOp "Sln"    a
  prettyDoc (UnaryFPOp LnOp    FPDouble a) = prettyDocUnaryOp "Dln"    a
  prettyDoc (UnaryFPOp ExpoOp  FPSingle a) = prettyDocUnaryOp "Sexp"   a
  prettyDoc (UnaryFPOp ExpoOp  FPDouble a) = prettyDocUnaryOp "Dexp"   a
  --
  prettyDoc (BinaryFPOp AddOp   TInt     a1 a2) = prettyDocBinaryOp "Iadd"  a1 a2
  prettyDoc (BinaryFPOp AddOp   FPSingle a1 a2) = prettyDocBinaryOp "Sadd"  a1 a2
  prettyDoc (BinaryFPOp AddOp   FPDouble a1 a2) = prettyDocBinaryOp "Dadd"  a1 a2
  prettyDoc (BinaryFPOp SubOp   TInt     a1 a2) = prettyDocBinaryOp "Isub"  a1 a2
  prettyDoc (BinaryFPOp SubOp   FPSingle a1 a2) = prettyDocBinaryOp "Ssub"  a1 a2
  prettyDoc (BinaryFPOp SubOp   FPDouble a1 a2) = prettyDocBinaryOp "Dsub"  a1 a2
  prettyDoc (BinaryFPOp MulOp   TInt     a1 a2) = prettyDocBinaryOp "Imul"  a1 a2
  prettyDoc (BinaryFPOp MulOp   FPSingle a1 a2) = prettyDocBinaryOp "Smul"  a1 a2
  prettyDoc (BinaryFPOp MulOp   FPDouble a1 a2) = prettyDocBinaryOp "Dmul"  a1 a2
  prettyDoc (BinaryFPOp DivOp   TInt     a1 a2) = prettyDocBinaryOp "Idiv"  a1 a2
  prettyDoc (BinaryFPOp DivOp   FPSingle a1 a2) = prettyDocBinaryOp "Sdiv"  a1 a2
  prettyDoc (BinaryFPOp DivOp   FPDouble a1 a2) = prettyDocBinaryOp "Ddiv"  a1 a2
  prettyDoc (BinaryFPOp ItDivOp TInt     a1 a2) = prettyDocBinaryOp "Itdiv" a1 a2
  prettyDoc (BinaryFPOp ModOp   TInt     a1 a2) = prettyDocBinaryOp "Imod"  a1 a2
  prettyDoc (BinaryFPOp ModOp   FPSingle a1 a2) = prettyDocBinaryOp "Smod"  a1 a2
  prettyDoc (BinaryFPOp ModOp   FPDouble a1 a2) = prettyDocBinaryOp "Dmod"  a1 a2
  prettyDoc (BinaryFPOp ItModOp TInt     a1 a2) = prettyDocBinaryOp "Itmod" a1 a2
  prettyDoc (BinaryFPOp PowOp   TInt     a1 a2) = prettyDocBinaryOp "pow_int"  a1 a2
  prettyDoc (BinaryFPOp PowOp   _        a1 a2) = prettyDoc a1 <+> text "^" <+> lparen <> prettyDoc a2 <> rparen
  --
  prettyDoc (FFma   FPSingle a1 a2 a3) = text "fma_single" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2 <> comma <+> prettyDoc a3)
  prettyDoc (FFma   FPDouble a1 a2 a3) = text "fma_double" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2 <> comma <+> prettyDoc a3)
  --
  prettyDoc (FMin as) = text "min" <> parens (hsep $ punctuate comma $ map prettyDoc as)
  prettyDoc (FMax as) = text "max" <> parens (hsep $ punctuate comma $ map prettyDoc as)
  --
  prettyDoc UnstWarning = text "warning"
  prettyDoc (Let letElem stm)
      = text "LET" <+> vcat (punctuate comma (map (\(x,t,ae) -> text x <> text ":" <> prettyDoc t
                                                 <> text "=" <> prettyDoc ae) letElem))
          $$ text "IN" <+> prettyDoc stm
  prettyDoc (Ite be stm1 stm2)
      = text "IF" <+> prettyDoc be
          $$ text "THEN" <+> prettyDoc stm1
          $$ text "ELSE" <+> prettyDoc stm2
          $$ text "ENDIF"
  prettyDoc (ListIte [] _) = error "prettyDoc RListIte: empty stmThen list"
  prettyDoc (ListIte ((beThen,stmThen):thenList) stmElse)
      = text "IF" <+> prettyDoc beThen $$ text "THEN" <+> prettyDoc stmThen
          $$ vcat (map (\(be,stm) -> text "ELSIF" <+> prettyDoc be
          $$ text "THEN" <+> prettyDoc stm) thenList)
          $$ text "ELSE" <+> prettyDoc stmElse $$ text "ENDIF"
  prettyDoc (ForLoop fp idxStart idxEnd initAcc idx acc forBody)
    = text "for[" <> prettyDoc fp <> text "]"
          <> parens (prettyDoc idxStart <> comma
          <> prettyDoc idxEnd   <> comma
          <> prettyDoc initAcc  <> comma
          <> text "LAMBDA"
          <+> parens (text idx <> colon <> text "subrange(" <> prettyDoc idxStart <> comma <> prettyDoc idxEnd
          <> comma <> text acc <> colon <> prettyDoc fp) <> colon
          <+> parens (prettyDoc forBody)
          )
  --
  prettyDoc ee = error $ "prettyDoc for " ++ show ee ++ "not implemented yet."

instance PPExt BExpr where
  prettyDoc BTrue  = text "TRUE"
  prettyDoc BFalse = text "FALSE"
  prettyDoc (Or  be1 be2) = parens $ prettyDoc be1 <+> text "OR"  <+> prettyDoc be2
  prettyDoc (And be1 be2) = parens $ prettyDoc be1 <+> text "AND" <+> prettyDoc be2
  prettyDoc (Not be) = text "NOT" <> parens (prettyDoc be)
  prettyDoc (Rel Eq  a1 a2) = parens $ prettyDoc a1 <+> text "="  <+> prettyDoc a2
  prettyDoc (Rel Neq a1 a2) = parens $ prettyDoc a1 <+> text "/=" <+> prettyDoc a2
  prettyDoc (Rel Lt  a1 a2) = parens $ prettyDoc a1 <+> text "<"  <+> prettyDoc a2
  prettyDoc (Rel LtE a1 a2) = parens $ prettyDoc a1 <+> text "<=" <+> prettyDoc a2
  prettyDoc (Rel Gt  a1 a2) = parens $ prettyDoc a1 <+> text ">"  <+> prettyDoc a2
  prettyDoc (Rel GtE a1 a2) = parens $ prettyDoc a1 <+> text ">=" <+> prettyDoc a2
  prettyDoc (EPred f args) = text f <> parens (hsep $ punctuate comma $ map prettyDoc args)

instance PPExt BExprStm where
  prettyDoc (RBLet listElem stm)
    = text "LET" <+> vcat (punctuate comma (map (\letElem -> text (letVar letElem)
          <> text ":" <> prettyDoc (letType letElem) <> text "=" <> prettyDoc (letExpr letElem)) listElem))
          $$ text "IN" <+> prettyDoc stm
  prettyDoc (RBIte be stm1 stm2)
         = text "IF" <+> prettyDoc be
          $$ text "THEN" <+> prettyDoc stm1
          $$ text "ELSE" <+> prettyDoc stm2
          $$ text "ENDIF"
  prettyDoc (RBListIte [] _) = error "prettyDoc RBListIte: empty stmThen list"
  prettyDoc (RBListIte ((beThen,stmThen):thenList) stmElse)
      = text "IF" <+> prettyDoc beThen $$ text "THEN" <+> prettyDoc stmThen
          $$ vcat (map (\(be,stm) -> text "ELSIF" <+> prettyDoc be
          $$ text "THEN" <+> prettyDoc stm) thenList)
          $$ text "ELSE" <+> prettyDoc stmElse $$ text "ENDIF"
  prettyDoc (RBExpr be) = prettyDoc be

instance PPExt FBExpr where
  prettyDoc FBTrue  = text "TRUE"
  prettyDoc FBFalse = text "FALSE"
  prettyDoc (FOr  be1 be2) = parens $ prettyDoc be1 <+> text "OR"  <+> prettyDoc be2
  prettyDoc (FAnd be1 be2) = parens $ prettyDoc be1 <+> text "AND" <+> prettyDoc be2
  prettyDoc (FNot be) = text "NOT" <> parens (prettyDoc be)
  prettyDoc (FRel Eq  a1 a2) | getPVSType a1 == FPSingle &&
                               getPVSType a2 == FPSingle
                               = text "qeq_efs" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
                             | getPVSType a1 == FPDouble &&
                               getPVSType a2 == FPDouble
                               = text "qeq_efd" <> parens (prettyDoc a1 <> comma <+> prettyDoc a2)
                             | otherwise = parens $ prettyDoc a1 <+> text "="  <+> prettyDoc a2
  prettyDoc (FRel Neq a1 a2) = parens $ prettyDoc a1 <+> text "/=" <+> prettyDoc a2
  prettyDoc (FRel Lt  a1 a2) = parens $ prettyDoc a1 <+> text "<"  <+> prettyDoc a2
  prettyDoc (FRel LtE a1 a2) = parens $ prettyDoc a1 <+> text "<=" <+> prettyDoc a2
  prettyDoc (FRel Gt  a1 a2) = parens $ prettyDoc a1 <+> text ">"  <+> prettyDoc a2
  prettyDoc (FRel GtE a1 a2) = parens $ prettyDoc a1 <+> text ">=" <+> prettyDoc a2
  prettyDoc (IsValid  ae) = text "isValid" <> parens (prettyDoc ae)
  prettyDoc (BIsValid be) = text "isValid" <> parens (prettyDoc be)
  prettyDoc (FEPred isTrans _ f args) = text f <> text suffix <> parens (hsep $ punctuate comma $ map prettyDoc args)
    where
      suffix = if isTrans then "_fp" else ""
  prettyDoc (BValue _) = error "prettyDoc niy for BValue."
  prettyDoc (BStructVar x) = text x

instance PPExt FBExprStm where
  prettyDoc (BLet listElem stm)
    = text "LET" <+> vcat (punctuate comma (map (\(x,t,ae) -> text x <> text ":" <> prettyDoc t <> text "=" <> prettyDoc ae) listElem))
          $$ text "IN" <+> prettyDoc stm
  prettyDoc (BIte be stm1 stm2)
         = text "IF" <+> prettyDoc be
          $$ text "THEN" <+> prettyDoc stm1
          $$ text "ELSE" <+> prettyDoc stm2
          $$ text "ENDIF"
  prettyDoc (BListIte [] _) = error "prettyDoc RBListIte: empty stmThen list"
  prettyDoc (BListIte ((beThen,stmThen):thenList) stmElse)
      = text "IF" <+> prettyDoc beThen $$ text "THEN" <+> prettyDoc stmThen
          $$ vcat (map (\(be,stm) -> text "ELSIF" <+> prettyDoc be
          $$ text "THEN" <+> prettyDoc stm) thenList)
          $$ text "ELSE" <+> prettyDoc stmElse $$ text "ENDIF"
  prettyDoc (BExpr be) = prettyDoc be
  prettyDoc BUnstWarning = error "Warning should not occur in a real-valued program."

