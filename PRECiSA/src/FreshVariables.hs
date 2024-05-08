-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module FreshVariables where

import AbstractDomain
import AbsPVSLang
import AbsSpecLang
import PVSTypes
import Control.Monad.State
import Data.List(find,zip4)
import Data.Maybe(mapMaybe,fromMaybe)
import Kodiak.Runnable
import Kodiak.Runner
import Common.DecisionPath
import Utils
import Translation.Float2Real
import Operators
import Common.TypesUtils
import Foreign.C

type ReplaceFPState = State (Int,[(VarName, FAExpr, PVSType)])
type ReplaceRState  = State (Int,[(VarName,  AExpr)])

data SMTmodel = SMT [(VarName, PVSType)] [VarName] Conditions

baseFPVarName :: String
baseFPVarName = "Temp_"

baseRVarNameTemp :: String
baseRVarNameTemp  = "Real_Temp_"

genFreshVar :: AExpr -> ReplaceRState AExpr
genFreshVar ae = do
    (counter,varMap) <- get
    let freshVar = baseRVarNameTemp ++ show counter
    let newMap = (freshVar, ae):varMap
    put (counter+1,newMap)
    return $ Var Real freshVar

replaceFreshVarInBExpr :: BExpr -> ReplaceRState BExpr
replaceFreshVarInBExpr BTrue = return BTrue
replaceFreshVarInBExpr BFalse = return BFalse
replaceFreshVarInBExpr (Or be1 be2) = do
    be1' <- replaceFreshVarInBExpr be1
    be2' <- replaceFreshVarInBExpr be2
    return $ uncurry Or (be1',be2')
replaceFreshVarInBExpr (And be1 be2) = do
    be1' <- replaceFreshVarInBExpr be1
    be2' <- replaceFreshVarInBExpr be2
    return $ uncurry And (be1',be2')
replaceFreshVarInBExpr (Not be) = do
    be' <- replaceFreshVarInBExpr be
    return $ Not be'
replaceFreshVarInBExpr (Rel rel ae1 ae2) = do
    ae1' <- replaceFreshVarInAExpr ae1
    ae2' <- replaceFreshVarInAExpr ae2
    return $ Rel rel ae1' ae2'
replaceFreshVarInBExpr (EPred f args)= do
    args' <- mapM replaceFreshVarInAExpr args
    return $ EPred f args'

replaceFreshVarInAExpr :: AExpr -> ReplaceRState AExpr
replaceFreshVarInAExpr ae@(Int _) = return ae
replaceFreshVarInAExpr ae@(Rat _) = return ae
replaceFreshVarInAExpr ae@(Var _ _) = return ae
replaceFreshVarInAExpr ae@(RealMark _ _) = return ae
replaceFreshVarInAExpr ae@(ErrorMark _ _ _) = return ae
replaceFreshVarInAExpr ae@(FromFloat _ (FVar _ _)) = return ae
replaceFreshVarInAExpr ae@(FromFloat _ (FInt _))   = return ae
replaceFreshVarInAExpr ae@(UnaryOp NegOp (Int _))   = return ae
replaceFreshVarInAExpr ae@(UnaryOp NegOp (Rat _))   = return ae
replaceFreshVarInAExpr ae         =
    do  (_,varMap) <- get
        case find ((== ae) . snd) varMap of
            Nothing            -> genFreshVar ae
            Just (var,_) -> return $ Var Real var

genFreshFVar :: FAExpr -> PVSType -> ReplaceFPState FAExpr
genFreshFVar ae fp = do
    (counter,varMap) <- get
    let freshVar = baseFPVarName ++ show counter
    let newMap = (freshVar, ae, fp):varMap
    put (counter+1,newMap)
    return $ FVar fp freshVar

replaceFreshVarInFBExpr :: FBExpr -> ReplaceFPState FBExpr
replaceFreshVarInFBExpr FBTrue  = return FBTrue
replaceFreshVarInFBExpr FBFalse = return FBFalse
replaceFreshVarInFBExpr v@(BStructVar _) = return v
replaceFreshVarInFBExpr (FOr be1 be2) = do
    be1' <- replaceFreshVarInFBExpr be1
    be2' <- replaceFreshVarInFBExpr be2
    return $ uncurry FOr (be1',be2')
replaceFreshVarInFBExpr (FAnd be1 be2) = do
    be1' <- replaceFreshVarInFBExpr be1
    be2' <- replaceFreshVarInFBExpr be2
    return $ uncurry FAnd (be1',be2')
replaceFreshVarInFBExpr (FNot be) = do
    be' <- replaceFreshVarInFBExpr be
    return $ FNot be'
replaceFreshVarInFBExpr (FRel rel ae1 ae2) = do
    ae1' <- replaceFreshVarInFAExpr ae1
    ae2' <- replaceFreshVarInFAExpr ae2
    return $ FRel rel ae1' ae2'
replaceFreshVarInFBExpr (IsValid ae) = do
    ae' <- replaceFreshVarInFAExpr ae
    return $ IsValid ae'
replaceFreshVarInFBExpr (FEPred isTrans predAbs f args)= do
    args' <- mapM replaceFreshVarInFAExpr args
    return $ FEPred isTrans predAbs f args'
replaceFreshVarInFBExpr (BIsValid be) = do
    be' <- replaceFreshVarInFBExpr be
    return $ BIsValid be'
replaceFreshVarInFBExpr (BValue be) = do
    be' <- replaceFreshVarInFBExpr be
    return $ BValue be'

replaceFreshVarInFAExpr :: FAExpr -> ReplaceFPState FAExpr
replaceFreshVarInFAExpr ae@(FInt _)    = return ae
replaceFreshVarInFAExpr ae@(FCnst _ _) = return ae
replaceFreshVarInFAExpr ae@(FVar _ _)    = return ae
replaceFreshVarInFAExpr ae@(ToFloat _ (Int _))   = return ae
replaceFreshVarInFAExpr ae@(ToFloat _ (Rat _))   = return ae
replaceFreshVarInFAExpr ae@(UnaryFPOp NegOp _ (FInt _))   = return ae
replaceFreshVarInFAExpr ae@(UnaryFPOp NegOp _ (ToFloat _ (Int _)))   = return ae
replaceFreshVarInFAExpr ae@(UnaryFPOp NegOp _ (ToFloat _ (Rat _)))   = return ae
replaceFreshVarInFAExpr ae =
    do  (_,varMap) <- get
        case find ((== ae) . second) varMap of
            Nothing        -> genFreshFVar ae fp
            Just (var,_,_) -> return $ FVar fp var
    where
        fp = getPVSType ae
        second (_,t,_) = t
