-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
 
-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
 
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module FramaC.PrettyPrint where

import AbsPVSLang
import AbsSpecLang
import AbstractDomain (Condition)
import AbstractSemantics (Interpretation,LocalEnv)
import Common.TypesUtils
import Common.TypeConversions (fprec2acsl)
import Data.Maybe (fromMaybe)
import FramaC.ACSLlang (prettyACSL)
import qualified FramaC.ACSLTypes as ACSL
import FramaC.GenerateACSL
import FramaC.PVS2C (decl2C,pred2C,generateNumericFunction)
import FramaC.Types (HasConditionals)
import Prelude hiding ((<>))
import PPExt
import PVSTypes
import Transformation
import Utils

data TauDeclInfo = TauDeclInfo {
    fpDecl :: Decl,
    realDecl :: RDecl,
    tauDecl :: Decl,
    symbErrVars :: [(String, FAExpr, FBExpr)],
    numErrVars  :: [(String, Double)],
    stableConds :: [Condition],
    symbError :: EExpr,
    numError :: Double,
    initValues ::  [VarBind],
    forMap :: [(FAExpr,AExpr)]
}
  deriving (Eq, Ord, Read, Show)
  
genFramaCFile :: PVSType
              -> Spec
              -> [RDecl]
              -> [Decl]
              -> [(Decl, ErrVarEnv, LocalEnv, [(FAExpr,AExpr)])]
              -> [(FunName, (EExpr, [Condition]))]
              -> [(FunName, Double)]
              -> [(Decl, [(VarName, b, c, d, e, Double, g, h, i, j)])]
              -> Interpretation
              -> Doc
genFramaCFile fp (Spec specBinds) realDecls decls tauDecls symbErrs errs funErrEnv semStable = 
    printHeader
    $$
    text (vspace 1)
    $$
    vcat (map (printDeclWithACSL fp hasCondsFun semStable . buildTauDeclInfo) tauDecls)
    $$
    text (vspace 1)
    $$
    printMain
  where
    hasCondsFun = funHasConds realDecls
    buildTauDeclInfo (declTau, errVarEnv, _, forExprs) = TauDeclInfo {
      fpDecl   = fromMaybe (errMsg f) (findOrigFunInProg     f decls),
      realDecl = fromMaybe (errMsg f) (findOrigFunInRealProg f realDecls),
      tauDecl  = declTau,
      symbErrVars = errVarEnv,
      numErrVars = findInErrEnv f funErrEnv,
      stableConds = snd $ fromMaybe (errMsg f) (lookupOrigFun f symbErrs),
      symbError   = fst $ fromMaybe (errMsg f) (lookupOrigFun f symbErrs),
      numError = fromMaybe (errMsg f) (lookupOrigFun f errs),
      initValues = fromMaybe (errMsg f) (findOrigFunInSpec f specBinds),
      forMap = forExprs
    }
      where
        f = declName declTau
        errMsg g = error $ "genFramaCFile: function " ++ show g ++ "not found."
        findInErrEnv fun [] = error $ "findInErrEnv: function "++ show fun ++ " not found."
        findInErrEnv fun ((decl,errEnv):ds) | fun == declName decl = map getErrVarValue errEnv
                                            | otherwise            = findInErrEnv fun ds
        getErrVarValue (ev,_,_,_,_,val,_,_,_,_) = (ev,val)


printDeclWithACSL :: PVSType
                  -> (FunName -> Bool)
                  -> Interpretation
                  -> TauDeclInfo
                  -> Doc
printDeclWithACSL fp hasCondsFun semStable tauDeclInfo =
  printSymbDeclWithACSL hasCondsFun fp semStable (realDecl tauDeclInfo)
                                     (fpDecl tauDeclInfo)
                                     (tauDecl tauDeclInfo)
                                     (forMap tauDeclInfo)
                                     (symbErrVars tauDeclInfo)
                                     (stableConds tauDeclInfo)
  $$
  text (vspace 1)
  $$
  printNumDeclWithACSL hasCondsFun (predAbstraction $ tauDecl tauDeclInfo)
                       fp
                       (declName $ fpDecl tauDeclInfo)
                       (realDeclArgs $ realDecl tauDeclInfo)
                       (declArgs $ fpDecl tauDeclInfo)
                       (forIdxList $ tauDecl tauDeclInfo)
                       (tauDecl tauDeclInfo)
                       (initValues tauDeclInfo)
                       (numError tauDeclInfo)
                       (numErrVars tauDeclInfo)
                       isFiniteExprList
  where
    isFiniteExprList = buildListIsFiniteCheck (declArgs $ fpDecl tauDeclInfo) (declBody $ fpDecl tauDeclInfo)
    forIdxList decl | isDecl decl = case declBody $ tauDecl tauDeclInfo of 
                                      Left  body -> forIndexes body
                                      Right _ -> error "printDeclWithACSL: boolean expression expected in predicate body."
                    | otherwise = []


printNumDeclWithACSL :: HasConditionals
                     -> Maybe PredAbs
                     -> PVSType
                     -> String
                     -> [Arg]
                     -> [Arg]
                     -> [(VarName, FAExpr, FAExpr)]
                     -> Decl
                     -> [VarBind]
                     -> Double
                     -> [(VarName,Double)]
                     -> [FAExpr]
                     -> Doc
printNumDeclWithACSL hasConds predAbs fp f realArgs initArgs forIdx decl varBinds roErr numErrExprs isFiniteExprList =
  text "/*@"
  $$
  prettyDoc assignsNothing
  $$
  prettyDoc (generateNumericProp hasConds predAbs fp f forIdx realArgs roErr varBinds isFiniteExprList)
  $$
  text "*/"
  $$
  prettyDoc (generateNumericFunction hasConds predAbs fpFun f initArgs  errArgs numErrExprs)
    where
      (_, errArgs) =  splitAt (length initArgs) args
      fpFun = declType decl
      args  = declArgs decl

printSymbDeclWithACSL :: HasConditionals
                      -> PVSType
                      -> Interpretation
                      -> RDecl
                      -> Decl
                      -> Decl
                      -> [(FAExpr,AExpr)]
                      -> [(VarName, FAExpr, FBExpr)]
                      -> [Condition]
                      -> Doc
printSymbDeclWithACSL hasConds fp interp rDecl@(RDecl _ f realArgs _)
                                         fDecl@(Decl _ _ _ fpargs stm)
                                         taudecl@(Decl _ _ _ tauargs taustm)
                                         forVarsMap
                                         errVars
                                         listStableCond =
  text (vspace 1)
  $$
  realDeclForDoc
  $$
  prettyACSL (genAxiomaticRealFun False realDeclMain)
  $$
  fpDeclForDoc
  $$
  text (vspace 1)
  $$
  prettyACSL (genAxiomaticFPFun False fpDeclMain)
  $$
  text (vspace 1)
  $$
  (if (hasConds f)
    then printAxiomaticTranPreds Nothing fp targetFPType f realArgs listStableCond forIdxs
         $$
         text (vspace 1)
    else empty)
  $$
  printFPSymbPrecond hasConds targetFPType f realArgs fpargs errVars localVariables forIdxs isFiniteExprList
  $$
  prettyDoc (decl2C forListExpr forVarsMap interp taudecl hasConds)
    where
      isFiniteExprList = buildListIsFiniteCheck tauargs (Left stm)
      localVariables = localVars taustm
      forIdxs = forIndexes taustm
      (realDeclMain,(realDeclFor, forListExpr)) = makeRealDeclRecursive rDecl
      (fpDeclMain,fpDeclFor) = makeFPDeclRecursive fDecl
      realDeclForDoc = if null realDeclFor
                       then emptyDoc
                       else vcat (map (prettyACSL . genAxiomaticRealFun True) realDeclFor)
      fpDeclForDoc = if null fpDeclFor
                       then emptyDoc
                       else vcat (map (prettyACSL . genAxiomaticFPFun True) fpDeclFor)
      targetFPType = fprec2acsl fp

printSymbDeclWithACSL hasConds fp interp rDecl@(RPred f realArgs _ )
                                fdecl@(Pred _ _ _ fpargs stm)
                                taudecl@(Pred _ predAbs _ tauargs _)
                                _ errVars listStableCond =
  text (vspace 1)
  $$
  (if (predAbs == TauMinus)
    then emptyDoc
    else (prettyACSL (genAxiomaticRealFun False rDecl)
          $$
          text (vspace 1)
          $$
          prettyACSL (genAxiomaticFPFun False fdecl))
  )
  $$
  text (vspace 1)
  $$
  axiomaticTransformationPredicatesDoc
  $$
  printFPSymbPrecondPred hasConds targetFPType predAbs f realArgs fpargs errVars [] isFiniteExprList
  $$
  prettyDoc (pred2C hasConds interp taudecl)
    where
      targetFPType = fprec2acsl fp
      axiomaticTransformationPredicatesDoc =
        if (hasConds f)
          then printAxiomaticTranPreds (Just predAbs) fp targetFPType f realArgs listStableCond [] $$ text (vspace 1)
          else empty
      isFiniteExprList = buildListIsFiniteCheck tauargs (Right stm)

printSymbDeclWithACSL _ _ _ _ _ _ _ _ _ = error "printSymbDeclWithACSL: mismatch type in declarations."

printAxiomaticTranPreds :: Maybe PredAbs
                        -> PVSType
                        -> ACSL.Type
                        -> String
                        -> [Arg]
                        -> [Condition]
                        -> [(VarName, FAExpr, FAExpr)]
                        -> Doc
printAxiomaticTranPreds predAbs pvsType targetFPType f realArgs listStableCond forIdxs =
  text "/*@ axiomatic " <> text (f ++ suffixPredAbs predAbs ++"_trans") <+> text "{"
  $$ prettyDoc stablePathPred
  $$ text "}"
  $$ text "*/"
  where
    stablePathPred = generateStablePathPred predAbs pvsType f realArgs listStableCond forIdxs
    suffixPredAbs (Just TauPlus)  = "_tauplus"
    suffixPredAbs (Just TauMinus) = "_tauminus"
    suffixPredAbs        _ = ""

printFPSymbPrecond ::
  HasConditionals ->
  ACSL.Type ->
  FunName ->
  [Arg] ->
  [Arg] ->
  [(VarName, FAExpr, FBExpr)] ->
  [(VarName, FAExpr)] ->
  [(VarName, FAExpr, FAExpr)] ->
  [FAExpr] ->
  Doc
printFPSymbPrecond hasConds targetFPType f realArgs fpArgs errVars locVars forIdxs isFiniteExprList =
  text "/*@"
    $$ vcat (map prettyDoc (errorVarPrecond (listFst3 errVars)))
    $$ text "\nbehavior structure:"
    $$ prettyDoc (behaviorStructure hasConds targetFPType f fpArgs isFiniteExprList)
    $$ (if (hasConds f)
      then
        text "\nbehavior stable_paths:"
          $$ prettyDoc (behaviorStablePaths hasConds f Nothing targetFPType realArgs errVars locVars forIdxs isFiniteExprList)
      else
        empty)
    $$ text "*/"

printFPSymbPrecondPred ::
  HasConditionals ->
  ACSL.Type ->
  PredAbs ->
  FunName ->
  [Arg] ->
  [Arg] ->
  [(VarName, FAExpr, FBExpr)] ->
  [(VarName, FAExpr)] ->
  [FAExpr] ->
  Doc
printFPSymbPrecondPred hasConds targetFPType predAbs f realArgs fpArgs errVars locVars isFiniteExprList =
  text "/*@"
    $$ vcat (map prettyDoc (errorVarPrecond (listFst3 errVars)))
    $$ ensureBehaviorDoc
    $$ ensureStabilityDoc
    $$ text "*/"
  where
    ensureBehaviorDoc =
      text "\nbehavior structure:"
        $$ prettyDoc (behaviorStructurePred hasConds targetFPType predAbs f realArgs fpArgs errVars locVars isFiniteExprList)
    ensureStabilityDoc =
      if (hasConds f)
        then
          text "\nbehavior stable_paths:"
            $$ prettyDoc (behaviorStablePaths hasConds f (Just predAbs) targetFPType realArgs errVars locVars [] isFiniteExprList)
        else empty

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






