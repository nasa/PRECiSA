-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
 
-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
 
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.
  
  
module TransformationUtils where

import Transformation
import AbsPVSLang
import AbstractSemantics (LocalEnv,emptyEnv,symbolicErrorStable, Interpretation)
import Translation.Float2Real
import AbsSpecLang
import NumericalError
import Data.Maybe (fromMaybe)
import Common.TypesUtils (VarName)

computeErrorGuards :: Spec
                  -> Interpretation
                  -> (Decl, ErrVarEnv,LocalEnv,[(FAExpr,AExpr)])
                  ->  IO (Decl,[(VarName,FAExpr,AExpr,FBExpr,EExpr,Double,[FAExpr],[AExpr],[EExpr],[VarBind])])
computeErrorGuards (Spec spec) interp (d@(Decl _ _ f _ _), exprList, _,_) = do
  aeErrs <- mapM (computeErrorVarValue varBinds interp) exprList
  return (d, aeErrs)
  where 
    varBinds = fromMaybe (error $ "computeErrorGuards: function " ++ show f ++ " not found in " ++ show spec) (findOrigFunInSpec f spec)
computeErrorGuards (Spec spec) interp (d@(Pred _ _ f _ _), exprList, _,_) = do
  aeErrs <- mapM (computeErrorVarValue varBinds interp) exprList
  return (d, aeErrs)
  where 
    varBinds = fromMaybe (error $ "computeErrorGuards: function " ++ show f ++ " not found in " ++ show spec) (findOrigFunInSpec f spec)

computeErrorVarValue :: [VarBind]
                     -> Interpretation
                     -> (VarName,FAExpr,FBExpr)
                     -> IO (VarName,FAExpr,AExpr,FBExpr,EExpr,Double,[FAExpr],[AExpr],[EExpr],[VarBind])
computeErrorVarValue varBinds interp (errorVariable, fae, be) = do
  err <- computeNumRoundOffError varBinds symbErr
  return (errorVariable,fae,fae2real fae,be,symbErr,err,faeVarList,realVarList,errVarList,varBinds)
  where
     faeVarList  = varList fae
     errVarList  = map errVar  faeVarList
     realVarList = map realVar faeVarList
     symbErr = symbolicErrorStable interp emptyEnv fae 

