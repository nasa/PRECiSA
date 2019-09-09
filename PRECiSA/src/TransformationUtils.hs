module TransformationUtils where

import Transformation
import AbsPVSLang
import AbstractDomain
import AbstractSemantics (Env, LocalEnv,unfoldLocalVars,emptyEnv)
import Translation.Float2Real
import ErrorExpression
import AbsSpecLang
import NumericalError
import FPrec(VarName)
import Debug.Trace

computeErrorGuards :: Spec -> (Decl, ErrVarEnv,LocalEnv) ->  IO (Decl,[(VarName,FAExpr,AExpr,FBExpr,EExpr,Double,[FAExpr],[AExpr],[EExpr],[VarBind])])
computeErrorGuards (Spec spec) (d@(Decl _ f _ _), exprList, localEnv) = do
  aeErrs <- mapM (computeErrorVarValue varBinds localEnv) exprList
  return (d, aeErrs)
  where 
    varBinds = findInSpec f spec

computeErrorVarValue :: [VarBind] -> LocalEnv -> (VarName,FAExpr,FBExpr) -> IO (VarName,FAExpr,AExpr,FBExpr,EExpr,Double,[FAExpr],[AExpr],[EExpr],[VarBind])
computeErrorVarValue varBinds localEnv (errorVariable, fae, be) = do
  err <- roError varBinds [] (unfoldLocalVars localEnv fae)
  -- err <- computeNumRoundOffError varBinds symbErr
  return (errorVariable,fae,fae2real fae,be,symbErr,err,faeVarList,realVarList,errVarList,varBinds)
  where
     faeVarList  = varList fae
     errVarList  = map errVar faeVarList
     realVarList = map realVar faeVarList
     symbErr = symbolicError [] emptyEnv (unfoldLocalVars localEnv fae) 



