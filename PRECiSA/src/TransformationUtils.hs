module TransformationUtils where

import Transformation
import AbsPVSLang
import AbstractDomain
import AbstractSemantics (Env)
import Translation.Float2Real
import ErrorExpression
import AbsSpecLang
import NumericalError
import FPrec(VarName)
import Debug.Trace

computeErrors :: Spec -> Env ACebS -> (Decl, ErrVarEnv) ->  IO (Decl,[(FAExpr,AExpr,FBExpr,EExpr,Double,[FAExpr],[AExpr],[EExpr],[VarBind])])
computeErrors (Spec spec) env (d@(Decl _ f _ _), exprList) = do
  aeErrs <- mapM (computeErrors' varBinds env) exprList
  return (d, aeErrs)
  where
    varBinds = findInSpec f spec

computeErrors' :: [VarBind] -> Env ACebS -> (VarName,FAExpr,FBExpr) -> IO (FAExpr,AExpr,FBExpr,EExpr,Double,[FAExpr],[AExpr],[EExpr],[VarBind])
computeErrors' varBinds env (_, fae, be) = do
  err <- computeNumRoundOffError varBinds symbErr
  return (fae,fae2real fae,be,symbErr,err,faeVarList,realVarList,errVarList,varBinds)
  where
     faeVarList  = varList fae
     errVarList  = map errVar faeVarList
     realVarList = map realVar faeVarList
     symbErr = symbolicError [] env fae -- trace ("symbolic error: " ++ show (symbolicError fae)) $ replaceLocalVarsInErrExpr localEnv $ symbolicError fae



