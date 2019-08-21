module TransformationUtils where

import Transformation
import AbsPVSLang
import Translation.Float2Real
import ErrorExpression
import AbsSpecLang
import NumericalError
import FPrec(VarName)

computeErrors :: Spec -> (Decl, ErrVarEnv) ->  IO (Decl,[(FAExpr,AExpr,FBExpr,EExpr,Double,[FAExpr],[AExpr],[EExpr],[VarBind])])
computeErrors (Spec spec) (d@(Decl _ f _ _), exprList) = do
  aeErrs <- mapM (computeErrors' varBinds) exprList
  return (d, aeErrs)
  where
    varBinds = findInSpec f spec

computeErrors' :: [VarBind] -> (VarName,FAExpr,FBExpr) -> IO (FAExpr,AExpr,FBExpr,EExpr,Double,[FAExpr],[AExpr],[EExpr],[VarBind])
computeErrors' varBinds (_, fae, be) = do
  err <- computeNumRoundOffError varBinds symbErr
  return (fae,fae2real fae,be,symbErr,err,faeVarList,realVarList,errVarList,varBinds)
  where
     faeVarList  = varList fae
     errVarList  = map errVar faeVarList
     realVarList = map realVar faeVarList
     symbErr = symbolicError fae



