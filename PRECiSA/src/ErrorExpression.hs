module ErrorExpression where

import AbsPVSLang
import AbstractSemantics
import AbstractDomain
import Common.DecisionPath

symbolicError :: Interpretation -> Env ACebS -> FAExpr -> EExpr
symbolicError interp env fae = eExpr $ mergeACebFold $ aexprSem fae interp env root ""

