module ErrorExpression where

import AbsPVSLang
import AbstractSemantics
import AbstractDomain
import Common.DecisionPath

symbolicError :: FAExpr -> EExpr
symbolicError fae = eExpr $ mergeACebFold $ aexprSem fae emptyInterpretation emptyEnv root ""

