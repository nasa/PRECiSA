module NumericalError where

import AbsPVSLang
import AbsSpecLang
import AbstractDomain
import AbstractSemantics
import Kodiak.KodiakRunnable
import Kodiak.KodiakRunner
import Common.DecisionPath
import FPrec

roError :: [VarBind] -> [(VarName,ACebS)] -> FAExpr -> IO Double
roError varBinds env ae =  maximumUpperBound <$> run kodiakInput ()
    where
        kodiakInput = KI { name = "error",
                             expression = initAExpr $ eExpr acebAExpr,
                             bindings = varBinds,
                             maxDepth = 7,
                             precision = 14
                           }
        acebAExpr = initErrAceb $ mergeACebFold $ aexprSem ae emptyInterpretation (Env env) (LDP []) "f"

computeNumRoundOffError :: [VarBind] -> EExpr -> IO Double
computeNumRoundOffError varBinds ee =  maximumUpperBound <$> run kodiakInput ()
    where
        kodiakInput = KI { name = "error",
                             expression = initAExpr ee,
                             bindings = varBinds,
                             maxDepth = 7,
                             precision = 14
                           }
   