-- Notices:
--
-- Copyright 2025 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PVSio.Runner where

import AbsPVSLang
import AbsSpecLang
import Data.List (intercalate, isInfixOf)
import PPExt
import qualified Common.ShowRational as SR
import System.Process
import System.FilePath
import System.Directory (withCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Control.Exception (catch, IOException)
import qualified Text.RawString.QQ as QQ
import Text.Read (readMaybe)

-- | Input data for PVSio runner
data PVSioInput = PVI { pviName :: String,
                        pviExpression :: EExpr,
                        pviBindings :: [VarBind]
                      }
  deriving Show

-- | Result from PVSio computation
-- Uses Rational to preserve full precision from PVSio output
data PVSioResult = PVR { maximumValue :: Rational
                       } deriving Show

-- | Generate a PVS theory file for the error expression
-- This creates an errorExpression.pvs file matching the format needed for maximization
generatePVSTheory :: String -> EExpr -> [VarBind] -> String
generatePVSTheory _theoryName errorExpr varBinds =
  let _concreteArgs = concatMap bindToConcreteValues varBinds
  in unlines $
    [ "errorExpression: THEORY BEGIN"
    , ""
    ] ++
    generateImports errorExpr ++
    [ "" ] ++
    generateIntervalDeclarations varBinds ++
    [ "" ] ++
    generateVariableDeclarations varBinds ++
    [ "" ] ++
    generateErrorExpressionFunction errorExpr varBinds ++
    [ ""
    , "END errorExpression"
    ]

-- | Generate necessary imports based on expression content
generateImports :: EExpr -> [String]
generateImports expr =
  let hasArrays = containsArrayTypes expr
      imports = [ "IMPORTING interval_arith@interval" ] ++
                [ "IMPORTING float_bounded_axiomatic@aerr_ulp__double" ] ++
                (if hasArrays
                 then [ "IMPORTING float_bounded_axiomatic@aerr_ulp__double_array"
                      , "IMPORTING float_bounded_axiomatic@ieee754_double_base_array"
                      , "IMPORTING structures@real_array"
                      ]
                 else []) ++
                [ "IMPORTING PRECiSA@bbiadp" ]
  in imports

-- | Check if expression contains array types
containsArrayTypes :: EExpr -> Bool
containsArrayTypes expr = case expr of
  ArrayElem _ _ _ -> True
  BinaryOp (ArrayDotOp _) _ _ -> True
  BinaryOp (ArrayDotFMAOp _) _ _ -> True
  BinaryOp (ArrayAddOp _) _ _ -> True
  BinaryOp _ e1 e2 -> containsArrayTypes e1 || containsArrayTypes e2
  UnaryOp _ e -> containsArrayTypes e
  ErrBinOp (ArrayDotOp _) _ _ _ _ _ -> True
  ErrBinOp (ArrayDotFMAOp _) _ _ _ _ _ -> True
  ErrBinOp (ArrayAddOp _) _ _ _ _ _ -> True
  ErrBinOp _ _ _ e1 _ e2 -> containsArrayTypes e1 || containsArrayTypes e2
  ErrFun fname _ _ _ _ _ -> "array" `isInfixOf` fname || "dot" `isInfixOf` fname || "dpa" `isInfixOf` fname
  HalfUlp _ (ArrayOf _ _) -> True
  _ -> False

-- | Generate interval range declarations
generateIntervalDeclarations :: [VarBind] -> [String]
generateIntervalDeclarations binds =
  [ vname ++ "_range : Interval = [|" ++ lbStr ++ "," ++ ubStr ++ "|]"
  | VarBind name _ _ lb ub <- binds
  , let vname = cVarName name ResValue
        lbStr = lboundToString lb
        ubStr = uboundToString ub
  ]

-- | Generate variable declarations
generateVariableDeclarations :: [VarBind] -> [String]
generateVariableDeclarations binds = concatMap generateVarDecl binds
  where
    generateVarDecl (VarBind name field typ _ _) =
      let baseName = cVarName name field
          rVar = "r_" ++ baseName
          eVar = "e_" ++ baseName
          pvsType = pvsTypeToString typ
      in [ rVar ++ " : VAR " ++ pvsType
         , eVar ++ " : VAR nonneg_real"
         ]

-- | Convert PVSType to PVS string representation
pvsTypeToString :: PVSType -> String
pvsTypeToString typ = case typ of
  FPSingle -> "real"
  FPDouble -> "real"
  TInt -> "real"
  Real -> "real"
  ArrayOf n FPDouble -> "rarray(" ++ show n ++ ")"
  ArrayOf n FPSingle -> "rarray(" ++ show n ++ ")"
  ArrayOf n _ -> "rarray(" ++ show n ++ ")"
  _ -> "real"

-- | Generate the errorExpression function
generateErrorExpressionFunction :: EExpr -> [VarBind] -> [String]
generateErrorExpressionFunction expr binds =
  let params = intercalateComma (concatMap bindToParams binds)
      exprStr = renderPVSExpr expr
  in [ "errorExpression(" ++ params ++ "): nonneg_real = " ++ exprStr ]

-- | Convert VarBind to function parameters
-- Note: In PVS syntax "x, y: T" means both x and y are of type T
-- So we need separate entries for each parameter with its type
bindToParams :: VarBind -> [String]
bindToParams (VarBind name field typ _ _) =
  let baseName = cVarName name field
      pvsType = pvsTypeToString typ
  in [ "r_" ++ baseName ++ ": " ++ pvsType
     , "e_" ++ baseName ++ ": nonneg_real"
     ]

-- -- | Convert VarBind to function arguments for the call
-- bindToArgs :: VarBind -> [String]
-- bindToArgs (VarBind name field _ _ _) =
--   let baseName = cVarName name field
--   in [ "r_" ++ baseName, "e_" ++ baseName ]

-- | Convert VarBind to concrete values for main function call
-- Uses interval midpoints for real values and 0 for error values
bindToConcreteValues :: VarBind -> [String]
bindToConcreteValues (VarBind _name _field typ lb ub) =
  let midpoint = intervalMidpoint lb ub
      concreteValue = case typ of
        ArrayOf n _ -> "LAMBDA (i: below(" ++ show n ++ ")): " ++ midpoint
        _ -> midpoint
  in [ concreteValue, "0" ]

-- | Calculate midpoint of an interval
intervalMidpoint :: LBound -> UBound -> String
intervalMidpoint lb ub =
  let lbVal = lboundToRational lb
      ubVal = uboundToRational ub
      mid = (lbVal + ubVal) / 2
  in showRat mid

-- | Convert LBound to Rational
lboundToRational :: LBound -> Rational
lboundToRational (LBInt i) = toRational i
lboundToRational (LBDouble r) = toRational r
lboundToRational LInf = error "LInf not supported"

-- | Convert UBound to Rational
uboundToRational :: UBound -> Rational
uboundToRational (UBInt i) = toRational i
uboundToRational (UBDouble r) = toRational r
uboundToRational UInf = error "UInf not supported"

-- | Render error expression to PVS using the prettyAExpr function
renderPVSExpr :: EExpr -> String
renderPVSExpr expr = render (prettyAExpr prettyDoc expr)

-- | Helper to intercalate with commas
intercalateComma :: [String] -> String
intercalateComma = intercalate ", "

-- | Create variable name with field suffix
cVarName :: String -> ResultField -> String
cVarName x ResValue = x
cVarName x (ResRecordField field) = x ++ "_" ++ field
cVarName x (ResTupleIndex idx) = x ++ "_" ++ show idx

-- | Show rational as PVS format
showRat :: Rational -> String
showRat = SR.showRational Nothing

-- | Convert LBound to string
lboundToString :: LBound -> String
lboundToString (LBInt i) = show i
lboundToString (LBDouble r) = showRat (toRational r)
lboundToString LInf = error "LInf not supported in PVSio intervals"

-- | Convert UBound to string
uboundToString :: UBound -> String
uboundToString (UBInt i) = show i
uboundToString (UBDouble r) = showRat (toRational r)
uboundToString UInf = error "UInf not supported in PVSio intervals"

-- | Run pvsio command and capture output
-- Note: pvsio can take more than a minute to execute
runPVSio :: FilePath -> String -> IO String
runPVSio workDir _theoryName = do
  let command = "pvs"
      args = [
              "-raw",
              "-E",
              [QQ.r|(progn
                (with-theory (th) "errorExpression"
                  (declare (ignore th))
                  (load-pvs-attachments)
                  (read-strategies-files)
                  (format t "~%~a~%" (compute-upper-bound-on-array-function-definition '|errorExpression| "errorExpression"))
                  (bye))
              )|]
            ]
  (_exitCode, output, errOutput) <-
    withCurrentDirectory workDir $
      readProcessWithExitCode command args ""
  return $ errOutput ++ output

-- | Parse the output from pvsio to extract the numerical result
-- Returns Rational directly to preserve full precision
parsePVSioOutput :: String -> Maybe Rational
parsePVSioOutput output =
  case lines output of
       [] -> Nothing
       ls -> readMaybe $ map (\c -> if c == '/' then '%' else c) (strip $ last ls)
  where
    strip = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- | Run the complete PVSio computation
runPVSioComputation :: PVSioInput -> IO PVSioResult
runPVSioComputation pviInput =
  withSystemTempDirectory "precisa-pvsio-" $ \tempDir -> do
    let name = pviName pviInput
        expr = pviExpression pviInput
        binds = pviBindings pviInput
        theoryContent = generatePVSTheory name expr binds
        theoryFile = tempDir </> "errorExpression.pvs"

    -- Write the theory file with fixed name errorExpression.pvs
    writeFile theoryFile theoryContent

    -- Run pvsio with the theory name errorExpression
    output <- runPVSio tempDir "errorExpression"
      `catch` \(e :: IOException) ->
        error $ "Failed to run pvsio: " ++ show e

    -- Parse output
    case parsePVSioOutput output of
      Nothing -> error $ "Failed to parse pvsio output.\nFull output:\n" ++ output ++ "\n\nGenerated file: " ++ theoryFile
      Just val -> return $ PVR { maximumValue = val }
