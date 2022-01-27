-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module Kodiak.PrettyPrint where

import Kodiak.Expression
import Kodiak.Runner (rat2interval)

import Data.Numbers.FloatingHex
import Data.Ratio (numerator,denominator)
import Foreign.C.Types (CInt)
import Prelude hiding((<>))
import Text.PrettyPrint

prettyAExpr :: AExpr -> Doc
prettyAExpr e
  | Cnst r <- e = let
      n = if isCInt r
          then
            let i = fromInteger $ numerator r
            in text "dec" <> parens (hcat $ punctuate comma [int i,int 1])
          else
            let (lb,ub) = rat2interval r
                ppBounds = map (\f -> text "hex_val" <> parens (text $ show $ showHFloat f "")) [lb,ub]
            in text "ival" <> parens (hcat $ punctuate comma ppBounds)
      in text "val" <> parens n

  | Var v <- e = text v

  | Add l r <- e = ppBinaryOp '+' l r

  | Sub l r <- e = ppBinaryOp '-' l r

  | Mul l r <- e = ppBinaryOp '*' l r

  | Div l r <- e = ppBinaryOp '/' l r

  | Neg n <- e = spacedParens $ char '-' <+> prettyAExpr n

  | Floor n <- e = ppFunction "Floor" n

  | Sqrt n <- e = ppFunction "Sqrt" n

  | Abs n <- e = ppFunction "Abs" n

  | Sin n <- e = ppFunction "Sin" n

  | Cos n <- e = ppFunction "Cos" n

  | ATan n <- e = ppFunction "ATan" n

  | Ln n <- e = ppFunction "Ln" n

  | Exp n <- e = ppFunction "Exp" n

  | Ulp FPSingle n <- e = ppFunction "SUlp"  n

  | Ulp FPDouble n <- e = ppFunction "DUlp"  n

  | Max es <- e = let ppEs = spacedBraces $ hcat $ punctuate spacedComma $ map prettyAExpr es
                  in text "Max" <> parens ppEs

  | otherwise = text "NONE"

  where
    innerSpace wrapper doc = wrapper $ space <> doc <> space

    spacedParens = innerSpace parens

    spacedBraces = innerSpace braces

    spacedComma = innerSpace id comma

    isCInt r = denominator r == 1 && isRepresentable (numerator r)
      where isRepresentable i = i <= toInteger (maxBound :: CInt)

    ppBinaryOp sym l r = parens $ space <> ppL <+> char sym <+> ppR <> space
      where (ppL,ppR) = (prettyAExpr l,prettyAExpr r)

    ppFunction sym e' = text sym <> parens (prettyAExpr e')


