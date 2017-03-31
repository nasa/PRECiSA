-- Copyright 2016 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.
--
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS,
-- ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS
-- AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT
-- AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
--
-- Waiver and Indemnity: RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS
-- IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT,
-- ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module AbsSpecLang where

import Data.Scientific
import Text.Printf
import PPExt

import AbsPVSLang (VarId, NonVarId)

data LBound
-- lower bound
    = LBInt Integer
    | LBDouble Double
    | LInf
  deriving (Eq, Ord, Show, Read)

data UBound
-- upper bound
    = UBInt Integer
    | UBDouble Double
    | UInf
  deriving (Eq, Ord, Show, Read)

data VarBind = VarBind String LBound UBound
-- variable ranges
  deriving (Eq, Ord, Show, Read)

data SpecBind
-- list of variable ranges for a function
    = SpecBind String [VarId] [VarBind]
  deriving (Eq, Ord, Show, Read)

data Spec = Spec [SpecBind]
-- specification for a program
  deriving (Eq, Ord, Show, Read)

emptySpec :: Spec
-- empty specification
emptySpec = Spec []

-----------------------
-- PPExt instances --
-----------------------

instance PPExt (LBound) where
  prettyDoc (LBInt i) = integer i
  prettyDoc (LBDouble d) = text $ printf ("%."++ show e ++"f") d
     where
      e = abs $ base10Exponent $ fromFloatDigits d
  prettyDoc (LInf) = text "-inf"

instance PPExt (UBound) where
  prettyDoc (UBInt i) = integer i
  prettyDoc (UBDouble d) = text $ printf ("%."++ show e ++"f") d
     where
      e = abs $ base10Exponent $ fromFloatDigits d
  prettyDoc (UInf) = text "+inf"

instance PPExt (VarBind) where
  prettyDoc (VarBind x lb ub) = text x <+> text "in" <+> lbrack <> prettyDoc lb <> comma <+> prettyDoc ub <>rbrack

instance PPExt (SpecBind) where
  prettyDoc (SpecBind f vs vbs) = text f <+> lparen
                                  <> (hsep $ punctuate comma $ map prettyDoc vs)
                                  <> rparen <+> colon
                                  <+> (hsep $ punctuate comma $ map prettyDoc vbs)

instance PPExt (Spec) where
  prettyDoc (Spec specs) = (vcat $ punctuate (text ".") $ map prettyDoc specs)

