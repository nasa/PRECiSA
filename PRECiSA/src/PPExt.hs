-- Notices:
--
-- Copyright 2017 United States Government as represented by the Administrator of the National Aeronautics and Space Administration.
-- All Rights Reserved.
--
-- Disclaimers:
--
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED,
-- IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT
-- SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS,
-- HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
--
-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT,
-- ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING
-- FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH
-- MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.

{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module PPExt (
  module PPExt
) where

import Text.PrettyPrint as PPExt hiding (empty)
import qualified Text.PrettyPrint as PP
import FPrec

class PPExt a where
  prettyDoc        :: a -> Doc
  prettyDocWith    :: FPrec -> a -> Doc
  prettyKodiak     :: a -> Doc
  prettyKodiak = prettyDoc
  prettyKodiakWith :: FPrec -> a -> Doc
  prettyKodiakWith = prettyDocWith

emptyDoc :: Doc
emptyDoc = PP.empty

prettyList :: PPExt a => [a] -> Doc -> Doc
prettyList list sep     = hsep $ punctuate sep $ (map prettyDoc list)

prettyListWith :: PPExt a => FPrec -> Doc -> [a] -> Doc
prettyListWith fp sep list =  hsep $ punctuate sep $ (map (prettyDocWith fp) list)

showRational :: Rational -> String
showRational x = map (\c -> if c=='%' then '/'; else c) (show x)

instance PPExt Int where
  prettyDoc = int
  prettyDocWith _ i = int i
  prettyKodiak i = text "val" <> (parens $ int i)
  prettyKodiakWith _ i = prettyKodiak i

instance PPExt Double where
  prettyDoc = double
  prettyDocWith _ d = double d

instance PPExt Char where
  prettyDoc = char
  prettyDocWith _ c = char c
