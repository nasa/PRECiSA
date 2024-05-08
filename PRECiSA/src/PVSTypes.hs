-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


module PVSTypes (
  module PVSTypes
) where

import Prelude hiding ((<>))
import PPExt
import Common.TypesUtils (Size,ArraySize(..))
import Data.Maybe (fromMaybe)

type RecordField = String

data PVSType = FPSingle
             | FPDouble
             | TInt
             | Real
             | Boolean
             | Array PVSType (Maybe ArraySize)
             | List PVSType
             | Tuple [PVSType]
             | Record [(RecordField,PVSType)]
             | TypeFun [PVSType] PVSType
  deriving (Eq, Ord, Show, Read)


fp2realType :: PVSType -> PVSType
fp2realType TInt = TInt
fp2realType Boolean = Boolean
fp2realType (Array t size) = (Array (fp2realType t) size)
fp2realType (List t) = List (fp2realType t)
fp2realType (Tuple ts) = Tuple (map fp2realType ts)
fp2realType (Record fields) = Record (map (\(field,t) -> (field,fp2realType t)) fields)
fp2realType _ = Real

tupleIdxType :: PVSType -> Integer -> PVSType
tupleIdxType (Tuple ts) idx =  ts!!(fromInteger $ idx - 1)
tupleIdxType t _ = error $ "tupleIdxType: " ++ show t ++ " is not a tuple."

recordFieldType :: PVSType -> String -> PVSType
recordFieldType (Record ts) field = fromMaybe errMsg $ lookup field ts
  where
    errMsg = error $ "recordIdxType: " ++ show field ++ " not found."
recordFieldType t _ = error $ "recordIdxType: " ++ show t ++ " is not a record."

lubPVSType :: PVSType -> PVSType -> PVSType
lubPVSType FPDouble _ = FPDouble
lubPVSType _ FPDouble = FPDouble
lubPVSType TInt t = t
lubPVSType t TInt = t
lubPVSType t1 t2 | t1 == t2 = t1
lubPVSType t1 t2 = error $ "PVSType not defined for " ++ show t1 ++ " and " ++ show t2

instance PPExt PVSType where
  prettyDoc FPSingle = text "single"
  prettyDoc FPDouble = text "double"
  prettyDoc TInt = text "int"
  prettyDoc Real = text "real"
  prettyDoc Boolean = text "bool"
  prettyDoc (Array t _) = text "ARRAY [int ->" <+> prettyDoc t <> text "]"
  prettyDoc (List t) = text "list[" <> prettyDoc t <> text "]"
  prettyDoc (Tuple ts) = text "[" <> hsep (punctuate comma (map prettyDoc ts)) <> text "]"
  prettyDoc (Record ts) = text "[#" <> hsep (punctuate comma (map prettyField ts)) <> text "#]"
    where
      prettyField (field, t) = text field <> colon <> prettyDoc t
  prettyDoc (TypeFun ts returnType) = text "[" <> hsep (punctuate (text "->") $ map prettyDoc ts)
                                      <+> text "->" <+> prettyDoc returnType <> text "]"