module AbsSpecLang where

import FPrec
import Text.Printf
import PPExt
import Prelude hiding ((<>))
import Data.Scientific

data LBound
    = LBInt Integer
    | LBDouble Rational
    | LInf
  deriving (Eq, Ord, Show, Read)

data UBound
    = UBInt Integer
    | UBDouble Rational
    | UInf
  deriving (Eq, Ord, Show, Read)

data VarBind = VarBind String FPrec LBound UBound
  deriving (Eq, Ord, Show, Read)

data SpecBind = SpecBind String [VarBind]
  deriving (Eq, Ord, Show, Read)

newtype Spec = Spec [SpecBind]
  deriving (Eq, Ord, Show, Read)

emptySpec :: Spec
emptySpec = Spec []

findInSpec :: String -> [SpecBind] -> [VarBind]
findInSpec fun [] = error ("findInSpec: function " ++ fun ++ " not found")
findInSpec fun (SpecBind g varBind : rest) | fun == g = varBind
                                           | otherwise = findInSpec fun rest

 
-----------------------
-- PPExt instances --
-----------------------

instance PPExt LBound where
  prettyDoc (LBInt i) = integer i
  prettyDoc (LBDouble d) = text $ printf ("%."++ show e ++"f") (fromRational d :: Double)
     where
      e = abs $ base10Exponent $ fromFloatDigits (fromRational d :: Double)
  prettyDoc LInf = text "-inf"


instance PPExt UBound where
  prettyDoc (UBInt i) = integer i
  prettyDoc (UBDouble d) = text $ printf ("%."++ show e ++"f") (fromRational d :: Double)
     where
      e = abs $ base10Exponent $ fromFloatDigits (fromRational d :: Double)
  prettyDoc UInf = text "+inf"


instance PPExt VarBind where
  prettyDoc (VarBind x _ lb ub) = text x <+> text "in" <+> lbrack <> prettyDoc lb <> comma <+> prettyDoc ub <> rbrack


instance PPExt SpecBind where
  prettyDoc (SpecBind f vbs) = text f <+> colon
                               <+> hsep (punctuate comma $ map prettyDoc vbs)


instance PPExt Spec where
  prettyDoc (Spec specs) = vcat $ punctuate (text ".") $ map prettyDoc specs

