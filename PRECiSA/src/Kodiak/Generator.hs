-- Notices:
--
-- Copyright 2020 United States Government as represented by the Administrator of the National Aeronautics and Space Administration. All Rights Reserved.

-- Disclaimers
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."

-- Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.  RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS AGREEMENT.


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Kodiak.Generator where

import Control.Applicative (liftA2)
import Control.Monad.Except

import qualified PVSTypes          as PVS
import qualified AbsPVSLang        as PVS
import           Kodiak.Expression (AExpr,BExpr)
import qualified Kodiak.Expression as K
import Operators

class KodiakType a

instance KodiakType AExpr
instance KodiakType BExpr

class KodiakType b => Kodiakable a b m | a -> b m where
    kodiakize :: (Monad m) => a -> m b

instance Kodiakable PVS.AExpr K.AExpr (Except (ToKodiakError PVS.AExpr)) where
    kodiakize = pvsAExpr2Kodiak

data ToKodiakError x =
    NonSupportedExpr x
    | EmptyNAryExpr x
    | AExprError (ToKodiakError PVS.AExpr)
    | InvalidIntegerExpr x
    deriving (Show,Eq)

pvsAExpr2Kodiak :: MonadError (ToKodiakError PVS.AExpr) m => PVS.AExpr -> m K.AExpr
pvsAExpr2Kodiak e

    -- Constants:

    | PVS.Int i        <- e = return $ K.Cnst $ toRational i

    | PVS.Rat r        <- e = return $ K.Cnst r

    -- Variables:

    | PVS.Var ty v     <- e = case ty of
                              PVS.TInt -> return $ K.Var v
                              PVS.Real -> return $ K.Var v
                              _ -> throwError $ NonSupportedExpr e

    | PVS.RealMark v   <- e = return $ K.Var v

    -- Binary operators:

    | PVS.BinaryOp AddOp l r      <- e = recBinary K.Add l r

    | PVS.BinaryOp SubOp l r      <- e = recBinary K.Sub l r

    | PVS.BinaryOp MulOp l r      <- e = recBinary K.Mul l r

    | PVS.BinaryOp DivOp l r      <- e = recBinary K.Div l r

    -- Unary Operators:

    | PVS.UnaryOp AbsOp e'       <- e = recUnary K.Abs e'

    | PVS.UnaryOp SqrtOp e'      <- e = recUnary K.Sqrt e'

    | PVS.UnaryOp NegOp e'       <- e = recUnary K.Neg e'

    | PVS.UnaryOp LnOp e'        <- e = recUnary K.Ln e'

    | PVS.UnaryOp ExpoOp e'      <- e = recUnary K.Exp e'

    | PVS.UnaryOp SinOp e'       <- e = recUnary K.Sin e'

    | PVS.UnaryOp CosOp e'       <- e = recUnary K.Cos e'

    | PVS.UnaryOp AtanOp e'      <- e = recUnary K.ATan e'

    | PVS.UnaryOp FloorOp e'     <- e = recUnary K.Floor e'

    | PVS.HalfUlp e' p <- e = let f x = K.Div (K.Ulp p x) (K.Cnst 2) in recUnary f e'

    -- N-ary operators:

    | PVS.MaxErr es    <- e = case es of
                                []  -> throwError $ EmptyNAryExpr e
                                [x] -> recUnary id x
                                xs  -> do xs' <- mapM pvsAExpr2Kodiak xs
                                          return $ K.Max xs'

    -- Integer operators:

    | PVS.BinaryOp ModOp _ _     <- e = (K.Cnst . toRational) <$> evalRI e

    | otherwise = throwError $ NonSupportedExpr e

    where
        recBinary op l r = do l' <- pvsAExpr2Kodiak l
                              r' <- pvsAExpr2Kodiak r
                              return $ op l' r'

        recUnary op = fmap op . pvsAExpr2Kodiak


instance Kodiakable PVS.BExpr K.BExpr (Except (ToKodiakError PVS.AExpr)) where
    kodiakize = pvsBExpr2Kodiak

pvsBExpr2Kodiak :: MonadError (ToKodiakError PVS.AExpr) m => PVS.BExpr -> m K.BExpr
pvsBExpr2Kodiak e

    -- Constants:

    | PVS.BTrue   <- e = return K.True

    | PVS.BFalse  <- e = return K.False

    -- Boolean operators:

    | PVS.Not e'  <- e = pvsBExpr2Kodiak e' >>= (return . K.Not)

    | PVS.And l r <- e = recBinary pvsBExpr2Kodiak K.And l r

    | PVS.Or l r  <- e = recBinary pvsBExpr2Kodiak K.Or l r

    -- Relational operators:

    | PVS.Rel Eq l r  <- e = recBinary pvsAExpr2Kodiak K.Eq l r

    | PVS.Rel Neq l r <- e = recBinary pvsAExpr2Kodiak K.NEq l r

    | PVS.Rel Lt l r  <- e = recBinary pvsAExpr2Kodiak K.LT l r

    | PVS.Rel LtE l r <- e = recBinary pvsAExpr2Kodiak K.LE l r

    | PVS.Rel Gt l r  <- e = recBinary pvsAExpr2Kodiak K.GT l r

    | PVS.Rel GtE l r <- e = recBinary pvsAExpr2Kodiak K.GE l r

    | otherwise = error $ "Kodiak does not support PVS BExpr: " ++ show e

    where
        recBinary rec op l r = do l' <- rec l
                                  r' <- rec r
                                  return $ op l' r'


instance Kodiakable PVS.FAExpr K.AExpr (Except (ToKodiakError PVS.FAExpr)) where
    kodiakize = pvsFAExpr2Kodiak

pvsFAExpr2Kodiak :: MonadError (ToKodiakError PVS.FAExpr) m => PVS.FAExpr -> m K.AExpr
pvsFAExpr2Kodiak e

    -- Constants:

    | PVS.FInt i           <- e = return $ K.Cnst $ toRational i

    | PVS.FCnst ty r       <- e = let success = return $ K.Cnst $ toRational r in
                                    case ty of
                                        PVS.FPDouble -> success
                                        PVS.FPSingle -> success
                                        _ -> throwError $ NonSupportedExpr e

    -- Variables:

    | PVS.FVar ty v        <- e = let success = return $ K.Var v in
                                    case ty of
                                        PVS.TInt     -> success
                                        PVS.FPDouble -> success
                                        _ -> throwError $ NonSupportedExpr e

    -- Type cast:

    | PVS.TypeCast PVS.TInt PVS.FPDouble fi@(PVS.FInt _) <- e = pvsFAExpr2Kodiak fi


    -- Binary operators:

    | PVS.BinaryFPOp AddOp PVS.FPDouble l r      <- e = recBinary K.Add l r

    | PVS.BinaryFPOp SubOp PVS.FPDouble l r      <- e = recBinary K.Sub l r

    | PVS.BinaryFPOp MulOp PVS.FPDouble l r      <- e = recBinary K.Mul l r

    | PVS.BinaryFPOp DivOp PVS.FPDouble l r      <- e = recBinary K.Div l r

    -- Unary Operators:

    | PVS.UnaryFPOp AbsOp  PVS.FPDouble e'       <- e = recUnary K.Abs e'

    | PVS.UnaryFPOp SqrtOp PVS.FPDouble e'      <- e = recUnary K.Sqrt e'

    | PVS.UnaryFPOp NegOp  PVS.FPDouble e'       <- e = recUnary K.Neg e'

    | PVS.UnaryFPOp LnOp   PVS.FPDouble e'        <- e = recUnary K.Ln e'

    | PVS.UnaryFPOp ExpoOp PVS.FPDouble e'      <- e = recUnary K.Exp e'

    | PVS.UnaryFPOp SinOp PVS.FPDouble e'       <- e = recUnary K.Sin e'

    | PVS.UnaryFPOp CosOp PVS.FPDouble e'       <- e = recUnary K.Cos e'

    | PVS.UnaryFPOp AtanOp PVS.FPDouble e'      <- e = recUnary K.ATan e'

    | PVS.UnaryFPOp FloorOp PVS.FPDouble e'     <- e = recUnary K.Floor e'

    | PVS.ToFloat PVS.FPDouble e'               <- e = case e' of
                                                PVS.Int i -> return $ K.Cnst $ toRational i
                                                PVS.Rat r -> return $ K.Cnst r
                                                _ -> throwError $ NonSupportedExpr e

    -- N-ary operators:

    | PVS.FMax es    <- e = case es of
                                []  -> throwError $ EmptyNAryExpr e
                                [x] -> recUnary id x
                                xs  -> do xs' <- mapM pvsFAExpr2Kodiak xs
                                          return $ K.Max xs'

    -- Integer operators:

    | PVS.BinaryFPOp _ PVS.TInt _ _     <- e = (K.Cnst . toRational) <$> evalFI e

    | otherwise = throwError $ NonSupportedExpr e

    where
        recBinary op l r = do l' <- pvsFAExpr2Kodiak l
                              r' <- pvsFAExpr2Kodiak r
                              return $ op l' r'

        recUnary op = fmap op . pvsFAExpr2Kodiak


evalFI :: MonadError (ToKodiakError PVS.FAExpr) m => PVS.FAExpr -> m Integer
evalFI e
    | PVS.FInt i    <- e = return i
    | PVS.BinaryFPOp AddOp PVS.TInt l r <- e = recBinary (+) l r
    | PVS.BinaryFPOp SubOp PVS.TInt l r <- e = recBinary (-) l r
    | PVS.BinaryFPOp MulOp PVS.TInt l r <- e = recBinary (*) l r
    | PVS.BinaryFPOp ModOp PVS.TInt l r <- e = recBinary mod l r
    | PVS.BinaryFPOp DivOp PVS.TInt l r <- e = recBinary div l r
    | PVS.ToFloat PVS.FPDouble ie   <- e = case runExcept $ evalRI ie of
                                             Left err -> throwError $ AExprError err
                                             Right  i -> return i
    | otherwise = throwError $ InvalidIntegerExpr e
    where
        recBinary op l r = liftA2 op (evalFI l) (evalFI r)

evalRI :: MonadError (ToKodiakError PVS.AExpr) m => PVS.AExpr -> m Integer
evalRI e
    | PVS.Int i    <- e = return i
    | PVS.BinaryOp ModOp l r <- e = liftA2 mod (evalRI l) (evalRI r)
    | otherwise = throwError $ InvalidIntegerExpr e


instance Kodiakable PVS.FBExpr K.BExpr (Except (ToKodiakError PVS.FAExpr)) where
    kodiakize = pvsFBExpr2Kodiak

pvsFBExpr2Kodiak :: MonadError (ToKodiakError PVS.FAExpr) m => PVS.FBExpr -> m K.BExpr
pvsFBExpr2Kodiak e

    -- Constants:

    | PVS.FBTrue   <- e = return K.True

    | PVS.FBFalse  <- e = return K.False

    -- Boolean operators:

    | PVS.FNot e'  <- e = K.Not <$> pvsFBExpr2Kodiak e'

    | PVS.FAnd l r <- e = recBinary pvsFBExpr2Kodiak K.And l r

    | PVS.FOr l r  <- e = recBinary pvsFBExpr2Kodiak K.Or l r

    -- Relational operators:

    | PVS.FRel Eq l r  <- e = recBinary pvsFAExpr2Kodiak K.Eq l r

    | PVS.FRel Neq l r <- e = recBinary pvsFAExpr2Kodiak K.NEq l r

    | PVS.FRel Lt l r  <- e = recBinary pvsFAExpr2Kodiak K.LT l r

    | PVS.FRel LtE l r <- e = recBinary pvsFAExpr2Kodiak K.LE l r

    | PVS.FRel Gt l r  <- e = recBinary pvsFAExpr2Kodiak K.GT l r

    | PVS.FRel GtE l r <- e = recBinary pvsFAExpr2Kodiak K.GE l r

    | otherwise = error $ "Kodiak does not support the PVS FBExpr: " ++ show e

    where
        recBinary rec op l r = do l' <- rec l
                                  r' <- rec r
                                  return $ op l' r'