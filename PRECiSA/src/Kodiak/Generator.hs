{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Kodiak.Generator where

import Control.Applicative (liftA2)
import Control.Monad.Except

import qualified FPrec             as PVS
import qualified AbsPVSLang        as PVS
import           Kodiak.Expression (AExpr,BExpr)
import qualified Kodiak.Expression as K

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

    | PVS.Add l r      <- e = recBinary K.Add l r

    | PVS.Sub l r      <- e = recBinary K.Sub l r

    | PVS.Mul l r      <- e = recBinary K.Mul l r

    | PVS.Div l r      <- e = recBinary K.Div l r

    -- Unary Operators:

    | PVS.Abs e'       <- e = recUnary K.Abs e'

    | PVS.Sqrt e'      <- e = recUnary K.Sqrt e'

    | PVS.Neg e'       <- e = recUnary K.Neg e'

    | PVS.Ln e'        <- e = recUnary K.Ln e'

    | PVS.Expo e'      <- e = recUnary K.Exp e'

    | PVS.Sin e'       <- e = recUnary K.Sin e'

    | PVS.Cos e'       <- e = recUnary K.Cos e'

    | PVS.ATan e'      <- e = recUnary K.ATan e'

    | PVS.Floor e'     <- e = recUnary K.Floor e'

    | PVS.HalfUlp e' p <- e = let f x = K.Div (K.Ulp p x) (K.Cnst 2) in recUnary f e'

    -- N-ary operators:

    | PVS.MaxErr es    <- e = case es of
                                []  -> throwError $ EmptyNAryExpr e
                                [x] -> recUnary id x
                                xs  -> do xs' <- mapM pvsAExpr2Kodiak xs
                                          return $ K.Max xs'

    -- Integer operators:

    | PVS.IMod l r     <- e = (K.Cnst . toRational) <$> evalRI e

    | otherwise = throwError $ NonSupportedExpr e

    where
        recBinary op l r = do l' <- pvsAExpr2Kodiak l
                              r' <- pvsAExpr2Kodiak r
                              return $ op l' r'

        recUnary op e = do e' <- pvsAExpr2Kodiak e
                           return $ op e'


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

    | PVS.Eq l r  <- e = recBinary pvsAExpr2Kodiak K.Eq l r

    | PVS.Neq l r <- e = recBinary pvsAExpr2Kodiak K.NEq l r

    | PVS.Lt l r  <- e = recBinary pvsAExpr2Kodiak K.LT l r

    | PVS.LtE l r <- e = recBinary pvsAExpr2Kodiak K.LE l r

    | PVS.Gt l r  <- e = recBinary pvsAExpr2Kodiak K.GT l r

    | PVS.GtE l r <- e = recBinary pvsAExpr2Kodiak K.GE l r

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

    -- Binary operators:

    | PVS.FAdd PVS.FPDouble l r      <- e = recBinary K.Add l r

    | PVS.FSub PVS.FPDouble l r      <- e = recBinary K.Sub l r

    | PVS.FMul PVS.FPDouble l r      <- e = recBinary K.Mul l r

    | PVS.FDiv PVS.FPDouble l r      <- e = recBinary K.Div l r

    -- Unary Operators:

    | PVS.FAbs PVS.FPDouble e'       <- e = recUnary K.Abs e'

    | PVS.FSqrt PVS.FPDouble e'      <- e = recUnary K.Sqrt e'

    | PVS.FNeg PVS.FPDouble e'       <- e = recUnary K.Neg e'

    | PVS.FLn PVS.FPDouble e'        <- e = recUnary K.Ln e'

    | PVS.FExpo PVS.FPDouble e'      <- e = recUnary K.Exp e'

    | PVS.FSin PVS.FPDouble e'       <- e = recUnary K.Sin e'

    | PVS.FCos PVS.FPDouble e'       <- e = recUnary K.Cos e'

    | PVS.FAtan PVS.FPDouble e'      <- e = recUnary K.ATan e'

    | PVS.FFloor PVS.FPDouble e'     <- e = recUnary K.Floor e'

    | PVS.RtoD e'                    <- e = case e' of
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

    | PVS.FISub l r     <- e = (K.Cnst . toRational) <$> evalFI e

    | PVS.FIMod l r     <- e = (K.Cnst . toRational) <$> evalFI e

    | otherwise = throwError $ NonSupportedExpr e

    where
        recBinary op l r = do l' <- pvsFAExpr2Kodiak l
                              r' <- pvsFAExpr2Kodiak r
                              return $ op l' r'

        recUnary op e = do e' <- pvsFAExpr2Kodiak e
                           return $ op e'

evalFI :: MonadError (ToKodiakError PVS.FAExpr) m => PVS.FAExpr -> m Integer
evalFI e
    | PVS.FInt i    <- e = return i
    | PVS.FISub l r <- e = recBinary (-) l r
    | PVS.FIMod l r <- e = recBinary mod l r
    | PVS.RtoD ie   <- e = case runExcept $ evalRI ie of
                            Left e -> throwError $ AExprError e
                            Right i -> return i
    | otherwise = throwError $ InvalidIntegerExpr e
    where
        recBinary op l r = liftA2 op (evalFI l) (evalFI r)

evalRI :: MonadError (ToKodiakError PVS.AExpr) m => PVS.AExpr -> m Integer
evalRI e
    | PVS.Int i    <- e = return i
    | PVS.IMod l r <- e = liftA2 mod (evalRI l) (evalRI r)
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

    | PVS.FEq l r  <- e = recBinary pvsFAExpr2Kodiak K.Eq l r

    | PVS.FNeq l r <- e = recBinary pvsFAExpr2Kodiak K.NEq l r

    | PVS.FLt l r  <- e = recBinary pvsFAExpr2Kodiak K.LT l r

    | PVS.FLtE l r <- e = recBinary pvsFAExpr2Kodiak K.LE l r

    | PVS.FGt l r  <- e = recBinary pvsFAExpr2Kodiak K.GT l r

    | PVS.FGtE l r <- e = recBinary pvsFAExpr2Kodiak K.GE l r

    where
        recBinary rec op l r = do l' <- rec l
                                  r' <- rec r
                                  return $ op l' r'