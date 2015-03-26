{-# LANGUAGE DeriveDataTypeable #-}
module Transformation.Formulae where

import Data.Typeable
import Data.Data
import Data.SBV
import qualified Data.Map as Map

import Control.Monad.Error

import FPromela.Ast as FP

import Transformation.Common



infixr 6 :&:
infixr 7 :|:
infixr 8 :=>:

data Formula = FVar String
             | FFalse
             | FTrue
             | (:!:) Formula
             | Formula :&: Formula
             | Formula :|: Formula
             | Formula :=>: Formula
     deriving (Eq, Show, Data, Typeable)


fromFPromelaExpr :: Monad m => String -> FP.Expr -> ErrorT String m Formula
fromFPromelaExpr prefix (FP.ELogic e1 "||" e2) = do
  phi1 <- fromFPromelaExpr prefix e1
  phi2 <- fromFPromelaExpr prefix e2
  return (phi1 :|: phi2)
fromFPromelaExpr prefix (FP.ELogic e1 "&&" e2) = do
  phi1 <- fromFPromelaExpr prefix e1
  phi2 <- fromFPromelaExpr prefix e2
  return (phi1 :&: phi2)
fromFPromelaExpr prefix (FP.EAnyExpr ae) = fromFPromelaAnyExpr ae
  where fromFPromelaAnyExpr (FP.AeVarRef (FP.VarRef prefix' Nothing (Just (FP.VarRef name Nothing Nothing))))
           | prefix' == prefix = return $ FVar name
        fromFPromelaAnyExpr (FP.AeConst FP.CstFalse) = return FFalse
        fromFPromelaAnyExpr (FP.AeConst FP.CstTrue)  = return FTrue
        fromFPromelaAnyExpr (FP.AeBinOp e1 "&&" e2) = do
          phi1 <- fromFPromelaAnyExpr e1
          phi2 <- fromFPromelaAnyExpr e2
          return (phi1 :&: phi2)
        fromFPromelaAnyExpr (FP.AeBinOp e1 "||" e2) = do
          phi1 <- fromFPromelaAnyExpr e1
          phi2 <- fromFPromelaAnyExpr e2
          return (phi1 :|: phi2)
        fromFPromelaAnyExpr (FP.AeUnOp "!" e1) = do
          phi1 <- fromFPromelaAnyExpr e1
          return $ (:!:) phi1
        fromFPromelaAnyExpr e = throwError ("Unsupported expression: " ++ show e)
fromFPromelaExpr prefix e               = throwError ("Unsupported expression: " ++ show e)

interpretAsFPromelaExpr :: String -> Formula -> FP.Expr
interpretAsFPromelaExpr prefix phi = FP.EAnyExpr $ interpretAsFPromelaAnyExpr prefix phi
  where interpretAsFPromelaAnyExpr :: String -> Formula -> FP.AnyExpr
        interpretAsFPromelaAnyExpr prefix (FVar name) =
                FP.AeVarRef $ FP.VarRef prefix Nothing (Just $ FP.VarRef name Nothing Nothing)
        interpretAsFPromelaAnyExpr prefix FFalse =
                FP.AeConst FP.CstFalse
        interpretAsFPromelaAnyExpr prefix FTrue =
                FP.AeConst FP.CstTrue
        interpretAsFPromelaAnyExpr prefix ((:!:) phi) =
           let phiExpr = interpretAsFPromelaAnyExpr prefix phi
           in FP.AeUnOp "!" phiExpr
        interpretAsFPromelaAnyExpr prefix (phi1 :&: phi2) =
           let phiExpr1 = interpretAsFPromelaAnyExpr prefix phi1
               phiExpr2 = interpretAsFPromelaAnyExpr prefix phi2
           in FP.AeBinOp phiExpr1 "&&" phiExpr2
        interpretAsFPromelaAnyExpr prefix (phi1 :|: phi2) =
           let phiExpr1 = interpretAsFPromelaAnyExpr prefix phi1
               phiExpr2 = interpretAsFPromelaAnyExpr prefix phi2
           in FP.AeBinOp phiExpr1 "||" phiExpr2
        interpretAsFPromelaAnyExpr prefix (phi1 :=>: phi2) =
           let phiExpr1 = interpretAsFPromelaAnyExpr prefix phi1
               phiExpr2 = interpretAsFPromelaAnyExpr prefix phi2
           in FP.AeBinOp (FP.AeUnOp "!" phiExpr1) "||" phiExpr2

interpretAsPredicate :: Monad m => Map.Map String Predicate -> Formula -> ErrorT String m Predicate
interpretAsPredicate env (FVar name) =
  case Map.lookup name env of
    Nothing  -> throwError ("Unassigned variable " ++ show name)
    (Just p) -> return p
interpretAsPredicate env FFalse      = return . return $ false
interpretAsPredicate env FTrue       = return . return $ true
interpretAsPredicate env ((:!:) phi) = do
  phip <- interpretAsPredicate env phi
  return $ liftM bnot phip
interpretAsPredicate env (phi1 :&: phi2) = do
  phi1p <- interpretAsPredicate env phi1
  phi2p <- interpretAsPredicate env phi2
  return $ liftM2 (&&&) phi1p phi2p
interpretAsPredicate env (phi1 :|: phi2) = do
  phi1p <- interpretAsPredicate env phi1
  phi2p <- interpretAsPredicate env phi2
  return $ liftM2 (|||) phi1p phi2p
interpretAsPredicate env (phi1 :=>: phi2) = do
  phi1p <- interpretAsPredicate env phi1
  phi2p <- interpretAsPredicate env phi2
  return $ liftM2 (==>) phi1p phi2p

interpretAsBool :: Monad m => Map.Map String Bool -> Formula -> ErrorT String m Bool
interpretAsBool env (FVar name) =
  case Map.lookup name env of
    Nothing  -> throwError ("Unassigned variable " ++ show name)
    (Just p) -> return p
interpretAsBool env FFalse      = return false
interpretAsBool env FTrue       = return true
interpretAsBool env ((:!:) phi) = do
  phip <- interpretAsBool env phi
  return $ bnot phip
interpretAsBool env (phi1 :&: phi2) = do
  phi1p <- interpretAsBool env phi1
  phi2p <- interpretAsBool env phi2
  return (phi1p &&& phi2p)
interpretAsBool env (phi1 :|: phi2) = do
  phi1p <- interpretAsBool env phi1
  phi2p <- interpretAsBool env phi2
  return (phi1p ||| phi2p)
interpretAsBool env (phi1 :=>: phi2) = do
  phi1p <- interpretAsBool env phi1
  phi2p <- interpretAsBool env phi2
  return (phi1p ==> phi2p)
