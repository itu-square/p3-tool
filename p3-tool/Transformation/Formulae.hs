{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
module Transformation.Formulae where

import Data.Typeable
import Data.Data
import Data.SBV
import qualified Data.Map.Strict as Map
import qualified Data.Set.Monad as Set
import Data.Generics.Uniplate.Data

import Control.Monad.Except

import FPromela.Ast as FP

import Transformation.Configurations as Cnfgs

data Formula = FVar String
             | FFalse
             | FTrue
             | (:!:) Formula
             | Formula :&: Formula
             | Formula :|: Formula
             | Formula :=>: Formula
     deriving (Eq, Show, Ord, Data, Typeable)

nnf :: Formula -> Formula
nnf = let a = rewrite nnf'
  where nnf' ((:!:) ((:!:) phi))   = Just phi
        nnf' ((:!:) (phi :&: psi)) = Just (((:!:) phi) :|: ((:!:) psi))
        nnf' ((:!:) (phi :|: psi)) = Just (((:!:) phi) :&: ((:!:) psi))
        nnf' (phi :=>: psi)        = Just (((:!:) phi) :|: psi)
        nnf' _                     = Nothing

fromBool :: Bool -> Formula
fromBool True = FTrue
fromBool False = FFalse

fromConfig :: Cnfgs.Config -> Formula
fromConfig (Cnfgs.Config incl excl) =
  let inclvars = Set.mapMonotonic FVar incl
      exclvars = Set.mapMonotonic ((:!:) . FVar) excl
      allvars  = Set.union inclvars exclvars
  in Set.foldr (:&:) FTrue allvars

fromFPromelaExpr :: (Monad m, MonadError String m) => String -> FP.Expr -> m Formula
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

interpretAsSBool :: (Monad m, MonadError String m) => Map.Map String SBool -> Formula -> m SBool
interpretAsSBool env (FVar name) =
  case Map.lookup name env of
    Nothing  -> throwError ("Unassigned variable " ++ show name)
    (Just p) -> return p
interpretAsSBool env FFalse      = return $ false
interpretAsSBool env FTrue       = return $ true
interpretAsSBool env ((:!:) phi) = do
  phip <- interpretAsSBool env phi
  return $ bnot phip
interpretAsSBool env (phi1 :&: phi2) = do
  phi1p <- interpretAsSBool env phi1
  phi2p <- interpretAsSBool env phi2
  return $ (phi1p &&& phi2p)
interpretAsSBool env (phi1 :|: phi2) = do
  phi1p <- interpretAsSBool env phi1
  phi2p <- interpretAsSBool env phi2
  return $ (phi1p ||| phi2p)
interpretAsSBool env (phi1 :=>: phi2) = do
  phi1p <- interpretAsSBool env phi1
  phi2p <- interpretAsSBool env phi2
  return $ (phi1p ==> phi2p)

interpretAsBool :: (Monad m, MonadError String m) => Map.Map String Bool -> Formula -> m Bool
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

graft :: Map.Map String Formula -> Formula -> Formula
graft env (FVar name) =
  case Map.lookup name env of
    Nothing -> FVar name
    Just val -> val
graft env phi =
  descend (transform $ graft env) phi
