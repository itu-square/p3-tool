{-# LANGUAGE FlexibleContexts, ViewPatterns, ConstraintKinds #-}
module Transformation.Abstraction (AbstractionMonad, Abstraction, joinAbs, ignoreAbs, projectAbs) where
import Data.Foldable as Fold
import qualified Data.Set.Monad as Set
import qualified Data.Map.Strict as Map
import qualified Data.List as List

import Abstraction.Ast (Lit(..), feature)

import qualified Transformation.Configurations as Cnfg
import qualified Transformation.Formulae as Frm

import qualified Data.SBV as SBV

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Loops

import Data.Generics.Uniplate.Data

type AbstractionMonad m = (Functor m, Applicative m, Monad m, MonadError String m, MonadIO m, MonadReader (Set.Set Cnfg.Config, [String]) m)
type Abstraction m = (m [String], Frm.Formula -> m Frm.Formula)

infix 5 |=

(|=) :: AbstractionMonad m => Cnfg.Config -> Frm.Formula -> m Bool
k |= phi = do
    (_, features) <- ask
    let completeFrm = Frm.fromConfig k Frm.:=>: phi
    let pred = do fs <- foldrM featureToPred Map.empty features
                  v <- runExceptT (Frm.interpretAsSBool fs completeFrm)
                  case v of
                    Left err -> error err
                    Right p -> return p
    tres@(SBV.ThmResult res) <- liftIO $ SBV.prove pred
    case res of
        SBV.Satisfiable _ _ -> return False
        SBV.Unsatisfiable _ -> return True
        _ -> throwError (show tres)
  where featureToPred f m = do
          val <- SBV.forall f
          return $ Map.insert f val m

joinAbs :: AbstractionMonad m => Abstraction m
joinAbs = (joinFeatures, joinFormula)
    where joinFeatures = return []
          joinFormula phi = do
            (cfgs, _) <- ask
            b <- anyM (|= phi) (Set.toList cfgs)
            return $ Frm.fromBool b

ignoreAbs :: AbstractionMonad m => Set.Set String -> Abstraction m
ignoreAbs ffs = (ignoreFeatures, ignoreFormula)
    where ignoreFeatures = do
            (_, features) <- ask
            return $ features List.\\ Set.toList ffs
          ignoreFormula phi = do
            let phi' = Frm.nnf phi
            return $ transform ignoreFormula' phi'
          ignoreFormula' ((Frm.:!:) (Frm.FVar f)) | f `Set.member` ffs = Frm.FTrue
          ignoreFormula' (Frm.FVar f) | f `Set.member` ffs = Frm.FTrue
          ignoreFormula' phi = phi

projectAbs :: AbstractionMonad m => Set.Set Lit -> Abstraction m
projectAbs lits = (projectFeatures, projectFormula)
    where ffs = Set.map feature lits
          projectFeatures = do
            (_, features) <- ask
            return $ features List.\\ Set.toList ffs
          projectFormula phi = do
              let phi' = Frm.nnf phi
              return $ transform projectFormula' phi'
          projectFormula' ((Frm.:!:) (Frm.FVar f)) | PosLit f `Set.member` lits = Frm.FFalse
          projectFormula' ((Frm.:!:) (Frm.FVar f)) | NegLit f `Set.member` lits = Frm.FTrue
          projectFormula' (Frm.FVar f)             | PosLit f `Set.member` lits = Frm.FTrue
          projectFormula' (Frm.FVar f)             | NegLit f `Set.member` lits = Frm.FFalse
          projectFormula' phi = phi

