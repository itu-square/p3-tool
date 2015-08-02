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
type Abstraction m = (m [String], [Lit], Frm.Formula -> m Frm.Formula)

joinAbs :: AbstractionMonad m => Abstraction m
joinAbs = (joinFeatures, [], joinFormula)
    where joinFeatures = return []
          joinFormula phi = do
            (cfgs, _) <- ask
            b <- anyM (Cnfg.|= phi) (Set.toList cfgs)
            return $ Frm.fromBool b

ignoreAbs :: AbstractionMonad m => Set.Set String -> Abstraction m
ignoreAbs ffs = (ignoreFeatures, [], ignoreFormula)
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
projectAbs lits = (projectFeatures, Set.toList $ lits, projectFormula)
    where projectFeatures = do
            (_, features) <- ask
            return features
          projectFormula = return
