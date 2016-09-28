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

infix 5 |=

(|=) :: AbstractionMonad m => Cnfg.Config -> Frm.Formula -> m Bool
k |= phi = do
    (_, features) <- ask
    let completeFrm = Cnfg.fromConfig k Frm.:=>: phi
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

joinAbs :: AbstractionMonad m => Bool -> Abstraction m
joinAbs neg = (joinFeatures, [], joinFormula)
    where joinFeatures = return []
          joinFormula phi = do
            (cfgs, _) <- ask
            b <- anyM (|= phi) (Set.toList cfgs)
            return $ if neg then Frm.FFalse else Frm.fromBool b

ignoreAbs :: AbstractionMonad m => Bool -> Set.Set String -> Abstraction m
ignoreAbs neg ffs = (ignoreFeatures, [], ignoreFormula)
    where ignoreFeatures = do
            (_, features) <- ask
            return $ features List.\\ Set.toList ffs
          ignoreFormula phi = do
            let phi' = Frm.nnf phi
            return $ transform ignoreFormula' phi'
          ignoreFormula' ((Frm.:!:) (Frm.FVar f)) | f `Set.member` ffs = Frm.fromBool neg
          ignoreFormula' (Frm.FVar f) | f `Set.member` ffs = Frm.fromBool neg
          ignoreFormula' phi = phi

projectAbs :: AbstractionMonad m => Set.Set Lit -> Abstraction m
projectAbs lits = (projectFeatures, Set.toList $ lits, projectFormula)
    where projectFeatures = do
            (_, features) <- ask
            return features
          projectFormula = return
