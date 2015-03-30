{-# LANGUAGE FlexibleContexts, ViewPatterns, ConstraintKinds #-}
module Transformation.Abstraction (AbstractionMonad, Abstraction, joinAbs, ignoreAbs) where
import Data.Foldable as Fold
import qualified Data.Set.Monad as Set
import qualified Data.Map as Map
import qualified Transformation.Configurations as Cnfg
import qualified Transformation.Formulae as Frm

import qualified Data.SBV as SBV

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Loops

type AbstractionMonad m = (Functor m, Applicative m, Monad m, MonadError String m, MonadIO m, MonadState (Set.Set Cnfg.Config, Set.Set String) m)
type Abstraction m = Frm.Formula -> m Frm.Formula

extractFeatures :: AbstractionMonad m => m (Set.Set String)
extractFeatures = do
    (cfgs, _) <- get
    return $ Set.foldr Set.union Set.empty $ Set.map joinCfg cfgs
  where joinCfg (Cnfg.Config incl excl) = Set.union incl excl

infix 5 |=

(|=) :: AbstractionMonad m => Cnfg.Config -> Frm.Formula -> m Bool
k |= phi = do
    features <- extractFeatures
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
joinAbs phi = do
  (cfgs, _) <- get
  res <- anyM (|= phi) (Set.toList cfgs)
  return $ Frm.fromBool res

ignoreAbs :: AbstractionMonad m => Set.Set String -> Abstraction m
ignoreAbs = undefined
