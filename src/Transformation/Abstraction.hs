{-# LANGUAGE FlexibleContexts, ViewPatterns, ConstraintKinds #-}
module Transformation.Abstraction where
import Data.Foldable
import qualified Data.Set.Monad as Set
import qualified Data.Map as Map
import qualified Transformation.Configurations as Cnfg
import qualified Transformation.Formulae as Frm

import qualified Data.SBV as SBV

import Control.Applicative
import Control.Monad
import Control.Monad.Except

type AbstractionMonad m = (Functor m, Applicative m, Monad m, MonadError String m, MonadIO m)
type Abstraction m = Set.Set Cnfg.Config -> Frm.Formula -> m Frm.Formula

joinAbs :: AbstractionMonad m => Abstraction m
joinAbs cfgs phi = do
  let features = Set.foldr Set.union Set.empty $ Set.map joinCfg cfgs
  joinAbs' features cfgs phi
    where joinAbs' :: AbstractionMonad m => Set.Set String -> Abstraction m
          joinAbs' _ (Set.maxView -> Nothing) phi = return Frm.FFalse
          joinAbs' features (Set.maxView -> Just (cfg, cfgs')) phi = do
              let completeFrm = Frm.fromConfig cfg Frm.:=>: phi
              let pred = do fs <- foldrM featureToPred Map.empty features
                            v <- runExceptT (Frm.interpretAsSBool fs completeFrm)
                            case v of
                              Left err -> error err
                              Right p -> return p
              tres@(SBV.ThmResult res) <- liftIO $ SBV.prove pred
              case res of
                SBV.Satisfiable _ _ -> joinAbs cfgs' phi
                SBV.Unsatisfiable _ -> return Frm.FTrue
                _ -> throwError (show tres)
          featureToPred f m = do
            val <- SBV.forall f
            return $ Map.insert f val m
          joinCfg (Cnfg.Config incl excl) = Set.union incl excl
