{-# LANGUAGE FlexibleContexts #-}
module Transformation.Abstraction where
import Data.Foldable
import qualified Data.Set.Monad as Set
import qualified Data.Map as Map
import qualified Transformation.Configurations as Cnfg
import qualified Transformation.Formulae as Frm

import qualified Data.SBV as SBV

import Control.Monad
import Control.Monad.Except

type Abstraction m = Set.Set Cnfg.Config -> Frm.Formula -> m Frm.Formula

joinAbs :: (Monad m, MonadError String m, MonadIO m) => Abstraction m
joinAbs cfgs phi = do
    let features = Set.foldr Set.union Set.empty $ Set.map joinCfg cfgs
    let completeFrm = Set.foldr (\cfg rest -> (cfg Frm.:=>: phi) Frm.:|: rest) Frm.FFalse $ Set.map Frm.fromConfig cfgs
    let pred = do fs <- foldrM featureToPred Map.empty features
                  v <- runExceptT (Frm.interpretAsSBool fs completeFrm)
                  case v of
                    Left err -> error err
                    Right p -> return p
    tres@(SBV.ThmResult res) <- liftIO $ SBV.prove pred
    case res of
      SBV.Satisfiable _ _ -> return Frm.FFalse
      SBV.Unsatisfiable _ -> return Frm.FTrue
      _ -> throwError (show tres)
  where featureToPred f m = do
          val <- SBV.forall f
          return $ Map.insert f val m
        joinCfg (Cnfg.Config incl excl) = Set.union incl excl
