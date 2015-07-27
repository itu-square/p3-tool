{-# LANGUAGE FlexibleContexts #-}
module Transformation.Configurations (Config(..), generateConfigs, removeFeature, excludeLitCfgs) where

import qualified TVL.Ast as T
import qualified TVL.Pretty as TVLPretty

import Abstraction.Ast (Lit(..), feature)

import Data.Generics.Uniplate.Data

import Control.Monad
import qualified Data.Set.Monad as Set
import Data.List

import Control.Monad.Except

data Config = Config { config_included :: Set.Set String, config_excluded :: Set.Set String }
  deriving (Eq, Show, Ord)

generateConfigs :: (Monad m, MonadError String m) => T.Model -> m (Set.Set Config)
generateConfigs [T.DFeature f] = do
  ConfigState sels alls <- generateFeatureConfigState True f
  return $ Set.map (\s -> Config s (alls `Set.difference` s)) sels
generateConfigs m = throwError ("Unsupported configuration model:\n" ++ (show . TVLPretty.prettyModel) m)

removeFeature :: String -> Config -> Config
removeFeature f cfg = cfg {
  config_included = Set.delete f (config_included cfg),
  config_excluded = Set.delete f (config_excluded cfg)
}

excludeLitCfgs :: Lit -> Set.Set Config -> Set.Set Config
excludeLitCfgs (PosLit f) cfgs = Set.filter (not . Set.member f . config_included) cfgs
excludeLitCfgs (NegLit f) cfgs = Set.filter (not . Set.member f . config_excluded) cfgs


data ConfigState = ConfigState { selections :: Set.Set (Set.Set String), allf :: Set.Set String }
  deriving (Eq, Show)

emptyConfigState :: ConfigState
emptyConfigState = ConfigState (Set.singleton Set.empty) $ Set.empty

generateFeatureConfigState :: (Monad m, MonadError String m) => Bool -> T.FeatureDecl -> m ConfigState
generateFeatureConfigState mustberoot (T.FtFeature root name items) | not root || mustberoot =  do
  itemConfigStates <- mapM generateItemConfigState items
  mergeAll (ConfigState { selections = Set.singleton (Set.singleton name), allf = Set.singleton name } : itemConfigStates)
generateFeatureConfigState mustberoot (T.FtGroup root name crd hidecls) | not root || mustberoot = do
  groupConfigState <- generateGroupConfigState crd hidecls
  return $ ConfigState (Set.map (Set.singleton name `Set.union`) (selections groupConfigState)) (Set.singleton name `Set.union` allf groupConfigState)
generateFeatureConfigState _ f = throwError ("Feature must be root: " ++ show f)

generateItemConfigState :: (Monad m, MonadError String m) => T.FtBodyItem -> m ConfigState
generateItemConfigState (T.FtiGroup crd hidecls) = generateGroupConfigState crd hidecls
generateItemConfigState f                        = throwError ("Unsupported feature item: " ++ show f)

generateGroupConfigState :: (Monad m, MonadError String m) => T.Cardinality -> [T.FtHiDecl] -> m ConfigState
generateGroupConfigState crd hidecls = do
    cfgs <- mapM generateHiDeclConfigState hidecls
    mergeRange (crdl crd) (crdh crd) cfgs
  where crdl T.CdOneOf       = 1
        crdl T.CdSomeOf      = 1
        crdl T.CdAllOf       = toInteger $ length hidecls
        crdl (T.CdRange l h) = l
        crdh T.CdOneOf       = Just 1
        crdh T.CdSomeOf      = Just . toInteger $ length hidecls
        crdh T.CdAllOf       = Just . toInteger $ length hidecls
        crdh (T.CdRange l h) = h

generateHiDeclConfigState :: (Monad m, MonadError String m) => T.FtHiDecl -> m ConfigState
generateHiDeclConfigState (T.FthFeature pr f) = do
 c <- generateFeatureConfigState False f
 generatePresenceConfigState pr c
generateHiDeclConfigState (T.FthItem pr name) = do
 generatePresenceConfigState pr (ConfigState { selections = Set.singleton (Set.singleton name), allf = Set.singleton name })

generatePresenceConfigState :: (Monad m, MonadError String m) => T.Presence -> ConfigState -> m ConfigState
generatePresenceConfigState T.PNone     c                   = return c
generatePresenceConfigState T.POptional (ConfigState sels alls)  = return $ ConfigState (Set.singleton Set.empty `Set.union` sels) alls
generatePresenceConfigState T.PShared   _                   = throwError "Shared features unsupported"

mergeRange' :: (Monad m, MonadError String m) => [Integer] -> [ConfigState] -> m ConfigState
mergeRange' range cfgs = do
    subs <- mapM (flip mergeExactly cfgs) range
    return $ ConfigState (Set.unions (map selections subs)) (Set.unions (map allf subs))

mergeRange :: (Monad m, MonadError String m) => Integer -> (Maybe Integer) -> [ConfigState] -> m ConfigState
mergeRange kmin (Just kmax) cfgs | 0 <= kmin && kmin <= kmax
                                             && kmax <= toInteger (length cfgs) = mergeRange' [kmin .. kmax] cfgs
mergeRange kmin Nothing     cfgs | 0 <= kmin                                    = mergeRange' [kmin .. toInteger (length cfgs)] cfgs
mergeRange kmin kmax _ = throwError ("Unsupported range [" ++ show kmin ++ ".." ++ maybe "*" (show . id) kmax ++ "]")

mergeExactly :: (Monad m, MonadError String m) => Integer -> [ConfigState] -> m ConfigState
mergeExactly k cfgs | k <= 0 = do
  allcfgs <- mergeAll cfgs
  return $ ConfigState (Set.singleton Set.empty) (allf allcfgs)
mergeExactly k cfgs = do
  allcfgs <- mergeAll cfgs
  let splitcfgs = map (\cfg1 -> (cfg1, (delete cfg1 cfgs))) cfgs
  allselections <- mapM (\(cfg, cfgs') -> do
      subsels <- mergeExactly (k - 1) cfgs'
      return $ mergeSelections (selections cfg) (selections subsels)
    ) splitcfgs
  return $ ConfigState (Set.unions allselections) (allf allcfgs)


mergeAll :: (Monad m, MonadError String m) => [ConfigState] -> m ConfigState
mergeAll = return . foldr mergeConfigStates emptyConfigState
  where mergeConfigStates :: ConfigState -> ConfigState -> ConfigState
        mergeConfigStates c1 c2 = ConfigState { selections = mergeSelections (selections c1) (selections c2), allf = allf c1 `Set.union` allf c2 }

mergeSelections :: Set.Set (Set.Set String) -> Set.Set (Set.Set String) -> Set.Set (Set.Set String)
mergeSelections c1s c2s = do
  c1s' <- c1s
  c2s' <- c2s
  return (c1s' `Set.union` c2s')
