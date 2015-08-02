{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
module Transformation.Configurations (Config(..), generateConfigs, removeFeature, excludeLitCfgs, (|=)) where

import qualified TVL.Ast as T
import qualified TVL.Pretty as TVLPretty

import Abstraction.Ast (Lit(..), feature)

import Data.Generics.Uniplate.Data
import qualified Data.SBV as SBV

import Data.Foldable as Fold
import qualified Data.Map.Strict as Map
import qualified Data.Set.Monad as Set
import qualified Data.List as List

import Transformation.Util
import qualified Transformation.Formulae as Frm

import Control.Monad
import Control.Monad.Except

data Config = Config { config_included :: Set.Set String, config_excluded :: Set.Set String }
  deriving (Eq, Show, Ord)

infix 5 |=

(|=) :: MonadIOExcept m => Config -> Frm.Formula -> m Bool
k |= phi = do
    let features = Set.toList $ Set.unions [config_included k, config_excluded k, Frm.vars phi]
    let completeFrm = fromConfig k Frm.:=>: phi
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


generateConfigs :: MonadIOExcept m => T.Model -> m (Set.Set Config)
generateConfigs [T.DFeature f] = do
  ConfigState sels alls cstr <- generateFeatureConfigState True f
  let cfgs = Set.map (\s -> Config s (alls `Set.difference` s)) sels
  liftM Set.fromList . filterM (|= cstr) . Set.toList $ cfgs
generateConfigs m = throwError ("Unsupported configuration model:\n" ++ (show . TVLPretty.prettyModel) m)

removeFeature :: String -> Config -> Config
removeFeature f cfg = cfg {
  config_included = Set.delete f (config_included cfg),
  config_excluded = Set.delete f (config_excluded cfg)
}

excludeLitCfgs :: Lit -> Set.Set Config -> Set.Set Config
excludeLitCfgs (PosLit f) cfgs = Set.filter (not . Set.member f . config_excluded) cfgs
excludeLitCfgs (NegLit f) cfgs = Set.filter (not . Set.member f . config_included) cfgs


data ConfigState = ConfigState { selections :: Set.Set (Set.Set String), allf :: Set.Set String, constraint :: Frm.Formula }
  deriving (Eq, Show)

emptyConfigState :: ConfigState
emptyConfigState = ConfigState (Set.singleton Set.empty) Set.empty Frm.FTrue

generateFeatureConfigState :: (Monad m, MonadError String m) => Bool -> T.FeatureDecl -> m ConfigState
generateFeatureConfigState mustberoot (T.FtFeature root name items) | not root || mustberoot =  do
  itemConfigStates <- mapM generateItemConfigState items
  mergeAll (emptyConfigState { selections = Set.singleton (Set.singleton name), allf = Set.singleton name } : itemConfigStates)
generateFeatureConfigState mustberoot (T.FtGroup root name crd hidecls) | not root || mustberoot = do
  groupConfigState <- generateGroupConfigState crd hidecls
  return $ ConfigState (Set.map (Set.singleton name `Set.union`) (selections groupConfigState)) (Set.singleton name `Set.union` allf groupConfigState) Frm.FTrue
generateFeatureConfigState _ f = throwError ("Feature must be root: " ++ show f)

generateItemConfigState :: (Monad m, MonadError String m) => T.FtBodyItem -> m ConfigState
generateItemConfigState (T.FtiGroup crd hidecls) = generateGroupConfigState crd hidecls
generateItemConfigState (T.FtiConstraint (T.CtExpr e)) = do
  phi <- Frm.fromTVLExpr e
  return $ emptyConfigState { constraint = phi }
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
 generatePresenceConfigState pr (emptyConfigState { selections = Set.singleton (Set.singleton name), allf = Set.singleton name })

generatePresenceConfigState :: (Monad m, MonadError String m) => T.Presence -> ConfigState -> m ConfigState
generatePresenceConfigState T.PNone     c                   = return c
generatePresenceConfigState T.POptional (ConfigState sels alls cstr)  = return $ ConfigState (Set.singleton Set.empty `Set.union` sels) alls cstr
generatePresenceConfigState T.PShared   _                   = throwError "Shared features unsupported"

mergeRange' :: (Monad m, MonadError String m) => [Integer] -> [ConfigState] -> m ConfigState
mergeRange' range cfgs = do
    subs <- mapM (flip mergeExactly cfgs) range
    return $ ConfigState (Set.unions (map selections subs)) (Set.unions (map allf subs)) Frm.FTrue

mergeRange :: (Monad m, MonadError String m) => Integer -> (Maybe Integer) -> [ConfigState] -> m ConfigState
mergeRange kmin (Just kmax) cfgs | 0 <= kmin && kmin <= kmax
                                             && kmax <= toInteger (length cfgs) = mergeRange' [kmin .. kmax] cfgs
mergeRange kmin Nothing     cfgs | 0 <= kmin                                    = mergeRange' [kmin .. toInteger (length cfgs)] cfgs
mergeRange kmin kmax _ = throwError ("Unsupported range [" ++ show kmin ++ ".." ++ maybe "*" (show . id) kmax ++ "]")

mergeExactly :: (Monad m, MonadError String m) => Integer -> [ConfigState] -> m ConfigState
mergeExactly k cfgs | k <= 0 = do
  allcfgs <- mergeAll cfgs
  return $ ConfigState (Set.singleton Set.empty) (allf allcfgs) (constraint allcfgs)
mergeExactly k cfgs = do
  allcfgs <- mergeAll cfgs
  let splitcfgs = map (\cfg1 -> (cfg1, (List.delete cfg1 cfgs))) cfgs
  allselections <- mapM (\(cfg, cfgs') -> do
      subsels <- mergeExactly (k - 1) cfgs'
      return $ mergeSelections (selections cfg) (selections subsels)
    ) splitcfgs
  return $ ConfigState (Set.unions allselections) (allf allcfgs) (constraint allcfgs)


mergeAll :: (Monad m, MonadError String m) => [ConfigState] -> m ConfigState
mergeAll = return . Fold.foldr mergeConfigStates emptyConfigState
  where mergeConfigStates :: ConfigState -> ConfigState -> ConfigState
        mergeConfigStates c1 c2 = ConfigState {
          selections = mergeSelections (selections c1) (selections c2),
          allf = allf c1 `Set.union` allf c2,
          constraint = constraint c1 Frm.:&: constraint c2 }

mergeSelections :: Set.Set (Set.Set String) -> Set.Set (Set.Set String) -> Set.Set (Set.Set String)
mergeSelections c1s c2s = do
  c1s' <- c1s
  c2s' <- c2s
  return (c1s' `Set.union` c2s')

fromConfig :: Config -> Frm.Formula
fromConfig (Config incl excl) =
  let inclvars = Set.mapMonotonic Frm.FVar incl
      exclvars = Set.mapMonotonic ((Frm.:!:) . Frm.FVar) excl
      allvars  = Set.union inclvars exclvars
  in Set.foldr (Frm.:&:) Frm.FTrue allvars
