module Transformation.Configurations (generateConfigs) where

import qualified TVL.Ast as T

import Data.Generics.Uniplate.Data

import Control.Monad
import qualified Data.Set.Monad as Set
import Data.List

data Config = Config { selections :: Set.Set (Set.Set String), allf :: Set.Set String }
  deriving (Eq, Show)

emptyConfig :: Config
emptyConfig = Config (Set.singleton Set.empty) $ Set.empty

generateConfigs :: T.Model -> Either String (Set.Set (Set.Set String, Set.Set String))
generateConfigs [T.DFeature f] = do
  Config sels alls <- generateFeatureConfig True f
  return $ Set.map (\s -> (s, alls `Set.difference` s)) sels
generateConfigs m = Left ("Unsupported configuration model: " ++ show m)

generateFeatureConfig :: Bool -> T.FeatureDecl -> Either String Config
generateFeatureConfig mustberoot (T.FtFeature root name items) | not root || mustberoot =  do
  itemConfigs <- mapM generateItemConfig items
  mergeAll (Config { selections = Set.singleton (Set.singleton name), allf = Set.singleton name } : itemConfigs)
generateFeatureConfig mustberoot (T.FtGroup root name crd hidecls) | not root || mustberoot = do
  groupConfig <- generateGroupConfig crd hidecls
  return $ Config (Set.map (Set.singleton name `Set.union`) (selections groupConfig)) (Set.singleton name `Set.union` allf groupConfig)
generateFeatureConfig _ f = Left ("Feature must be root: " ++ show f)

generateItemConfig :: T.FtBodyItem -> Either String Config
generateItemConfig (T.FtiGroup crd hidecls) = generateGroupConfig crd hidecls
generateItemConfig f                        = Left ("Unsupported feature item: " ++ show f)

generateGroupConfig :: T.Cardinality -> [T.FtHiDecl] -> Either String Config
generateGroupConfig crd hidecls = do
    cfgs <- mapM generateHiDeclConfig hidecls
    mergeRange (crdl crd) (crdh crd) cfgs
  where crdl T.CdOneOf       = 1
        crdl T.CdSomeOf      = 1
        crdl T.CdAllOf       = toInteger $ length hidecls
        crdl (T.CdRange l h) = l
        crdh T.CdOneOf       = Just 1
        crdh T.CdSomeOf      = Just . toInteger $ length hidecls
        crdh T.CdAllOf       = Just . toInteger $ length hidecls
        crdh (T.CdRange l h) = h

generateHiDeclConfig :: T.FtHiDecl -> Either String Config
generateHiDeclConfig (T.FthFeature pr f) = do
 c <- generateFeatureConfig False f
 generatePresenceConfig pr c
generateHiDeclConfig (T.FthItem pr name) = do
 generatePresenceConfig pr (Config { selections = Set.singleton (Set.singleton name), allf = Set.singleton name })

generatePresenceConfig :: T.Presence -> Config -> Either String Config
generatePresenceConfig T.PNone     c                   = return c
generatePresenceConfig T.POptional (Config sels alls)  = return $ Config (Set.singleton Set.empty `Set.union` sels) alls
generatePresenceConfig T.PShared   _                   = Left "Shared features unsupported"

mergeRange' :: [Integer] -> [Config] -> Either String Config
mergeRange' range cfgs = do
  subs <- mapM (flip mergeExactly cfgs) range
  mergeAll subs

mergeRange :: Integer -> (Maybe Integer) -> [Config] -> Either String Config
mergeRange kmin (Just kmax) cfgs | 0 <= kmin && kmin <= kmax
                                             && kmax <= toInteger (length cfgs) = mergeRange' [kmin .. kmax] cfgs
mergeRange kmin Nothing     cfgs | 0 <= kmin                                    = mergeRange' [kmin ..] cfgs
mergeRange kmin kmax _ = Left ("Unsupported range [" ++ show kmin ++ ".." ++ maybe "*" (show . id) kmax ++ "]")

mergeExactly :: Integer -> [Config] -> Either String Config
mergeExactly k cfgs | k <= 0 = do
  allcfgs <- mergeAll cfgs
  return $ Config (Set.singleton Set.empty) (allf allcfgs)
mergeExactly k cfgs = do
  allcfgs <- mergeAll cfgs
  let splitcfgs = map (\cfg1 -> (cfg1, (delete cfg1 cfgs))) cfgs
  allselections <- mapM (\(cfg, cfgs') -> do
      subsels <- mergeExactly (k - 1) cfgs'
      return $ mergeSelections (selections cfg) (selections subsels)
    ) splitcfgs
  return $ Config (Set.unions allselections) (allf allcfgs)


mergeAll :: [Config] -> Either String Config
mergeAll = return . foldr mergeConfigs emptyConfig
  where mergeConfigs :: Config -> Config -> Config
        mergeConfigs c1 c2 = Config { selections = mergeSelections (selections c1) (selections c2), allf = allf c1 `Set.union` allf c2 }

mergeSelections :: Set.Set (Set.Set String) -> Set.Set (Set.Set String) -> Set.Set (Set.Set String)
mergeSelections c1s c2s = do
  c1s' <- c1s
  c2s' <- c2s
  return (c1s' `Set.union` c2s')
