{-# LANGUAGE ScopedTypeVariables #-}
module Transformation.Configurations where

import qualified TVL.Ast as T

import Data.Generics.Uniplate.Data

import Control.Monad
import qualified Data.Set.Monad as Set
import Data.List

import Debug.Trace

data Config = Config { selections :: Set.Set (Set.Set String), allf :: Set.Set String }
  deriving (Eq, Show)

emptyConfig :: Config
emptyConfig = Config (Set.singleton Set.empty) $ Set.empty

generateConfigs :: T.Model -> Either String Config
generateConfigs [T.DFeature (T.FtFeature True name items)] = do
  itemConfigs <- mapM generateItemConfig items
  mergeAll (Config { selections = Set.singleton (Set.singleton name), allf = Set.singleton name } : itemConfigs)
generateConfigs [T.DFeature (T.FtGroup True name crd hidecls)] = do
  groupConfig <- generateGroupConfig crd hidecls
  trace ("Cardinality: " ++ show crd ++ " , " ++ "Configs: " ++ show groupConfig) $ return ()
  mergeAll (Config { selections = Set.singleton (Set.singleton name), allf = Set.singleton name } : [groupConfig])
generateConfigs m = Left ("Unsupported configuration model: " ++ show m)

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
      subsels <- mergeExactly (k-1) cfgs'
      return $ foldr mergeSelections (Set.singleton Set.empty) (selections cfg : map selections cfgs')
    ) splitcfgs
  return $ Config (Set.unions allselections) (allf allcfgs)


mergeAll :: [Config] -> Either String Config
mergeAll = return . foldr mergeConfigs emptyConfig
  where mergeConfigs :: Config -> Config -> Config
        mergeConfigs c1 c2 = Config { selections = (selections c1) `Set.union` (selections c2), allf = allf c1 `Set.union` allf c2 }

mergeSelections :: Set.Set (Set.Set String) -> Set.Set (Set.Set String) -> Set.Set (Set.Set String)
mergeSelections c1s c2s = do
  c1s' <- c1s
  c2s' <- c2s
  return (c1s' `Set.union` c2s')

generateItemConfig :: T.FtBodyItem -> Either String Config
generateItemConfig (T.FtiGroup crd hidecls) = generateGroupConfig crd hidecls
generateItemConfig f                        = Left ("Unsupported feature item: " ++ show f)

generateGroupConfig :: T.Cardinality -> [T.FtHiDecl] -> Either String Config
generateGroupConfig crd hidecls = do
    cfgs <- mapM generateHiDeclConfig hidecls
    traceShow cfgs$ return()
    mergeRange (crdl crd) (crdh crd) cfgs
  where crdl T.CdOneOf       = 1
        crdl T.CdSomeOf      = 1
        crdl T.CdAllOf       = toInteger $ length hidecls
        crdl (T.CdRange l h) = l
        crdh T.CdOneOf       = Just 1
        crdh T.CdSomeOf      = Just . toInteger $ length hidecls
        crdh T.CdAllOf       = Just . toInteger $ length hidecls
        crdh (T.CdRange l h) = h


-- TODO remove code duplication
generateHiDeclConfig :: T.FtHiDecl -> Either String Config
generateHiDeclConfig (T.FthFeature _ (T.FtFeature _ name items)) = do
  itemConfigs <- mapM generateItemConfig items
  mergeAll (Config { selections = Set.singleton (Set.singleton name), allf = Set.singleton name } : itemConfigs)
generateHiDeclConfig (T.FthFeature _ (T.FtGroup _ name crd hidecls)) = do
  groupConfig <- generateGroupConfig crd hidecls
  mergeAll (Config { selections = Set.singleton (Set.singleton name), allf = Set.singleton name } : [groupConfig])
generateHiDeclConfig (T.FthItem _ name) = do
   return $ Config { selections = Set.singleton (Set.singleton name), allf = Set.singleton name }
