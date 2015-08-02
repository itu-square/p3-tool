{-# LANGUAGE FlexibleContexts, ConstraintKinds, TypeFamilies #-}
module Transformation.Transformation where

import qualified FPromela.Ast as FP
import qualified TVL.Ast as TVL

import qualified TVL.Util as TUtil

import Transformation.Util
import Transformation.Configurations
import Transformation.Formulae
import Transformation.Abstraction

import Data.List
import Data.Maybe
import Data.SBV
import qualified Data.Set.Monad as Set
import qualified Data.Map.Strict as Map
import Data.Generics.Uniplate.Data

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

type Features = (String, [String])

abstractSpec :: (MonadIOExcept n, AbstractionMonad m, m ~ ReaderT (Set.Set Config, [String]) n) =>
                    Abstraction m -> FP.Spec -> TVL.Model -> Set.Set Config -> n (FP.Spec, TVL.Model, Set.Set Config)
abstractSpec alpha spec tvl cfgs = do
  let allf = Set.foldr (\e ss -> config_included e `Set.union` config_excluded e `Set.union` ss) Set.empty cfgs
  (fname, features) <- getFeatures spec
  let difff = allf `Set.difference` Set.fromList features
  let cfgs' = Set.foldr (\f cfgs' ->  Set.map (removeFeature f) cfgs') cfgs difff
  let (af, lits, aphi) = alpha
  features' <- runReaderT af (cfgs', features)
  spec' <- setFeatures (fname, features') spec
  spec'' <- runReaderT (transformBiM (rewriteFeatureBranches aphi (fname, features)) spec') (cfgs', features)
  spec''' <- removeFeatureDeclsIfEmpty spec''
  let cfgs'' = foldr (\lit cfgs'' -> excludeLitCfgs lit cfgs'') cfgs' lits
  tvl' <-
    if lits /= []
      then do
        prod <- fromLits lits
        TUtil.addConstraint (interpretAsTVLConstraint prod) tvl
    else return tvl
  pure (spec''', tvl', cfgs'')

setFeatures :: MonadIOExcept m => Features -> FP.Spec -> m FP.Spec
setFeatures (fname, fs) spec = transformBiM updateFeatureDecl spec
    where updateFeatureDecl :: MonadIOExcept m => FP.Module -> m FP.Module
          updateFeatureDecl (FP.MUType "features" ds) = do ds' <- updateFeatures ds; return $ FP.MUType "features" ds'
          updateFeatureDecl d = return d
          updateFeatures :: MonadIOExcept m => [FP.Decl] -> m [FP.Decl]
          updateFeatures (f@(FP.Decl Nothing FP.TBool [FP.IVar name Nothing Nothing]) : ds) | name `elem` fs = liftM (f :) (updateFeatures ds)
                                                                                            | otherwise = updateFeatures ds
          updateFeatures [] = return []
          updateFeatures (d : ds') = throwError ("Unsupported decl " ++ show d)


getFeatures :: MonadIOExcept m => FP.Spec -> m Features
getFeatures spec = do
    let featureDecls = filter isFeaturesDecl $ universeBi spec
    when (length featureDecls <= 0) $ throwError "No features declaration found"
    when (length featureDecls >= 2) $ throwError "Too many feature declarations found"
    let featureDecl = head featureDecls
    fs <- extractFeatures featureDecl
    let featurePrefixes = mapMaybe extractFeaturePrefix $ universeBi spec
    when (length featurePrefixes <= 0) $ throwError "No features instance found"
    when (length featurePrefixes >= 2) $ throwError "Too many features instance found"
    let featurePrefix = head featurePrefixes
    return (featurePrefix, fs)
  where -- TODO Convert with mapMaybe
        isFeaturesDecl (FP.MUType "features" _) = True
        isFeaturesDecl _                        = False
        extractFeatures (FP.MUType _ ds)        = mapM extractFeature ds
        extractFeature (FP.Decl Nothing FP.TBool [FP.IVar name Nothing Nothing]) = return name
        extractFeature d = throwError ("Unsupported feature declaration: " ++ show d)
        extractFeaturePrefix (FP.Decl Nothing (FP.TUName "features") [FP.IVar name Nothing Nothing]) = Just name
        extractFeaturePrefix _                                          = Nothing

rewriteFeatureBranches :: AbstractionMonad m => (Formula -> m Formula) -> Features -> FP.Stmt -> m FP.Stmt
rewriteFeatureBranches alpha (f, fs) stmt@(FP.StIf opts) = FP.StIf <$> rewriteFeatureOpts alpha (f, fs) opts
rewriteFeatureBranches alpha (f, fs) stmt@(FP.StDo opts) = FP.StDo <$> rewriteFeatureOpts alpha (f, fs) opts
rewriteFeatureBranches alpha fs stmt =
  descendM (transformM (rewriteFeatureBranches alpha fs)) stmt

rewriteFeatureOpts :: AbstractionMonad m => (Formula -> m Formula) -> Features -> FP.Options -> m FP.Options
rewriteFeatureOpts alpha (f, fs) opts | any hasStaticVarRef opts = do
    phis <- mapM mapOption opts
    let phis' = map (fixElse phis) phis
    mapM convertOption (zip opts phis')
  where hasStaticVarRef (FP.SStmt (FP.StExpr e) _ : _) = any isStaticVarRef $ childrenBi e
        hasStaticVarRef _ = False
        isStaticVarRef (FP.VarRef f' _ _) | f == f' = True
        isStaticVarRef _                            = False
        mapOption o@((FP.SStmt (FP.StExpr e) Nothing):_) = do
            phi <- fromFPromelaExpr f e
            return $ Just phi
        mapOption o@((FP.SStmt FP.StElse Nothing):_) = return $ Nothing
        mapOption o = throwError ("Unsupported option: " ++ show o)
        fixElse phis Nothing  = let ophis = catMaybes phis in foldr (\phi phis' -> (:!:) phi :&: phis') ((:!:) $ head ophis) (tail ophis)
        fixElse phis (Just a) = a
        convertOption (_:steps, phi) = do
             newPhi <- alpha phi
             let newE = interpretAsFPromelaExpr f newPhi
             return ((FP.SStmt (FP.StExpr newE) Nothing):steps)
        convertOption o              = throwError ("Unsupported option: " ++ show o)
rewriteFeatureOpts alpha fs o = transformBiM (rewriteFeatureBranches alpha fs) o

-- Remove empty features decl since they are invalid
removeFeatureDeclsIfEmpty :: MonadIOExcept n => FP.Spec -> n FP.Spec
removeFeatureDeclsIfEmpty spec = do
    (spec', res) <- runStateT (transformBiM removeFeatureTypeDecl spec) $ False
    if res
      then
        transformBiM removeFeatureVarDecl spec'
      else
        pure spec'
  where removeFeatureTypeDecl :: MonadIOExcept n => [FP.Module] -> StateT Bool n [FP.Module]
        removeFeatureTypeDecl ms | f `elem` ms = do
          put True
          return $ delete f ms
          where f :: FP.Module
                f = (FP.MUType "features" [])
        removeFeatureTypeDecl ms = return ms
        removeFeatureVarDecl :: MonadIOExcept n => [FP.Decl] -> n [FP.Decl]
        removeFeatureVarDecl ds = return $ filter (not . isFeatureVarDecl) ds
          where isFeatureVarDecl :: FP.Decl -> Bool
                isFeatureVarDecl (FP.Decl _ (FP.TUName "features") [FP.IVar _ _ _]) = True
                isFeatureVarDecl _ = False
