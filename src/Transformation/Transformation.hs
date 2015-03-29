{-# LANGUAGE FlexibleContexts #-}
module Transformation.Transformation where

import qualified FPromela.Ast as FP

import Transformation.Configurations
import Transformation.Formulae
import Transformation.Abstraction

import Data.List
import Data.Maybe
import Data.SBV
import qualified Data.Set.Monad as Set
import qualified Data.Map as Map
import Data.Generics.Uniplate.Data

import Control.Monad
import Control.Monad.Except

import Debug.Trace

type Features = (String, Set.Set String)

abstractSpec :: (Monad m, MonadError String m, MonadIO m) => Set.Set Config -> Abstraction m -> FP.Spec -> m FP.Spec
abstractSpec cfgs alpha spec = do
  features <- getFeatures spec
  transformBiM (rewriteFeatureIfs cfgs alpha features) spec

getFeatures :: (Monad m, MonadError String m, MonadIO m) => FP.Spec -> m Features
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
    return (featurePrefix, Set.fromList fs)
  where -- TODO Convert with mapMaybe
        isFeaturesDecl (FP.MUType "features" _) = True
        isFeaturesDecl _                        = False
        extractFeatures (FP.MUType _ ds)        = mapM extractFeature ds
        extractFeature (FP.Decl Nothing FP.TBool [FP.IVar name Nothing Nothing]) = return name
        extractFeature d = throwError ("Unsupported feature declaration: " ++ show d)
        extractFeaturePrefix (FP.Decl Nothing (FP.TUName "features") [FP.IVar name Nothing Nothing]) = Just name
        extractFeaturePrefix _                                          = Nothing

rewriteFeatureIfs :: (Monad m, MonadError String m, MonadIO m) => Set.Set Config -> Abstraction m -> Features -> FP.Stmt -> m FP.Stmt
rewriteFeatureIfs cfgs alpha (f, fs) stmt@(FP.StIf opts) | any isStaticVarRef $ universeBi opts = do
    opts' <- mapM convertOption opts
    return $ FP.StIf opts'
  where isStaticVarRef (FP.VarRef f' _ _) | f == f' = True
        isStaticVarRef _                            = False
        convertOption o@((FP.SStmt (FP.StExpr e) Nothing):steps)         = do
          phi <- fromFPromelaExpr f e
          newPhi <- alpha cfgs phi
          let newE = interpretAsFPromelaExpr f newPhi
          return ((FP.SStmt (FP.StExpr newE) Nothing):steps)
        convertOption o@(FP.SStmt FP.StElse Nothing:steps)              = return o
        convertOption o                             = throwError ("Unsupported option: " ++ show o)
rewriteFeatureIfs cfgs alpha fs stmt =
  descendM (transformM (rewriteFeatureIfs cfgs alpha fs)) stmt

