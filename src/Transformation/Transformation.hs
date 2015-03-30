{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
module Transformation.Transformation where

import qualified FPromela.Ast as FP

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

type Features = (String, Set.Set String)

abstractSpec :: AbstractionMonad m => Abstraction m -> FP.Spec -> m FP.Spec
abstractSpec alpha spec = do
  features <- getFeatures spec
  transformBiM (rewriteFeatureBranches alpha features) spec

getFeatures :: AbstractionMonad m => FP.Spec -> m Features
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

rewriteFeatureBranches :: AbstractionMonad m => Abstraction m -> Features -> FP.Stmt -> m FP.Stmt
rewriteFeatureBranches alpha (f, fs) stmt@(FP.StIf opts) = FP.StIf <$> rewriteFeatureOpts alpha (f, fs) opts
rewriteFeatureBranches alpha (f, fs) stmt@(FP.StDo opts) = FP.StDo <$> rewriteFeatureOpts alpha (f, fs) opts
rewriteFeatureBranches alpha fs stmt =
  descendM (transformM (rewriteFeatureBranches alpha fs)) stmt

rewriteFeatureOpts :: AbstractionMonad m => Abstraction m -> Features -> FP.Options -> m FP.Options
rewriteFeatureOpts alpha (f, fs) opts | any hasStaticVarRef opts = do
    phis <- mapM mapOption opts
    let phis' = map (fixElse phis) phis
    mapM convertOption (zip opts phis')
  where hasStaticVarRef (FP.SStmt (FP.StExpr e) _ : _) = any isStaticVarRef $ childrenBi e
        hasStaticVarRef _ = False
        isStaticVarRef (FP.VarRef f' _ _) | f == f' = True
        isStaticVarRef _                            = False
        mapOption o@((FP.SStmt (FP.StExpr e) Nothing):_) = do
            phi <- fromFPromelaExpr f e
            return $ Just phi
        mapOption o@((FP.SStmt FP.StElse Nothing):_) = return $ Nothing
        mapOption o = throwError ("Unsupported option: " ++ show o)
        fixElse phis Nothing  = foldr (\phi phis' -> (:!:) phi :&: phis') FTrue (catMaybes phis)
        fixElse phis (Just a) = a
        convertOption (_:steps, phi) = do
             newPhi <- alpha phi
             let newE = interpretAsFPromelaExpr f newPhi
             return ((FP.SStmt (FP.StExpr newE) Nothing):steps)
        convertOption o              = throwError ("Unsupported option: " ++ show o)
rewriteFeatureOpts alpha fs o = transformBiM (rewriteFeatureBranches alpha fs) o
