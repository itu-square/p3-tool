module Transformation.Transformation where

import qualified FPromela.Ast as FP

import Transformation.Configurations

import Control.Monad.Error

import Data.List
import Data.Maybe
import Data.SBV
import qualified Data.Set.Monad as Set
import Data.Generics.Uniplate.Data

type ErrorIO = ErrorT String IO

type Abstraction b = Set.Set Config -> b -> b

type Features = (String, Set.Set String)

abstractSpec :: Boolean b => Set.Set Config -> Abstraction b -> FP.Spec -> ErrorIO FP.Spec
abstractSpec cfgs alpha spec = do
  features <- getFeatures spec
  transformBiM (rewriteFeatureIfs cfgs alpha features) spec

getFeatures :: FP.Spec -> ErrorIO Features
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

rewriteFeatureIfs :: Boolean b => Set.Set Config -> Abstraction b -> Features -> FP.Stmt -> ErrorIO FP.Stmt
rewriteFeatureIfs cfgs alpha fs stmt = throwError "undeclared"
