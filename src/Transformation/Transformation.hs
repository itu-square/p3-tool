module Transformation.Transformation where

import qualified FPromela.Ast as FP

import Transformation.Configurations

import Control.Monad.Error

import Data.List
import Data.Maybe
import Data.SBV
import qualified Data.Set.Monad as Set
import qualified Data.Map as Map
import Data.Generics.Uniplate.Data

import Debug.Trace

type ErrorIO = ErrorT String IO

type Abstraction = Set.Set Config -> Predicate -> Predicate

type Features = (String, Set.Set String)

abstractSpec :: Set.Set Config -> Abstraction -> FP.Spec -> ErrorIO FP.Spec
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

rewriteFeatureIfs :: Set.Set Config -> Abstraction -> Features -> FP.Stmt -> ErrorIO FP.Stmt
rewriteFeatureIfs cfgs alpha (f, fs) stmt@(FP.StIf opts) | any isStaticVarRef $ universeBi opts = do
    opts' <- mapM convertOption opts
    return stmt
  where isStaticVarRef (FP.VarRef f' _ _) | f == f' = True
        isStaticVarRef _                            = False
        convertOption o@((FP.SStmt (FP.StExpr e) Nothing):steps)         = do
          let env = Set.foldr (\f m -> Map.insert f (forall f) m) Map.empty fs
          phi <- extractPhi e env
          return o
        convertOption o@(FP.SStmt FP.StElse Nothing:steps)              = return o
        convertOption o                             = throwError ("Unsupported option: " ++ show o)
        extractPhi :: FP.Expr -> Map.Map String Predicate -> ErrorIO Predicate
        extractPhi (FP.ELogic e1 "||" e2) m = do
          phi1 <- extractPhi e1 m
          phi2 <- extractPhi e2 m
          return $ do
            phi1' <- phi1
            phi2' <- phi2
            return (phi1' ||| phi2')
        extractPhi (FP.ELogic e1 "&&" e2) m = do
          phi1 <- extractPhi e1 m
          phi2 <- extractPhi e2 m
          return $ do
            phi1' <- phi1
            phi2' <- phi2
            return (phi1' &&& phi2')
        extractPhi (FP.EAnyExpr ae) m = extractPhi' ae m
        extractPhi e m = throwError ("Unsupported expression: " ++ show e)
        extractPhi' :: FP.AnyExpr -> Map.Map String Predicate -> ErrorIO Predicate
        extractPhi' (FP.AeBinOp e1 "||" e2) m = do
          phi1 <- extractPhi' e1 m
          phi2 <- extractPhi' e2 m
          return $ do
            phi1' <- phi1
            phi2' <- phi2
            return (phi1' ||| phi2')
        extractPhi' (FP.AeBinOp e1 "&&" e2) m = do
          phi1 <- extractPhi' e1 m
          phi2 <- extractPhi' e2 m
          return $ do
            phi1' <- phi1
            phi2' <- phi2
            return (phi1' &&& phi2')
        extractPhi' (FP.AeUnOp "!" e0) m = do
          phi0 <- extractPhi' e0 m
          return $ do
            phi0' <- phi0
            return (bnot phi0')
        extractPhi' (FP.AeVarRef (FP.VarRef f' Nothing (Just (FP.VarRef p Nothing Nothing)))) m | f == f' = do
           let v = Map.lookup p m
           case v of
            Nothing -> throwError ("Unknown feature: " ++ p)
            Just v -> return v
        extractPhi' e m = throwError ("Unsupported expression: " ++ show e)
rewriteFeatureIfs cfgs alpha fs stmt =
  descendM (transformM (rewriteFeatureIfs cfgs alpha fs)) stmt

