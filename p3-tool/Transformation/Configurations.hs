{-# LANGUAGE FlexibleContexts #-}
module Transformation.Configurations (generateConfigs, excludeLit) where

import qualified TVL.Ast as T
import qualified TVL.Pretty as TVLPretty

import Abstraction.Ast (Lit(..), feature)

import Data.Generics.Uniplate.Data

import Control.Monad
import qualified Data.Set.Monad as Set
import Data.Maybe
import Data.List

import Control.Monad.Except

import qualified Transformation.Formulae as Frm

transformLit ::  (String -> Maybe Frm.Formula) -> (String -> Maybe Frm.Formula) -> Frm.Formula -> Maybe Frm.Formula
transformLit  onneg _onpos ((Frm.:!:) (Frm.FVar x))  = onneg x
transformLit _onneg onpos  (Frm.FVar x)              = onpos x
transformLit _onneg _onpos _frm                      = Nothing

excludeLit :: Lit -> Frm.Formula -> Frm.Formula
excludeLit (PosLit feat) frm = rewrite (transformLit (const $ Just Frm.FTrue)
                                        (const $ Just Frm.FFalse)) $ Frm.nnf frm
excludeLit (NegLit feat) frm = rewrite (transformLit (const $ Just Frm.FFalse)
                                        (const $ Just Frm.FTrue)) $ Frm.nnf frm

generateConfigs :: (Monad m, MonadError String m) => T.Model -> m Frm.Formula
generateConfigs [T.DFeature f] = snd <$> generateFeatureFormula Nothing f
generateConfigs m = throwError ("Unsupported configuration model:\n" ++ (show . TVLPretty.prettyModel) m)

generateFeatureFormula :: (Monad m, MonadError String m) => Maybe T.Name -> T.FeatureDecl -> m (T.Name, Frm.Formula)
generateFeatureFormula parent (T.FtFeature root name items) | not root || isNothing parent =  do
  itemFormulae <- mapM (generateItemFormula name) items
  return (name, Frm.fAll (maybe (Frm.FVar name) ((Frm.FVar name Frm.:=>:) . Frm.FVar) parent :
                          itemFormulae))
generateFeatureFormula parent (T.FtGroup root name crd hidecls) | not root || isNothing parent = do
  groupFormula <- generateGroupFormula name crd hidecls
  return (name, Frm.fAll [maybe (Frm.FVar name) ((Frm.FVar name Frm.:=>:) . Frm.FVar) parent,
                          groupFormula])
generateFeatureFormula _ f = throwError ("Feature must be root: " ++ show f)

generateItemFormula :: (Monad m, MonadError String m) => T.Name -> T.FtBodyItem -> m Frm.Formula
generateItemFormula parent (T.FtiGroup crd hidecls) = generateGroupFormula parent crd hidecls
generateItemFormula parent f                        = throwError ("Unsupported feature item: " ++ show f)

generateGroupFormula :: (Monad m, MonadError String m) => T.Name -> T.Cardinality -> [T.FtHiDecl] -> m Frm.Formula
generateGroupFormula parent crd hidecls = do
    (hiDeclNameOpts, hiDeclFormulae) <- unzip <$> mapM (generateHiDeclFormula parent) hidecls
    cardFormulae <- generateGroupCardFormula crd hiDeclNameOpts
    return $ Frm.fAll (cardFormulae:hiDeclFormulae)
  where generateGroupCardFormula :: (Monad m, MonadError String m) => T.Cardinality -> [(T.Name, Bool)] -> m Frm.Formula
        generateGroupCardFormula T.CdOneOf nameOpts = return $ Frm.fAll [Frm.fAll [Frm.FVar ci Frm.:=>: ((Frm.:!:) $ Frm.FVar cj)
                                                                                  | (ci, _) <- nameOpts, (cj,_) <- nameOpts, ci /= cj]
                                                                        , atLeastOne nameOpts]
        generateGroupCardFormula T.CdSomeOf nameOpts = return Frm.FTrue
        generateGroupCardFormula T.CdAllOf nameOpts  = return $ Frm.FVar parent Frm.:=>: Frm.fAll (map (Frm.FVar . fst) (filter (not . snd) nameOpts))
        generateGroupCardFormula (T.CdRange l h) nameOpts | l == 0 && h == Just 1 = return $ Frm.fAll [Frm.FVar ci Frm.:=>: ((Frm.:!:) $ Frm.FVar cj)
                                                                                                      | (ci, _) <- nameOpts, (cj, _) <- nameOpts, ci /= cj]
                                                          | l == 1 && h == Just 1 = generateGroupCardFormula T.CdOneOf nameOpts
                                                          | l == 0 && h == Just (toInteger $ length nameOpts) = return Frm.FTrue
                                                          | l == 1 && h == Just (toInteger $ length nameOpts) = return $ atLeastOne nameOpts
                                                          | otherwise = throwError ("Unsupported cardinality: " ++ "[" ++ show l ++ "," ++ show h ++ "]")
        atLeastOne nameOpts = Frm.fAny [Frm.FVar c | (c, isOpt) <- nameOpts, isOpt] Frm.:|:
                                          (Frm.FVar parent Frm.:=>: Frm.fAny [Frm.FVar c | (c, isOpt) <- nameOpts, not isOpt])

generateHiDeclFormula :: (Monad m, MonadError String m) => T.Name -> T.FtHiDecl -> m ((T.Name, Bool), Frm.Formula)
generateHiDeclFormula parent (T.FthFeature pr f) = do
 (fName, fFormula) <- generateFeatureFormula (Just parent) f
 isOpt <- optionalPresence pr
 return ((fName, isOpt), Frm.fAll [fFormula])
generateHiDeclFormula parent (T.FthItem pr name) = do
 isOpt <- optionalPresence pr
 return ((name, isOpt), Frm.fAll [Frm.FVar name Frm.:=>: Frm.FVar parent])

optionalPresence :: (Monad m, MonadError String m) => T.Presence -> m Bool
optionalPresence T.PNone     = return False
optionalPresence T.POptional = return True
optionalPresence T.PShared   = throwError "Shared features unsupported"
