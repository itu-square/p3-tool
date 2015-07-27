{-# LANGUAGE FlexibleContexts #-}
module TVL.Util(addConstraint) where

import Control.Monad.Except
import TVL.Ast
import TVL.Pretty

addConstraint :: (Monad m, MonadError String m) => ConstraintDecl -> Model -> m Model
addConstraint cstr [DFeature (FtFeature True n itms)] = return $ [DFeature (FtFeature True n (itms ++ [FtiConstraint cstr]))]
addConstraint cstr [DFeature (FtGroup   True n crd ds)] = return $ [DFeature (FtFeature True n [FtiGroup crd ds, FtiConstraint cstr])]
addConstraint cstr model = throwError ("Unsupported model" ++ (show . prettyModel $ model))
