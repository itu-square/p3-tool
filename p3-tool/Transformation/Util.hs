{-# LANGUAGE FlexibleContexts, ViewPatterns, ConstraintKinds #-}
module Transformation.Util where

import Control.Applicative
import Control.Monad
import Control.Monad.Except

type MonadIOExcept m = (Functor m, Applicative m, Monad m, MonadError String m, MonadIO m)
