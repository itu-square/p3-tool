module Transformation.Common where

import qualified Control.Monad as Monad
import qualified Control.Monad.Error as Error

type ErrorIO = Error.ErrorT String IO
