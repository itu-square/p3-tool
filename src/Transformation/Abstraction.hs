module Transformation.Abstraction where

import qualified Data.Set.Monad as Set
import qualified Transformation.Common as Cmn
import qualified Transformation.Configurations as Cnfg
import qualified Transformation.Formulae as Frm

type Abstraction = Set.Set String -> Set.Set Cnfg.Config -> Frm.Formula -> Cmn.ErrorIO Frm.Formula
