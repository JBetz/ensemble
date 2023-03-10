module Ensemble.Type where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Writer
import Data.Aeson (Value)
import Data.Aeson.KeyMap (KeyMap)
import Ensemble.Error
import Ensemble.Server

type Ensemble = Eff '[Reader Server, Writer (KeyMap Value), Writer String, Error ApiError, IO]
