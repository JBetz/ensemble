module Ensemble.Effects where

import Control.Monad.Reader
import Ensemble.Server
import Ensemble.Type

runEnsemble :: Server -> Ensemble a -> IO a
runEnsemble server action = runReaderT (unEnsemble action) server
