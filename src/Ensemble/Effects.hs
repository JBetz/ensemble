module Ensemble.Effects where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Writer
import Ensemble.Config
import Ensemble.Error
import Ensemble.Server

type Ensemble = Eff '[Reader Server, Writer String, Error APIError, IO]

runEnsemble :: Server -> Ensemble a -> IO (Either APIError a)
runEnsemble server action = runM $ runError $ runLogWriter $ runReader server $ action
    where
        runLogWriter :: LastMember IO effs => Eff (Writer String : effs) result -> Eff effs result
        runLogWriter = interpret $ \case
            Tell message -> 
                case logFile config of
                    Just filePath -> sendM $ appendFile filePath message
                    Nothing -> case interface config of
                        Interface_Http -> sendM $ putStrLn message
                        Interface_Pipes -> pure ()
        config = server_config server