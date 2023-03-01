module Ensemble.Effects where

import Control.Concurrent.Chan
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Writer
import Data.Aeson (Value(..))
import Data.Aeson.KeyMap (KeyMap)
import Ensemble.Config
import Ensemble.Error
import Ensemble.Server
import Ensemble.Type

runEnsemble :: Server -> Ensemble a -> IO (Either ApiError a)
runEnsemble server action = runM $ runError $ runLogWriter $ runMessageWriter $ runReader server $ action
    where
        config = server_config server

        runLogWriter :: LastMember IO effs => Eff (Writer String : effs) result -> Eff effs result
        runLogWriter = interpret $ \case
            Tell message -> 
                case logFile config of
                    Just filePath -> sendM $ appendFile filePath message
                    Nothing -> case interface config of
                        Interface_Http -> sendM $ putStrLn message
                        Interface_Pipes -> pure ()
                        Interface_WebSocket -> pure ()
        
        runMessageWriter :: LastMember IO effs => Eff (Writer (KeyMap Value) : effs) result -> Eff effs result
        runMessageWriter = interpret $ \case
            Tell message -> sendM $ writeChan (server_messageChannel server) (Object message)