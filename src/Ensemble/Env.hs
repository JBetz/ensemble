{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ensemble.Env where

import Clap.Interface.Host
import Control.Concurrent
import Control.Monad.Reader
import Data.Aeson (Value)
import Data.IORef
import Ensemble.Config
import Ensemble.Engine
import Ensemble.Sequencer

newtype Ensemble a = Ensemble {unEnsemble :: ReaderT Env IO a}
  deriving newtype (Monad, Applicative, Functor, MonadReader Env, MonadIO)

data Env = Env
  { env_config :: Config,
    env_sequencer :: Sequencer,
    env_engine :: Engine,
    env_messageChannel :: Chan Value,
    env_pluginGuiThreadId :: IORef (Maybe ThreadId)
  }

createEnv :: Config -> IO Env
createEnv config = do
  sequencer <- createSequencer
  engine <- createEngine defaultHostConfig
  messageChannel <- newChan
  pluginGuiThreadId <- newIORef Nothing
  pure $
    Env
      { env_config = config,
        env_sequencer = sequencer,
        env_engine = engine,
        env_messageChannel = messageChannel,
        env_pluginGuiThreadId = pluginGuiThreadId
      }

runEnsemble :: Env -> Ensemble a -> IO a
runEnsemble env action = runReaderT (unEnsemble action) env
