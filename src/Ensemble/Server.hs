module Ensemble.Server where

import Clap.Interface.Host
import Control.Concurrent.Chan
import Data.Aeson (Value)
import Ensemble.Config
import Ensemble.Engine
import Ensemble.Sequencer

data Server = Server 
  { server_config :: Config
  , server_sequencer :: Sequencer
  , server_engine :: Engine
  , server_messageChannel :: Chan Value
  }

createServer :: Config -> IO Server
createServer config = do
  sequencer <- createSequencer
  engine <- createEngine defaultHostConfig
  messageChannel <- newChan
  pure $ Server
    { server_config = config
    , server_sequencer = sequencer
    , server_engine = engine
    , server_messageChannel = messageChannel
    }