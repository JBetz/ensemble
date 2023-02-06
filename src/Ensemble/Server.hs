module Ensemble.Server where

import Clap.Interface.Host
import Ensemble.Config
import Ensemble.Engine
import Ensemble.Sequencer

data Server = Server 
  { server_config :: Config
  , server_sequencer :: Sequencer
  , server_engine :: Engine
  }

createServer :: Config -> IO Server
createServer config = do
  sequencer <- createSequencer
  engine <- createEngine defaultHostConfig
  pure $ Server
    { server_config = config
    , server_sequencer = sequencer
    , server_engine = engine
    }