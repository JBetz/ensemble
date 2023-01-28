module Ensemble.Server where

import Clap.Interface.Host
import Ensemble.Engine
import Ensemble.Sequencer

data Server = Server 
  { server_sequencer :: Sequencer
  , server_engine :: Engine
  }

createServer :: IO Server
createServer = do
  sequencer <- createSequencer
  engine <- createEngine defaultHostConfig
  pure $ Server
    { server_sequencer = sequencer
    , server_engine = engine
    }