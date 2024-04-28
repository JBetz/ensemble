module Ensemble.Config where

import GHC.Generics
import Options.Generic

data Config = Config
    { port :: Maybe Int 
    , logFile :: Maybe FilePath
    } deriving (Show, Generic)

defaultConfig :: Config
defaultConfig = Config
    { port = Nothing
    , logFile = Nothing
    }

instance ParseRecord Config
