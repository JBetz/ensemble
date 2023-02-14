module Ensemble.Config where

import GHC.Generics
import Options.Generic
import Text.Read

data Interface 
    = Interface_Pipes
    | Interface_Http
    | Interface_WebSocket
    deriving (Generic)

instance Show Interface where
    show Interface_Pipes = "pipes"
    show Interface_Http = "http"
    show Interface_WebSocket = "web-socket"

instance Read Interface where
    readPrec = do
        lexeme <- lexP
        pure $ case lexeme of
          Char char -> readString [char]
          String string -> readString string
          Punc string -> readString string
          Ident string -> readString string
          Symbol string -> readString string
          Number number -> readString $ show number
          EOF -> error $ "Invalid interface argument: " <> show lexeme
        where
            readString = \case       
                "pipes" -> Interface_Pipes
                "http" -> Interface_Http
                "web-socket" -> Interface_WebSocket
                "websocket" -> Interface_WebSocket
                "webSocket" -> Interface_WebSocket
                "ws" -> Interface_WebSocket
                other -> error $ "Invalid interface argument: " <> show other

instance ParseRecord Interface
instance ParseField Interface
instance ParseFields Interface

data Config = Config
    { interface :: Interface 
    , port :: Maybe Int 
    , logFile :: Maybe FilePath
    } deriving (Show, Generic)

instance ParseRecord Config