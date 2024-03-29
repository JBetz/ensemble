module Ensemble.Config where

import Data.Char (toLower)
import GHC.Generics
import Options.Generic
import Text.Read

data Interface 
    = Interface_Http
    | Interface_WebSocket
    | Interface_Library
    deriving (Generic)

instance Show Interface where
    show Interface_Http = "http"
    show Interface_WebSocket = "web-socket"
    show Interface_Library = "library"

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
            readString string = case toLower <$> string of       
                "http" -> Interface_Http
                "web-socket" -> Interface_WebSocket
                "websocket" -> Interface_WebSocket
                "ws" -> Interface_WebSocket
                "library" -> Interface_Library
                other -> error $ "Invalid interface argument: " <> show other

instance ParseRecord Interface
instance ParseField Interface
instance ParseFields Interface

data Config = Config
    { interface :: Interface 
    , port :: Maybe Int 
    , logFile :: Maybe FilePath
    } deriving (Show, Generic)

defaultConfig :: Config
defaultConfig = Config
    { interface = Interface_Http
    , port = Nothing
    , logFile = Nothing
    }

instance ParseRecord Config
