{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Ensemble.Config where

import GHC.Generics
import Options.Generic
import Text.Read

data Interface 
    = Interface_Pipe
    | Interface_Http
    deriving (Generic)

instance Show Interface where
    show Interface_Pipe = "pipe"
    show Interface_Http = "http"

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
                "pipe" -> Interface_Pipe
                "http" -> Interface_Http
                other -> error $ "Invalid interface argument: " <> show other

instance ParseRecord Interface
instance ParseField Interface
instance ParseFields Interface

data Config = Config
    { interface :: Maybe Interface 
    , port :: Maybe Int 
    } deriving (Show, Generic)


instance ParseRecord Config