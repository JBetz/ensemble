{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Ensemble.API where

import Clap.Interface.Plugin
import Data.Aeson
import Ensemble.Soundfont
import GHC.Generics

data InMessage = InMessage
    { inMessage_content :: InMessageContent 
    , inMessage_extra :: String
    } deriving (Show, Generic, ToJSON, FromJSON)

data InMessageContent
    = In_ClapPluginPaths
    | In_ScanForClapPlugins [FilePath]
    | In_LoadClapPlugin FilePath Int
    | In_LoadSoundfont FilePath
    deriving (Show, Generic, ToJSON, FromJSON)

data OutMessage = OutMessage
     { outMessage_content :: OutMessageContent
     , outMessage_extra :: String
     } deriving (Show, Generic, ToJSON, FromJSON)

data OutMessageContent 
    = Out_ClapPluginPaths [FilePath]
    | Out_ScanForClapPlugins [PluginDescriptor]
    | Out_LoadClapPlugin
    | Out_LoadSoundfont SoundfontId
    deriving (Show, Generic, ToJSON, FromJSON)