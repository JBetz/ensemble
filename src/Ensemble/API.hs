module Ensemble.API where

import Clap.Interface.Plugin
import Ensemble.Soundfont

data InMessage = InMessage
    { inMessage_content :: InMessageContent 
    , inMessage_extra :: String
    } deriving (Show)

data OutMessage = OutMessage
     { outMessage_content :: OutMessageContent
     , outMessage_extra :: String
     } deriving (Show)

data InMessageContent
    = In_ClapPluginPaths
    | In_ScanForClapPlugins [FilePath]
    | In_LoadClapPlugin FilePath Int
    | In_InitializeSoundfontPlayer FilePath
    | In_LoadSoundfont FilePath
    deriving (Show)

data OutMessageContent 
    = Out_ClapPluginPathsResponse [FilePath]
    | Out_ScanForClapPluginsResponse [PluginDescriptor]
    | Out_LoadClapPluginResponse
    | Out_InitializeSoundfontPlayerResponse
    | Out_LoadSoundfontResponse SoundfontId
    deriving (Show)
