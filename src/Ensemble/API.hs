{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ensemble.API where

import Clap.Host
import Clap.Interface.Plugin
import Clap.Interface.PluginFeatures
import Clap.Interface.Version
import Data.Aeson
import GHC.Generics

data InMessage = InMessage
    { clientId :: Int
    , content :: InMessageContent 
    , extra :: String
    } deriving (Show, Generic, ToJSON, FromJSON)

data InMessageContent
    = In_ClapPluginPaths
    | In_ScanForClapPlugins [FilePath]
    | In_LoadClapPlugin FilePath Int
    deriving (Show, Generic, ToJSON, FromJSON)

data OutMessage = OutMessage
     { clientId :: Int
     , content :: OutMessageContent
     , extra :: String
     } deriving (Show, Generic, ToJSON, FromJSON)

data OutMessageContent 
    = Out_ClapPluginPaths [FilePath]
    | Out_ScanForClapPlugins [PluginDescriptor]
    | Out_LoadClapPlugin
    deriving (Show, Generic, ToJSON, FromJSON)

deriving instance Generic ClapId
deriving instance ToJSON ClapId
deriving instance FromJSON ClapId


deriving instance Generic PluginFeature
deriving instance ToJSON PluginFeature
deriving instance FromJSON PluginFeature

deriving instance Generic ClapVersion
deriving instance ToJSON ClapVersion
deriving instance FromJSON ClapVersion

deriving instance Generic PluginDescriptor
deriving instance ToJSON PluginDescriptor
deriving instance FromJSON PluginDescriptor