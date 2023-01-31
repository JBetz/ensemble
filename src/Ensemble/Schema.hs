{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Schema where

import Clap.Interface.Plugin
import Clap.Interface.Version (ClapVersion (..))
import Ensemble.Soundfont
import Ensemble.Schema.TH

data Ok = Ok

newtype PluginLocations = PluginLocations { filePaths :: [FilePath] }

newtype PluginDescriptors = PluginDescriptors { pluginDescriptors :: [PluginDescriptor] }

deriveJSONs 
    [ ''ClapVersion
    , ''PluginDescriptor
    , ''SoundfontId
    , ''Ok
    , ''PluginLocations
    , ''PluginDescriptors
    ]

generateSchema :: IO ()
generateSchema = pure ()