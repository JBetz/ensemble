{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Schema where

import Clap.Interface.Plugin
import Clap.Interface.Version (ClapVersion (..))
import Ensemble.Soundfont (SoundfontId(..))
import Ensemble.API
import Ensemble.Schema.TH

makeGenerateSchema
    -- types
    [ ''ClapVersion
    , ''PluginDescriptor
    , ''SoundfontId
    , ''Ok
    , ''PluginLocations
    , ''PluginDescriptors
    ]
    -- functions
    [ 'getClapPluginLocations
    , 'scanForClapPlugins
    , 'loadClapPlugin
    , 'initializeSoundfontPlayer
    , 'loadSoundfont
    ]