{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Schema where

import qualified Clap.Host as Clap
import qualified Clap.Interface.Id as Clap
import qualified Clap.Interface.Events as Clap
import qualified Clap.Interface.Plugin as Clap
import qualified Clap.Interface.Version as Clap
import Data.Aeson
import Ensemble.Event (SequencerEvent(..))
import Ensemble.Soundfont (SoundfontId(..))
import qualified Ensemble.Soundfont as Soundfont
import Ensemble.Sequencer (Tick(..))
import Ensemble.API
import Ensemble.Schema.TH
import Foreign.Ptr 

instance ToJSON a => ToJSON (Ptr a) where 
    toJSON = undefined

instance FromJSON a => FromJSON (Ptr a) where
    parseJSON = undefined

deriveJSONs
    [ ''Clap.ClapId
    , ''Clap.ParamId
    , ''Clap.PluginId
    , ''Clap.ClapVersion
    , ''Clap.PluginDescriptor
    , ''Soundfont.SoundfontId
    , ''Ok
    , ''PluginLocations
    , ''PluginDescriptors
    , ''Tick
    , ''Soundfont.NoteOnEvent
    , ''Soundfont.NoteOffEvent
    , ''Soundfont.SoundfontEvent
    , ''Clap.EventFlag
    , ''Clap.NoteEvent
    , ''Clap.NoteKillEvent
    , ''Clap.NoteExpression
    , ''Clap.NoteExpressionEvent
    , ''Clap.ParamValueEvent
    , ''Clap.ParamModEvent
    , ''Clap.ParamGestureEvent
    , ''Clap.TransportFlag
    , ''Clap.TransportEvent
    , ''Clap.MidiData
    , ''Clap.MidiEvent
    , ''Clap.MidiSysexEvent
    , ''Clap.Midi2Data
    , ''Clap.Midi2Event
    , ''Clap.ClapEventConfig
    , ''Clap.ClapEvent
    , ''SequencerEvent
    ]

makeGenerateSchema
    -- types
    [ ''Clap.ClapId
    , ''Clap.ParamId
    , ''Clap.PluginId
    , ''Clap.ClapVersion
    , ''Clap.PluginDescriptor
    , ''Soundfont.SoundfontId
    , ''Ok
    , ''PluginLocations
    , ''PluginDescriptors
    , ''Tick
    , ''Soundfont.SoundfontEvent
    , ''Clap.EventFlag
    , ''Clap.NoteExpression
    , ''Clap.TransportFlag
    , ''Clap.MidiData
    , ''Clap.Midi2Data
    , ''Clap.ClapEventConfig
    , ''Clap.ClapEvent
    , ''SequencerEvent
    ]
    -- functions
    [ 'getClapPluginLocations
    , 'scanForClapPlugins
    , 'loadClapPlugin
    , 'initializeSoundfontPlayer
    , 'loadSoundfont
    , 'scheduleEvent
    ]