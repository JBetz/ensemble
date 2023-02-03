{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Ensemble.Schema where

import qualified Clap.Host as Clap
import qualified Clap.Interface.Id as Clap
import qualified Clap.Interface.Events as Clap
import qualified Clap.Interface.Plugin as Clap
import qualified Clap.Interface.Version as Clap
import qualified Data.Aeson as A
import Data.Text (pack)
import Ensemble.Engine (AudioDevice(..), AudioOutput(..))
import Ensemble.Event
import Ensemble.Soundfont (SoundfontId(..))
import qualified Ensemble.Soundfont as Soundfont
import Ensemble.Sequencer (Tick(..))
import Ensemble.API
import Ensemble.Schema.TH
import Foreign.C.Types
import Foreign.Ptr

instance A.ToJSON (Ptr a) where 
    toJSON ptr = A.String (pack $ show ptr)

instance A.FromJSON (Ptr a) where
    parseJSON _ = pure nullPtr

deriveJSONs
    [ ''Ok
    , ''EnsembleError
    , ''PluginLocations
    , ''PluginDescriptors
    , ''SoundfontPresets
    , ''AudioDevice
    , ''AudioDevices
    , ''CFloat
    , ''AudioOutput
    , ''Tick
    , ''Soundfont.SoundfontId
    , ''Soundfont.SoundfontPreset
    , ''Soundfont.NoteOnEvent
    , ''Soundfont.NoteOffEvent
    , ''Clap.ClapId
    , ''Clap.ParamId
    , ''Clap.PluginId
    , ''Clap.ClapVersion
    , ''Clap.PluginDescriptor
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
    ]

deriveCustomJSONs
    [ ''Soundfont.SoundfontEvent
    , ''Clap.ClapEvent
    ] 

deriveJSONs
    [ ''SoundfontEventData
    , ''ClapEventData
    ]

deriveCustomJSONs
    [ ''SequencerEvent
    ] 

makeGenerateSchema
    -- types
    [  ''Ok
    , ''EnsembleError
    , ''PluginLocations
    , ''PluginDescriptors
    , ''SoundfontPresets
    , ''Tick
    , ''AudioDevice
    , ''AudioDevices
    , ''AudioOutput
    , ''Clap.ClapId
    , ''Clap.ParamId
    , ''Clap.PluginId
    , ''Clap.ClapVersion
    , ''Clap.PluginDescriptor
    , ''Soundfont.SoundfontId
    , ''Soundfont.SoundfontPreset
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
    [ 'getAudioDevices
    , 'startEngine
    , 'stopEngine
    , 'getClapPluginLocations
    , 'scanForClapPlugins
    , 'loadClapPlugin
    , 'initializeSoundfontPlayer
    , 'loadSoundfont
    , 'getSoundfontPresets
    , 'scheduleEvent
    , 'playSequence
    , 'renderSequence
    , 'clearSequence
    , 'playAudio
    ]