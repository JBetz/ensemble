{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Schema where

import Prelude hiding (FilePath)

import qualified Clap.Host as Clap
import qualified Clap.Interface.Events as Clap
import qualified Clap.Interface.Plugin as Clap
import qualified Clap.Interface.Version as Clap
import qualified Clap.Library as Clap
import Ensemble.API
import Ensemble.Engine (AudioDevice(..), AudioOutput(..), MidiDevice(..))
import Ensemble.Error
import Ensemble.Event
import Ensemble.Schema.TH
import Ensemble.Type

makeAPI
    -- types
    [ ''Ok
    , ''Size
    , ''ApiError
    , ''AudioDevice
    , ''AudioOutput
    , ''MidiDevice
    , ''Clap.PluginId
    , ''Clap.ClapVersion
    , ''Clap.PluginDescriptor
    , ''Clap.PluginInfo
    , ''Clap.EventFlag
    , ''Clap.NoteExpression
    , ''Clap.TransportFlag
    , ''Clap.MidiData
    , ''Clap.Midi2Data
    , ''Clap.EventConfig
    , ''Clap.Event
    , ''PlaybackEvent
    , ''SequencerEvent
    ]
    -- functions
    [ 'getAudioDevices
    , 'getMidiDevices
    , 'getPluginLocations
    , 'scanForPlugins
    , 'createEmbeddedWindow
    , 'createFloatingWindow
    , 'startEngine
    , 'stopEngine
    , 'createMidiDeviceNode
    , 'createPluginNode
    , 'deleteNode
    , 'scheduleEvent
    , 'playSequence
    , 'clearSequence
    , 'getCurrentTick
    , 'ping
    , 'echo
    ]