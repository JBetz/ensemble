{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Schema where

import Prelude hiding (FilePath)

import qualified Clap.Host as Clap
import qualified Clap.Interface.Events as Clap
import qualified Clap.Interface.Plugin as Clap
import qualified Clap.Interface.Version as Clap
import Ensemble.API
import Ensemble.Engine (AudioDevice(..), AudioOutput(..))
import Ensemble.Error
import Ensemble.Event
import Ensemble.Schema.TH
import Ensemble.Type

makeAPI
    -- types
    [ ''Ok
    , ''ApiError
    , ''AudioDevice
    , ''AudioOutput
    , ''Clap.PluginId
    , ''Clap.ClapVersion
    , ''Clap.PluginDescriptor
    , ''Clap.EventFlag
    , ''Clap.NoteExpression
    , ''Clap.TransportFlag
    , ''Clap.MidiData
    , ''Clap.Midi2Data
    , ''Clap.ClapEventConfig
    , ''Clap.ClapEvent
    , ''PlaybackEvent
    , ''SequencerEvent
    ]
    -- functions
    [ 'getAudioDevices
    , 'startEngine
    , 'stopEngine
    , 'activateEngine
    , 'deactivateEngine
    , 'deleteInstrument
    , 'getPluginLocations
    , 'scanForPlugins
    , 'loadPlugin
    , 'scheduleEvent
    , 'sendEvents
    , 'playSequence
    , 'renderSequence
    , 'clearSequence
    , 'getCurrentTick
    , 'ping
    , 'echo
    ]