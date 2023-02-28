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
import qualified Ensemble.Soundfont as Soundfont
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
    , ''Soundfont.SoundfontPreset
    , ''SequencerEvent
    ]
    -- functions
    [ 'getAudioDevices
    , 'startEngine
    , 'stopEngine
    , 'deleteInstrument
    , 'getClapPluginLocations
    , 'scanForClapPlugins
    , 'loadClapPlugin
    , 'loadFluidSynthLibrary
    , 'createSoundfontInstrument    
    , 'getSoundfontInstrumentPresets
    , 'selectSoundfontInstrumentPreset
    , 'scheduleEvent
    , 'playSequence
    , 'renderSequence
    , 'clearSequence
    , 'playAudio
    , 'getCurrentTick
    , 'ping
    , 'echo
    ]