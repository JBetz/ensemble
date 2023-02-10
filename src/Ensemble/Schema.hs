{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Schema where

import Prelude hiding (FilePath)

import qualified Clap.Host as Clap
import qualified Clap.Interface.Id as Clap
import qualified Clap.Interface.Events as Clap
import qualified Clap.Interface.Plugin as Clap
import qualified Clap.Interface.Version as Clap
import qualified Data.Aeson as A
import Data.Text (pack)
import Ensemble.Engine (AudioDevice(..), AudioOutput(..))
import Ensemble.Error
import Ensemble.Event
import Ensemble.Instrument
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
    , ''APIError
    , ''AudioDevice
    , ''CFloat
    , ''AudioOutput
    , ''Tick
    , ''InstrumentId
    , ''Soundfont.SoundfontId
    , ''Soundfont.Soundfont
    , ''Soundfont.SoundfontPreset
    , ''SoundfontInstrument
    , ''ClapInstrument
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
    [ ''Clap.ClapEvent
    , ''Instrument
    ] 

deriveJSONs
    [ ''SequencerEvent 
    , ''InstrumentInfo
    ]

makeAPI
    -- types
    [ ''Ok
    , ''APIError
    , ''AudioDevice
    , ''AudioOutput
    , ''Instrument
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
    , ''Soundfont.Soundfont
    , ''Soundfont.SoundfontPreset
    , ''SequencerEvent
    ]
    -- functions
    [ 'getAudioDevices
    , 'startEngine
    , 'stopEngine
    , 'getInstruments
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
    ]