{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Schema where

import qualified Clap.Host as Clap
import qualified Clap.Interface.Id as Clap
import qualified Clap.Interface.Events as Clap
import qualified Clap.Interface.Plugin as Clap
import qualified Clap.Interface.Version as Clap
import Data.Aeson
import Data.Aeson.Key (toString)
import Data.Aeson.Types (parseFail)
import Data.Text (pack)
import Ensemble.Event
import Ensemble.Soundfont (SoundfontId(..))
import qualified Ensemble.Soundfont as Soundfont
import Ensemble.Sequencer (Tick(..))
import Ensemble.API
import Ensemble.Schema.TaggedJSON
import Ensemble.Schema.TH
import Foreign.Ptr

instance ToJSON (Ptr a) where 
    toJSON ptr = String (pack $ show ptr)

instance FromJSON (Ptr a) where
    parseJSON _ = pure nullPtr

deriveJSONs
    [ ''Clap.ClapId
    , ''Clap.ParamId
    , ''Clap.PluginId
    , ''Clap.ClapVersion
    , ''Clap.PluginDescriptor
    , ''Soundfont.SoundfontId
    , ''Soundfont.SoundfontPreset
    , ''Ok
    , ''EnsembleError
    , ''PluginLocations
    , ''PluginDescriptors
    , ''SoundfontPresets
    , ''Tick
    , ''Soundfont.NoteOnEvent
    , ''Soundfont.NoteOffEvent
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
    , ''SoundfontEventData
    , ''ClapEventData
    ]

instance ToJSON SequencerEvent where 
    toJSON = \case
        SequencerEvent_Soundfont event -> defaultToTaggedJSON "sequencerEventSoundfont" event
        SequencerEvent_Clap event -> defaultToTaggedJSON "sequencerEventClap" event
        
instance FromJSON SequencerEvent where
    parseJSON = withObject "SequencerEvent" $ \v -> do
        eventType <- v.: "@type"
        case toString eventType of
            "sequencerEventSoundfont" -> SequencerEvent_Soundfont <$> parseJSON (Object v)
            "sequencerEventClap" -> SequencerEvent_Clap <$> parseJSON (Object v)
            other -> parseFail $ "Invalid SequencerEvent type: " <> other     

instance ToJSON Soundfont.SoundfontEvent where 
    toJSON = \case
        Soundfont.SoundfontEvent_NoteOn event -> defaultToTaggedJSON "soundfontEventNoteOn" event
        Soundfont.SoundfontEvent_NoteOff event -> defaultToTaggedJSON "soundfontEventNoteOff" event
        
instance FromJSON Soundfont.SoundfontEvent where
    parseJSON = withObject "SoundfontEvent" $ \v -> do
        eventType <- v.: "@type"
        case toString eventType of
            "soundfontEventNoteOn" -> Soundfont.SoundfontEvent_NoteOn <$> parseJSON (Object v)
            "soundfontEventNoteOff" -> Soundfont.SoundfontEvent_NoteOff <$> parseJSON (Object v)
            other -> parseFail $ "Invalid SoundfontEvent type: " <> other     

instance ToJSON Clap.ClapEvent where 
    toJSON = \case
        Clap.ClapEvent_NoteOn event -> defaultToTaggedJSON "clapEventNoteOn" event
        Clap.ClapEvent_NoteOff event -> defaultToTaggedJSON "clapEventNoteOff" event
        Clap.ClapEvent_NoteChoke event -> defaultToTaggedJSON "clapEventNoteChoke" event
        Clap.ClapEvent_NoteEnd event -> defaultToTaggedJSON "clapEventNoteEnd" event
        Clap.ClapEvent_NoteExpression event -> defaultToTaggedJSON "clapEventNoteExpression" event
        Clap.ClapEvent_ParamValue event -> defaultToTaggedJSON "clapEventParamValue" event
        Clap.ClapEvent_ParamMod event -> defaultToTaggedJSON "clapEventParamMod" event
        Clap.ClapEvent_ParamGestureBegin event -> defaultToTaggedJSON "clapEventParamGestureBegin" event
        Clap.ClapEvent_ParamGestureEnd event -> defaultToTaggedJSON "clapEventParamGestureEnd" event
        Clap.ClapEvent_Transport event -> defaultToTaggedJSON "clapEventTransport" event
        Clap.ClapEvent_Midi event -> defaultToTaggedJSON "clapEventMidi" event
        Clap.ClapEvent_MidiSysex event -> defaultToTaggedJSON "clapEventMidiSysex" event
        Clap.ClapEvent_Midi2 event -> defaultToTaggedJSON "clapEventMidi2" event

instance FromJSON Clap.ClapEvent where
    parseJSON = withObject "ClapEvent" $ \v -> do
        eventType <- v.: "@type"
        case toString eventType of
            "clapEventNoteOn" -> Clap.ClapEvent_NoteOn <$> parseJSON (Object v)
            "clapEventNoteOff" -> Clap.ClapEvent_NoteOff <$> parseJSON (Object v)
            "clapEventNoteChoke" -> Clap.ClapEvent_NoteChoke <$> parseJSON (Object v)
            "clapEventNoteEnd" -> Clap.ClapEvent_NoteEnd <$> parseJSON (Object v)
            "clapEventNoteExpression" -> Clap.ClapEvent_NoteExpression <$> parseJSON (Object v)
            "clapEventParamValue" -> Clap.ClapEvent_ParamValue <$> parseJSON (Object v)
            "clapEventParamMod" -> Clap.ClapEvent_ParamMod <$> parseJSON (Object v)
            "clapEventParamGestureBegin" -> Clap.ClapEvent_ParamGestureBegin <$> parseJSON (Object v)
            "clapEventParamGestureEnd" -> Clap.ClapEvent_ParamGestureEnd <$> parseJSON (Object v)
            "clapEventTransport" -> Clap.ClapEvent_Transport <$> parseJSON (Object v)
            "clapEventMidi" -> Clap.ClapEvent_Midi <$> parseJSON (Object v)
            "clapEventMidiSysex" -> Clap.ClapEvent_MidiSysex <$> parseJSON (Object v)
            "clapEventMidi2" -> Clap.ClapEvent_Midi2 <$> parseJSON (Object v)
            other -> parseFail $ "Invalid ClapEvent type: " <> other                


makeGenerateSchema
    -- types
    [ ''Clap.ClapId
    , ''Clap.ParamId
    , ''Clap.PluginId
    , ''Clap.ClapVersion
    , ''Clap.PluginDescriptor
    , ''Soundfont.SoundfontId
    , ''Soundfont.SoundfontPreset
    , ''Ok
    , ''EnsembleError
    , ''PluginLocations
    , ''PluginDescriptors
    , ''SoundfontPresets
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
    , 'getSoundfontPresets
    , 'scheduleEvent
    ]