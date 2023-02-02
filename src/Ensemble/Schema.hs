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
import Ensemble.Event (SequencerEvent(..))
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
    , ''SequencerEvent
    ]

instance ToJSON Soundfont.SoundfontEvent where 
    toJSON = \case
        Soundfont.SoundfontEvent_NoteOn event -> defaultToTaggedJSON "SoundfontEventNoteOn" event
        Soundfont.SoundfontEvent_NoteOff event -> defaultToTaggedJSON "SoundfontEventNoteOff" event
        
instance FromJSON Soundfont.SoundfontEvent where
    parseJSON = withObject "SoundfontEvent" $ \v -> do
        eventType <- v.: "@type"
        case toString eventType of
            "SoundfontEventNoteOn" -> Soundfont.SoundfontEvent_NoteOn <$> parseJSON (Object v)
            "SoundfontEventNoteOff" -> Soundfont.SoundfontEvent_NoteOff <$> parseJSON (Object v)
            other -> parseFail $ "Invalid SoundfontEvent type: " <> other     

instance ToJSON Clap.ClapEvent where 
    toJSON = \case
        Clap.ClapEvent_NoteOn event -> defaultToTaggedJSON "ClapEventNoteOn" event
        Clap.ClapEvent_NoteOff event -> defaultToTaggedJSON "ClapEventNoteOff" event
        Clap.ClapEvent_NoteChoke event -> defaultToTaggedJSON "ClapEventNoteChoke" event
        Clap.ClapEvent_NoteEnd event -> defaultToTaggedJSON "ClapEventNoteEnd" event
        Clap.ClapEvent_NoteExpression event -> defaultToTaggedJSON "ClapEventNoteExpression" event
        Clap.ClapEvent_ParamValue event -> defaultToTaggedJSON "ClapEventParamValue" event
        Clap.ClapEvent_ParamMod event -> defaultToTaggedJSON "ClapEventParamMod" event
        Clap.ClapEvent_ParamGestureBegin event -> defaultToTaggedJSON "ClapEventParamGestureBegin" event
        Clap.ClapEvent_ParamGestureEnd event -> defaultToTaggedJSON "ClapEventParamGestureEnd" event
        Clap.ClapEvent_Transport event -> defaultToTaggedJSON "ClapEventTransport" event
        Clap.ClapEvent_Midi event -> defaultToTaggedJSON "ClapEventMidi" event
        Clap.ClapEvent_MidiSysex event -> defaultToTaggedJSON "ClapEventMidiSysex" event
        Clap.ClapEvent_Midi2 event -> defaultToTaggedJSON "ClapEventMidi2" event

instance FromJSON Clap.ClapEvent where
    parseJSON = withObject "ClapEvent" $ \v -> do
        eventType <- v.: "@type"
        case toString eventType of
            "ClapEventNoteOn" -> Clap.ClapEvent_NoteOn <$> parseJSON (Object v)
            "ClapEventNoteOff" -> Clap.ClapEvent_NoteOff <$> parseJSON (Object v)
            "ClapEventNoteChoke" -> Clap.ClapEvent_NoteChoke <$> parseJSON (Object v)
            "ClapEventNoteEnd" -> Clap.ClapEvent_NoteEnd <$> parseJSON (Object v)
            "ClapEventNoteExpression" -> Clap.ClapEvent_NoteExpression <$> parseJSON (Object v)
            "ClapEventParamValue" -> Clap.ClapEvent_ParamValue <$> parseJSON (Object v)
            "ClapEventParamMod" -> Clap.ClapEvent_ParamMod <$> parseJSON (Object v)
            "ClapEventParamGestureBegin" -> Clap.ClapEvent_ParamGestureBegin <$> parseJSON (Object v)
            "ClapEventParamGestureEnd" -> Clap.ClapEvent_ParamGestureEnd <$> parseJSON (Object v)
            "ClapEventTransport" -> Clap.ClapEvent_Transport <$> parseJSON (Object v)
            "ClapEventMidi" -> Clap.ClapEvent_Midi <$> parseJSON (Object v)
            "ClapEventMidiSysex" -> Clap.ClapEvent_MidiSysex <$> parseJSON (Object v)
            "ClapEventMidi2" -> Clap.ClapEvent_Midi2 <$> parseJSON (Object v)
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
    , 'getSoundfontPresets
    , 'scheduleEvent
    ]