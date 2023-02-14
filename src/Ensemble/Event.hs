{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Event where

import qualified Data.Aeson as A
import Ensemble.Schema.TH
import qualified Clap.Host as Clap
import qualified Clap.Interface.Id as Clap
import qualified Clap.Interface.Events as Clap
import qualified Clap.Interface.Plugin as Clap
import qualified Clap.Interface.Version as Clap
import Data.Text (pack)
import Ensemble.Instrument
import Foreign.Ptr
import Foreign.C.Types

instance A.ToJSON (Ptr a) where 
    toJSON ptr = A.String (pack $ show ptr)

instance A.FromJSON (Ptr a) where
    parseJSON _ = pure nullPtr

data SequencerEvent = SequencerEvent
    { sequencerEvent_instrumentId :: InstrumentId
    , sequencerEvent_eventConfig :: Maybe Clap.ClapEventConfig
    , sequencerEvent_event :: Clap.ClapEvent
    } deriving (Show)

data PlaybackEvent 
    = PlaybackEvent_Rendering
    | PlaybackEvent_Started
    | PlaybackEvent_Stopped
    | PlaybackEvent_Looped
    deriving (Show)

deriveJSONs
    [ ''CFloat
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
    ] 

deriveJSONs
    [ ''SequencerEvent 
    , ''PlaybackEvent
    ]