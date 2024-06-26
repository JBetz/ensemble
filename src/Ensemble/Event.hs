{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ensemble.Event where

import qualified Clap.Host as Clap
import qualified Clap.Interface.Events as Clap
import qualified Clap.Interface.Extension.Params as Clap
import qualified Clap.Interface.Id as Clap
import qualified Clap.Interface.Plugin as Clap
import qualified Clap.Interface.Version as Clap
import qualified Clap.Library as Clap
import qualified Data.Aeson as A
import Data.Text (pack)
import Ensemble.Node
import Ensemble.Schema.TH
import Ensemble.Tick
import Foreign.C.Types
import Foreign.Ptr

instance A.ToJSON (Ptr a) where
  toJSON ptr = A.String (pack $ show ptr)

instance A.FromJSON (Ptr a) where
  parseJSON _ = pure nullPtr

data SequencerEvent = SequencerEvent
  { sequencerEvent_nodeId :: NodeId,
    sequencerEvent_eventConfig :: Maybe Clap.EventConfig,
    sequencerEvent_event :: Clap.Event
  }
  deriving (Show)

data PlaybackEvent
  = PlaybackEvent_Rendering
  | PlaybackEvent_Started
  | PlaybackEvent_Stopped
  | PlaybackEvent_Looped
  | PlaybackEvent_CurrentTick Tick
  deriving (Show)

deriveJSONs
  [ ''CFloat,
    ''Clap.ClapId,
    ''Clap.ParamId,
    ''Clap.PluginId,
    ''Clap.ClapVersion,
    ''Clap.PluginDescriptor,
    ''Clap.PluginInfo,
    ''Clap.EventFlag,
    ''Clap.NoteEvent,
    ''Clap.NoteKillEvent,
    ''Clap.NoteExpression,
    ''Clap.NoteExpressionEvent,
    ''Clap.ParameterFlag,
    ''Clap.ParameterInfo,
    ''Clap.ParamValueEvent,
    ''Clap.ParamModEvent,
    ''Clap.ParamGestureEvent,
    ''Clap.TransportFlag,
    ''Clap.TransportEvent,
    ''Clap.MidiData,
    ''Clap.MidiEvent,
    ''Clap.MidiSysexEvent,
    ''Clap.Midi2Data,
    ''Clap.Midi2Event,
    ''Clap.EventConfig
  ]

deriveCustomJSONs
  [ ''Clap.Event
  ]

deriveJSONs
  [ ''SequencerEvent,
    ''PlaybackEvent
  ]
