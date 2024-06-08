{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Schema where

import qualified Clap.Host as Clap
import qualified Clap.Interface.Events as Clap
import qualified Clap.Interface.Extension.Params as Clap
import qualified Clap.Interface.Plugin as Clap
import qualified Clap.Interface.Version as Clap
import qualified Clap.Library as Clap
import Ensemble.API
import Ensemble.Engine (AudioDevice (..), AudioOutput (..), MidiDevice (..))
import Ensemble.Env
import Ensemble.Error
import Ensemble.Event
import Ensemble.Schema.TH
import Prelude hiding (FilePath)

makeAPI
  -- types
  [ ''ApiError,
    ''AudioDevice,
    ''AudioOutput,
    ''Clap.ClapVersion,
    ''Clap.Event,
    ''Clap.EventConfig,
    ''Clap.EventFlag,
    ''Clap.ParameterInfo,
    ''Clap.PluginDescriptor,
    ''Clap.PluginId,
    ''Clap.PluginInfo,
    ''Clap.MidiData,
    ''Clap.Midi2Data,
    ''Clap.NoteExpression,
    ''Clap.TransportFlag,
    ''MidiDevice,
    ''Ok,
    ''PlaybackEvent,
    ''SequencerEvent,
    ''Size,
    ''WindowInfo
  ]
  -- functions
  [ 'clearSequence,
    'createMidiDeviceNode,
    'createPluginNode,
    'deleteNode,
    'echo,
    'getAudioDevices,
    'getCurrentTick,
    'getMidiDevices,
    'getPluginLocations,
    'getPluginParameters,
    'getPluginParameterValue,
    'openPluginGUI,
    'ping,
    'playSequence,
    'scanForPlugins,
    'sendEvent,
    'scheduleEvent,
    'startEngine,
    'stopEngine,
    'stopPlayback
  ]
