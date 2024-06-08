{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Node where

import Clap.Host
import Data.IORef
import Ensemble.Schema.TH
import Sound.PortMidi (PMStream)

newtype NodeId = NodeId {nodeId_id :: Int}
  deriving (Show, Ord, Eq)

newtype DeviceId = DeviceId {deviceId_id :: Int}
  deriving (Show, Ord, Eq)

data Node
  = Node_MidiDevice MidiDeviceNode
  | Node_Plugin PluginNode

data MidiDeviceNode = MidiDeviceNode
  { midiDeviceNode_deviceId :: DeviceId,
    midiDeviceNode_latency :: Int,
    midiDeviceNode_startTime :: IORef (Maybe Int),
    midiDeviceNode_stream :: PMStream
  }

data PluginNode = PluginNode
  { pluginNode_id :: PluginId,
    pluginNode_plugin :: Plugin
  }

deriveJSON ''NodeId
