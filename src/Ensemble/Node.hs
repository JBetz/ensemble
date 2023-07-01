{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Node where

import Clap.Host
import Ensemble.Schema.TH
import Sound.PortMidi (PMStream)

newtype NodeId = NodeId { nodeId_id :: Int }
    deriving (Show, Ord, Eq)

data Node 
    = MidiDeviceNode PMStream
    | PluginNode PluginId Plugin

deriveJSON ''NodeId