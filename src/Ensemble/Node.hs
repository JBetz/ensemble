{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Node where

import Clap.Host
import Ensemble.Schema.TH

newtype NodeId = NodeId { nodeId_id :: Int }
    deriving (Show, Ord, Eq)

data Node = PluginNode PluginId Plugin

deriveJSON ''NodeId