{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Instrument where

import Clap.Host
import Ensemble.Schema.TH

newtype InstrumentId = InstrumentId { instrumentId_id :: Int }
    deriving (Show, Ord, Eq)

data Instrument = Instrument
    { instrument_id :: InstrumentId
    , instrument_pluginId :: PluginId
    } deriving (Show)

deriveJSON ''InstrumentId
