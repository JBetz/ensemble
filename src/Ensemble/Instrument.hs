{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Instrument where

import Clap.Host
import Ensemble.Soundfont
import Ensemble.Schema.TH

newtype InstrumentId = InstrumentId { instrumentId_id :: Int }
    deriving (Show, Ord, Eq)

data Instrument
    = Instrument_Soundfont SoundfontInstrument
    | Instrument_Clap ClapInstrument
    deriving (Show)

isSoundfont :: Instrument -> Bool
isSoundfont = \case
    Instrument_Soundfont _ -> True
    Instrument_Clap _ -> False

isClap :: Instrument -> Bool
isClap = \case
    Instrument_Soundfont _ -> False
    Instrument_Clap _ -> True

data SoundfontInstrument = SoundfontInstrument
    { soundfontInstrument_soundfont :: Soundfont
    , soundfontInstrument_settings :: FluidSettings
    , soundfontInstrument_synth :: FluidSynth
    } deriving (Show)

data ClapInstrument = ClapInstrument
    { clapInstrument_pluginId :: PluginId
    } deriving (Show)

deriveJSON ''InstrumentId
