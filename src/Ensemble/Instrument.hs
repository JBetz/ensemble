{-# LANGUAGE LambdaCase #-}

module Ensemble.Instrument where

import Clap.Host
import Ensemble.Soundfont

newtype InstrumentId = InstrumentId { instrumentId_id :: Int }
    deriving (Show, Ord, Eq)

data Instrument
    = Instrument_Soundfont SoundfontInstrument
    | Instrument_Clap ClapInstrument

isSoundfont :: Instrument -> Bool
isSoundfont = \case
    Instrument_Soundfont _ -> True
    Instrument_Clap _ -> False

isClap :: Instrument -> Bool
isClap = \case
    Instrument_Soundfont _ -> False
    Instrument_Clap _ -> True

data InstrumentInfo = InstrumentInfo
    { instrumentInfo_id :: InstrumentId
    , instrumentInfo_instrument :: Instrument
    }

data SoundfontInstrument = SoundfontInstrument
    { soundfontInstrument_soundfont :: Soundfont
    , soundfontInstrument_synth :: FluidSynth
    }

data ClapInstrument = ClapInstrument
    { clapInstrument_pluginId :: PluginId
    }

