module Ensemble.Event where

import qualified Ensemble.Soundfont as Soundfont
import qualified Clap.Host as Clap
import qualified Clap.Interface.Events as Clap

data SoundfontEventData = SoundfontEventData 
    { soundfontEventData_soundfontId :: Soundfont.SoundfontId
    , soundfontEventData_event :: Soundfont.SoundfontEvent 
    } deriving (Show)

data ClapEventData = ClapEventData
    { clapEventData_pluginId :: Clap.PluginId
    , clapEventData_eventConfig :: Clap.ClapEventConfig
    , clapEventData_event :: Clap.ClapEvent
    } deriving (Show)

data SequencerEvent
    = SequencerEvent_Soundfont SoundfontEventData
    | SequencerEvent_Clap ClapEventData
    deriving (Show)
