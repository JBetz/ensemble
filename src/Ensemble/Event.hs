module Ensemble.Event where

import qualified Ensemble.Soundfont as Soundfont
import qualified Clap.Host as Clap
import qualified Clap.Interface.Events as Clap

data SoundfontEvent = SoundfontEvent
    { soundfontEvent_soundfontId :: Soundfont.SoundfontId
    , soundfontEvent_event :: Soundfont.Event 
    } deriving (Show)

data ClapEvent = ClapEvent
    { clapEvent_pluginId :: Clap.PluginId
    , clapEvent_eventConfig :: Clap.EventConfig
    , clapEvent_event :: Clap.Event
    } deriving (Show)

data Event
    = Soundfont SoundfontEvent
    | Clap ClapEvent
    deriving (Show)
