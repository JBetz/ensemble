module Ensemble.Event where

import qualified Ensemble.Soundfont as Soundfont
import qualified Clap.Host as Clap
import qualified Clap.Interface.Events as Clap

data Event
    = Soundfont 
        { event_soundfontId :: Soundfont.SoundfontId
        , event_soundfontEvent :: Soundfont.SoundfontEvent 
        } 
    | Clap 
        { clapEvent_pluginId :: Clap.PluginId
        , clapEvent_eventConfig :: Clap.ClapEventConfig
        , clapEvent_event :: Clap.ClapEvent
        }
    deriving (Show)
