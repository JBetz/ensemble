module Ensemble.Event where

import qualified Ensemble.Soundfont as Soundfont
import qualified Clap.Host as Clap
import qualified Clap.Interface.Events as Clap

data Event
    = Event_Soundfont 
        { event_soundfontId :: Soundfont.SoundfontId
        , event_soundfontEvent :: Soundfont.SoundfontEvent 
        } 
    | Event_Clap 
        { event_clapPluginId :: Clap.PluginId
        , event_clapEventConfig :: Clap.ClapEventConfig
        , event_clapEvent :: Clap.ClapEvent
        }
    deriving (Show)
