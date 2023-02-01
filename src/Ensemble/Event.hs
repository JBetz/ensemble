module Ensemble.Event where

import qualified Ensemble.Soundfont as Soundfont
import qualified Clap.Host as Clap
import qualified Clap.Interface.Events as Clap

data SequencerEvent
    = SequencerEvent_Soundfont 
        { sequencerEvent_soundfontId :: Soundfont.SoundfontId
        , sequencerEvent_soundfontEvent :: Soundfont.SoundfontEvent 
        } 
    | SequencerEvent_Clap 
        { sequencerEvent_clapPluginId :: Clap.PluginId
        , sequencerEvent_clapEventConfig :: Clap.ClapEventConfig
        , sequencerEvent_clapEvent :: Clap.ClapEvent
        }
    deriving (Show)
