module Ensemble.Event where

import qualified Ensemble.Soundfont as SF
import qualified Clap.Host as CLAP
import qualified Clap.Interface.Events as CLAP

data Event
    = Soundfont SF.SoundfontId SF.Event
    | Clap CLAP.ClapId CLAP.EventConfig CLAP.Event
    deriving (Show)
