module Ensemble.Event where

import qualified Clap.Interface.Events as Clap
import Ensemble.Instrument

data SequencerEvent = SequencerEvent
    { sequencerEvent_instrumentId :: InstrumentId
    , sequencerEvent_eventConfig :: Clap.ClapEventConfig
    , sequencerEvent_event :: Clap.ClapEvent
    } 