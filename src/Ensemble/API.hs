{-# LANGUAGE KindSignatures #-}

module Ensemble.API where

import Clap.Host (PluginId (..))
import qualified Clap.Library as CLAP
import Clap.Interface.Plugin
import Control.Concurrent
import Control.Monad (void)
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Data.IORef
import Data.Text (Text, unpack, pack)
import Ensemble.Effects
import Ensemble.Engine (AudioDevice, AudioOutput)
import qualified Ensemble.Engine as Engine
import Ensemble.Instrument
import Ensemble.Event (SequencerEvent(..))
import Ensemble.Sequencer (Tick(..))
import qualified Ensemble.Sequencer as Sequencer
import Ensemble.Server
import Ensemble.Soundfont (SoundfontPreset)
import GHC.TypeLits

data Ok = Ok

data Argument (name :: Symbol) t = Argument t

-- Audio
getAudioDevices :: Ensemble [AudioDevice]
getAudioDevices = Engine.getAudioDevices

startEngine :: Ensemble Ok
startEngine = do
    engine <- asks server_engine
    Engine.start engine
    pure Ok

stopEngine :: Ensemble Ok
stopEngine = do
    engine <- asks server_engine
    Engine.stop engine
    pure Ok

-- CLAP
getClapPluginLocations :: Ensemble [Text]
getClapPluginLocations = 
    sendM $ fmap pack <$> CLAP.pluginLibraryPaths 

scanForClapPlugins :: Argument "filePaths" [Text] -> Ensemble [PluginDescriptor]
scanForClapPlugins (Argument filePaths) = 
    sendM $ CLAP.scanForPluginsIn $ unpack <$> filePaths

loadClapPlugin :: Argument "filePath" Text -> Argument "pluginIndex" Int -> Ensemble Ok
loadClapPlugin (Argument filePath) (Argument pluginIndex) = do
    engine <- asks server_engine
    sendM $ Engine.loadPlugin engine $ PluginId (unpack filePath) pluginIndex
    pure Ok

-- Soundfont
loadFluidSynthLibrary :: Argument "filePath" Text -> Ensemble Ok
loadFluidSynthLibrary (Argument filePath) = do
   engine <- asks server_engine
   sendM $ Engine.loadFluidSynthLibrary engine (unpack filePath)
   pure Ok

createSoundfontInstrument :: Argument "filePath" Text -> Ensemble InstrumentId
createSoundfontInstrument (Argument filePath) = do
    engine <- asks server_engine
    Engine.createSoundfontInstrument engine (unpack filePath)

selectSoundfontInstrumentPreset :: Argument "instrumentId" InstrumentId  -> Argument "bankNumber" Int -> Argument "programNumber" Int -> Ensemble Ok
selectSoundfontInstrumentPreset (Argument instrumentId) (Argument bank) (Argument program) = do
    engine <- asks server_engine
    Engine.selectSoundfontInstrumentPreset engine instrumentId bank program
    pure Ok

getSoundfontInstrumentPresets :: Argument "instrumentId" InstrumentId -> Ensemble [SoundfontPreset]
getSoundfontInstrumentPresets (Argument instrumentId) = do
    engine <- asks server_engine
    Engine.getSoundfontInstrumentPresets engine instrumentId

-- Sequencer
scheduleEvent :: Argument "tick" Tick -> Argument "sequencerEvent" SequencerEvent -> Ensemble Ok
scheduleEvent (Argument tick) (Argument event) = do
    sequencer <- asks server_sequencer
    sendM $ Sequencer.sendAt sequencer tick event
    pure Ok

playSequence :: Argument "startTick" Tick -> Ensemble Ok
playSequence (Argument startTick) = do
    server <- ask
    sequencer <- asks server_sequencer
    engine <- asks server_engine
    void $ sendM $ forkIO $ void $ runEnsemble server $ Sequencer.playSequence sequencer engine startTick
    pure Ok

renderSequence :: Argument "startTick" Tick -> Argument "endTick" Tick -> Ensemble AudioOutput
renderSequence (Argument startTick) (Argument endTick) = do
    sequencer <- asks server_sequencer
    engine <- asks server_engine
    Sequencer.render sequencer engine startTick endTick

clearSequence :: Ensemble Ok
clearSequence = do
    events <- asks (Sequencer.sequencer_eventQueue . server_sequencer)
    sendM $ writeIORef events []
    pure Ok

playAudio :: Argument "audioOutput" AudioOutput -> Ensemble Ok
playAudio (Argument audioOutput) = do
    engine <- asks server_engine
    Engine.playAudio engine audioOutput
    pure Ok
