module Ensemble.API where

import Clap.Host (PluginId (..))
import qualified Clap.Library as CLAP
import Clap.Interface.Plugin
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Writer
import Data.IORef
import qualified Data.Map as Map
import Ensemble.Engine (AudioDevice, AudioOutput)
import qualified Ensemble.Engine as Engine
import Ensemble.Error
import Ensemble.Instrument
import Ensemble.Event (SequencerEvent(..))
import Ensemble.Sequencer (Tick(..))
import qualified Ensemble.Sequencer as Sequencer
import Ensemble.Server
import Ensemble.Soundfont

data Ok = Ok

newtype PluginLocations = PluginLocations { pluginLocations_filePaths :: [FilePath] }
newtype PluginDescriptors = PluginDescriptors { pluginDescriptors_descriptors :: [PluginDescriptor] }
newtype SoundfontPresets = SoundfontPresets { sounfontPresets_presets :: [SoundfontPreset] }
newtype AudioDevices = AudioDevices { audioDevices_audioDevices :: [AudioDevice] }
newtype Instruments = Instruments { instruments_instruments :: [Instrument] }

type FilePaths = [FilePath]
type PluginIndex = Int
type StartTick = Tick
type EndTick = Tick

type Ensemble = Eff '[Reader Server, Writer String, Error APIError, IO]

-- Audio
getAudioDevices :: Ensemble AudioDevices
getAudioDevices = AudioDevices <$> Engine.getAudioDevices

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

getInstruments :: Ensemble Instruments
getInstruments = do
    instrumentsIORef <- asks (Engine.engine_instruments . server_engine)
    sendM $ Instruments . Map.elems <$> readIORef instrumentsIORef
    
-- CLAP
getClapPluginLocations :: Ensemble PluginLocations
getClapPluginLocations = 
    sendM $ PluginLocations <$> CLAP.pluginLibraryPaths 

scanForClapPlugins :: FilePaths -> Ensemble PluginDescriptors
scanForClapPlugins filePaths = 
    sendM $ PluginDescriptors <$> CLAP.scanForPluginsIn filePaths

loadClapPlugin :: FilePath -> PluginIndex -> Ensemble Ok
loadClapPlugin filePath index = do
    engine <- asks server_engine
    sendM $ Engine.loadPlugin engine $ PluginId filePath index
    pure Ok

-- Soundfont
initializeSoundfontPlayer :: FilePath -> Ensemble Ok
initializeSoundfontPlayer filePath = do
   engine <- asks server_engine
   sendM $ Engine.initializeSoundfontPlayer engine filePath
   pure Ok

createSoundfontInstrument :: FilePath -> Ensemble InstrumentInfo
createSoundfontInstrument filePath = do
    engine <- asks server_engine
    Engine.createSoundfontInstrument engine filePath

-- Sequencer
scheduleEvent :: Tick -> SequencerEvent -> Ensemble Ok
scheduleEvent tick event = do
    sequencer <- asks server_sequencer
    sendM $ Sequencer.sendAt sequencer tick event
    pure Ok

playSequence :: StartTick -> Ensemble Ok
playSequence startTick = do
    sequencer <- asks server_sequencer
    engine <- asks server_engine
    Sequencer.playSequence sequencer engine startTick
    pure Ok

renderSequence :: StartTick -> EndTick -> Ensemble AudioOutput
renderSequence startTick endTick = do
    sequencer <- asks server_sequencer
    engine <- asks server_engine
    Sequencer.render sequencer engine startTick endTick

clearSequence :: Ensemble Ok
clearSequence = do
    events <- asks (Sequencer.sequencer_eventQueue . server_sequencer)
    sendM $ writeIORef events []
    pure Ok

playAudio :: AudioOutput -> Ensemble Ok
playAudio audioOutput = do
    engine <- asks server_engine
    Engine.playAudio engine audioOutput
    pure Ok