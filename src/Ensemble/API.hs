module Ensemble.API where

import Prelude hiding (FilePath)

import Clap.Host (PluginId (..))
import qualified Clap.Library as CLAP
import Clap.Interface.Plugin
import Control.Concurrent
import Control.Monad (void)
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Writer
import Data.IORef
import qualified Data.Map as Map
import Ensemble.Config
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

-- Newtypes for generating argument names in TL schema.
-- TODO: Use type-level string instead?
newtype PluginLocations = PluginLocations [String]
newtype PluginDescriptors = PluginDescriptors [PluginDescriptor]
newtype SoundfontPresets = SoundfontPresets [SoundfontPreset]
newtype AudioDevices = AudioDevices [AudioDevice]
newtype Instruments = Instruments [Instrument]
newtype FilePaths = FilePaths [String]
newtype PluginIndex = PluginIndex Int
newtype StartTick = StartTick Tick
newtype EndTick = EndTick Tick
newtype FilePath = FilePath String

type Ensemble = Eff '[Reader Server, Writer String, Error APIError, IO]

runEnsemble :: Server -> Ensemble a -> IO (Either APIError a)
runEnsemble server action = runM $ runError $ runLogWriter $ runReader server $ action
    where
        runLogWriter :: LastMember IO effs => Eff (Writer String : effs) result -> Eff effs result
        runLogWriter = interpret $ \case
            Tell message -> 
                case logFile config of
                    Just filePath -> sendM $ appendFile filePath message
                    Nothing -> case interface config of
                        Interface_Http -> sendM $ putStrLn message
                        Interface_Pipes -> pure ()
        config = server_config server

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
scanForClapPlugins (FilePaths filePaths) = 
    sendM $ PluginDescriptors <$> CLAP.scanForPluginsIn filePaths

loadClapPlugin :: FilePath -> PluginIndex -> Ensemble Ok
loadClapPlugin (FilePath filePath) (PluginIndex index) = do
    engine <- asks server_engine
    sendM $ Engine.loadPlugin engine $ PluginId filePath index
    pure Ok

-- Soundfont
loadFluidSynthLibrary :: FilePath -> Ensemble Ok
loadFluidSynthLibrary (FilePath filePath) = do
   engine <- asks server_engine
   sendM $ Engine.loadFluidSynthLibrary engine filePath
   pure Ok

createSoundfontInstrument :: FilePath -> Ensemble InstrumentInfo
createSoundfontInstrument (FilePath filePath) = do
    engine <- asks server_engine
    Engine.createSoundfontInstrument engine filePath

-- Sequencer
scheduleEvent :: Tick -> SequencerEvent -> Ensemble Ok
scheduleEvent tick event = do
    sequencer <- asks server_sequencer
    sendM $ Sequencer.sendAt sequencer tick event
    pure Ok

playSequence :: StartTick -> Ensemble Ok
playSequence (StartTick startTick) = do
    server <- ask
    sequencer <- asks server_sequencer
    engine <- asks server_engine
    void $ sendM $ forkIO $ void $ runEnsemble server $ Sequencer.playSequence sequencer engine startTick
    pure Ok

renderSequence :: StartTick -> EndTick -> Ensemble AudioOutput
renderSequence (StartTick startTick) (EndTick endTick) = do
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