{-# LANGUAGE TemplateHaskell #-}
module Ensemble.API where

import Clap.Host (PluginId (..))
import qualified Clap.Library as CLAP
import Clap.Interface.Plugin
import Control.Concurrent
import Control.Monad (void, unless)
import Control.Monad.Extra (whenJust)
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Data.IORef
import Data.Maybe (isJust)
import Data.Text (Text, unpack, pack)
import Ensemble.Effects
import Ensemble.Engine (AudioDevice, AudioOutput, Tick)
import qualified Ensemble.Engine as Engine
import Ensemble.Instrument
import Ensemble.Event (SequencerEvent(..))
import Ensemble.Schema.TH
import qualified Ensemble.Sequencer as Sequencer
import Ensemble.Server
import Ensemble.Soundfont (SoundfontPreset)
import Ensemble.Type

data Ok = Ok

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

deleteInstrument :: Argument "instrumentId" InstrumentId -> Ensemble Ok
deleteInstrument (Argument instrumentId) = do
    engine <- asks server_engine
    Engine.deleteInstrument engine instrumentId
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

playSequence :: Argument "startTick" Tick -> Argument "endTick" (Maybe Tick) -> Argument "loop" Bool -> Ensemble Ok
playSequence (Argument startTick) (Argument maybeEndTick) (Argument loop) = do
    server <- ask
    sequencer <- asks server_sequencer
    engine <- asks server_engine
    void $ sendM $ do
        maybeThreadId <- readIORef (Engine.engine_audioThread engine)
        unless (isJust maybeThreadId) $ do
            threadId <- forkFinally 
                (void $ runEnsemble server $ Sequencer.playSequence sequencer engine startTick maybeEndTick loop)
                (\_ -> writeIORef (Engine.engine_audioThread engine) Nothing)
            writeIORef (Engine.engine_audioThread engine) (Just threadId)
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

stopPlayback :: Ensemble Ok
stopPlayback = do
    engine <- asks server_engine
    sendM $ do
        Engine.stopInstruments engine
        maybeAudioThreadId <- readIORef (Engine.engine_audioThread engine)
        whenJust maybeAudioThreadId killThread
        writeIORef (Engine.engine_steadyTime engine) (-1)
    pure Ok

getCurrentTick :: Ensemble Tick
getCurrentTick = do
    engine <- asks server_engine
    Engine.getCurrentTick engine

ping :: Ensemble Ok
ping = pure Ok

echo :: Argument "string" Text -> Ensemble Text
echo (Argument string) = pure string

deriveJSON ''Ok