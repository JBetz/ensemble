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
import Ensemble.Engine (AudioDevice, AudioOutput, Tick, MidiDevice)
import qualified Ensemble.Engine as Engine
import Ensemble.Node
import Ensemble.Event (SequencerEvent(..))
import Ensemble.Schema.TH
import qualified Ensemble.Sequencer as Sequencer
import Ensemble.Server
import Ensemble.Type

data Ok = Ok

-- Audio
getAudioDevices :: Ensemble [AudioDevice]
getAudioDevices = Engine.getAudioDevices

getMidiDevices :: Ensemble [MidiDevice]
getMidiDevices = Engine.getMidiDevices

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

activateEngine :: Ensemble Ok
activateEngine = do
    engine <- asks server_engine 
    Engine.startAudioThread engine
    pure Ok

deactivateEngine :: Ensemble Ok
deactivateEngine = do
    engine <- asks server_engine
    sendM $ do
        maybeAudioThreadId <- readIORef (Engine.engine_audioThread engine)
        whenJust maybeAudioThreadId killThread
        writeIORef (Engine.engine_steadyTime engine) (-1)
    pure Ok

createMidiDeviceNode :: Argument "deviceId" Int -> Ensemble NodeId
createMidiDeviceNode (Argument deviceId) = do
    engine <- asks server_engine
    Engine.createMidiDeviceNode engine deviceId

deleteNode :: Argument "nodeId" NodeId -> Ensemble Ok
deleteNode (Argument nodeId) = do
    engine <- asks server_engine
    Engine.deleteNode engine nodeId
    pure Ok 

-- CLAP
getPluginLocations :: Ensemble [Text]
getPluginLocations = 
    sendM $ fmap pack <$> CLAP.pluginLibraryPaths 

scanForPlugins :: Argument "filePaths" [Text] -> Ensemble [PluginDescriptor]
scanForPlugins (Argument filePaths) = 
    sendM $ CLAP.scanForPluginsIn $ unpack <$> filePaths

loadPlugin :: Argument "filePath" Text -> Argument "pluginIndex" Int -> Ensemble Ok
loadPlugin (Argument filePath) (Argument pluginIndex) = do
    engine <- asks server_engine
    sendM $ Engine.loadPlugin engine $ PluginId (unpack filePath) pluginIndex
    pure Ok

createEmbeddedWindow :: Argument "pluginId" PluginId -> Argument "parentWindow" Text -> Argument "scale" Int -> Argument "canResize" Bool -> Argument "size" (Int, Int) -> Ensemble Ok
createEmbeddedWindow (Argument _pluginId) (Argument _parentWindow) (Argument _scale) (Argument _canResize) (Argument _size) = do
    pure Ok

createFloatingWindow :: Argument "pluginId" PluginId -> Argument "transient" Int -> Argument "title" Text -> Ensemble Ok
createFloatingWindow (Argument _pluginId) (Argument _transient) (Argument _title) = do
    pure Ok

-- Sequencer
scheduleEvent :: Argument "tick" Tick -> Argument "sequencerEvent" SequencerEvent -> Ensemble Ok
scheduleEvent (Argument tick) (Argument event) = do
    sequencer <- asks server_sequencer
    sendM $ Sequencer.sendAt sequencer tick event
    pure Ok

sendEvents :: Argument "sequencerEvents" [SequencerEvent] -> Ensemble Ok
sendEvents (Argument events) = do
    engine <- asks server_engine
    Engine.sendEvents engine events
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

getCurrentTick :: Ensemble Tick
getCurrentTick = do
    engine <- asks server_engine
    Engine.getCurrentTick engine

ping :: Ensemble Ok
ping = pure Ok

echo :: Argument "string" Text -> Ensemble Text
echo (Argument string) = pure string

deriveJSON ''Ok