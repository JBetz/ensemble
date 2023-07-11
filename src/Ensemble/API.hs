{-# LANGUAGE TemplateHaskell #-}
module Ensemble.API where

import qualified Clap as Clap
import qualified Clap.Host as Clap
import Clap.Library (PluginInfo (..))
import qualified Clap.Interface.Extension.Gui as Gui
import Clap.Interface.Extension.Params (ParameterInfo(..))
import qualified Clap.Interface.Extension.Params as Params
import Clap.Interface.Id (ClapId (..))
import qualified Clap.Library as Clap
import Control.Concurrent
import Control.Monad (unless, void)
import Control.Monad.Extra (whenJust)
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Data.IORef
import Data.Maybe
import Data.Text (Text, unpack, pack)
import Ensemble.Engine (AudioDevice, MidiDevice, AudioOutput)
import qualified Ensemble.Engine as Engine
import Ensemble.Node
import Ensemble.Effects
import Ensemble.Event (SequencerEvent(..), PlaybackEvent(..))
import Ensemble.Schema.TH
import qualified Ensemble.Sequencer as Sequencer
import Ensemble.Server
import Ensemble.Tick
import Ensemble.Type
import Foreign.Ptr

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

createMidiDeviceNode :: Argument "deviceId" Int -> Ensemble NodeId
createMidiDeviceNode (Argument deviceId) = do
    engine <- asks server_engine
    Engine.createMidiDeviceNode engine deviceId

deleteNode :: Argument "nodeId" NodeId -> Ensemble Ok
deleteNode (Argument nodeId) = do
    engine <- asks server_engine
    sendM $ Engine.deleteNode engine nodeId
    pure Ok 

-- CLAP
getPluginLocations :: Ensemble [Text]
getPluginLocations = 
    sendM $ fmap pack <$> Clap.pluginLibraryPaths 

scanForPlugins :: Argument "filePaths" [Text] -> Ensemble [PluginInfo]
scanForPlugins (Argument filePaths) =
    sendM $ Clap.scanForPluginsIn $ unpack <$> filePaths
    
createPluginNode :: Argument "filePath" Text -> Argument "pluginIndex" Int -> Ensemble NodeId
createPluginNode (Argument filePath) (Argument pluginIndex) = do
    engine <- asks server_engine
    Engine.createPluginNode engine $ Clap.PluginLocation (unpack filePath) pluginIndex

data Size = Size
    { size_width :: Int
    , size_height :: Int 
    } deriving (Show)

createEmbeddedWindow :: Argument "nodeId" NodeId -> Argument "parentWindow" Int -> Argument "scale" Double -> Argument "size" Size -> Ensemble Size
createEmbeddedWindow (Argument nodeId) (Argument parentWindow) (Argument scale) (Argument size) = do
    engine <- asks server_engine
    maybeNode <- sendM $ Engine.lookupNode engine nodeId
    case maybeNode of
        Just (Node_Plugin pluginNode) -> do
            let plugin = pluginNode_plugin pluginNode
            let pluginHandle = Clap.plugin_handle plugin
            case Clap.pluginExtensions_gui (Clap.plugin_extensions plugin) of
                Just pluginGuiHandle -> do
                    createResult <- sendM $ Gui.createEmbedded pluginGuiHandle pluginHandle Gui.Win32
                    unless createResult $ Engine.throwApiError "Error creating plugin GUI"    
                    _setScaleResult <- sendM $ Gui.setScale pluginGuiHandle pluginHandle scale
                    canResize <- sendM $ Gui.canResize pluginGuiHandle pluginHandle
                    actualSize <- if canResize 
                        then do
                            setSizeResult <- sendM $ Gui.setSize pluginGuiHandle pluginHandle (size_width size) (size_height size)
                            unless setSizeResult $ Engine.throwApiError "Error setting size of plugin GUI"
                            pure size
                        else do
                            maybeSize <- sendM $ Gui.getSize pluginGuiHandle pluginHandle
                            case maybeSize of
                                Just (width, height) -> pure $ Size width height
                                Nothing -> Engine.throwApiError "Error setting size of plugin GUI"
                    windowHandle <- sendM $ Gui.createWindow Gui.Win32 (intPtrToPtr $ IntPtr parentWindow)
                    setParentResult <- sendM $ Gui.setParent pluginGuiHandle pluginHandle windowHandle
                    unless setParentResult $ Engine.throwApiError "Error setting parent window of plugin GUI"
                    showResult <- sendM $ Gui.show pluginGuiHandle pluginHandle
                    unless showResult $ Engine.throwApiError "Error showing plugin GUI"
                    pure actualSize
                Nothing -> Engine.throwApiError "Plugin does not support GUI extension"
        Just _ -> Engine.throwApiError "Invalid node type"
        Nothing -> Engine.throwApiError $ "Node " <> show (nodeId_id nodeId) <> " not found"
    
createFloatingWindow :: Argument "nodeId" NodeId -> Argument "transientWindow" Int -> Argument "title" Text -> Ensemble Ok
createFloatingWindow (Argument nodeId) (Argument transientWindow) (Argument title) = do
    engine <- asks server_engine
    maybeNode <- sendM $ Engine.lookupNode engine nodeId
    case maybeNode of
        Just (Node_Plugin pluginNode) -> do 
            let plugin = pluginNode_plugin pluginNode
            let pluginHandle = Clap.plugin_handle plugin
            case Clap.pluginExtensions_gui (Clap.plugin_extensions plugin) of
                Just pluginGuiHandle -> do 
                    createResult <- sendM $ Gui.createFloating pluginGuiHandle pluginHandle
                    unless createResult $ Engine.throwApiError "Error creating plugin GUI"
                    windowHandle <- sendM $ Gui.createWindow Gui.Win32 (intPtrToPtr $ IntPtr transientWindow)
                    setTransientResult <- sendM $ Gui.setTransient pluginGuiHandle pluginHandle windowHandle
                    unless setTransientResult $ sendM $ putStrLn "Error setting transient window of plugin GUI"
                    _ <- sendM $ Gui.suggestTitle pluginGuiHandle pluginHandle (unpack title)
                    showResult <- sendM $ Gui.show pluginGuiHandle pluginHandle
                    unless showResult $ Engine.throwApiError "Error showing plugin GUI"
                    pure Ok
                Nothing -> Engine.throwApiError "Plugin does not support GUI extension"
        Just _ -> Engine.throwApiError "Invalid node type"
        Nothing -> Engine.throwApiError $ "Node " <> show (nodeId_id nodeId) <> " not found"


getPluginParameters :: Argument "nodeId" NodeId -> Ensemble [ParameterInfo]
getPluginParameters  (Argument nodeId) = do
    engine <- asks server_engine
    maybeNode <- sendM $ Engine.lookupNode engine nodeId
    case maybeNode of
        Just (Node_Plugin pluginNode) -> do 
            let plugin = pluginNode_plugin pluginNode
            let pluginHandle = Clap.plugin_handle plugin
            case Clap.pluginExtensions_params (Clap.plugin_extensions plugin) of
                Just pluginParamsHandle -> sendM $ do 
                    count <- Params.count pluginParamsHandle pluginHandle
                    parameterInfos <- traverse (Params.getInfo pluginParamsHandle pluginHandle) [0 .. count - 1]
                    pure $ catMaybes parameterInfos
                Nothing -> Engine.throwApiError "Plugin does not support params extension"
        Just _ -> Engine.throwApiError "Invalid node type"
        Nothing -> Engine.throwApiError $ "Node " <> show (nodeId_id nodeId) <> " not found"

getPluginParameterValue :: Argument "nodeId" NodeId -> Argument "parameterId" Int -> Ensemble (Maybe Double)
getPluginParameterValue  (Argument nodeId) (Argument parameterId) = do
    engine <- asks server_engine
    maybeNode <- sendM $ Engine.lookupNode engine nodeId
    case maybeNode of
        Just (Node_Plugin pluginNode) -> do 
            let plugin = pluginNode_plugin pluginNode
            let pluginHandle = Clap.plugin_handle plugin
            case Clap.pluginExtensions_params (Clap.plugin_extensions plugin) of
                Just pluginParamsHandle -> sendM $ Params.getValue pluginParamsHandle pluginHandle (ClapId parameterId)
                Nothing -> Engine.throwApiError "Plugin does not support params extension"
        Just _ -> Engine.throwApiError "Invalid node type"
        Nothing -> Engine.throwApiError $ "Node " <> show (nodeId_id nodeId) <> " not found"

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
        maybeThreadId <- readIORef (Engine.engine_playbackThread engine)
        unless (isJust maybeThreadId) $ do
            threadId <- forkFinally 
                (void $ runEnsemble server $ Sequencer.playSequenceRealtime sequencer engine startTick maybeEndTick loop)
                (\_ -> writeIORef (Engine.engine_playbackThread engine) Nothing)
            writeIORef (Engine.engine_playbackThread engine) (Just threadId)
    pure Ok

renderSequence :: Argument "startTick" Tick -> Argument "endTick" (Maybe Tick) -> Ensemble AudioOutput
renderSequence (Argument startTick) (Argument maybeEndTick) = do
    sequencer <- asks server_sequencer
    engine <- asks server_engine
    endTick <- case maybeEndTick of
        Just endTick -> pure endTick
        Nothing -> sendM $ Sequencer.getEndTick sequencer
    sendM $ Sequencer.renderSequence sequencer engine startTick endTick
    
clearSequence :: Ensemble Ok
clearSequence = do
    eventQueue <- asks (Sequencer.sequencer_eventQueue . server_sequencer)
    sendM $ writeIORef eventQueue []
    pure Ok

stopPlayback :: Ensemble Ok
stopPlayback = do
    engine <- asks server_engine
    sendM $ do
        maybePlaybackThreadId <- readIORef (Engine.engine_playbackThread engine)
        whenJust maybePlaybackThreadId killThread
        writeIORef (Engine.engine_steadyTime engine) (-1)
    Engine.tellEvent PlaybackEvent_Stopped
    pure Ok

getCurrentTick :: Ensemble Tick
getCurrentTick = do
    engine <- asks server_engine
    sendM $ Engine.getCurrentTick engine

ping :: Ensemble Ok
ping = pure Ok

echo :: Argument "string" Text -> Ensemble Text
echo (Argument string) = pure string

deriveJSONs 
    [ ''Ok
    , ''Size 
    ]
