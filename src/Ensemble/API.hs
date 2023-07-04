{-# LANGUAGE TemplateHaskell #-}
module Ensemble.API where

import qualified Clap as Clap
import Clap.Host (PluginId (..))
import qualified Clap.Host as Clap
import Clap.Library (PluginInfo (..))
import qualified Clap.Interface.Extension.Gui as Gui
import qualified Clap.Interface.Plugin as Clap
import Clap.Interface.Plugin (PluginHandle)
import qualified Clap.Library as Clap
import Control.Monad (unless)
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Data.IORef
import Data.Text (Text, unpack, pack)
import Ensemble.Engine (AudioDevice, MidiDevice)
import qualified Ensemble.Engine as Engine
import Ensemble.Node
import Ensemble.Event (SequencerEvent(..))
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
    Engine.createPluginNode engine $ PluginId (unpack filePath) pluginIndex

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
            let pluginHandle = Clap.plugin_handle (pluginNode_plugin pluginNode)
            maybePluginGuiHandle <- sendM $ getGuiExtension pluginHandle
            case maybePluginGuiHandle of
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

    where
        getGuiExtension :: PluginHandle -> IO (Maybe Gui.PluginGuiHandle)
        getGuiExtension plugin = do
            maybePtr <- Clap.getPluginExtension plugin "clap.gui"
            pure $ castPtr <$> maybePtr
    
createFloatingWindow :: Argument "nodeId" NodeId -> Argument "transientWindow" Int -> Argument "title" Text -> Ensemble Ok
createFloatingWindow (Argument nodeId) (Argument transientWindow) (Argument title) = do
    engine <- asks server_engine
    maybeNode <- sendM $ Engine.lookupNode engine nodeId
    case maybeNode of
        Just (Node_Plugin pluginNode) -> do 
            let pluginHandle = Clap.plugin_handle (pluginNode_plugin pluginNode)
            maybePluginGuiHandle <- sendM $ getGuiExtension pluginHandle
            case maybePluginGuiHandle of
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

    where
        getGuiExtension :: PluginHandle -> IO (Maybe Gui.PluginGuiHandle)
        getGuiExtension plugin = do
            maybePtr <- Clap.getPluginExtension plugin "clap.gui"
            pure $ castPtr <$> maybePtr

-- Sequencer
scheduleEvent :: Argument "tick" Tick -> Argument "sequencerEvent" SequencerEvent -> Ensemble Ok
scheduleEvent (Argument tick) (Argument event) = do
    sequencer <- asks server_sequencer
    sendM $ Sequencer.sendAt sequencer tick event
    pure Ok

playSequence :: Argument "startTick" Tick -> Argument "endTick" (Maybe Tick) -> Argument "loop" Bool -> Ensemble Ok
playSequence (Argument startTick) (Argument maybeEndTick) (Argument loop) = do
    sequencer <- asks server_sequencer
    engine <- asks server_engine
    sendM $ Clap.activateAll (Engine.engine_pluginHost engine) (Engine.engine_sampleRate engine) (Engine.engine_numberOfFrames engine)                                    
    Sequencer.playSequence sequencer engine startTick maybeEndTick loop
    sendM $ Clap.deactivateAll (Engine.engine_pluginHost engine)                              
    pure Ok

clearSequence :: Ensemble Ok
clearSequence = do
    eventQueue <- asks (Sequencer.sequencer_eventQueue . server_sequencer)
    sendM $ atomicModifyIORef' eventQueue $ \_eventQueue -> ([], ())
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