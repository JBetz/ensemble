{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Ensemble.Engine where

import Clap.Interface.AudioBuffer (BufferData (..))
import Clap.Interface.Events (Event (..), MidiEvent(..), MidiData(..), defaultEventConfig)
import Clap.Interface.Host (HostConfig)
import Clap.Host (PluginHost (..), PluginId)
import qualified Clap.Host as CLAP
import Control.Concurrent
import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad.Extra (whenJust)
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Writer
import Data.Aeson (Value(..), ToJSON)
import Data.Aeson.KeyMap (KeyMap)
import Data.Foldable (for_)
import Data.IORef
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Word
import Data.Traversable (for)
import Ensemble.Error
import Ensemble.Event
import Ensemble.Node
import Ensemble.Schema.TaggedJSON
import Ensemble.Schema.TH
import Ensemble.Tick
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.Exts (IsList (..))
import GHC.Stack
import qualified Sound.PortAudio as PortAudio
import qualified Sound.PortAudio.Base as PortAudio
import Sound.PortAudio (Stream, StreamResult, StreamCallbackFlag)
import Sound.PortAudio.Base (PaDeviceIndex(..), PaDeviceInfo(..), PaStreamCallbackTimeInfo, PaStreamInfo(..), PaTime(..))
import qualified Sound.PortMidi as PortMidi
import Sound.PortMidi (PMEvent)

data Engine = Engine
    { engine_state :: IORef EngineState
    , engine_pluginHost :: PluginHost
    , engine_nodeCounter :: IORef Int
    , engine_nodes :: IORef (Map NodeId Node)
    , engine_steadyTime :: IORef Int64
    , engine_sampleRate :: Double
    , engine_numberOfFrames :: Word32
    , engine_inputs :: Ptr (Ptr CFloat)
    , engine_outputs :: Ptr (Ptr CFloat)
    , engine_audioStream :: IORef (Maybe (Stream CFloat CFloat))
    , engine_eventBuffer :: IORef [SequencerEvent]
    , engine_playbackThread :: IORef (Maybe ThreadId)
    }

data EngineState
    = StateStopped
    | StateRunning
    | StateStopping

type EngineEffects effs = (Members '[Writer (KeyMap Value), Writer String, Error ApiError] effs, LastMember IO effs, HasCallStack)

createEngine :: HostConfig -> IO Engine
createEngine hostConfig = do
    state <- newIORef StateStopped
    pluginHost <- CLAP.createPluginHost hostConfig
    nodeCounter <- newIORef 1
    nodes <- newIORef mempty
    steadyTime <- newIORef (-1)
    inputs <- newArray [nullPtr, nullPtr]
    outputs <- newArray [nullPtr, nullPtr]
    audioStream <- newIORef Nothing
    eventBuffer <- newIORef []
    playbackThread <- newIORef Nothing
    pure $ Engine
        { engine_state = state
        , engine_pluginHost = pluginHost
        , engine_nodeCounter = nodeCounter
        , engine_nodes = nodes
        , engine_steadyTime = steadyTime
        , engine_sampleRate = 44100
        , engine_numberOfFrames = 1024
        , engine_inputs = inputs   
        , engine_outputs = outputs
        , engine_audioStream = audioStream
        , engine_eventBuffer = eventBuffer
        , engine_playbackThread = playbackThread
        }

data AudioDevice = AudioDevice
    { audioDevice_index :: Int
    , audioDevice_name :: String
    } deriving (Show)

data MidiDevice = MidiDevice 
    { midiDevice_index :: Int
    , midiDevice_interface :: String
    , midiDevice_name :: String
    , midiDevice_input :: Bool
    , midiDevice_output :: Bool 
    , midiDevice_opened :: Bool 
    } deriving (Show)

getAudioDevices :: EngineEffects effs => Eff effs [AudioDevice]
getAudioDevices = do
    eitherResult <- sendM $ PortAudio.withPortAudio $ do
        count <- PortAudio.getNumDevices
        let indices = fromIntegral <$> [0 .. count]
        devices <- for indices $ \index -> do
            eitherInfo <- PortAudio.getDeviceInfo index
            pure $ case eitherInfo of
                Right info -> Just $ AudioDevice
                    { audioDevice_index = fromIntegral $ unPaDeviceIndex index
                    , audioDevice_name = name_PaDeviceInfo info
                    }
                Left _deviceError -> Nothing
        pure $ Right $ catMaybes devices
    case eitherResult of
        Right devices -> pure devices
        Left portAudioError -> throwApiError $ "Error getting audio devices: " <> show portAudioError

getMidiDevices :: EngineEffects effs => Eff effs [MidiDevice]
getMidiDevices = sendM $ do
    deviceCount <- PortMidi.countDevices
    for [0 .. deviceCount - 1] $ \index -> do 
        deviceInfo <- PortMidi.getDeviceInfo index
        pure $ toMidiDevice index deviceInfo
    where
        toMidiDevice deviceId deviceInfo = MidiDevice 
            { midiDevice_index = deviceId
            , midiDevice_interface = PortMidi.interface deviceInfo 
            , midiDevice_name = PortMidi.name deviceInfo 
            , midiDevice_input = PortMidi.input deviceInfo
            , midiDevice_output = PortMidi.output deviceInfo
            , midiDevice_opened = PortMidi.opened deviceInfo
            }

start :: EngineEffects effs => Engine -> Eff effs ()
start engine = startAudio >> startMidi
    where
        startAudio = do
            maybeAudioStream <- sendM $ readIORef (engine_audioStream engine)
            when (isNothing maybeAudioStream) $ do    
                initializeResult <- sendM PortAudio.initialize
                whenJust initializeResult $ \initializeError ->
                    throwApiError $ "Error when initializing audio driver: " <> show initializeError
                eitherStream <- sendM $ PortAudio.openDefaultStream 
                    0 -- Number of input channels 
                    2 -- Number of output channels
                    (engine_sampleRate engine) -- Sample rate
                    (Just $ fromIntegral $ engine_numberOfFrames engine) -- Frames per buffer
                    Nothing -- Callback
                    Nothing -- Callback on completion
                case eitherStream of
                    Left portAudioError -> 
                        throwApiError $ "Error when opening audio stream: " <> show portAudioError
                    Right stream -> do
                        maybeError <- do
                            sendM $ writeIORef (engine_audioStream engine) (Just stream)
                            sendM $ allocateBuffers engine (64 * 1024)
                            setState engine StateRunning
                            sendM $ PortAudio.startStream stream                                
                        whenJust maybeError $ \startError ->
                            throwApiError $ "Error when starting audio stream: " <> show startError

        startMidi = do 
            initializeResult <- sendM PortMidi.initialize
            case initializeResult of
                Right _success -> pure ()
                Left portMidiError -> do
                    errorText <- sendM $ PortMidi.getErrorText portMidiError
                    throwApiError $ "Error when initializing PortMidi: " <> errorText

audioCallback :: Engine -> PaStreamCallbackTimeInfo -> [StreamCallbackFlag] -> CULong -> Ptr CFloat -> Ptr CFloat -> IO StreamResult
audioCallback engine _timeInfo _flags numberOfInputSamples inputPtr outputPtr = do
    receiveInputs engine numberOfInputSamples inputPtr
    audioOutput <- generateOutputs engine (fromIntegral numberOfInputSamples)
    unless (outputPtr == nullPtr) $ do 
        let !output = interleave (audioOutput_left audioOutput) (audioOutput_right audioOutput)
        pokeArray outputPtr output
    atomicModifyIORef' (engine_steadyTime engine) $ \steadyTime -> 
        (steadyTime + fromIntegral numberOfInputSamples, ())
    state <- readIORef (engine_state engine)
    pure $ case state of
        StateRunning -> PortAudio.Continue
        StateStopping -> PortAudio.Complete
        StateStopped -> PortAudio.Abort

receiveInputs :: Engine -> CULong -> Ptr CFloat -> IO ()
receiveInputs engine numberOfInputSamples inputPtr =
    unless (inputPtr == nullPtr) $ do 
        input <- peekArray (fromIntegral $ numberOfInputSamples * 2) inputPtr
        let (leftInput, rightInput) = partition (\(i, _) -> even (i * 2)) (zip [0 :: Int ..] input)
        [!leftInputBuffer, !rightInputBuffer] <- peekArray 2 $ engine_inputs engine
        pokeArray leftInputBuffer (snd <$> leftInput)
        pokeArray rightInputBuffer (snd <$> rightInput) 
        
generateOutputs ::  Engine -> Int -> IO AudioOutput
generateOutputs engine frameCount = do
    events <- atomicModifyIORef' (engine_eventBuffer engine) $ \eventBuffer -> ([], eventBuffer)
    sendEvents engine events
    receiveOutputs engine frameCount

sendEvents :: Engine -> [SequencerEvent] -> IO ()
sendEvents engine events = do
    let clapHost = engine_pluginHost engine
    for_ events $ \(SequencerEvent nodeId eventConfig event) -> do
        maybeNode <- lookupNode engine nodeId
        whenJust maybeNode $ \case
            Node_MidiDevice midiDeviceNode -> 
                sendMidiDevice midiDeviceNode event
            Node_Plugin pluginNode ->
                CLAP.processEvent clapHost (pluginNode_id pluginNode) (fromMaybe defaultEventConfig eventConfig) event
    where
        sendMidiDevice :: MidiDeviceNode -> Event -> IO ()
        sendMidiDevice midiDeviceNode event = do
            maybeStartTime <- readIORef (midiDeviceNode_startTime midiDeviceNode)
            startTime <- case maybeStartTime of
                Just startTime -> pure startTime
                Nothing -> do
                    let (PaTime currentTime) = PortAudio.currentTime undefined
                    let startTime = round $ currentTime * 1000 - (fromIntegral $ midiDeviceNode_latency midiDeviceNode) 
                    writeIORef (midiDeviceNode_startTime midiDeviceNode) (Just startTime)
                    pure startTime 
            steadyTime <- readIORef (engine_steadyTime engine)
            let maybePortMidiEvent = toPortMidiEvent event startTime (steadyTimeToTick (engine_sampleRate engine) steadyTime)
            whenJust maybePortMidiEvent $ \portMidiEvent -> do
                void $ PortMidi.writeShort (midiDeviceNode_stream midiDeviceNode) portMidiEvent

        toPortMidiEvent :: Event -> Int -> Tick -> Maybe PMEvent
        toPortMidiEvent event startTime (Tick currentTick) = 
            case toMidiData event of
                Just midiData ->
                    Just $ PortMidi.PMEvent
                        { PortMidi.message = PortMidi.encodeMsg $ PortMidi.PMMsg
                            { PortMidi.status = fromIntegral $ midiData_first midiData 
                            , PortMidi.data1 = fromIntegral $ midiData_second midiData
                            , PortMidi.data2 = fromIntegral $ midiData_third midiData
                            }
                        , PortMidi.timestamp = fromIntegral $ startTime + currentTick
                        }
                Nothing -> Nothing
        
        toMidiData :: Event -> Maybe MidiData
        toMidiData = \case
            Event_Midi midiEvent -> Just (midiEvent_data midiEvent)
            _ -> Nothing 

receiveOutputs :: Engine -> Int -> IO AudioOutput
receiveOutputs engine frameCount = do
    let clapHost = engine_pluginHost engine
    steadyTime <- readIORef (engine_steadyTime engine)
    CLAP.processBeginAll clapHost (fromIntegral frameCount) steadyTime
    pluginOutputs <- CLAP.processAll clapHost
    pure $ mixOutputs pluginOutputs
    
data AudioOutput = AudioOutput
    { audioOutput_left :: [CFloat] 
    , audioOutput_right :: [CFloat] 
    } deriving (Eq, Ord, Show)

instance Semigroup AudioOutput where
    a <> b = AudioOutput
        { audioOutput_left = audioOutput_left a <> audioOutput_left b
        , audioOutput_right = audioOutput_right a <> audioOutput_right b
        }

instance Monoid AudioOutput where
    mempty = AudioOutput [] []

size :: AudioOutput -> Int
size (AudioOutput left right) = min (length left) (length right) 

isEmpty :: AudioOutput -> Bool
isEmpty audioOutput = size audioOutput == 0

mixOutputs :: [CLAP.PluginOutput] -> AudioOutput
mixOutputs [] = mempty
mixOutputs pluginOutputs = AudioOutput    
    { audioOutput_left = foldl1 (zipWith (+)) (CLAP.pluginOutput_leftChannel <$> pluginOutputs)
    , audioOutput_right = foldl1 (zipWith (+)) (CLAP.pluginOutput_rightChannel <$> pluginOutputs)
    }

playAudio :: EngineEffects effs => Engine -> Tick -> Bool -> AudioOutput -> Eff effs ()
playAudio engine startTick loop audioOutput = do
    maybeAudioStream <- sendM $ do 
        writeIORef (engine_steadyTime engine) (tickToSteadyTime (engine_sampleRate engine) startTick)
        readIORef (engine_audioStream engine)
    tellEvent PlaybackEvent_Started
    whenJust maybeAudioStream $ \audioStream ->
        writeChunks audioStream audioOutput
    if loop 
        then playAudio engine startTick loop audioOutput
        else sendM $ writeIORef (engine_steadyTime engine) (-1)
    tellEvent PlaybackEvent_Stopped
    where
        writeChunks stream output = do
            eitherAvailableChunkSize <- sendM $ PortAudio.writeAvailable stream
            case eitherAvailableChunkSize of
                Right availableChunkSize -> do 
                    let (chunk, remaining) = takeChunk availableChunkSize output
                    let actualChunkSize = size chunk
                    sendOutputs engine (fromIntegral actualChunkSize) chunk
                    steadyTime <- sendM $ atomicModifyIORef' (engine_steadyTime engine) $ \steadyTime ->
                        let newSteadyTime = steadyTime + fromIntegral actualChunkSize 
                        in (newSteadyTime, newSteadyTime)
                    tellEvent $ PlaybackEvent_CurrentTick (steadyTimeToTick (engine_sampleRate engine) steadyTime)
                    unless (size remaining == 0) $ writeChunks stream remaining
                Left audioPortError ->
                    throwApiError $ "Error getting available frames of audio stream: " <> show audioPortError

tellEvent :: Member (Writer (KeyMap Value)) effs => (HasTypeTag a, ToJSON a) => a -> Eff effs ()
tellEvent = tell . toTaggedJSON

takeChunk :: Int -> AudioOutput -> (AudioOutput, AudioOutput)
takeChunk chunkSize (AudioOutput left right) = 
    let chunk = AudioOutput (take chunkSize left) (take chunkSize right)
        remaining = AudioOutput (drop chunkSize left) (drop chunkSize right)
    in (chunk, remaining)

sendOutputs :: EngineEffects effs => Engine -> CULong -> AudioOutput -> Eff effs () 
sendOutputs engine frameCount audioOutput  = do
    let output = interleave (audioOutput_left audioOutput) (audioOutput_right audioOutput)
    stream <- getAudioStream engine
    maybeError <- sendM $ withArray output $ \outputPtr -> do
        outputForeignPtr <- newForeignPtr_ outputPtr
        PortAudio.writeStream stream frameCount outputForeignPtr
    whenJust maybeError $ \writeError -> 
        throwApiError $ "Error writing to audio stream: " <> show writeError
    
getCurrentTick :: Engine -> IO Tick
getCurrentTick engine = do
    steadyTime <- readIORef (engine_steadyTime engine)
    pure $ steadyTimeToTick (engine_sampleRate engine) steadyTime

stop :: EngineEffects effs => Engine -> Eff effs ()
stop engine = do
    maybeStream <- sendM $ readIORef (engine_audioStream engine)
    case maybeStream of
        Just stream -> do
            maybeError <- sendM $ do
                CLAP.deactivateAll (engine_pluginHost engine)
                void $ PortAudio.stopStream stream
                void $ PortAudio.closeStream stream
                freeBuffers engine
                PortAudio.terminate
            case maybeError  of
                Just stopError ->
                    throwApiError $ "Error when stopping audio stream: " <> show stopError
                Nothing -> 
                    sendM $ writeIORef (engine_audioStream engine) Nothing
            setState engine StateStopped
        Nothing -> pure ()
    terminateResult <- sendM PortMidi.terminate
    case terminateResult of
        Right _success -> pure ()
        Left portMidiError -> do
            errorText <- sendM $ PortMidi.getErrorText portMidiError
            throwApiError $ "Error when terminating PortMidi: " <> errorText

createMidiDeviceNode :: EngineEffects effs => Engine -> Int -> Eff effs NodeId
createMidiDeviceNode engine deviceId = do
    nodes <- sendM $ readIORef (engine_nodes engine)
    let existingNode = find (\(_, node) -> case node of
            Node_MidiDevice midiDeviceNode -> 
                let (DeviceId existingDeviceId) = midiDeviceNode_deviceId midiDeviceNode
                in deviceId == existingDeviceId
            _ -> False
            ) (Map.assocs nodes)
    case existingNode of
        Just (nodeId, _) -> pure nodeId
        Nothing -> do 
            deviceInfo <- sendM $ PortMidi.getDeviceInfo deviceId 
            if PortMidi.output deviceInfo 
                then do
                    stream <- getAudioStream engine
                    streamInfo <- getAudioStreamInfo engine
                    let (PaTime (CDouble streamOutputLatency)) = PortAudio.outputLatency streamInfo 
                    let streamLatency = streamOutputLatency * 1000
                        sampleLatency  = fromIntegral (engine_numberOfFrames engine * 1000 * 2) / engine_sampleRate engine
                        latency = round $ maximum [streamLatency, sampleLatency, 10]
                    openOutputResult <- sendM $ PortMidi.openOutputWithCustomTiming deviceId latency  
                        (do
                            Right (PaTime streamTime) <- PortAudio.getStreamTime stream
                            pure $ round $ streamTime * 1000)
                    case openOutputResult of
                        Right outputStream -> do 
                            startTime <- sendM $ newIORef Nothing
                            let node = Node_MidiDevice $ MidiDeviceNode
                                        { midiDeviceNode_deviceId = DeviceId deviceId
                                        , midiDeviceNode_latency = latency
                                        , midiDeviceNode_startTime = startTime
                                        , midiDeviceNode_stream = outputStream
                                        }
                            nodeId <- sendM $ createNodeId engine
                            sendM $ modifyIORef' (engine_nodes engine) $ \nodeMap ->
                                Map.insert nodeId node nodeMap
                            pure nodeId
                        Left portMidiError -> do
                            errorText <- sendM $ PortMidi.getErrorText portMidiError
                            throwApiError $ "Error when initializing PortMidi: " <> errorText
                else throwApiError $ PortMidi.name deviceInfo <> " is not an output device"

getAudioStream :: EngineEffects effs => Engine -> Eff effs (Stream CFloat CFloat)
getAudioStream engine = do
    maybeAudioStream <- sendM $ readIORef (engine_audioStream engine)
    case maybeAudioStream of
        Just audioStream -> pure audioStream
        Nothing -> throwApiError $ "Audio Stream not available"

getAudioStreamInfo :: EngineEffects effs => Engine -> Eff effs PaStreamInfo
getAudioStreamInfo engine = do
    audioStream <- getAudioStream engine
    eitherStreamInfo <- sendM $ PortAudio.getStreamInfo audioStream
    case eitherStreamInfo of
        Right streamInfo -> pure streamInfo
        Left portAudioError -> do
            throwApiError $ "Error when retrieving stream info: " <> show portAudioError

deleteNode :: Engine -> NodeId -> IO ()
deleteNode engine nodeId =
    modifyIORef' (engine_nodes engine) $ Map.delete nodeId

createNodeId :: Engine -> IO NodeId
createNodeId engine = atomicModifyIORef' (engine_nodeCounter engine) $ \nodeCounter ->
    (nodeCounter + 1, NodeId nodeCounter)

lookupNode :: Engine -> NodeId -> IO (Maybe Node)
lookupNode engine nodeId = do
    nodes <- readIORef $ engine_nodes engine
    pure $ Map.lookup nodeId nodes

createPluginNode :: EngineEffects effs => Engine -> PluginId -> Eff effs NodeId
createPluginNode engine pluginId = sendM $ do
    plugin <- CLAP.load (engine_pluginHost engine) pluginId
    nodeId <- createNodeId engine
    let node = Node_Plugin $ PluginNode
            { pluginNode_id = pluginId
            , pluginNode_plugin = plugin
            }
    CLAP.setPorts plugin (Data32 $ engine_inputs engine) (Data32 $ engine_outputs engine)
    CLAP.activate plugin (engine_sampleRate engine) (engine_numberOfFrames engine)
    modifyIORef' (engine_nodes engine) $ Map.insert nodeId node
    pure nodeId

-- unloadPlugin :: Engine -> PluginId -> IO ()

-- isRunning :: Engine -> IO Bool

allocateBuffers :: Engine -> Int -> IO ()
allocateBuffers engine bufferSize = do
    freeBuffers engine
    
    inputLeft <- newArray $ replicate bufferSize 0
    inputRight <- newArray $ replicate bufferSize 0
    pokeArray (engine_inputs engine) [inputLeft, inputRight]
    
    outputLeft <- newArray $ replicate bufferSize 0
    outputRight <- newArray $ replicate bufferSize 0
    pokeArray (engine_outputs engine) [outputLeft, outputRight]

freeBuffers :: Engine -> IO ()
freeBuffers engine = do
    [inputLeft, inputRight] <- peekArray 2 $ engine_inputs engine
    free inputLeft
    free inputRight

    [outputLeft, outputRight] <- peekArray 2 $ engine_outputs engine
    free outputLeft
    free outputRight

    pokeArray (engine_inputs engine) [nullPtr, nullPtr]
    pokeArray (engine_outputs engine) [nullPtr, nullPtr]

setState :: EngineEffects effs => Engine -> EngineState -> Eff effs ()
setState engine = sendM . writeIORef (engine_state engine)

interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave _ [] = []
interleave xs ys = concat (transpose [xs, ys])

throwApiError :: (Members '[Writer String, Error ApiError] effs, LastMember IO effs, HasCallStack) => String -> Eff effs a
throwApiError message = do
    tell $ message <> "\n" <> prettyCallStack (fromList $ init $ toList callStack) 
    throwError $ ApiError { apiError_message = message }

deriveJSONs
    [ ''AudioDevice
    , ''AudioOutput
    , ''MidiDevice
    ]

deriving instance NFData AudioOutput
