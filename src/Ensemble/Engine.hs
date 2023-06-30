{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Ensemble.Engine where

import Clap.Interface.Events (defaultEventConfig)
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
import Data.Aeson (Value(..))
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
import Ensemble.Schema.TH
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.Exts (IsList (..))
import GHC.Stack
import qualified Sound.PortAudio as PortAudio
import Sound.PortAudio (Stream)
import Sound.PortAudio.Base (PaDeviceIndex(..), PaDeviceInfo(..))
import qualified Sound.PortMidi as PortMidi

data Engine = Engine
    { engine_state :: IORef EngineState
    , engine_pluginHost :: PluginHost
    , engine_nodes :: IORef (Map NodeId Node)
    , engine_steadyTime :: IORef Int64
    , engine_sampleRate :: Double
    , engine_numberOfFrames :: Word32
    , engine_inputs :: Ptr (Ptr CFloat)
    , engine_outputs :: Ptr (Ptr CFloat)
    , engine_audioStream :: IORef (Maybe (Stream CFloat CFloat))
    , engine_audioThread :: IORef (Maybe ThreadId)
    }

data EngineState
    = StateStopped
    | StateRunning
    | StateStopping

newtype Tick = Tick { tick_value :: Int }
    deriving newtype (Eq, Ord, Show, Enum, Num, Real, Integral)

type EngineEffects effs = (Members '[Writer (KeyMap Value), Writer String, Error ApiError] effs, LastMember IO effs, HasCallStack)

createEngine :: HostConfig -> IO Engine
createEngine hostConfig = do
    state <- newIORef StateStopped
    pluginHost <- CLAP.createPluginHost hostConfig
    nodes <- newIORef mempty
    steadyTime <- newIORef (-1)
    inputs <- newArray [nullPtr, nullPtr]
    outputs <- newArray [nullPtr, nullPtr]
    audioStream <- newIORef Nothing
    audioThread <- newIORef Nothing
    pure $ Engine
        { engine_state = state
        , engine_pluginHost = pluginHost
        , engine_nodes = nodes
        , engine_steadyTime = steadyTime
        , engine_sampleRate = 44100
        , engine_numberOfFrames = 1024
        , engine_inputs = inputs   
        , engine_outputs = outputs
        , engine_audioStream = audioStream
        , engine_audioThread = audioThread
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
                case initializeResult of
                    Just initializeError -> throwApiError $ "Error when initializing audio driver: " <> show initializeError
                    Nothing -> do
                        sendM $ allocateBuffers engine (32 * 1024)
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
                                    setState engine StateRunning
                                    let pluginHost = engine_pluginHost engine
                                    sendM $ do
                                        CLAP.activateAll pluginHost (engine_sampleRate engine) (engine_numberOfFrames engine)
                                        PortAudio.startStream stream                                
                                whenJust maybeError $ \startError ->
                                    throwApiError $ "Error when starting audio stream: " <> show startError

        startMidi = do 
            initializeResult <- sendM PortMidi.initialize
            case initializeResult of
                Right _success -> pure ()
                Left portMidiError -> do
                    errorText <- sendM $ PortMidi.getErrorText portMidiError
                    throwApiError $ "Error when initializing PortMidi: " <> errorText

receiveInputs :: Engine -> CULong -> Ptr CFloat -> IO ()
receiveInputs engine numberOfInputSamples inputPtr = 
    unless (inputPtr == nullPtr) $ do 
        input <- peekArray (fromIntegral $ numberOfInputSamples * 2) inputPtr
        let (leftInput, rightInput) = partition (\(i, _) -> even (i * 2)) (zip [0 :: Int ..] input)
        [!leftInputBuffer, !rightInputBuffer] <- peekArray 2 $ engine_inputs engine
        pokeArray leftInputBuffer (snd <$> leftInput)
        pokeArray rightInputBuffer (snd <$> rightInput) 

lookupNode :: EngineEffects effs => Engine -> NodeId -> Eff effs Node
lookupNode engine nodeId = do
    nodes <- sendM $ readIORef $ engine_nodes engine
    case Map.lookup nodeId nodes of
        Just node -> pure node
        Nothing -> throwApiError $ 
            "Invalid node id: " <> show (nodeId_id nodeId) <> ". " <>
            "Valid node ids are: " <> show (nodeId_id <$> Map.keys nodes)

generateOutputs ::  EngineEffects effs => Engine -> Int -> [SequencerEvent] -> Eff effs AudioOutput
generateOutputs engine frameCount events = do
    sendEvents engine events
    sendM $ receiveOutputs engine frameCount

receiveOutputs :: Engine -> Int -> IO AudioOutput
receiveOutputs engine frameCount = do
    let clapHost = engine_pluginHost engine
    steadyTime <- readIORef (engine_steadyTime engine)
    CLAP.processBeginAll clapHost (fromIntegral frameCount) steadyTime
    pluginOutputs <- CLAP.processAll clapHost
    pure $ mixOutputs pluginOutputs


sendEvents :: EngineEffects effs => Engine -> [SequencerEvent] -> Eff effs ()
sendEvents engine events = do
    let clapHost = engine_pluginHost engine
    for_ events $ \(SequencerEvent nodeId eventConfig event) -> do
        node <- lookupNode engine nodeId
        case node of
            PluginNode pluginId _ -> 
                sendM $ CLAP.processEvent clapHost pluginId (fromMaybe defaultEventConfig eventConfig) event

data AudioOutput = AudioOutput
    { audioOutput_left :: [CFloat] 
    , audioOutput_right :: [CFloat] 
    } deriving (Eq, Ord)

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
mixOutputs pluginOutputs = AudioOutput    
    { audioOutput_left = foldl1 (zipWith (+)) (CLAP.pluginOutput_leftChannel <$> pluginOutputs)
    , audioOutput_right = foldl1 (zipWith (+)) (CLAP.pluginOutput_rightChannel <$> pluginOutputs)
    }

playAudio :: EngineEffects effs => Engine -> Tick -> Bool -> AudioOutput -> Eff effs ()
playAudio engine startTick loop audioOutput = do
    maybeAudioStream <- sendM $ do 
        writeIORef (engine_steadyTime engine) (tickToSteadyTime (engine_sampleRate engine) startTick)
        readIORef (engine_audioStream engine)
    whenJust maybeAudioStream $ \audioStream ->
        writeChunks audioStream audioOutput
    if loop 
        then playAudio engine startTick loop audioOutput
        else sendM $ writeIORef (engine_steadyTime engine) (-1)

    where
        writeChunks stream output = do
            eitherAvailableChunkSize <- sendM $ PortAudio.writeAvailable stream
            case eitherAvailableChunkSize of
                Right availableChunkSize -> do 
                    let (chunk, remaining) = takeChunk availableChunkSize output
                    let actualChunkSize = size chunk
                    sendOutputs engine (fromIntegral actualChunkSize) chunk
                    sendM $ modifyIORef' (engine_steadyTime engine) (+ fromIntegral actualChunkSize)
                    unless (size remaining == 0) $ 
                        writeChunks stream remaining
                Left audioPortError ->
                    throwApiError $ "Error getting available frames of audio stream: " <> show audioPortError


startAudioThread :: EngineEffects effs => Engine -> Eff effs ()
startAudioThread engine = do
    maybeStream <- sendM $ readIORef (engine_audioStream engine)
    case maybeStream of
        Just stream -> sendM $ do
            maybeThreadId <- readIORef (engine_audioThread engine)
            unless (isJust maybeThreadId) $ do
                threadId <- forkFinally threadBlock (\_ -> writeIORef (engine_audioThread engine) Nothing)
                writeIORef (engine_audioThread engine) (Just threadId)
            where 
                threadBlock = forever $ do 
                    eitherAvailableChunkSize <- PortAudio.writeAvailable stream
                    case eitherAvailableChunkSize of
                        Right availableChunkSize -> do 
                            audioOutput <- receiveOutputs engine (fromIntegral availableChunkSize)
                            let output = interleave (audioOutput_left audioOutput) (audioOutput_right audioOutput)
                            withArray output $ \outputPtr -> do
                                outputForeignPtr <- newForeignPtr_ outputPtr
                                void $ PortAudio.writeStream stream (fromIntegral availableChunkSize) outputForeignPtr
                        Left audioPortError ->
                            error $ "Error getting available frames of audio stream: " <> show audioPortError
        Nothing -> throwApiError "Audio stream not available"

takeChunk :: Int -> AudioOutput -> (AudioOutput, AudioOutput)
takeChunk chunkSize (AudioOutput left right) = 
    let chunk = AudioOutput (take chunkSize left) (take chunkSize right)
        remaining = AudioOutput (drop chunkSize left) (drop chunkSize right)
    in (chunk, remaining)

sendOutputs :: EngineEffects effs => Engine -> CULong -> AudioOutput -> Eff effs () 
sendOutputs engine frameCount audioOutput  = do
    let output = interleave (audioOutput_left audioOutput) (audioOutput_right audioOutput)
    maybeStream <- sendM $ readIORef $ engine_audioStream engine
    case maybeStream of
        Just stream -> do 
            maybeError <- sendM $ withArray output $ \outputPtr -> do
                outputForeignPtr <- newForeignPtr_ outputPtr
                PortAudio.writeStream stream frameCount outputForeignPtr
            whenJust maybeError $ \writeError -> 
                throwApiError $ "Error writing to audio stream: " <> show writeError
        Nothing -> throwApiError "PortAudio not initialized"

getCurrentTick :: EngineEffects effs => Engine -> Eff effs Tick
getCurrentTick engine = do
    steadyTime <- sendM $ readIORef (engine_steadyTime engine)
    pure $ steadyTimeToTick (engine_sampleRate engine) steadyTime

steadyTimeToTick :: Double -> Int64 -> Tick
steadyTimeToTick sampleRate steadyTime = 
    Tick $ fromIntegral steadyTime * 1000 `div` floor sampleRate

tickToSteadyTime :: Double -> Tick -> Int64
tickToSteadyTime sampleRate (Tick tick) =
    (fromIntegral tick `div` 1000) * floor sampleRate

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

deleteNode :: EngineEffects effs => Engine -> NodeId -> Eff effs ()
deleteNode engine nodeId =
    sendM $ modifyIORef' (engine_nodes engine) $ Map.delete nodeId

createNode :: Engine -> Node -> IO NodeId
createNode engine node =
    atomicModifyIORef' (engine_nodes engine) $ \nodes ->
        let newId = NodeId $ case Map.keys nodes of
                        [] -> 1
                        ids -> succ $ maximum $ nodeId_id <$> ids 
        in (Map.insert newId node nodes, newId) 

loadPlugin :: Engine -> PluginId -> IO ()
loadPlugin engine pluginId = void $ CLAP.load (engine_pluginHost engine) pluginId

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
interleave xs ys = concat (transpose [xs, ys])

throwApiError :: (Members '[Writer String, Error ApiError] effs, HasCallStack) => String -> Eff effs a
throwApiError message = do
    tell $ message <> "\n" <> prettyCallStack (fromList $ init $ toList callStack) 
    throwError $ ApiError { apiError_message = message }

deriveJSONs
    [ ''Tick
    , ''AudioDevice
    , ''AudioOutput
    , ''MidiDevice
    ]

deriving instance NFData AudioOutput