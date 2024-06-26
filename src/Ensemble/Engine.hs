{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Engine where

import Clap.Host (PluginHost (..), PluginLocation)
import qualified Clap.Host as CLAP
import Clap.Interface.AudioBuffer (BufferData (..))
import Clap.Interface.Events (Event (..), MidiData (..), MidiEvent (..), defaultEventConfig)
import Clap.Interface.Host (HostConfig)
import Control.Concurrent
import Control.DeepSeq (NFData)
import Control.Exception
import Control.Monad
import Control.Monad.Extra (whenJust)
import Control.Monad.Reader
import Data.Foldable (for_)
import Data.IORef
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Traversable (for)
import Data.Word
import Ensemble.Error
import Ensemble.Event
import Ensemble.Node
import Ensemble.Schema.TH
import Ensemble.Tick
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Sound.PortAudio (Stream, StreamCallbackFlag, StreamResult)
import qualified Sound.PortAudio as PortAudio
import Sound.PortAudio.Base (PaDeviceIndex (..), PaDeviceInfo (..), PaStreamCallbackTimeInfo, PaStreamInfo (..), PaTime (..))
import qualified Sound.PortAudio.Base as PortAudio
import Sound.PortMidi (PMEvent)
import qualified Sound.PortMidi as PortMidi

data Engine = Engine
  { engine_state :: IORef EngineState,
    engine_pluginHost :: PluginHost,
    engine_nodeCounter :: IORef Int,
    engine_nodes :: IORef (Map NodeId Node),
    engine_steadyTime :: IORef Int64,
    engine_sampleRate :: Double,
    engine_numberOfFrames :: Word32,
    engine_inputs :: Ptr (Ptr CFloat),
    engine_outputs :: Ptr (Ptr CFloat),
    engine_audioStream :: IORef (Maybe (Stream CFloat CFloat)),
    engine_eventBuffer :: IORef [SequencerEvent],
    engine_playbackThread :: IORef (Maybe ThreadId)
  }

data EngineState
  = StateStopped
  | StateRunning
  | StateStopping

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
  pure $
    Engine
      { engine_state = state,
        engine_pluginHost = pluginHost,
        engine_nodeCounter = nodeCounter,
        engine_nodes = nodes,
        engine_steadyTime = steadyTime,
        engine_sampleRate = 44100,
        engine_numberOfFrames = 1024,
        engine_inputs = inputs,
        engine_outputs = outputs,
        engine_audioStream = audioStream,
        engine_eventBuffer = eventBuffer,
        engine_playbackThread = playbackThread
      }

data AudioDevice = AudioDevice
  { audioDevice_index :: Int,
    audioDevice_name :: String
  }
  deriving (Show)

data MidiDevice = MidiDevice
  { midiDevice_index :: Int,
    midiDevice_interface :: String,
    midiDevice_name :: String,
    midiDevice_input :: Bool,
    midiDevice_output :: Bool,
    midiDevice_opened :: Bool
  }
  deriving (Show)

getAudioDevices :: IO [AudioDevice]
getAudioDevices = do
  eitherResult <- PortAudio.withPortAudio $ do
    count <- PortAudio.getNumDevices
    let indices = fromIntegral <$> [0 .. count]
    devices <- for indices $ \index -> do
      eitherInfo <- PortAudio.getDeviceInfo index
      pure $ case eitherInfo of
        Right info ->
          Just $
            AudioDevice
              { audioDevice_index = fromIntegral $ unPaDeviceIndex index,
                audioDevice_name = name_PaDeviceInfo info
              }
        Left _deviceError -> Nothing
    pure $ Right $ catMaybes devices
  case eitherResult of
    Right devices -> pure devices
    Left portAudioError -> throwApiError $ "Error getting audio devices: " <> show portAudioError

getMidiDevices :: IO [MidiDevice]
getMidiDevices = do
  deviceCount <- PortMidi.countDevices
  for [0 .. deviceCount - 1] $ \index -> do
    deviceInfo <- PortMidi.getDeviceInfo index
    pure $ toMidiDevice index deviceInfo
  where
    toMidiDevice deviceId deviceInfo =
      MidiDevice
        { midiDevice_index = deviceId,
          midiDevice_interface = PortMidi.interface deviceInfo,
          midiDevice_name = PortMidi.name deviceInfo,
          midiDevice_input = PortMidi.input deviceInfo,
          midiDevice_output = PortMidi.output deviceInfo,
          midiDevice_opened = PortMidi.opened deviceInfo
        }

start :: Engine -> IO ()
start engine = startAudio >> startMidi
  where
    startAudio = do
      maybeAudioStream <- liftIO $ readIORef (engine_audioStream engine)
      when (isNothing maybeAudioStream) $ do
        initializeResult <- liftIO PortAudio.initialize
        whenJust initializeResult $ \initializeError ->
          throwApiError $ "Error when initializing audio driver: " <> show initializeError
        eitherStream <-
          liftIO $
            PortAudio.openDefaultStream
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
              liftIO $ writeIORef (engine_audioStream engine) (Just stream)
              liftIO $ allocateBuffers engine (64 * 1024)
              setState engine StateRunning
              liftIO $ PortAudio.startStream stream
            whenJust maybeError $ \startError ->
              throwApiError $ "Error when starting audio stream: " <> show startError

    startMidi = do
      initializeResult <- liftIO PortMidi.initialize
      case initializeResult of
        Right _success -> pure ()
        Left portMidiError -> do
          errorText <- liftIO $ PortMidi.getErrorText portMidiError
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

generateOutputs :: Engine -> Int -> IO AudioOutput
generateOutputs engine frameCount = do
  events <- atomicModifyIORef' (engine_eventBuffer engine) $ \eventBuffer -> ([], eventBuffer)
  sendEvents engine events
  receiveOutputs engine frameCount

sendEventNow :: Engine -> SequencerEvent -> IO ()
sendEventNow engine event =
  sendEvents engine [event]

sendEvents :: Engine -> [SequencerEvent] -> IO ()
sendEvents engine events = do
  let clapHost = engine_pluginHost engine
  for_ events $ \(SequencerEvent nodeId eventConfig event) -> do
    maybeNode <- lookupNode engine nodeId
    whenJust maybeNode $ \case
      Node_MidiDevice midiDeviceNode ->
        liftIOidiDevice midiDeviceNode event
      Node_Plugin pluginNode ->
        CLAP.processEvent clapHost (pluginNode_id pluginNode) (fromMaybe defaultEventConfig eventConfig) event
  where
    liftIOidiDevice :: MidiDeviceNode -> Event -> IO ()
    liftIOidiDevice midiDeviceNode event = do
      maybeStartTime <- readIORef (midiDeviceNode_startTime midiDeviceNode)
      startTime <- case maybeStartTime of
        Just startTime -> pure startTime
        Nothing -> do
          let (PaTime currentTime) = PortAudio.currentTime undefined
          let startTime = round $ currentTime * 1000 - fromIntegral (midiDeviceNode_latency midiDeviceNode)
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
          Just $
            PortMidi.PMEvent
              { PortMidi.message =
                  PortMidi.encodeMsg $
                    PortMidi.PMMsg
                      { PortMidi.status = fromIntegral $ midiData_first midiData,
                        PortMidi.data1 = fromIntegral $ midiData_second midiData,
                        PortMidi.data2 = fromIntegral $ midiData_third midiData
                      },
                PortMidi.timestamp = fromIntegral $ startTime + currentTick
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
  { audioOutput_left :: [CFloat],
    audioOutput_right :: [CFloat]
  }
  deriving (Eq, Ord, Show)

instance Semigroup AudioOutput where
  a <> b =
    AudioOutput
      { audioOutput_left = audioOutput_left a <> audioOutput_left b,
        audioOutput_right = audioOutput_right a <> audioOutput_right b
      }

instance Monoid AudioOutput where
  mempty = AudioOutput [] []

size :: AudioOutput -> Int
size (AudioOutput left right) = min (length left) (length right)

isEmpty :: AudioOutput -> Bool
isEmpty audioOutput = size audioOutput == 0

mixOutputs :: [CLAP.PluginOutput] -> AudioOutput
mixOutputs [] = mempty
mixOutputs pluginOutputs =
  AudioOutput
    { audioOutput_left = foldl1 (zipWith (+)) (CLAP.pluginOutput_leftChannel <$> pluginOutputs),
      audioOutput_right = foldl1 (zipWith (+)) (CLAP.pluginOutput_rightChannel <$> pluginOutputs)
    }

playAudio :: Engine -> Tick -> Bool -> AudioOutput -> IO ()
playAudio engine startTick loop audioOutput = do
  liftIO $ writeIORef (engine_steadyTime engine) 0
  maybeAudioStream <- liftIO $ do
    writeIORef (engine_steadyTime engine) (tickToSteadyTime (engine_sampleRate engine) startTick)
    readIORef (engine_audioStream engine)
  whenJust maybeAudioStream $ \audioStream ->
    let play = do
          writeChunks audioStream audioOutput
          liftIO $ writeIORef (engine_steadyTime engine) (-1)
     in if loop then forever play else play
  where
    writeChunks stream output = do
      eitherAvailableChunkSize <- liftIO $ PortAudio.writeAvailable stream
      case eitherAvailableChunkSize of
        Right availableChunkSize -> do
          let (chunk, remaining) = takeChunk availableChunkSize output
          let actualChunkSize = size chunk
          sendOutputs engine stream (fromIntegral actualChunkSize) chunk
          _steadyTime <- liftIO $ atomicModifyIORef' (engine_steadyTime engine) $ \steadyTime ->
            let newSteadyTime = steadyTime + fromIntegral actualChunkSize
             in (newSteadyTime, newSteadyTime)
          -- TODO: Why is this necessary? Without it, the current tick events are extremely desychronized.
          liftIO $ threadDelay 1
          unless (size remaining == 0) $ writeChunks stream remaining
        Left audioPortError ->
          throwApiError $ "Error getting available frames of audio stream: " <> show audioPortError

takeChunk :: Int -> AudioOutput -> (AudioOutput, AudioOutput)
takeChunk chunkSize (AudioOutput left right) =
  let (chunkLeft, remainingLeft) = splitAt chunkSize left
      (chunkRight, remainingRight) = splitAt chunkSize right
   in (AudioOutput chunkLeft chunkRight, AudioOutput remainingLeft remainingRight)

sendOutputs :: Engine -> Stream CFloat CFloat -> CULong -> AudioOutput -> IO ()
sendOutputs _engine stream frameCount audioOutput = do
  let output = interleave (audioOutput_left audioOutput) (audioOutput_right audioOutput)
  maybeError <- liftIO $ withArray output $ \outputPtr -> do
    outputForeignPtr <- newForeignPtr_ outputPtr
    PortAudio.writeStream stream frameCount outputForeignPtr
  whenJust maybeError $ \writeError ->
    throwApiError $ "Error writing to audio stream: " <> show writeError

getCurrentTick :: Engine -> IO Tick
getCurrentTick engine = do
  steadyTime <- readIORef (engine_steadyTime engine)
  pure $ steadyTimeToTick (engine_sampleRate engine) steadyTime

stop :: Engine -> IO ()
stop engine = do
  maybeStream <- liftIO $ readIORef (engine_audioStream engine)
  case maybeStream of
    Just stream -> do
      maybeError <- liftIO $ do
        CLAP.deactivateAll (engine_pluginHost engine)
        void $ PortAudio.stopStream stream
        void $ PortAudio.closeStream stream
        freeBuffers engine
        PortAudio.terminate
      case maybeError of
        Just stopError ->
          throwApiError $ "Error when stopping audio stream: " <> show stopError
        Nothing ->
          liftIO $ writeIORef (engine_audioStream engine) Nothing
      setState engine StateStopped
    Nothing -> pure ()
  terminateResult <- liftIO PortMidi.terminate
  case terminateResult of
    Right _success -> pure ()
    Left portMidiError -> do
      errorText <- liftIO $ PortMidi.getErrorText portMidiError
      throwApiError $ "Error when terminating PortMidi: " <> errorText

createMidiDeviceNode :: Engine -> Int -> IO NodeId
createMidiDeviceNode engine deviceId = do
  nodes <- liftIO $ readIORef (engine_nodes engine)
  let existingNode =
        find
          ( \(_, node) -> case node of
              Node_MidiDevice midiDeviceNode ->
                let (DeviceId existingDeviceId) = midiDeviceNode_deviceId midiDeviceNode
                 in deviceId == existingDeviceId
              _ -> False
          )
          (Map.assocs nodes)
  case existingNode of
    Just (nodeId, _) -> pure nodeId
    Nothing -> do
      deviceInfo <- liftIO $ PortMidi.getDeviceInfo deviceId
      if PortMidi.output deviceInfo
        then do
          _stream <- getAudioStream engine
          streamInfo <- getAudioStreamInfo engine
          let (PaTime (CDouble streamOutputLatency)) = PortAudio.outputLatency streamInfo
          let streamLatency = streamOutputLatency * 1000
              sampleLatency = fromIntegral (engine_numberOfFrames engine * 1000 * 2) / engine_sampleRate engine
              latency = round $ maximum [streamLatency, sampleLatency, 10]
          openOutputResult <- liftIO $ PortMidi.openOutput deviceId latency
          case openOutputResult of
            Right outputStream -> do
              startTime <- liftIO $ newIORef Nothing
              let node =
                    Node_MidiDevice $
                      MidiDeviceNode
                        { midiDeviceNode_deviceId = DeviceId deviceId,
                          midiDeviceNode_latency = latency,
                          midiDeviceNode_startTime = startTime,
                          midiDeviceNode_stream = outputStream
                        }
              nodeId <- liftIO $ createNodeId engine
              liftIO $ modifyIORef' (engine_nodes engine) $ \nodeMap ->
                Map.insert nodeId node nodeMap
              pure nodeId
            Left portMidiError -> do
              errorText <- liftIO $ PortMidi.getErrorText portMidiError
              throwApiError $ "Error when initializing PortMidi: " <> errorText
        else throwApiError $ PortMidi.name deviceInfo <> " is not an output device"

getAudioStream :: Engine -> IO (Stream CFloat CFloat)
getAudioStream engine = do
  maybeAudioStream <- liftIO $ readIORef (engine_audioStream engine)
  case maybeAudioStream of
    Just audioStream -> pure audioStream
    Nothing -> throwApiError "Audio Stream not available"

getAudioStreamInfo :: Engine -> IO PaStreamInfo
getAudioStreamInfo engine = do
  audioStream <- getAudioStream engine
  eitherStreamInfo <- liftIO $ PortAudio.getStreamInfo audioStream
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

createPluginNode :: Engine -> PluginLocation -> IO NodeId
createPluginNode engine pluginLocation = liftIO $ do
  (pluginId, plugin) <- CLAP.load (engine_pluginHost engine) pluginLocation
  nodeId <- createNodeId engine
  let node =
        Node_Plugin $
          PluginNode
            { pluginNode_id = pluginId,
              pluginNode_plugin = plugin
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

setState :: Engine -> EngineState -> IO ()
setState engine = liftIO . writeIORef (engine_state engine)

interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave _ [] = []
interleave xs ys = concat (transpose [xs, ys])

throwApiError :: String -> a
throwApiError message =
  throw $ ApiError {apiError_message = message}

deriveJSONs
  [ ''AudioDevice,
    ''AudioOutput,
    ''MidiDevice
  ]

deriving instance NFData AudioOutput
