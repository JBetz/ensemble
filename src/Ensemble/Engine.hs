{-# LANGUAGE BangPatterns #-}

module Ensemble.Engine where

import Clap.Interface.AudioBuffer
import Clap.Interface.Events
import Clap.Interface.Host
import Clap.Host (PluginHost (..), ClapId)
import qualified Clap.Host as CLAP
import Control.Monad
import Data.Foldable (for_)
import Data.IORef
import Data.Int
import Data.List
import Data.Word
import Ensemble.Soundfont (SoundfontId (..))
import Ensemble.Soundfont qualified as SF
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Sound.PortAudio as PortAudio
import Sound.PortAudio.Base

data Destination
    = SoundfontPlayer SoundfontId
    | ClapPlugin ClapId EventConfig
    deriving (Show)

data Engine = Engine
    { engine_state :: IORef EngineState
    , engine_pluginHost :: PluginHost
    , engine_soundfontPlayer :: SF.SoundfontPlayer
    , engine_steadyTime :: IORef Int64
    , engine_sampleRate :: Double
    , engine_numberOfFrames :: Word32
    , engine_inputs :: Ptr (Ptr CFloat)
    , engine_outputs :: Ptr (Ptr CFloat)
    , engine_audioStream :: IORef (Maybe (Stream CFloat CFloat))
    , engine_eventBuffer :: IORef [(Destination, Event)]
    }

data EngineState
    = StateStopped
    | StateRunning
    | StateStopping

createEngine :: HostConfig -> IO Engine
createEngine hostConfig = do
    state <- newIORef StateStopped
    pluginHost <- CLAP.createPluginHost hostConfig
    soundfontPlayer <- SF.createSoundfontPlayer
    steadyTime <- newIORef 0
    inputs <- newArray [nullPtr, nullPtr]
    outputs <- newArray [nullPtr, nullPtr]
    audioStream <- newIORef Nothing
    eventBuffer <- newIORef mempty
    pure $ Engine
        { engine_state = state
        , engine_pluginHost = pluginHost
        , engine_soundfontPlayer = soundfontPlayer
        , engine_steadyTime = steadyTime
        , engine_sampleRate = 44100
        , engine_numberOfFrames = 1024
        , engine_inputs = inputs   
        , engine_outputs = outputs
        , engine_audioStream = audioStream
        , engine_eventBuffer = eventBuffer
        }

pushEvent :: Destination -> Engine -> Event -> IO ()
pushEvent destination engine event =
    modifyIORef' (engine_eventBuffer engine) (<> [(destination, event)])

start :: Engine -> IO (Maybe Error)
start engine = do
    initializeResult <- initialize
    case initializeResult of
        Just initializeError -> pure $ Just initializeError
        Nothing -> do
            allocateBuffers engine (32 * 1024)
            eitherStream <- openDefaultStream 
                0 -- Number of input channels 
                2 -- Number of output channels
                (engine_sampleRate engine) -- Sample rate
                (Just $ fromIntegral $ engine_numberOfFrames engine) -- Frames per buffer
                Nothing -- Callback
                Nothing -- Callback on completion
            case eitherStream of
                Left portAudioError -> 
                    pure $ Just portAudioError
                Right stream -> do
                    writeIORef (engine_audioStream engine) (Just stream)
                    setState engine StateRunning
                    let pluginHost = engine_pluginHost engine
                    CLAP.setPorts pluginHost (Data32 $ engine_inputs engine) (Data32 $ engine_outputs engine)
                    CLAP.activateAll pluginHost (engine_sampleRate engine) (engine_numberOfFrames engine)
                    startStream stream
            
audioCallback :: Engine -> PaStreamCallbackTimeInfo -> [StreamCallbackFlag] -> CULong -> Ptr CFloat -> Ptr CFloat -> IO StreamResult
audioCallback engine _timeInfo _flags numberOfInputSamples inputPtr outputPtr = do
    let soundfontPlayer = engine_soundfontPlayer engine 
    let clapHost = engine_pluginHost engine
    
    -- Receive inputs
    unless (inputPtr == nullPtr) $ do 
        input <- peekArray (fromIntegral $ numberOfInputSamples * 2) inputPtr
        let (leftInput, rightInput) = partition (\(i, _) -> even (i * 2)) (zip [0 :: Int ..] input)
        [!leftInputBuffer, !rightInputBuffer] <- peekArray 2 $ engine_inputs engine
        pokeArray leftInputBuffer (snd <$> leftInput)
        pokeArray rightInputBuffer (snd <$> rightInput)    
        
    -- Generate outputs
    steadyTime <- readIORef (engine_steadyTime engine)
    CLAP.processBegin clapHost (fromIntegral numberOfInputSamples) steadyTime
    eventBuffer <- readIORef (engine_eventBuffer engine)
    for_ eventBuffer $ \(destination, event) ->
        case destination of
            SoundfontPlayer soundfontId -> SF.processEvent soundfontPlayer soundfontId event
            ClapPlugin pluginId eventConfig -> CLAP.processEvent clapHost pluginId eventConfig event
    writeIORef (engine_eventBuffer engine) []
    SF.process soundfontPlayer
    CLAP.process clapHost
    
    -- Send outputs
    unless (outputPtr == nullPtr) $ do 
        [leftOutputBuffer, rightOutputBuffer] <- peekArray 2 $ engine_outputs engine
        leftOutputs <- peekArray (fromIntegral numberOfInputSamples) leftOutputBuffer
        rightOutputs <- peekArray (fromIntegral numberOfInputSamples) rightOutputBuffer
        let !output = interleave leftOutputs rightOutputs
        pokeArray outputPtr output

    -- Return status
    modifyIORef' (engine_steadyTime engine) (+ fromIntegral numberOfInputSamples)
    state <- readIORef (engine_state engine)
    pure $ case state of
        StateRunning -> PortAudio.Continue
        StateStopping -> PortAudio.Complete
        StateStopped -> PortAudio.Abort

stop :: Engine -> IO (Maybe Error)
stop engine = do
    maybeStream <- readIORef (engine_audioStream engine)
    case maybeStream of
        Just stream -> do
            SF.deleteSoundfontPlayer (engine_soundfontPlayer engine)
            CLAP.deactivateAll (engine_pluginHost engine)
            _ <- stopStream stream
            _ <- closeStream stream
            freeBuffers engine
            result <- terminate
            setState engine StateStopped
            pure result
        Nothing -> pure Nothing 

loadPlugin :: Engine -> ClapId -> IO ()
loadPlugin engine =
    CLAP.load (engine_pluginHost engine)

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
setState engine = writeIORef (engine_state engine)

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (transpose [xs, ys])
