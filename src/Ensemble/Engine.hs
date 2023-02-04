{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Ensemble.Engine where

import Clap.Interface.Host (HostConfig)
import Clap.Host (PluginHost (..), PluginId)
import qualified Clap.Host as CLAP
import Control.Exception
import Control.Monad
import Control.Monad.Extra (whenJust)
import Control.Monad.Freer
import Control.Monad.Freer.Error
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
import Ensemble.Instrument
import Ensemble.Soundfont (SoundfontId (..))
import qualified Ensemble.Soundfont as SF
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Sound.PortAudio as PortAudio
import Sound.PortAudio (StreamCallbackFlag, Stream, StreamResult)
import Sound.PortAudio.Base (PaStreamCallbackTimeInfo, PaDeviceIndex(..), PaDeviceInfo(..))

data Engine = Engine
    { engine_state :: IORef EngineState
    , engine_pluginHost :: PluginHost
    , engine_soundfontPlayer :: IORef (Maybe SF.SoundfontPlayer)
    , engine_instruments :: IORef (Map InstrumentId Instrument)
    , engine_steadyTime :: IORef Int64
    , engine_sampleRate :: Double
    , engine_numberOfFrames :: Word32
    , engine_inputs :: Ptr (Ptr CFloat)
    , engine_outputs :: Ptr (Ptr CFloat)
    , engine_audioStream :: IORef (Maybe (Stream CFloat CFloat))
    , engine_eventBuffer :: IORef [SequencerEvent]
    }

data EngineState
    = StateStopped
    | StateRunning
    | StateStopping

createEngine :: HostConfig -> IO Engine
createEngine hostConfig = do
    state <- newIORef StateStopped
    pluginHost <- CLAP.createPluginHost hostConfig
    soundfontPlayer <- newIORef Nothing
    instruments <- newIORef mempty
    steadyTime <- newIORef 0
    inputs <- newArray [nullPtr, nullPtr]
    outputs <- newArray [nullPtr, nullPtr]
    audioStream <- newIORef Nothing
    eventBuffer <- newIORef mempty
    pure $ Engine
        { engine_state = state
        , engine_pluginHost = pluginHost
        , engine_soundfontPlayer = soundfontPlayer
        , engine_instruments = instruments
        , engine_steadyTime = steadyTime
        , engine_sampleRate = 44100
        , engine_numberOfFrames = 1024
        , engine_inputs = inputs   
        , engine_outputs = outputs
        , engine_audioStream = audioStream
        , engine_eventBuffer = eventBuffer
        }

data AudioDevice = AudioDevice
    { audioDevice_index :: Int
    , audioDevice_name :: String
    } deriving (Show)

getAudioDevices :: (LastMember IO effs, Member (Error APIError) effs) => Eff effs [AudioDevice]
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
                Left deviceError -> Nothing
        pure $ Right $ catMaybes devices
    case eitherResult of
        Right devices -> pure devices
        Left portAudioError -> throwError $ APIError $ "Error getting audio devices: " <> show portAudioError

pushEvent :: Engine -> SequencerEvent -> IO ()
pushEvent engine event =
    modifyIORef' (engine_eventBuffer engine) (<> [event])

pushEvents :: Engine -> [SequencerEvent] -> IO ()
pushEvents engine events =
    modifyIORef' (engine_eventBuffer engine) (<> events)

start :: (LastMember IO effs, Member (Error APIError) effs) => Engine -> Eff effs ()
start engine = do
    initializeResult <- sendM PortAudio.initialize
    case initializeResult of
        Just initializeError -> throwError $ APIError $ "Error when initializing audio driver: " <> show initializeError
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
                    throwError $ APIError $ "Error when opening audio stream: " <> show portAudioError
                Right stream -> do
                    maybeError <- sendM $ do
                        writeIORef (engine_audioStream engine) (Just stream)
                        setState engine StateRunning
                        let pluginHost = engine_pluginHost engine
                        CLAP.activateAll pluginHost (engine_sampleRate engine) (engine_numberOfFrames engine)
                        PortAudio.startStream stream
                    whenJust maybeError $ \startError ->
                        throwError $ APIError $ "Error when starting audio stream: " <> show startError 

            
audioCallback :: Engine -> PaStreamCallbackTimeInfo -> [StreamCallbackFlag] -> CULong -> Ptr CFloat -> Ptr CFloat -> IO StreamResult
audioCallback engine _timeInfo _flags numberOfInputSamples inputPtr outputPtr = do
    receiveInputs engine numberOfInputSamples inputPtr   
    audioOutput <- generateOutputs engine (fromIntegral numberOfInputSamples)
    unless (outputPtr == nullPtr) $ do 
        let !output = interleave (audioOutput_left audioOutput) (audioOutput_right audioOutput)
        pokeArray outputPtr output
    modifyIORef' (engine_steadyTime engine) (+ fromIntegral numberOfInputSamples)
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

lookupInstrument :: Engine -> InstrumentId -> IO Instrument
lookupInstrument engine instrumentId = do
    instruments <- readIORef $ engine_instruments engine
    case Map.lookup instrumentId instruments of
        Just instrument -> pure instrument
        Nothing -> error $ "Invalid instrument id: " <> show instrumentId

getSoundfontInstruments :: Engine -> IO [SoundfontInstrument]
getSoundfontInstruments engine = do
    instruments <- readIORef $ engine_instruments engine
    pure $ mapMaybe (\case
        Instrument_Soundfont soundfont -> Just soundfont
        _ -> Nothing
        ) (Map.elems instruments)

generateOutputs :: Engine -> Int -> IO AudioOutput
generateOutputs engine frameCount = do
    maybeSoundfontPlayer <- readIORef $ engine_soundfontPlayer engine 
    let clapHost = engine_pluginHost engine
    steadyTime <- readIORef (engine_steadyTime engine)
    eventBuffer <- readIORef (engine_eventBuffer engine)
    CLAP.processBeginAll clapHost (fromIntegral frameCount) steadyTime
    for_ eventBuffer $ \(SequencerEvent instrumentId eventConfig event) -> do
        instrument <- lookupInstrument engine instrumentId
        case instrument of
            Instrument_Soundfont (SoundfontInstrument _ synth) -> 
                case maybeSoundfontPlayer of
                    Just soundfontPlayer -> SF.processEvent soundfontPlayer synth event
                    Nothing -> error "Attempting to play Soundfont instrument before initializing FluidSynth"
            Instrument_Clap (ClapInstrument pluginId) -> 
                CLAP.processEvent clapHost pluginId eventConfig event
    writeIORef (engine_eventBuffer engine) []
    soundfontOutput <- case maybeSoundfontPlayer of
        Just soundfontPlayer -> do
            soundfonts <- getSoundfontInstruments engine 
            traverse (\synth -> SF.process soundfontPlayer synth (fromIntegral frameCount)) (soundfontInstrument_synth <$> soundfonts)
        Nothing -> pure mempty
    pluginOutputs <- CLAP.processAll clapHost
    pure $ mixAudioOutputs (mconcat soundfontOutput) pluginOutputs

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
    mempty = AudioOutput 
        { audioOutput_left = [] 
        , audioOutput_right = []
        }

mixAudioOutputs :: SF.SoundfontOutput -> [CLAP.PluginOutput] -> AudioOutput
mixAudioOutputs (SF.SoundfontOutput wetLeft wetRight dryLeft dryRight) pluginOutputs =
    let (mixedSoundfontLeft, mixedSoundfontRight) = (zipWith (+) wetLeft dryLeft , zipWith (+) wetRight dryRight)
    in AudioOutput    
        { audioOutput_left = foldl (zipWith (+)) mixedSoundfontLeft (CLAP.pluginOutput_leftChannel <$> pluginOutputs)
        , audioOutput_right = foldl (zipWith (+)) mixedSoundfontRight (CLAP.pluginOutput_rightChannel <$> pluginOutputs)
        }


playAudio :: Engine -> AudioOutput -> IO ()
playAudio engine audioOutput = do
    maybeAudioStream <- readIORef $ engine_audioStream engine
    whenJust maybeAudioStream $ \audioStream ->
        writeChunks audioStream audioOutput
    where
        writeChunks stream output = do
            eitherChunkSize <- PortAudio.writeAvailable stream
            case eitherChunkSize of
                Right chunkSize -> do 
                    let (chunk, remaining) = takeChunk chunkSize output
                    maybeAudioPortError <- sendOutputs engine (fromIntegral $ min chunkSize (size chunk)) chunk
                    whenJust maybeAudioPortError $ \audioPortError -> 
                        error $ "Error writing to audio stream: " <> show audioPortError
                    unless (remaining == mempty) $ 
                        writeChunks stream remaining
                Left audioPortError ->
                    error $ "Error getting available frames of audio stream: " <> show audioPortError


takeChunk :: Int -> AudioOutput -> (AudioOutput, AudioOutput)
takeChunk chunkSize (AudioOutput left right) = 
    let chunk = AudioOutput (take chunkSize left) (take chunkSize right)
        remaining = AudioOutput (drop chunkSize left) (drop chunkSize right)
    in (chunk, remaining)

size :: AudioOutput -> Int
size (AudioOutput left right) = min (length left) (length right) 

sendOutputs :: Engine -> CULong -> AudioOutput -> IO (Maybe PortAudio.Error) 
sendOutputs engine frameCount audioOutput  = do
    maybeStream <- readIORef $ engine_audioStream engine
    case maybeStream of
        Just stream -> 
            withArray (interleave (audioOutput_left audioOutput) (audioOutput_right audioOutput)) $ \outputPtr -> do
                outputForeignPtr <- newForeignPtr_ outputPtr
                PortAudio.writeStream stream frameCount outputForeignPtr
        Nothing -> pure $ Just PortAudio.NotInitialized

stop :: (LastMember IO effs, Member (Error APIError) effs) => Engine -> Eff effs ()
stop engine = do
    maybeStream <- sendM $ readIORef (engine_audioStream engine)
    case maybeStream of
        Just stream -> do
            maybeError <- sendM $ do
                maybeSoundfontPlayer <- readIORef $ engine_soundfontPlayer engine
                case maybeSoundfontPlayer of
                    Just soundfontPlayer -> SF.deleteSoundfontPlayer soundfontPlayer
                    Nothing -> pure () 
                CLAP.deactivateAll (engine_pluginHost engine)
                _ <- PortAudio.stopStream stream
                _ <- PortAudio.closeStream stream
                freeBuffers engine
                maybeError <- PortAudio.terminate
                setState engine StateStopped
                pure maybeError
            whenJust maybeError $ \stopError ->
                throwError $ APIError $ "Error when stopping audio stream: " <> show stopError
        Nothing -> pure () 

createSoundfontInstrument :: Engine -> FilePath -> IO InstrumentInfo
createSoundfontInstrument engine filePath = do
    player <- getSoundfontPlayer engine
    synth <- SF.createSynth player
    soundfont <- SF.loadSoundfont player synth filePath True
    let instrument =Instrument_Soundfont $ SoundfontInstrument 
            { soundfontInstrument_soundfont = soundfont
            , soundfontInstrument_synth = synth
            }
    instrumentId <- addInstrument engine instrument
    pure $ InstrumentInfo
        { instrumentInfo_id = instrumentId
        , instrumentInfo_instrument = instrument
        }

addInstrument :: Engine -> Instrument -> IO InstrumentId
addInstrument engine instrument =
    atomicModifyIORef (engine_instruments engine) $ \instruments ->
        let newId = InstrumentId $ Map.size instruments 
        in (Map.insert newId instrument instruments, newId)

getSoundfontPlayer :: Engine -> IO SF.SoundfontPlayer
getSoundfontPlayer engine = do
    maybePlayer <- readIORef $ engine_soundfontPlayer engine
    case maybePlayer of 
        Just player -> pure player
        Nothing -> throw SF.SoundfontPlayerNotInitialized
                        
initializeSoundfontPlayer :: Engine -> FilePath -> IO ()
initializeSoundfontPlayer engine path = do
    player <- SF.createSoundfontPlayer path
    writeIORef (engine_soundfontPlayer engine) (Just player)    


loadPlugin :: Engine -> PluginId -> IO ()
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
