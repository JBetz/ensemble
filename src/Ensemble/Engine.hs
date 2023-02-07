{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonoLocalBinds #-}

module Ensemble.Engine where

import Clap.Interface.Events (defaultClapEventConfig)
import Clap.Interface.Host (HostConfig)
import Clap.Host (PluginHost (..), PluginId)
import qualified Clap.Host as CLAP
import Control.Monad
import Control.Monad.Extra (whenJust)
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Writer
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
import qualified Ensemble.Soundfont as SF
import Ensemble.Soundfont.FluidSynth.Library (FluidSynthLibrary)
import qualified Ensemble.Soundfont.FluidSynth.Library as FS
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

data Engine = Engine
    { engine_state :: IORef EngineState
    , engine_pluginHost :: PluginHost
    , ending_fluidSynthLibrary :: IORef (Maybe FluidSynthLibrary)
    , engine_instruments :: IORef (Map InstrumentId Instrument)
    , engine_steadyTime :: IORef Int64
    , engine_sampleRate :: Double
    , engine_numberOfFrames :: Word32
    , engine_inputs :: Ptr (Ptr CFloat)
    , engine_outputs :: Ptr (Ptr CFloat)
    , engine_audioStream :: IORef (Maybe (Stream CFloat CFloat))
    }

data EngineState
    = StateStopped
    | StateRunning
    | StateStopping

type EngineEffects effs = (Members '[Writer String, Error APIError] effs, LastMember IO effs, HasCallStack)

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
    pure $ Engine
        { engine_state = state
        , engine_pluginHost = pluginHost
        , ending_fluidSynthLibrary = soundfontPlayer
        , engine_instruments = instruments
        , engine_steadyTime = steadyTime
        , engine_sampleRate = 44100
        , engine_numberOfFrames = 1024
        , engine_inputs = inputs   
        , engine_outputs = outputs
        , engine_audioStream = audioStream
        }

data AudioDevice = AudioDevice
    { audioDevice_index :: Int
    , audioDevice_name :: String
    } deriving (Show)

instance Semigroup AudioOutput where
    a <> b = AudioOutput
        { audioOutput_left = audioOutput_left a <> audioOutput_left b
        , audioOutput_right = audioOutput_right a <> audioOutput_right b
        }

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
        Left portAudioError -> throwAPIError $ "Error getting audio devices: " <> show portAudioError

start :: EngineEffects effs => Engine -> Eff effs ()
start engine = do
    initializeResult <- sendM PortAudio.initialize
    case initializeResult of
        Just initializeError -> throwAPIError $ "Error when initializing audio driver: " <> show initializeError
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
                    throwAPIError $ "Error when opening audio stream: " <> show portAudioError
                Right stream -> do
                    maybeError <- sendM $ do
                        writeIORef (engine_audioStream engine) (Just stream)
                        setState engine StateRunning
                        let pluginHost = engine_pluginHost engine
                        CLAP.activateAll pluginHost (engine_sampleRate engine) (engine_numberOfFrames engine)
                        PortAudio.startStream stream
                    whenJust maybeError $ \startError ->
                        throwAPIError $ "Error when starting audio stream: " <> show startError 

receiveInputs :: Engine -> CULong -> Ptr CFloat -> IO ()
receiveInputs engine numberOfInputSamples inputPtr = 
    unless (inputPtr == nullPtr) $ do 
        input <- peekArray (fromIntegral $ numberOfInputSamples * 2) inputPtr
        let (leftInput, rightInput) = partition (\(i, _) -> even (i * 2)) (zip [0 :: Int ..] input)
        [!leftInputBuffer, !rightInputBuffer] <- peekArray 2 $ engine_inputs engine
        pokeArray leftInputBuffer (snd <$> leftInput)
        pokeArray rightInputBuffer (snd <$> rightInput) 

lookupInstrument :: EngineEffects effs => Engine -> InstrumentId -> Eff effs Instrument
lookupInstrument engine instrumentId = do
    instruments <- sendM $ readIORef $ engine_instruments engine
    case Map.lookup instrumentId instruments of
        Just instrument -> pure instrument
        Nothing -> throwAPIError $ 
            "Invalid instrument id: " <> show (instrumentId_id instrumentId) <> ". " <>
            "Valid instrument ids are: " <> show (instrumentId_id <$> Map.keys instruments)

getSoundfontInstruments :: Engine -> IO [SoundfontInstrument]
getSoundfontInstruments engine = do
    instruments <- readIORef $ engine_instruments engine
    pure $ mapMaybe (\case
        Instrument_Soundfont soundfont -> Just soundfont
        _ -> Nothing
        ) (Map.elems instruments)

generateOutputs ::  EngineEffects effs => Engine -> Int -> [SequencerEvent] -> Eff effs AudioOutput
generateOutputs engine frameCount events = do
    maybeSoundfontPlayer <- sendM $ readIORef $ ending_fluidSynthLibrary engine 
    let clapHost = engine_pluginHost engine
    steadyTime <- sendM $ readIORef (engine_steadyTime engine)
    sendM $ CLAP.processBeginAll clapHost (fromIntegral frameCount) steadyTime
    for_ events $ \(SequencerEvent instrumentId eventConfig event) -> do
        instrument <- lookupInstrument engine instrumentId
        case instrument of
            Instrument_Soundfont (SoundfontInstrument _ _ synth) -> 
                case maybeSoundfontPlayer of
                    Just soundfontPlayer -> sendM $ SF.processEvent soundfontPlayer synth event
                    Nothing -> throwAPIError "Attempting to play Soundfont instrument before loading FluidSynth DLL"
            Instrument_Clap (ClapInstrument pluginId) -> 
                sendM $ CLAP.processEvent clapHost pluginId (fromMaybe defaultClapEventConfig eventConfig) event
    soundfontOutputs <- case maybeSoundfontPlayer of
        Just soundfontPlayer -> do
            soundfonts <- sendM $ getSoundfontInstruments engine 
            for soundfonts $ \soundfont -> do
                let synth = soundfontInstrument_synth soundfont
                sendM $ SF.process soundfontPlayer synth (fromIntegral frameCount)
        Nothing -> pure mempty
    let soundfontOutput = SF.mixSoundfontOutputs frameCount soundfontOutputs
    pluginOutputs <- sendM $ CLAP.processAll clapHost
    pure $ mixSoundfontAndClapOutputs soundfontOutput  pluginOutputs

data AudioOutput = AudioOutput
    { audioOutput_left :: [CFloat] 
    , audioOutput_right :: [CFloat] 
    } deriving (Eq, Ord)

mixSoundfontAndClapOutputs :: SF.SoundfontOutput -> [CLAP.PluginOutput] -> AudioOutput
mixSoundfontAndClapOutputs (SF.SoundfontOutput wetLeft wetRight dryLeft dryRight) pluginOutputs =
    let (mixedSoundfontLeft, mixedSoundfontRight) = (zipWith (+) wetLeft dryLeft , zipWith (+) wetRight dryRight)
    in AudioOutput    
        { audioOutput_left = foldl (zipWith (+)) mixedSoundfontLeft (CLAP.pluginOutput_leftChannel <$> pluginOutputs)
        , audioOutput_right = foldl (zipWith (+)) mixedSoundfontRight (CLAP.pluginOutput_rightChannel <$> pluginOutputs)
        }

playAudio :: EngineEffects effs => Engine -> AudioOutput -> Eff effs ()
playAudio engine audioOutput = do
    maybeAudioStream <- sendM $ readIORef $ engine_audioStream engine
    whenJust maybeAudioStream $ \audioStream ->
        writeChunks audioStream audioOutput
    where
        writeChunks stream output = do
            eitherChunkSize <- sendM $ PortAudio.writeAvailable stream
            case eitherChunkSize of
                Right chunkSize -> do 
                    let (chunk, remaining) = takeChunk chunkSize output
                    sendOutputs engine (fromIntegral $ size chunk) chunk
                    unless (size remaining == 0) $ 
                        writeChunks stream remaining
                Left audioPortError ->
                    throwAPIError $ "Error getting available frames of audio stream: " <> show audioPortError


takeChunk :: Int -> AudioOutput -> (AudioOutput, AudioOutput)
takeChunk chunkSize (AudioOutput left right) = 
    let chunk = AudioOutput (take chunkSize left) (take chunkSize right)
        remaining = AudioOutput (drop chunkSize left) (drop chunkSize right)
    in (chunk, remaining)

size :: AudioOutput -> Int
size (AudioOutput left right) = min (length left) (length right) 

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
                throwAPIError $ "Error writing to audio stream: " <> show writeError
        Nothing -> throwAPIError "PortAudio not initialized"

stop :: EngineEffects effs => Engine -> Eff effs ()
stop engine = do
    maybeStream <- sendM $ readIORef (engine_audioStream engine)
    case maybeStream of
        Just stream -> do
            maybeError <- sendM $ do
                CLAP.deactivateAll (engine_pluginHost engine)
                _ <- PortAudio.stopStream stream
                _ <- PortAudio.closeStream stream
                freeBuffers engine
                maybeError <- PortAudio.terminate
                setState engine StateStopped
                pure maybeError
            whenJust maybeError $ \stopError ->
                throwAPIError $ "Error when stopping audio stream: " <> show stopError
        Nothing -> pure () 

createSoundfontInstrument :: EngineEffects effs => Engine -> FilePath -> Int -> Int -> Eff effs InstrumentInfo
createSoundfontInstrument engine filePath bankNumber programNumber = do
    library <- getFluidSynthLibrary engine
    settings <- sendM $ SF.createSettings library
    synth <- sendM $ SF.createSynth library settings
    soundfont <- sendM $ SF.loadSoundfont library synth filePath True
    sendM $ FS.programSelect library synth (fromIntegral $ SF.soundfontId_id $ SF.soundfont_id soundfont) 0 (fromIntegral bankNumber) (fromIntegral programNumber)
    let instrument = Instrument_Soundfont $ SoundfontInstrument 
            { soundfontInstrument_soundfont = soundfont
            , soundfontInstrument_settings = settings
            , soundfontInstrument_synth = synth
            }
    instrumentId <- sendM $ addInstrument engine instrument
    pure $ InstrumentInfo
        { instrumentInfo_id = instrumentId
        , instrumentInfo_instrument = instrument
        }

addInstrument :: Engine -> Instrument -> IO InstrumentId
addInstrument engine instrument =
    atomicModifyIORef' (engine_instruments engine) $ \instruments ->
        let newId = InstrumentId $ Map.size instruments 
        in (Map.insert newId instrument instruments, newId)

getFluidSynthLibrary :: EngineEffects effs => Engine -> Eff effs FluidSynthLibrary
getFluidSynthLibrary engine = do
    maybeLibrary <- sendM $ readIORef $ ending_fluidSynthLibrary engine
    case maybeLibrary of 
        Just library -> pure library
        Nothing -> throwAPIError "FluidSynth DLL not loaded"
                        
loadFluidSynthLibrary :: Engine -> FilePath -> IO ()
loadFluidSynthLibrary engine path = do
    fluidSynthLibrary <- FS.openFluidSynthLibrary path
    writeIORef (ending_fluidSynthLibrary engine) (Just fluidSynthLibrary)    

loadPlugin :: Engine -> PluginId -> IO ()
loadPlugin engine =
    CLAP.load (engine_pluginHost engine)

stopInstruments :: Engine -> IO ()
stopInstruments engine = do
    maybePlayer <- readIORef $ ending_fluidSynthLibrary engine
    case maybePlayer of
        Just player -> do
            soundfontInstruments <- getSoundfontInstruments engine
            SF.allSynthsOff player $ soundfontInstrument_synth <$> soundfontInstruments
        Nothing -> pure ()

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

throwAPIError :: (Members '[Writer String, Error APIError] effs, HasCallStack) => String -> Eff effs a
throwAPIError message = do
    tell $ prettyCallStack (fromList $ init $ toList callStack) 
    throwError $ APIError { apiError_message = message }