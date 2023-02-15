{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Ensemble.Engine where

import Clap.Interface.Events (defaultClapEventConfig)
import Clap.Interface.Host (HostConfig)
import Clap.Host (PluginHost (..), PluginId)
import qualified Clap.Host as CLAP
import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad.Extra (whenJust)
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Writer
import Data.Aeson (Value(..))
import Data.Foldable (for_, traverse_)
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
import Ensemble.Schema.TH
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

newtype Tick = Tick { tick_value :: Int }
    deriving newtype (Eq, Ord, Show, Enum, Num, Real, Integral)

type EngineEffects effs = (Members '[Writer Value, Writer String, Error ApiError] effs, LastMember IO effs, HasCallStack)

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

start :: EngineEffects effs => Engine -> Eff effs ()
start engine = do
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
        Nothing -> throwApiError $ 
            "Invalid instrument id: " <> show (instrumentId_id instrumentId) <> ". " <>
            "Valid instrument ids are: " <> show (instrumentId_id <$> Map.keys instruments)

lookupSoundfontInstrument :: EngineEffects effs => Engine -> InstrumentId -> Eff effs SoundfontInstrument
lookupSoundfontInstrument engine instrumentId = do
    instrument <- lookupInstrument engine instrumentId
    case instrument of
        Instrument_Soundfont soundfontInstrument -> pure soundfontInstrument
        Instrument_Clap _  -> throwApiError $ "Expected Soundfont instrument id, received CLAP instrument id: " <> show instrumentId

getSoundfontInstruments :: Engine -> IO [SoundfontInstrument]
getSoundfontInstruments engine = do
    instruments <- readIORef $ engine_instruments engine
    pure $ mapMaybe (\case
        Instrument_Soundfont soundfont -> Just soundfont
        _ -> Nothing
        ) (Map.elems instruments)

getSoundfontInstrumentPresets :: EngineEffects effs => Engine -> InstrumentId -> Eff effs [SF.SoundfontPreset]
getSoundfontInstrumentPresets engine instrumentId = do
    library <- getFluidSynthLibrary engine
    soundfontInstrument <- lookupSoundfontInstrument engine instrumentId
    sendM $ SF.loadSoundfontPresets library (SF.soundfont_handle $ soundfontInstrument_soundfont soundfontInstrument)

generateOutputs ::  EngineEffects effs => Engine -> Int -> [SequencerEvent] -> Eff effs AudioOutput
generateOutputs engine frameCount events = do
    maybeSoundfontPlayer <- sendM $ readIORef $ ending_fluidSynthLibrary engine 
    let clapHost = engine_pluginHost engine
    steadyTime <- sendM $ readIORef (engine_steadyTime engine)
    sendM $ CLAP.processBeginAll clapHost (fromIntegral frameCount) steadyTime
    for_ events $ \(SequencerEvent instrumentId eventConfig event) -> do
        instrument <- lookupInstrument engine instrumentId
        case instrument of
            Instrument_Soundfont soundfontInstrument -> 
                case maybeSoundfontPlayer of
                    Just soundfontPlayer -> sendM $ SF.processEvent soundfontPlayer (soundfontInstrument_synth soundfontInstrument) event
                    Nothing -> throwApiError "Attempting to play Soundfont instrument before loading FluidSynth DLL"
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

instance Semigroup AudioOutput where
    a <> b = AudioOutput
        { audioOutput_left = audioOutput_left a <> audioOutput_left b
        , audioOutput_right = audioOutput_right a <> audioOutput_right b
        }

mixSoundfontAndClapOutputs :: SF.SoundfontOutput -> [CLAP.PluginOutput] -> AudioOutput
mixSoundfontAndClapOutputs (SF.SoundfontOutput wetLeft wetRight dryLeft dryRight) pluginOutputs =
    let (mixedSoundfontLeft, mixedSoundfontRight) = (zipWith (+) wetLeft dryLeft , zipWith (+) wetRight dryRight)
    in AudioOutput    
        { audioOutput_left = foldl (zipWith (+)) mixedSoundfontLeft (CLAP.pluginOutput_leftChannel <$> pluginOutputs)
        , audioOutput_right = foldl (zipWith (+)) mixedSoundfontRight (CLAP.pluginOutput_rightChannel <$> pluginOutputs)
        }

playAudio :: EngineEffects effs => Engine -> AudioOutput -> Eff effs ()
playAudio engine audioOutput = do
    sendM $ writeIORef (engine_steadyTime engine) 0
    maybeAudioStream <- sendM $ readIORef $ engine_audioStream engine
    whenJust maybeAudioStream $ \audioStream ->
        writeChunks audioStream audioOutput
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
                throwApiError $ "Error writing to audio stream: " <> show writeError
        Nothing -> throwApiError "PortAudio not initialized"

getCurrentTick :: EngineEffects effs => Engine -> Eff effs Tick
getCurrentTick engine = do
    steadyTime <- sendM $ readIORef (engine_steadyTime engine)
    pure $ Tick $ fromIntegral steadyTime * 1000 `div` floor (engine_sampleRate engine)

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
                PortAudio.terminate
            whenJust maybeError $ \stopError ->
                throwApiError $ "Error when stopping audio stream: " <> show stopError
            setState engine StateStopped
        Nothing -> pure () 

deleteInstrument :: EngineEffects effs => Engine -> InstrumentId -> Eff effs ()
deleteInstrument engine instrumentId = do
    instrument <- lookupInstrument engine instrumentId
    case instrument of
        Instrument_Clap _ -> pure ()
        Instrument_Soundfont soundfontInstrument -> do
            library <- getFluidSynthLibrary engine
            sendM $ FS.deleteFluidSynth library (soundfontInstrument_synth soundfontInstrument)
            sendM $ FS.deleteFluidSettings library (soundfontInstrument_settings soundfontInstrument)
    sendM $ modifyIORef' (engine_instruments engine) $ Map.delete instrumentId

createSoundfontInstrument :: EngineEffects effs => Engine -> FilePath -> Eff effs InstrumentId
createSoundfontInstrument engine filePath = do
    library <- getFluidSynthLibrary engine
    settings <- sendM $ FS.newFluidSettings library
    synth <- sendM $ FS.newFluidSynth library settings
    soundfont <- sendM $ SF.loadSoundfont library synth filePath True
    sendM $ addInstrument engine $ Instrument_Soundfont $ SoundfontInstrument 
        { soundfontInstrument_soundfont = soundfont
        , soundfontInstrument_settings = settings
        , soundfontInstrument_synth = synth
        }

selectSoundfontInstrumentPreset :: EngineEffects effs => Engine -> InstrumentId -> Int -> Int -> Eff effs ()
selectSoundfontInstrumentPreset engine instrumentId bankNumber programNumber = do
    soundfontInstrument <- lookupSoundfontInstrument engine instrumentId
    library <- getFluidSynthLibrary engine
    let soundfont = soundfontInstrument_soundfont soundfontInstrument
    sendM $ FS.programSelect library (soundfontInstrument_synth soundfontInstrument) 0 
        (fromIntegral $ SF.soundfontId_id $ SF.soundfont_id soundfont) 
        (fromIntegral bankNumber) (fromIntegral programNumber)

addInstrument :: Engine -> Instrument -> IO InstrumentId
addInstrument engine instrument =
    atomicModifyIORef' (engine_instruments engine) $ \instruments ->
        let newId = InstrumentId $ case Map.keys instruments of
                        [] -> 1
                        ids -> succ $ maximum $ instrumentId_id <$> ids 
        in (Map.insert newId instrument instruments, newId)

getFluidSynthLibrary :: EngineEffects effs => Engine -> Eff effs FluidSynthLibrary
getFluidSynthLibrary engine = do
    maybeLibrary <- sendM $ readIORef $ ending_fluidSynthLibrary engine
    case maybeLibrary of 
        Just library -> pure library
        Nothing -> throwApiError "FluidSynth DLL not loaded"
                        
loadFluidSynthLibrary :: Engine -> FilePath -> IO ()
loadFluidSynthLibrary engine path = do
    fluidSynthLibrary <- FS.openFluidSynthLibrary path
    writeIORef (ending_fluidSynthLibrary engine) (Just fluidSynthLibrary)    

loadPlugin :: Engine -> PluginId -> IO ()
loadPlugin engine =
    CLAP.load (engine_pluginHost engine)

stopInstruments :: Engine -> IO ()
stopInstruments engine = do
    maybeLibrary <- readIORef $ ending_fluidSynthLibrary engine
    case maybeLibrary of
        Just library -> do
            soundfontInstruments <- getSoundfontInstruments engine
            traverse_ (\synth -> FS.allSoundsOff library synth (-1)) (soundfontInstrument_synth <$> soundfontInstruments)
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
    ]

deriving instance NFData AudioOutput