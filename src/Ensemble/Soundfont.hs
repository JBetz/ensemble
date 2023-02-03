{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Ensemble.Soundfont where

import Control.Exception
import Data.IORef
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Extra (whileM)
import Ensemble.Soundfont.FluidSynth.Library as FS
import Ensemble.Soundfont.FluidSynth.Foreign.Settings
import Ensemble.Soundfont.FluidSynth.Foreign.SoundFonts
import Ensemble.Soundfont.FluidSynth.Foreign.Synth
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import GHC.Stack

newtype SoundfontId = SoundfontId { soundfontId_id :: Int }
    deriving (Eq, Ord, Show)

data SoundfontPlayer = SoundfontPlayer
    { soundfontPlayer_fluidSynthLibrary :: FluidSynthLibrary
    , soundfontPlayer_settings :: FluidSettings
    , soundfontPlayer_synth :: FluidSynth
    , soundfontPlayer_soundfonts :: IORef (Map SoundfontId Soundfont)
    }

data SoundfontPreset = SoundfontPreset
    { soundfontPreset_name :: String
    , soundfontPreset_bankNumber :: Int
    , soundfontPreset_instrumentNumber :: Int
    } deriving (Show)

data Soundfont = Soundfont
    { soundfont_id :: SoundfontId
    , soundfont_filePath :: FilePath 
    , soundfont_handle :: FluidSoundfont
    , soundfont_presets :: IORef (Maybe [SoundfontPreset])
    }

data SoundfontException
    = SoundfontPlayerNotInitialized
    deriving (Show)

data SoundfontEvent 
    = SoundfontEvent_NoteOn NoteOnEvent
    | SoundfontEvent_NoteOff NoteOffEvent
    | SoundfontEvent_ProgramSelect ProgramSelectEvent
    deriving (Show)

data NoteOnEvent = NoteOnEvent
    { noteOnEvent_channel :: Int16
    , noteOnEvent_key :: Int16
    , noteOnEvent_velocity :: Double 
    } deriving (Show)

data NoteOffEvent = NoteOffEvent
    { noteOffEvent_channel :: Int16
    , noteOffEvent_key :: Int16
    } deriving (Show)

data ProgramSelectEvent = ProgramSelectEvent
    { programSelectEvent_channel :: Int16
    , programSelectEvent_soundfontId :: SoundfontId
    , programSelectEvent_bank :: Int16
    , programSelectEvent_program :: Int16
    } deriving (Show)

instance Exception SoundfontException

type FluidSettings = Ptr C'fluid_settings_t
type FluidSynth = Ptr C'fluid_synth_t
type FluidSoundfont = Ptr C'fluid_sfont_t
type FluidPreset = Ptr C'fluid_preset_t

withSoundfontPlayer :: HasCallStack => FilePath -> (SoundfontPlayer -> IO a) -> IO a
withSoundfontPlayer filePath f = do
    player <- createSoundfontPlayer filePath
    result <- f player
    deleteSoundfontPlayer player
    pure result

createSoundfontPlayer :: FilePath -> IO SoundfontPlayer
createSoundfontPlayer filePath = do
    library <- openFluidSynthLibrary filePath
    settings <-newFluidSettings library
    synth <- newFluidSynth library settings 
    soundfonts <- newIORef mempty
    pure $ SoundfontPlayer
        { soundfontPlayer_fluidSynthLibrary = library
        , soundfontPlayer_settings = settings
        , soundfontPlayer_synth = synth
        , soundfontPlayer_soundfonts = soundfonts
        }

deleteSoundfontPlayer :: SoundfontPlayer -> IO ()
deleteSoundfontPlayer player = do
    let library = soundfontPlayer_fluidSynthLibrary player
    deleteFluidSynth library (soundfontPlayer_synth player)
    deleteFluidSettings library (soundfontPlayer_settings player)

loadSoundfont :: SoundfontPlayer -> FilePath -> Bool -> IO Soundfont
loadSoundfont player filePath resetPresets = do
    let library = soundfontPlayer_fluidSynthLibrary player
    let synth = soundfontPlayer_synth player
    soundfontId <- sfLoad library synth filePath resetPresets
    handle <- getSfontById library synth soundfontId
    presets <- newIORef mempty
    let soundfont = Soundfont 
            { soundfont_id = SoundfontId soundfontId
            , soundfont_filePath = filePath
            , soundfont_handle = handle
            , soundfont_presets = presets
            }
    modifyIORef' (soundfontPlayer_soundfonts player) $ Map.insert (soundfont_id soundfont) soundfont 
    pure soundfont

getSoundfont :: SoundfontPlayer -> SoundfontId -> IO (Maybe Soundfont)
getSoundfont player soundfontId = do
    soundfonts <- readIORef (soundfontPlayer_soundfonts player)
    pure $ Map.lookup soundfontId soundfonts

loadSoundfontPresets :: SoundfontPlayer -> Soundfont -> IO [SoundfontPreset]
loadSoundfontPresets player soundfont = do
    let library = soundfontPlayer_fluidSynthLibrary player
    let soundfontHandle = soundfont_handle soundfont
    sfontIterationStart library soundfontHandle
    presetsVar <- newIORef mempty
    first <- sfontIterationNext library soundfontHandle
    if first /= nullPtr 
        then do
            current <- newIORef first
            whileM $ do
                preset <- readIORef current
                modifyIORef' presetsVar (preset:)
                next <- sfontIterationNext library soundfontHandle
                writeIORef current next
                pure $ next /= nullPtr 
        else pure ()
    presetPtrs <- readIORef presetsVar
    presets <- traverse createPreset presetPtrs
    writeIORef (soundfont_presets soundfont) (Just presets)
    pure presets
    where 
        createPreset :: Ptr C'fluid_preset_t -> IO SoundfontPreset
        createPreset presetPtr = do
            let library = soundfontPlayer_fluidSynthLibrary player
            name <- presetGetName library presetPtr
            bankNumber <- presetGetBankNum library presetPtr
            instrumentNumber <- presetGetNum library presetPtr
            pure $ SoundfontPreset
                { soundfontPreset_name = name
                , soundfontPreset_bankNumber = bankNumber
                , soundfontPreset_instrumentNumber = instrumentNumber
                }


processEvent :: SoundfontPlayer -> SoundfontId -> SoundfontEvent -> IO ()
processEvent player _soundfontId = \case
    SoundfontEvent_NoteOn event -> noteOn library synth (noteOnEvent_channel event) (noteOnEvent_key event) (noteOnEvent_velocity event)
    SoundfontEvent_NoteOff event -> noteOff library synth (noteOffEvent_channel event) (noteOffEvent_key event)
    SoundfontEvent_ProgramSelect event -> programSelect library synth 
        (programSelectEvent_channel event) (soundfontId_id $ programSelectEvent_soundfontId event) 
        (programSelectEvent_bank event) (programSelectEvent_program event)
    where
        library = soundfontPlayer_fluidSynthLibrary player
        synth = soundfontPlayer_synth player

data SoundfontOutput = SoundfontOutput
    { soundfontOutput_wetChannelLeft :: [CFloat]
    , soundfontOutput_wetChannelRight :: [CFloat]
    , soundfontOutput_dryChannelLeft :: [CFloat]
    , soundfontOutput_dryChannelRight :: [CFloat]
    }

instance Semigroup SoundfontOutput where 
    a <> b = SoundfontOutput
        { soundfontOutput_wetChannelLeft = zipWith (+) (soundfontOutput_wetChannelLeft a) (soundfontOutput_wetChannelLeft b)
        , soundfontOutput_wetChannelRight = zipWith (+) (soundfontOutput_wetChannelRight a) (soundfontOutput_wetChannelRight b)
        , soundfontOutput_dryChannelLeft = zipWith (+) (soundfontOutput_dryChannelLeft a) (soundfontOutput_dryChannelLeft b)
        , soundfontOutput_dryChannelRight = zipWith (+) (soundfontOutput_dryChannelRight a) (soundfontOutput_dryChannelRight b)
        }

instance Monoid SoundfontOutput where
    mempty = SoundfontOutput [] [] [] [] 

process :: SoundfontPlayer -> Int -> IO SoundfontOutput
process player frameCount = do
    let synth = soundfontPlayer_synth player
    let wetChannelCount = 2
    wetLeft <- newArray $ replicate frameCount 0
    wetRight <- newArray $ replicate frameCount 0
    wetBuffers <- newArray [wetLeft, wetRight]
    let dryChannelCount = 2
    dryLeft <- newArray $ replicate frameCount 0
    dryRight <- newArray $ replicate frameCount 0
    dryBuffers <- newArray [dryLeft, dryRight]
    _ <- FS.process 
        (soundfontPlayer_fluidSynthLibrary player) synth 
        (fromIntegral frameCount) 
        (fromIntegral wetChannelCount) wetBuffers 
        (fromIntegral dryChannelCount) dryBuffers
    wetChannels <- peekArray wetChannelCount wetBuffers
    [wetChannelLeft, wetChannelRight] <- traverse (peekArray frameCount) wetChannels
    dryChannels <- peekArray dryChannelCount dryBuffers
    [dryChannelLeft, dryChannelRight] <- traverse (peekArray frameCount) dryChannels
    pure $ SoundfontOutput
        { soundfontOutput_wetChannelLeft = wetChannelLeft
        , soundfontOutput_wetChannelRight = wetChannelRight
        , soundfontOutput_dryChannelLeft = dryChannelLeft
        , soundfontOutput_dryChannelRight = dryChannelRight
        }
