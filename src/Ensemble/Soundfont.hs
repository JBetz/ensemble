{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Ensemble.Soundfont where

import Clap.Interface.Events
import Control.Exception
import Data.IORef
import Data.Int
import Data.List (find)
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
    , soundfont_presets :: [SoundfontPreset]
    }

data SoundfontException
    = SoundfontPlayerNotInitialized
    deriving (Show)

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
    soundfonts <- newIORef mempty
    pure $ SoundfontPlayer
        { soundfontPlayer_fluidSynthLibrary = library
        , soundfontPlayer_settings = settings
        , soundfontPlayer_soundfonts = soundfonts
        }

createSynth :: SoundfontPlayer -> IO FluidSynth
createSynth player = do
    let library = soundfontPlayer_fluidSynthLibrary player
    newFluidSynth library (soundfontPlayer_settings player)

deleteSoundfontPlayer :: SoundfontPlayer -> IO ()
deleteSoundfontPlayer player = do
    let library = soundfontPlayer_fluidSynthLibrary player
    deleteFluidSettings library (soundfontPlayer_settings player)

lookupSoundfont :: SoundfontPlayer -> FilePath -> IO (Maybe Soundfont)
lookupSoundfont player filePath = do
    soundfonts <- readIORef $ soundfontPlayer_soundfonts player
    pure $ find (\soundfont -> soundfont_filePath soundfont == filePath) soundfonts

loadSoundfont :: SoundfontPlayer -> FluidSynth -> FilePath -> Bool -> IO Soundfont
loadSoundfont player synth filePath resetPresets = do
    let library = soundfontPlayer_fluidSynthLibrary player
    soundfontId <- sfLoad library synth filePath resetPresets
    handle <- getSfontById library synth soundfontId
    presets <- loadSoundfontPresets player handle
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

loadSoundfontPresets :: SoundfontPlayer -> FluidSoundfont -> IO [SoundfontPreset]
loadSoundfontPresets player soundfontHandle = do
    let library = soundfontPlayer_fluidSynthLibrary player
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
    traverse createPreset presetPtrs
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


processEvent :: SoundfontPlayer -> FluidSynth -> ClapEvent -> IO ()
processEvent player synth = \case
    ClapEvent_NoteOn event -> noteOn library synth (noteEvent_channel event) (noteEvent_key event) (noteEvent_velocity event)
    ClapEvent_NoteOff event -> noteOff library synth (noteEvent_channel event) (noteEvent_key event)
    where
        library = soundfontPlayer_fluidSynthLibrary player

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

process :: SoundfontPlayer -> FluidSynth -> Int -> IO SoundfontOutput
process player synth frameCount = do
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
