{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ensemble.Soundfont where

import Clap.Interface.Events
import Data.Foldable (traverse_)
import Data.IORef
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
    } deriving (Show)

type FluidSettings = Ptr C'fluid_settings_t
type FluidSynth = Ptr C'fluid_synth_t
type FluidSoundfont = Ptr C'fluid_sfont_t
type FluidPreset = Ptr C'fluid_preset_t

createSoundfontPlayer :: FilePath -> IO SoundfontPlayer
createSoundfontPlayer filePath = do
    library <- openFluidSynthLibrary filePath
    soundfonts <- newIORef mempty
    pure $ SoundfontPlayer
        { soundfontPlayer_fluidSynthLibrary = library
        , soundfontPlayer_soundfonts = soundfonts
        }

createSettings :: SoundfontPlayer -> IO FluidSettings 
createSettings player = do
    let library = soundfontPlayer_fluidSynthLibrary player
    newFluidSettings library

createSynth :: SoundfontPlayer -> FluidSettings -> IO FluidSynth
createSynth player settings = do
    let library = soundfontPlayer_fluidSynthLibrary player
    newFluidSynth library settings

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
    } deriving (Show)

mixSoundfontOutputs :: Int -> [SoundfontOutput] -> SoundfontOutput
mixSoundfontOutputs frameCount outputs = SoundfontOutput 
    { soundfontOutput_wetChannelLeft = mix soundfontOutput_wetChannelLeft
    , soundfontOutput_wetChannelRight = mix soundfontOutput_wetChannelRight
    , soundfontOutput_dryChannelLeft = mix soundfontOutput_dryChannelLeft
    , soundfontOutput_dryChannelRight = mix soundfontOutput_dryChannelRight
    }
    where 
        zeroBuffer = take frameCount $ repeat 0
        mix selector = foldl (\acc cur -> zipWith (+) acc cur) zeroBuffer (selector <$> outputs) 

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

allSynthsOff :: SoundfontPlayer -> [FluidSynth] -> IO ()
allSynthsOff player synths = do
    let library = soundfontPlayer_fluidSynthLibrary player
    traverse_ (\synth -> allNotesOff library synth (-1)) synths