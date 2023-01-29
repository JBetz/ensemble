{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Ensemble.Soundfont where

import Clap.Interface.Events
import Data.Aeson
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Ensemble.Soundfont.FluidSynth.Library as FS
import Ensemble.Soundfont.FluidSynth.Foreign.Settings
import Ensemble.Soundfont.FluidSynth.Foreign.Synth
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import GHC.Generics
import GHC.Stack

newtype SoundfontId = SoundfontId { unSoundfontId :: Int }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data SoundfontPlayer = SoundfontPlayer
    { soundfontPlayer_fluidSynthLibrary :: FluidSynthLibrary
    , soundfontPlayer_settings :: FluidSettings
    , soundfontPlayer_synth :: FluidSynth
    , soundfontPlayer_soundfonts :: IORef (Map SoundfontId Soundfont)
    }

data Soundfont = Soundfont
    { soundfont_id :: SoundfontId
    , soundfont_filePath :: FilePath 
    , soundfont_handle :: FluidSoundfont
    } deriving (Show)

type FluidSettings = Ptr C'fluid_settings_t
type FluidSynth = Ptr C'fluid_synth_t
type FluidSoundfont = Ptr C'fluid_sfont_t

withSoundfontPlayer :: HasCallStack => (SoundfontPlayer -> IO a) -> IO a
withSoundfontPlayer f = do
    player <- createSoundfontPlayer
    result <- f player
    deleteSoundfontPlayer player
    pure result

createSoundfontPlayer :: IO SoundfontPlayer
createSoundfontPlayer = do
    library <- openFluidSynthLibrary "fluidsynth.dll"
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
    let soundfont = Soundfont 
            { soundfont_id = SoundfontId soundfontId
            , soundfont_filePath = filePath
            , soundfont_handle = handle
            }
    modifyIORef' (soundfontPlayer_soundfonts player) $ Map.insert (soundfont_id soundfont) soundfont 
    pure soundfont

processEvent :: SoundfontPlayer -> SoundfontId -> Event -> IO ()
processEvent player _soundfontId = \case
    NoteOn event -> noteOn library synth (noteEvent_channel event) (noteEvent_key event) (noteEvent_velocity event)
    NoteOff event -> noteOff library synth (noteEvent_channel event) (noteEvent_key event)
    NoteChoke event -> noteOff library synth (noteKillEvent_channel event) (noteKillEvent_key event)
    NoteEnd event -> noteOff library synth (noteKillEvent_channel event) (noteKillEvent_key event)
    NoteExpression _ -> pure ()
    ParamValue _ -> pure ()
    ParamMod _ -> pure ()
    ParamGestureBegin _ -> pure ()
    ParamGestureEnd _ -> pure ()
    Transport _ -> pure ()
    Midi _ -> pure ()
    MidiSysex _ -> pure ()
    Midi2 _ -> pure ()
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
