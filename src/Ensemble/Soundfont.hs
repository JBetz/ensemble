{-# LANGUAGE LambdaCase #-}

module Ensemble.Soundfont where

import Clap.Interface.Events
import Control.Monad
import Data.IORef
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Ensemble.Soundfont.FluidSynth.Foreign.Settings
import Ensemble.Soundfont.FluidSynth.Foreign.Synth
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import GHC.Stack

newtype SoundfontId = SoundfontId { unSoundfontId :: Int }
    deriving (Eq, Ord, Show)

data SoundfontPlayer = SoundfontPlayer
    { soundfontPlayer_settings :: FluidSettings
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
    fluidSettings <- c'new_fluid_settings
    fluidSynth <- c'new_fluid_synth fluidSettings 
    soundfonts <- newIORef mempty
    pure $ SoundfontPlayer
        { soundfontPlayer_settings = fluidSettings
        , soundfontPlayer_synth = fluidSynth
        , soundfontPlayer_soundfonts = soundfonts
        }

deleteSoundfontPlayer :: SoundfontPlayer -> IO ()
deleteSoundfontPlayer player = do
    c'delete_fluid_synth (soundfontPlayer_synth player)
    c'delete_fluid_settings (soundfontPlayer_settings player)

loadSoundfont :: SoundfontPlayer -> FilePath -> Bool -> IO Soundfont
loadSoundfont player filePath resetPresets = do
    let synth = soundfontPlayer_synth player
    withCString filePath $ \cFilePath -> do
        soundfontId <- c'fluid_synth_sfload synth cFilePath (fromBool resetPresets)
        handle <- c'fluid_synth_get_sfont_by_id synth soundfontId
        let soundfont = Soundfont 
                { soundfont_id = SoundfontId (fromIntegral soundfontId)
                , soundfont_filePath = filePath
                , soundfont_handle = handle
                }
        modifyIORef' (soundfontPlayer_soundfonts player) $ Map.insert (soundfont_id soundfont) soundfont 
        pure soundfont

processEvent :: SoundfontPlayer -> SoundfontId -> Event -> IO ()
processEvent player _soundfontId = \case
    NoteOn event -> noteOn player (noteEvent_channel event) (noteEvent_key event) (noteEvent_velocity event)
    NoteOff event -> noteOff player (noteEvent_channel event) (noteEvent_key event)
    NoteChoke event -> noteOff player (noteKillEvent_channel event) (noteKillEvent_key event)
    NoteEnd event -> noteOff player (noteKillEvent_channel event) (noteKillEvent_key event)
    NoteExpression _ -> pure ()
    ParamValue _ -> pure ()
    ParamMod _ -> pure ()
    ParamGestureBegin _ -> pure ()
    ParamGestureEnd _ -> pure ()
    Transport _ -> pure ()
    Midi _ -> pure ()
    MidiSysex _ -> pure ()
    Midi2 _ -> pure ()

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
    _ <- c'fluid_synth_process 
        synth (fromIntegral frameCount) 
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

noteOn :: SoundfontPlayer -> Int16 -> Int16 -> Double -> IO ()
noteOn player channel key velocity = do
    let synth = soundfontPlayer_synth player
    void $ c'fluid_synth_noteon synth (fromIntegral channel) (fromIntegral key) (truncate velocity)

noteOff :: SoundfontPlayer -> Int16 -> Int16 -> IO ()
noteOff player channel key = do
    let synth = soundfontPlayer_synth player
    void $ c'fluid_synth_noteoff synth (fromIntegral channel) (fromIntegral key)
