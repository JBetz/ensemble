{-# LANGUAGE CPP #-}

module Ensemble.Soundfont.FluidSynth.Library 
    ( FluidSynthLibrary
    , openFluidSynthLibrary
    , closeFluidSynthLibrary
    , withFluidSynthLibrary
    , newFluidSettings
    , newFluidSynth
    , deleteFluidSettings
    , deleteFluidSynth
    , noteOn
    , noteOff
    , allNotesOff
    , allSoundsOff
    , programSelect
    , sfLoad
    , getSfontById
    , sfontIterationStart
    , sfontIterationNext
    , presetGetName
    , presetGetBankNum
    , presetGetNum
    , process
    , setIntSetting
    , setNumSetting
    ) where

import Data.Int
#ifdef WINDOWS
import Ensemble.Soundfont.FluidSynth.Library.Windows
#else
import Ensemble.Soundfont.FluidSynth.Library.POSIX
#endif
import Ensemble.Soundfont.FluidSynth.Foreign.Settings
import Ensemble.Soundfont.FluidSynth.Foreign.SoundFonts
import Ensemble.Soundfont.FluidSynth.Foreign.Synth
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr

withFluidSynthLibrary :: FilePath -> (FluidSynthLibrary -> IO a) -> IO a
withFluidSynthLibrary filePath f = do
    fluidSynthLibrary <- openFluidSynthLibrary filePath
    result <- f fluidSynthLibrary
    closeFluidSynthLibrary fluidSynthLibrary
    pure result


newFluidSettings :: FluidSynthLibrary -> IO (Ptr C'fluid_settings_t)
newFluidSettings library = do
    f <- lookupProcedure library "new_fluid_settings"
    mK'new_fluid_settings f

setIntSetting :: FluidSynthLibrary -> Ptr C'fluid_settings_t -> String -> Int -> IO ()
setIntSetting library settings name value = do
    f <- lookupProcedure library "fluid_settings_setint"
    withCString name $ \cName -> do
        _result <- mK'fluid_settings_setint f settings cName (fromIntegral value)
        pure ()

setNumSetting :: FluidSynthLibrary -> Ptr C'fluid_settings_t -> String -> Double -> IO ()
setNumSetting library settings name value = do
    f <- lookupProcedure library "fluid_settings_setnum"
    withCString name $ \cName -> do
        _result <- mK'fluid_settings_setnum f settings cName (CDouble value)
        pure ()

newFluidSynth :: FluidSynthLibrary -> Ptr C'fluid_settings_t -> IO (Ptr C'fluid_synth_t)
newFluidSynth library settings = do
    f <- lookupProcedure library "new_fluid_synth"
    mK'new_fluid_synth f settings

deleteFluidSettings :: FluidSynthLibrary -> Ptr C'fluid_settings_t -> IO ()
deleteFluidSettings library settings = do
    f <- lookupProcedure library "delete_fluid_settings"
    mK'delete_fluid_settings f settings

deleteFluidSynth :: FluidSynthLibrary -> Ptr C'fluid_synth_t -> IO ()
deleteFluidSynth library synth = do
    f <- lookupProcedure library "delete_fluid_synth"
    mK'delete_fluid_synth f synth

noteOn :: FluidSynthLibrary -> Ptr C'fluid_synth_t -> Int16 -> Int16 -> Double -> IO ()
noteOn library synth channel key velocity = do
    f <- lookupProcedure library "fluid_synth_noteon"
    _result <- mK'fluid_synth_noteon f synth (fromIntegral channel) (fromIntegral key) (truncate velocity)
    pure ()

noteOff :: FluidSynthLibrary -> Ptr C'fluid_synth_t -> Int16 -> Int16 -> IO ()
noteOff library synth channel key = do
    f <- lookupProcedure library "fluid_synth_noteoff"
    _result <- mK'fluid_synth_noteoff f synth (fromIntegral channel) (fromIntegral key)
    pure ()

allNotesOff :: FluidSynthLibrary -> Ptr C'fluid_synth_t -> Int16 -> IO ()
allNotesOff library synth channel = do
    f <- lookupProcedure library "fluid_synth_all_notes_off"
    _result <- mK'fluid_synth_all_notes_off f synth (fromIntegral channel)
    pure ()

allSoundsOff :: FluidSynthLibrary -> Ptr C'fluid_synth_t -> Int16 -> IO ()
allSoundsOff library synth channel = do
    f <- lookupProcedure library "fluid_synth_all_sounds_off"
    _result <- mK'fluid_synth_all_sounds_off f synth (fromIntegral channel)
    pure ()

programSelect :: FluidSynthLibrary -> Ptr C'fluid_synth_t -> Int16 -> Int -> Int16 -> Int16 -> IO ()
programSelect library synth channel soundfontId bank program = do
    f <- lookupProcedure library "fluid_synth_program_select"
    _result <- mK'fluid_synth_program_select f synth (fromIntegral channel) (fromIntegral soundfontId) (fromIntegral bank) (fromIntegral program)
    pure ()

sfLoad  :: FluidSynthLibrary -> Ptr C'fluid_synth_t -> FilePath -> Bool -> IO Int
sfLoad library synth filePath resetPresets = do
    f <- lookupProcedure library "fluid_synth_sfload"
    withCString filePath $ \cFilePath -> do
        result <- mK'fluid_synth_sfload f synth cFilePath (fromBool resetPresets)
        pure $ fromIntegral result

getSfontById :: FluidSynthLibrary -> Ptr C'fluid_synth_t -> Int -> IO (Ptr C'fluid_sfont_t)
getSfontById library synth soundfontId = do
    f <- lookupProcedure library "fluid_synth_get_sfont_by_id"
    mK'fluid_synth_get_sfont_by_id f synth (fromIntegral soundfontId)
    
sfontIterationStart :: FluidSynthLibrary -> Ptr C'fluid_sfont_t -> IO ()
sfontIterationStart library soundfont = do
    f <- lookupProcedure library "fluid_sfont_iteration_start"
    mK'fluid_sfont_iteration_start f soundfont

sfontIterationNext :: FluidSynthLibrary -> Ptr C'fluid_sfont_t -> IO (Ptr C'fluid_preset_t)
sfontIterationNext library soundfont = do
    f <- lookupProcedure library "fluid_sfont_iteration_next"
    mK'fluid_sfont_iteration_next f soundfont


presetGetName :: FluidSynthLibrary -> Ptr C'fluid_preset_t -> IO String
presetGetName library preset = do
    f <- lookupProcedure library "fluid_preset_get_name"
    cName <- mK'fluid_preset_get_name f preset
    peekCString cName
    
presetGetBankNum :: FluidSynthLibrary -> Ptr C'fluid_preset_t -> IO Int
presetGetBankNum library preset = do
    f <- lookupProcedure library "fluid_preset_get_banknum"
    fromIntegral <$> mK'fluid_preset_get_banknum f preset

presetGetNum :: FluidSynthLibrary -> Ptr C'fluid_preset_t -> IO Int
presetGetNum library preset = do
    f <- lookupProcedure library "fluid_preset_get_num"
    fromIntegral <$> mK'fluid_preset_get_num f preset


process ::  FluidSynthLibrary -> Ptr C'fluid_synth_t -> CInt -> CInt -> Ptr (Ptr CFloat) -> CInt -> Ptr (Ptr CFloat) -> IO CInt
process library synth frameCount wetChannelCount wetBuffers dryChannelCount dryBuffers = do
    f <- lookupProcedure library "fluid_synth_process"
    mK'fluid_synth_process f synth frameCount wetChannelCount wetBuffers dryChannelCount dryBuffers