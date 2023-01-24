{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
-- #include <fluidsynth.h>
module Ensemble.Soundfont.FluidSynth.Foreign.Audio where
import Ensemble.Soundfont.FluidSynth.Foreign.Settings
import Ensemble.Soundfont.FluidSynth.Foreign.Synth
import Foreign.Ptr
#strict_import


#opaque_t fluid_audio_driver_t

#ccall new_fluid_audio_driver , Ptr <fluid_settings_t> -> Ptr <fluid_synth_t> -> IO (Ptr <fluid_audio_driver_t>)
#ccall delete_fluid_audio_driver , Ptr <fluid_audio_driver_t> -> IO ()