{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
-- #include <fluidsynth.h>
module Ensemble.Soundfont.FluidSynth.Foreign.Midi where
import Foreign.Ptr
import Ensemble.Soundfont.FluidSynth.Foreign.Settings
#strict_import


#opaque_t fluid_midi_driver_t

#ccall new_fluid_midi_driver , Ptr <fluid_settings_t> -> FunPtr (Ptr () -> Ptr () -> IO ()) -> Ptr () -> IO (Ptr <fluid_midi_driver_t>)
#ccall delete_fluid_midi_driver , Ptr <fluid_midi_driver_t> -> IO ()