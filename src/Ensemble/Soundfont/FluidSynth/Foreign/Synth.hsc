{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <fluidsynth.h>
module Ensemble.Soundfont.FluidSynth.Foreign.Synth where
import Ensemble.Soundfont.FluidSynth.Foreign.Settings
import Foreign.Ptr
#strict_import


#opaque_t fluid_synth_t

#ccall new_fluid_synth , Ptr <fluid_settings_t> -> IO (Ptr <fluid_synth_t>)
#ccall delete_fluid_synth , Ptr <fluid_synth_t> -> IO ()
#ccall fluid_synth_noteon , Ptr <fluid_synth_t> -> CInt -> CInt -> CInt -> IO CInt
#ccall fluid_synth_noteoff , Ptr <fluid_synth_t> -> CInt -> CInt -> IO CInt
#ccall fluid_synth_cc , Ptr <fluid_synth_t> -> CInt -> CInt -> CInt -> IO CInt
#ccall fluid_synth_pitch_bend , Ptr <fluid_synth_t> -> CInt -> CInt -> IO CInt
#ccall fluid_synth_pitch_wheel_sens , Ptr <fluid_synth_t> -> CInt -> CInt -> IO CInt
#ccall fluid_synth_program_change , Ptr <fluid_synth_t> -> CInt -> CInt -> IO CInt
#ccall fluid_synth_bank_select , Ptr <fluid_synth_t> -> CInt -> CInt -> IO CInt

#opaque_t fluid_sfont_t

#ccall fluid_synth_sfload , Ptr <fluid_synth_t> -> CString -> CInt -> IO CInt
#ccall fluid_synth_sfreload , Ptr <fluid_synth_t> -> CUInt -> IO CInt
#ccall fluid_synth_sfunload , Ptr <fluid_synth_t> -> CUInt -> CInt -> IO CInt
#ccall fluid_synth_get_sfont , Ptr <fluid_synth_t> -> CUInt -> IO (Ptr <fluid_sfont_t)
#ccall fluid_synth_get_sfont_by_id , Ptr <fluid_synth_t> -> CInt -> IO (Ptr <fluid_sfont_t)
#ccall fluid_synth_get_sfont_by_name , Ptr <fluid_synth_t> -> CString -> IO (Ptr <fluid_sfont_t)

#ccall fluid_synth_nwrite_float , Ptr <fluid_synth_t> -> CInt -> Ptr (Ptr CFloat) -> Ptr (Ptr CFloat) -> Ptr (Ptr CFloat) -> Ptr (Ptr CFloat) -> IO CInt
#ccall fluid_synth_process , Ptr <fluid_synth_t> -> CInt -> CInt -> Ptr (Ptr CFloat) -> CInt -> Ptr (Ptr CFloat) -> IO CInt
#ccall fluid_synth_write_float , Ptr <fluid_synth_t> -> CInt -> Ptr () -> CInt -> CInt -> Ptr () -> CInt -> CInt -> IO CInt
#ccall fluid_synth_write_s16 , Ptr <fluid_synth_t> -> CInt -> Ptr () -> CInt -> CInt -> Ptr () -> CInt -> CInt -> IO CInt