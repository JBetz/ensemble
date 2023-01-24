{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <fluidsynth.h>
module Ensemble.Soundfont.FluidSynth.Foreign.Settings where
import Foreign.Ptr
#strict_import


#opaque_t fluid_settings_t

#ccall new_fluid_settings , IO (Ptr <fluid_settings_t>)
#ccall delete_fluid_settings , Ptr <fluid_settings_t> -> IO ()
#ccall fluid_settings_get_type , Ptr <fluid_settings_t> -> CString -> IO CInt
#ccall fluid_settings_setstr , Ptr <fluid_settings_t> -> CString -> CString -> IO CInt
#ccall fluid_settings_getstr_default , Ptr <fluid_settings_t> -> CString -> Ptr CString -> IO CInt
#ccall fluid_settings_setnum , Ptr <fluid_settings_t> -> CString -> CDouble -> IO CInt
#ccall fluid_settings_getnum , Ptr <fluid_settings_t> -> CString -> Ptr CDouble -> IO CInt
#ccall fluid_settings_setint , Ptr <fluid_settings_t> -> CString -> CInt -> IO CInt
#ccall fluid_settings_getint , Ptr <fluid_settings_t> -> CString -> Ptr CInt -> IO CInt
#callback_t fluid_settings_foreach_t , Ptr () -> CString -> CInt -> IO ()
#ccall fluid_settings_foreach , Ptr <fluid_settings_t> -> Ptr () -> FunPtr fluid_settings_foreach_t -> IO ()
#callback_t fluid_settings_foreach_option_t , Ptr () -> CString -> CString -> IO ()
#ccall fluid_settings_foreach_option , Ptr <fluid_settings_t> -> CString -> Ptr () -> FunPtr fluid_settings_foreach_option_t -> IO ()

#num FLUID_NO_TYPE
#num FLUID_NUM_TYPE
#num FLUID_INT_TYPE
#num FLUID_STR_TYPE
#num FLUID_SET_TYPE