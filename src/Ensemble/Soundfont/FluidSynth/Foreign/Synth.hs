module Ensemble.Soundfont.FluidSynth.Foreign.Synth where

import Ensemble.Soundfont.FluidSynth.Foreign.Settings
import Ensemble.Soundfont.FluidSynth.Foreign.SoundFonts
import Foreign.Ptr (Ptr,FunPtr)
import Foreign.C.Types
import Foreign.C.String (CString)

data C'fluid_synth_t = C'fluid_synth_t

foreign import ccall "wrapper" mk'new_fluid_synth :: (Ptr C'fluid_settings_t -> IO (Ptr C'fluid_synth_t)) -> IO (FunPtr (Ptr C'fluid_settings_t -> IO (Ptr C'fluid_synth_t)))
foreign import ccall "dynamic" mK'new_fluid_synth :: FunPtr (Ptr C'fluid_settings_t -> IO (Ptr C'fluid_synth_t)) -> (Ptr C'fluid_settings_t -> IO (Ptr C'fluid_synth_t))

foreign import ccall "wrapper" mk'delete_fluid_synth :: (Ptr C'fluid_synth_t -> IO ()) -> IO (FunPtr (Ptr C'fluid_synth_t -> IO ()))
foreign import ccall "dynamic" mK'delete_fluid_synth :: FunPtr (Ptr C'fluid_synth_t -> IO ()) -> (Ptr C'fluid_synth_t -> IO ())

foreign import ccall "wrapper" mk'fluid_synth_noteon :: (Ptr C'fluid_synth_t -> CInt -> CInt -> CInt -> IO CInt) -> IO (FunPtr (Ptr C'fluid_synth_t -> CInt -> CInt -> CInt -> IO CInt))
foreign import ccall "dynamic" mK'fluid_synth_noteon :: FunPtr (Ptr C'fluid_synth_t -> CInt -> CInt -> CInt -> IO CInt) -> (Ptr C'fluid_synth_t -> CInt -> CInt -> CInt -> IO CInt)

foreign import ccall "wrapper" mk'fluid_synth_noteoff :: (Ptr C'fluid_synth_t -> CInt -> CInt -> IO CInt) -> IO (FunPtr (Ptr C'fluid_synth_t -> CInt -> CInt -> IO CInt))
foreign import ccall "dynamic" mK'fluid_synth_noteoff :: FunPtr (Ptr C'fluid_synth_t -> CInt -> CInt -> IO CInt) -> (Ptr C'fluid_synth_t -> CInt -> CInt -> IO CInt)

foreign import ccall "wrapper" mk'fluid_synth_sfload :: (Ptr C'fluid_synth_t -> CString -> CBool -> IO CInt) -> IO (FunPtr (Ptr C'fluid_synth_t -> CString -> CBool -> IO CInt))
foreign import ccall "dynamic" mK'fluid_synth_sfload :: FunPtr (Ptr C'fluid_synth_t -> CString -> CBool -> IO CInt) -> (Ptr C'fluid_synth_t -> CString -> CBool -> IO CInt)

foreign import ccall "wrapper" mk'fluid_synth_get_sfont_by_id :: (Ptr C'fluid_synth_t -> CInt -> IO (Ptr C'fluid_sfont_t)) -> IO (FunPtr (Ptr C'fluid_synth_t -> CInt -> IO (Ptr C'fluid_sfont_t)))
foreign import ccall "dynamic" mK'fluid_synth_get_sfont_by_id :: FunPtr (Ptr C'fluid_synth_t -> CInt -> IO (Ptr C'fluid_sfont_t)) -> (Ptr C'fluid_synth_t -> CInt -> IO (Ptr C'fluid_sfont_t))

foreign import ccall "wrapper" mk'fluid_synth_process :: (Ptr C'fluid_synth_t -> CInt -> CInt -> Ptr (Ptr CFloat) -> CInt -> Ptr (Ptr CFloat) -> IO CInt) -> IO (FunPtr (Ptr C'fluid_synth_t -> CInt -> CInt -> Ptr (Ptr CFloat) -> CInt -> Ptr (Ptr CFloat) -> IO CInt))
foreign import ccall "dynamic" mK'fluid_synth_process :: FunPtr (Ptr C'fluid_synth_t -> CInt -> CInt -> Ptr (Ptr CFloat) -> CInt -> Ptr (Ptr CFloat) -> IO CInt) -> (Ptr C'fluid_synth_t -> CInt -> CInt -> Ptr (Ptr CFloat) -> CInt -> Ptr (Ptr CFloat) -> IO CInt)