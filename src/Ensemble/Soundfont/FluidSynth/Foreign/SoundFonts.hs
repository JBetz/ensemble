module Ensemble.Soundfont.FluidSynth.Foreign.SoundFonts where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

data C'fluid_sfont_t = C'fluid_sfont_t
data C'fluid_preset_t = C'fluid_preset_t

foreign import ccall "wrapper" mk'fluid_sfont_iteration_start :: (Ptr C'fluid_sfont_t -> IO ()) -> IO (FunPtr (Ptr C'fluid_sfont_t -> IO ())) 
foreign import ccall "dynamic" mK'fluid_sfont_iteration_start :: FunPtr (Ptr C'fluid_sfont_t -> IO ()) -> (Ptr C'fluid_sfont_t -> IO ())

foreign import ccall "wrapper" mk'fluid_sfont_iteration_next :: (Ptr C'fluid_sfont_t -> IO (Ptr C'fluid_preset_t)) -> IO (FunPtr (Ptr C'fluid_sfont_t -> IO (Ptr C'fluid_preset_t)))
foreign import ccall "dynamic" mK'fluid_sfont_iteration_next :: FunPtr (Ptr C'fluid_sfont_t -> IO (Ptr C'fluid_preset_t)) -> (Ptr C'fluid_sfont_t -> IO (Ptr C'fluid_preset_t))

foreign import ccall "wrapper" mk'fluid_preset_get_name :: (Ptr C'fluid_preset_t -> IO CString) -> IO (FunPtr (Ptr C'fluid_preset_t -> IO CString))
foreign import ccall "dynamic" mK'fluid_preset_get_name :: FunPtr (Ptr C'fluid_preset_t -> IO CString) -> (Ptr C'fluid_preset_t -> IO CString)

foreign import ccall "wrapper" mk'fluid_preset_get_banknum :: (Ptr C'fluid_preset_t -> IO CInt) -> IO (FunPtr (Ptr C'fluid_preset_t -> IO CInt))
foreign import ccall "dynamic" mK'fluid_preset_get_banknum :: FunPtr (Ptr C'fluid_preset_t -> IO CInt) -> (Ptr C'fluid_preset_t -> IO CInt)

foreign import ccall "wrapper" mk'fluid_preset_get_num :: (Ptr C'fluid_preset_t -> IO CInt) -> IO (FunPtr (Ptr C'fluid_preset_t -> IO CInt))
foreign import ccall "dynamic" mK'fluid_preset_get_num :: FunPtr (Ptr C'fluid_preset_t -> IO CInt) -> (Ptr C'fluid_preset_t -> IO CInt)
