module Ensemble.Soundfont.FluidSynth.Foreign.Synth where

import Ensemble.Soundfont.FluidSynth.Foreign.Settings
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


-- foreign import ccall "fluid_synth_cc" c'fluid_synth_cc
--   :: Ptr C'fluid_synth_t -> CInt -> CInt -> CInt -> IO CInt
-- foreign import ccall "&fluid_synth_cc" p'fluid_synth_cc
--   :: FunPtr (Ptr C'fluid_synth_t -> CInt -> CInt -> CInt -> IO CInt)

-- foreign import ccall "fluid_synth_pitch_bend" c'fluid_synth_pitch_bend
--   :: Ptr C'fluid_synth_t -> CInt -> CInt -> IO CInt
-- foreign import ccall "&fluid_synth_pitch_bend" p'fluid_synth_pitch_bend
--   :: FunPtr (Ptr C'fluid_synth_t -> CInt -> CInt -> IO CInt)

-- foreign import ccall "fluid_synth_pitch_wheel_sens" c'fluid_synth_pitch_wheel_sens
--   :: Ptr C'fluid_synth_t -> CInt -> CInt -> IO CInt
-- foreign import ccall "&fluid_synth_pitch_wheel_sens" p'fluid_synth_pitch_wheel_sens
--   :: FunPtr (Ptr C'fluid_synth_t -> CInt -> CInt -> IO CInt)

-- foreign import ccall "fluid_synth_program_change" c'fluid_synth_program_change
--   :: Ptr C'fluid_synth_t -> CInt -> CInt -> IO CInt
-- foreign import ccall "&fluid_synth_program_change" p'fluid_synth_program_change
--   :: FunPtr (Ptr C'fluid_synth_t -> CInt -> CInt -> IO CInt)

-- foreign import ccall "fluid_synth_bank_select" c'fluid_synth_bank_select
--   :: Ptr C'fluid_synth_t -> CInt -> CInt -> IO CInt
-- foreign import ccall "&fluid_synth_bank_select" p'fluid_synth_bank_select
--   :: FunPtr (Ptr C'fluid_synth_t -> CInt -> CInt -> IO CInt)

data C'fluid_sfont_t = C'fluid_sfont_t

foreign import ccall "wrapper" mk'fluid_synth_sfload :: (Ptr C'fluid_synth_t -> CString -> CBool -> IO CInt) -> IO (FunPtr (Ptr C'fluid_synth_t -> CString -> CBool -> IO CInt))
foreign import ccall "dynamic" mK'fluid_synth_sfload :: FunPtr (Ptr C'fluid_synth_t -> CString -> CBool -> IO CInt) -> (Ptr C'fluid_synth_t -> CString -> CBool -> IO CInt)


-- foreign import ccall "fluid_synth_sfreload" c'fluid_synth_sfreload
--   :: Ptr C'fluid_synth_t -> CUInt -> IO CInt
-- foreign import ccall "&fluid_synth_sfreload" p'fluid_synth_sfreload
--   :: FunPtr (Ptr C'fluid_synth_t -> CUInt -> IO CInt)

-- foreign import ccall "fluid_synth_sfunload" c'fluid_synth_sfunload
--   :: Ptr C'fluid_synth_t -> CUInt -> CInt -> IO CInt
-- foreign import ccall "&fluid_synth_sfunload" p'fluid_synth_sfunload
--   :: FunPtr (Ptr C'fluid_synth_t -> CUInt -> CInt -> IO CInt)

-- foreign import ccall "fluid_synth_get_sfont" c'fluid_synth_get_sfont
--   :: Ptr C'fluid_synth_t -> CUInt -> IO (Ptr C'fluid_sfont_t)
-- foreign import ccall "&fluid_synth_get_sfont" p'fluid_synth_get_sfont
--   :: FunPtr (Ptr C'fluid_synth_t -> CUInt -> IO (Ptr C'fluid_sfont_t))

foreign import ccall "wrapper" mk'fluid_synth_get_sfont_by_id :: (Ptr C'fluid_synth_t -> CInt -> IO (Ptr C'fluid_sfont_t)) -> IO (FunPtr (Ptr C'fluid_synth_t -> CInt -> IO (Ptr C'fluid_sfont_t)))
foreign import ccall "dynamic" mK'fluid_synth_get_sfont_by_id :: FunPtr (Ptr C'fluid_synth_t -> CInt -> IO (Ptr C'fluid_sfont_t)) -> (Ptr C'fluid_synth_t -> CInt -> IO (Ptr C'fluid_sfont_t))

-- foreign import ccall "fluid_synth_get_sfont_by_name" c'fluid_synth_get_sfont_by_name
--   :: Ptr C'fluid_synth_t -> CString -> IO (Ptr C'fluid_sfont_t)
-- foreign import ccall "&fluid_synth_get_sfont_by_name" p'fluid_synth_get_sfont_by_name
--   :: FunPtr (Ptr C'fluid_synth_t -> CString -> IO (Ptr C'fluid_sfont_t))

-- foreign import ccall "fluid_synth_nwrite_float" c'fluid_synth_nwrite_float
--   :: Ptr C'fluid_synth_t -> CInt -> Ptr (Ptr CFloat) -> Ptr (Ptr CFloat) -> Ptr (Ptr CFloat) -> Ptr (Ptr CFloat) -> IO CInt
-- foreign import ccall "&fluid_synth_nwrite_float" p'fluid_synth_nwrite_float
--   :: FunPtr (Ptr C'fluid_synth_t -> CInt -> Ptr (Ptr CFloat) -> Ptr (Ptr CFloat) -> Ptr (Ptr CFloat) -> Ptr (Ptr CFloat) -> IO CInt)

foreign import ccall "wrapper" mk'fluid_synth_process :: (Ptr C'fluid_synth_t -> CInt -> CInt -> Ptr (Ptr CFloat) -> CInt -> Ptr (Ptr CFloat) -> IO CInt) -> IO (FunPtr (Ptr C'fluid_synth_t -> CInt -> CInt -> Ptr (Ptr CFloat) -> CInt -> Ptr (Ptr CFloat) -> IO CInt))
foreign import ccall "dynamic" mK'fluid_synth_process :: FunPtr (Ptr C'fluid_synth_t -> CInt -> CInt -> Ptr (Ptr CFloat) -> CInt -> Ptr (Ptr CFloat) -> IO CInt) -> (Ptr C'fluid_synth_t -> CInt -> CInt -> Ptr (Ptr CFloat) -> CInt -> Ptr (Ptr CFloat) -> IO CInt)


-- foreign import ccall "fluid_synth_write_float" c'fluid_synth_write_float
--   :: Ptr C'fluid_synth_t -> CInt -> Ptr () -> CInt -> CInt -> Ptr () -> CInt -> CInt -> IO CInt
-- foreign import ccall "&fluid_synth_write_float" p'fluid_synth_write_float
--   :: FunPtr (Ptr C'fluid_synth_t -> CInt -> Ptr () -> CInt -> CInt -> Ptr () -> CInt -> CInt -> IO CInt)

-- foreign import ccall "fluid_synth_write_s16" c'fluid_synth_write_s16
--   :: Ptr C'fluid_synth_t -> CInt -> Ptr () -> CInt -> CInt -> Ptr () -> CInt -> CInt -> IO CInt
-- foreign import ccall "&fluid_synth_write_s16" p'fluid_synth_write_s16
--   :: FunPtr (Ptr C'fluid_synth_t -> CInt -> Ptr () -> CInt -> CInt -> Ptr () -> CInt -> CInt -> IO CInt)
