{-# LINE 1 "src/Ensemble/Soundfont/FluidSynth/Foreign/Settings.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Ensemble.Soundfont.FluidSynth.Foreign.Settings where
import Foreign.Ptr
import Foreign.Ptr (Ptr,FunPtr,plusPtr)
import Foreign.Ptr (wordPtrToPtr,castPtrToFunPtr)
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String (CString,CStringLen,CWString,CWStringLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray,pokeArray)
import Data.Int
import Data.Word

{-# LINE 7 "src/Ensemble/Soundfont/FluidSynth/Foreign/Settings.hsc" #-}


data C'fluid_settings_t = C'fluid_settings_t

foreign import ccall "wrapper" mk'new_fluid_settings :: IO (Ptr C'fluid_settings_t) -> IO (FunPtr (IO (Ptr C'fluid_settings_t))) 
foreign import ccall "dynamic" mK'new_fluid_settings :: FunPtr (IO (Ptr C'fluid_settings_t)) -> IO (Ptr C'fluid_settings_t)

foreign import ccall "wrapper" mk'delete_fluid_settings :: (Ptr C'fluid_settings_t -> IO ()) -> IO (FunPtr (Ptr C'fluid_settings_t -> IO ()))
foreign import ccall "dynamic" mK'delete_fluid_settings :: FunPtr (Ptr C'fluid_settings_t -> IO ()) -> (Ptr C'fluid_settings_t -> IO ())


-- {-# LINE 13 "src/Ensemble/Soundfont/FluidSynth/Foreign/Settings.hsc" #-}
-- foreign import ccall "fluid_settings_get_type" c'fluid_settings_get_type
--   :: Ptr C'fluid_settings_t -> CString -> IO CInt
-- foreign import ccall "&fluid_settings_get_type" p'fluid_settings_get_type
--   :: FunPtr (Ptr C'fluid_settings_t -> CString -> IO CInt)

-- {-# LINE 14 "src/Ensemble/Soundfont/FluidSynth/Foreign/Settings.hsc" #-}
-- foreign import ccall "fluid_settings_setstr" c'fluid_settings_setstr
--   :: Ptr C'fluid_settings_t -> CString -> CString -> IO CInt
-- foreign import ccall "&fluid_settings_setstr" p'fluid_settings_setstr
--   :: FunPtr (Ptr C'fluid_settings_t -> CString -> CString -> IO CInt)

-- {-# LINE 15 "src/Ensemble/Soundfont/FluidSynth/Foreign/Settings.hsc" #-}
-- foreign import ccall "fluid_settings_getstr_default" c'fluid_settings_getstr_default
--   :: Ptr C'fluid_settings_t -> CString -> Ptr CString -> IO CInt
-- foreign import ccall "&fluid_settings_getstr_default" p'fluid_settings_getstr_default
--   :: FunPtr (Ptr C'fluid_settings_t -> CString -> Ptr CString -> IO CInt)

-- {-# LINE 16 "src/Ensemble/Soundfont/FluidSynth/Foreign/Settings.hsc" #-}
-- foreign import ccall "fluid_settings_setnum" c'fluid_settings_setnum
--   :: Ptr C'fluid_settings_t -> CString -> CDouble -> IO CInt
-- foreign import ccall "&fluid_settings_setnum" p'fluid_settings_setnum
--   :: FunPtr (Ptr C'fluid_settings_t -> CString -> CDouble -> IO CInt)

-- {-# LINE 17 "src/Ensemble/Soundfont/FluidSynth/Foreign/Settings.hsc" #-}
-- foreign import ccall "fluid_settings_getnum" c'fluid_settings_getnum
--   :: Ptr C'fluid_settings_t -> CString -> Ptr CDouble -> IO CInt
-- foreign import ccall "&fluid_settings_getnum" p'fluid_settings_getnum
--   :: FunPtr (Ptr C'fluid_settings_t -> CString -> Ptr CDouble -> IO CInt)

-- {-# LINE 18 "src/Ensemble/Soundfont/FluidSynth/Foreign/Settings.hsc" #-}
-- foreign import ccall "fluid_settings_setint" c'fluid_settings_setint
--   :: Ptr C'fluid_settings_t -> CString -> CInt -> IO CInt
-- foreign import ccall "&fluid_settings_setint" p'fluid_settings_setint
--   :: FunPtr (Ptr C'fluid_settings_t -> CString -> CInt -> IO CInt)

-- {-# LINE 19 "src/Ensemble/Soundfont/FluidSynth/Foreign/Settings.hsc" #-}
-- foreign import ccall "fluid_settings_getint" c'fluid_settings_getint
--   :: Ptr C'fluid_settings_t -> CString -> Ptr CInt -> IO CInt
-- foreign import ccall "&fluid_settings_getint" p'fluid_settings_getint
--   :: FunPtr (Ptr C'fluid_settings_t -> CString -> Ptr CInt -> IO CInt)

-- {-# LINE 20 "src/Ensemble/Soundfont/FluidSynth/Foreign/Settings.hsc" #-}
-- type C'fluid_settings_foreach_t = FunPtr (Ptr () -> CString -> CInt -> IO ())
-- foreign import ccall "wrapper" mk'fluid_settings_foreach_t
--   :: (Ptr () -> CString -> CInt -> IO ()) -> IO C'fluid_settings_foreach_t
-- foreign import ccall "dynamic" mK'fluid_settings_foreach_t
--   :: C'fluid_settings_foreach_t -> (Ptr () -> CString -> CInt -> IO ())

-- {-# LINE 21 "src/Ensemble/Soundfont/FluidSynth/Foreign/Settings.hsc" #-}
-- foreign import ccall "fluid_settings_foreach" c'fluid_settings_foreach
--   :: Ptr C'fluid_settings_t -> Ptr () -> FunPtr fluid_settings_foreach_t -> IO ()
-- foreign import ccall "&fluid_settings_foreach" p'fluid_settings_foreach
--   :: FunPtr (Ptr C'fluid_settings_t -> Ptr () -> FunPtr fluid_settings_foreach_t -> IO ())

-- {-# LINE 22 "src/Ensemble/Soundfont/FluidSynth/Foreign/Settings.hsc" #-}
-- type C'fluid_settings_foreach_option_t = FunPtr (Ptr () -> CString -> CString -> IO ())
-- foreign import ccall "wrapper" mk'fluid_settings_foreach_option_t
--   :: (Ptr () -> CString -> CString -> IO ()) -> IO C'fluid_settings_foreach_option_t
-- foreign import ccall "dynamic" mK'fluid_settings_foreach_option_t
--   :: C'fluid_settings_foreach_option_t -> (Ptr () -> CString -> CString -> IO ())

-- {-# LINE 23 "src/Ensemble/Soundfont/FluidSynth/Foreign/Settings.hsc" #-}
-- foreign import ccall "fluid_settings_foreach_option" c'fluid_settings_foreach_option
--   :: Ptr C'fluid_settings_t -> CString -> Ptr () -> FunPtr fluid_settings_foreach_option_t -> IO ()
-- foreign import ccall "&fluid_settings_foreach_option" p'fluid_settings_foreach_option
--   :: FunPtr (Ptr C'fluid_settings_t -> CString -> Ptr () -> FunPtr fluid_settings_foreach_option_t -> IO ())

-- {-# LINE 24 "src/Ensemble/Soundfont/FluidSynth/Foreign/Settings.hsc" #-}

-- c'FLUID_NO_TYPE = -1
-- c'FLUID_NO_TYPE :: (Num a) => a

-- {-# LINE 26 "src/Ensemble/Soundfont/FluidSynth/Foreign/Settings.hsc" #-}
-- c'FLUID_NUM_TYPE = 0
-- c'FLUID_NUM_TYPE :: (Num a) => a

-- {-# LINE 27 "src/Ensemble/Soundfont/FluidSynth/Foreign/Settings.hsc" #-}
-- c'FLUID_INT_TYPE = 1
-- c'FLUID_INT_TYPE :: (Num a) => a

-- {-# LINE 28 "src/Ensemble/Soundfont/FluidSynth/Foreign/Settings.hsc" #-}
-- c'FLUID_STR_TYPE = 2
-- c'FLUID_STR_TYPE :: (Num a) => a

-- {-# LINE 29 "src/Ensemble/Soundfont/FluidSynth/Foreign/Settings.hsc" #-}
-- c'FLUID_SET_TYPE = 3
-- c'FLUID_SET_TYPE :: (Num a) => a
