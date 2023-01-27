{-# LINE 1 "src/Ensemble/Soundfont/FluidSynth/Foreign/Audio.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- #include <fluidsynth.h>
module Ensemble.Soundfont.FluidSynth.Foreign.Audio where
import Ensemble.Soundfont.FluidSynth.Foreign.Settings
import Ensemble.Soundfont.FluidSynth.Foreign.Synth
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

{-# LINE 9 "src/Ensemble/Soundfont/FluidSynth/Foreign/Audio.hsc" #-}


data C'fluid_audio_driver_t = C'fluid_audio_driver_t

{-# LINE 12 "src/Ensemble/Soundfont/FluidSynth/Foreign/Audio.hsc" #-}

foreign import ccall "new_fluid_audio_driver" c'new_fluid_audio_driver
  :: Ptr C'fluid_settings_t -> Ptr C'fluid_synth_t -> IO (Ptr C'fluid_audio_driver_t)
foreign import ccall "&new_fluid_audio_driver" p'new_fluid_audio_driver
  :: FunPtr (Ptr C'fluid_settings_t -> Ptr C'fluid_synth_t -> IO (Ptr C'fluid_audio_driver_t))

{-# LINE 14 "src/Ensemble/Soundfont/FluidSynth/Foreign/Audio.hsc" #-}
foreign import ccall "delete_fluid_audio_driver" c'delete_fluid_audio_driver
  :: Ptr C'fluid_audio_driver_t -> IO ()
foreign import ccall "&delete_fluid_audio_driver" p'delete_fluid_audio_driver
  :: FunPtr (Ptr C'fluid_audio_driver_t -> IO ())
