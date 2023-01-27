{-# LINE 1 "src/Ensemble/Soundfont/FluidSynth/Foreign/Midi.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- #include <fluidsynth.h>
module Ensemble.Soundfont.FluidSynth.Foreign.Midi where
import Foreign.Ptr
import Ensemble.Soundfont.FluidSynth.Foreign.Settings
import Foreign.Ptr (Ptr,FunPtr,plusPtr)
import Foreign.Ptr (wordPtrToPtr,castPtrToFunPtr)
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String (CString,CStringLen,CWString,CWStringLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray,pokeArray)
import Data.Int
import Data.Word

{-# LINE 8 "src/Ensemble/Soundfont/FluidSynth/Foreign/Midi.hsc" #-}


data C'fluid_midi_driver_t = C'fluid_midi_driver_t

{-# LINE 11 "src/Ensemble/Soundfont/FluidSynth/Foreign/Midi.hsc" #-}

foreign import ccall "new_fluid_midi_driver" c'new_fluid_midi_driver
  :: Ptr C'fluid_settings_t -> FunPtr (Ptr () -> Ptr () -> IO ()) -> Ptr () -> IO (Ptr C'fluid_midi_driver_t)
foreign import ccall "&new_fluid_midi_driver" p'new_fluid_midi_driver
  :: FunPtr (Ptr C'fluid_settings_t -> FunPtr (Ptr () -> Ptr () -> IO ()) -> Ptr () -> IO (Ptr C'fluid_midi_driver_t))

{-# LINE 13 "src/Ensemble/Soundfont/FluidSynth/Foreign/Midi.hsc" #-}
foreign import ccall "delete_fluid_midi_driver" c'delete_fluid_midi_driver
  :: Ptr C'fluid_midi_driver_t -> IO ()
foreign import ccall "&delete_fluid_midi_driver" p'delete_fluid_midi_driver
  :: FunPtr (Ptr C'fluid_midi_driver_t -> IO ())
