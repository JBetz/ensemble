module Ensemble.Soundfont.FluidSynth.Foreign.MidiDriver where

import Ensemble.Soundfont.FluidSynth.Foreign.Settings
import Foreign.Ptr
import Foreign.C.Types

data C'fluid_midi_driver_t = C'fluid_midi_driver_t
data C'fluid_midi_event_t = C'fluid_midi_event_t

type C'handle_midi_event_func_t = FunPtr (Ptr () -> Ptr C'fluid_midi_event_t -> IO CInt)

foreign import ccall "wrapper" mk'new_fluid_midi_driver 
    :: (Ptr C'fluid_settings_t -> C'handle_midi_event_func_t -> Ptr () -> IO (Ptr C'fluid_midi_driver_t)) 
    -> IO (FunPtr (Ptr C'fluid_settings_t -> C'handle_midi_event_func_t -> Ptr () -> IO (Ptr C'fluid_midi_driver_t)))
foreign import ccall "dynamic" mK'new_fluid_midi_driver 
    :: FunPtr (Ptr C'fluid_settings_t -> C'handle_midi_event_func_t -> Ptr () -> IO (Ptr C'fluid_midi_driver_t)) 
    -> (Ptr C'fluid_settings_t -> C'handle_midi_event_func_t -> Ptr () -> IO (Ptr C'fluid_midi_driver_t))

foreign import ccall "wrapper" mk'delete_fluid_midi_driver 
    :: (Ptr C'fluid_midi_driver_t -> IO ()) -> IO (FunPtr (Ptr C'fluid_midi_driver_t -> IO ()))
foreign import ccall "dynamic" mK'delete_fluid_midi_driver 
    :: FunPtr (Ptr C'fluid_midi_driver_t -> IO ()) -> (Ptr C'fluid_midi_driver_t -> IO ())