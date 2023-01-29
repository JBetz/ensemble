module Ensemble.Soundfont.FluidSynth.Library.POSIX where

import Foreign.Ptr
import System.Posix.DynamicLinker

newtype FluidSynthLibrary = FluidSynthLibrary { unFluidSynthLibrary :: DL }
    deriving (Show)

openFluidSynthLibrary :: FilePath -> IO FluidSynthLibrary
openFluidSynthLibrary path = FluidSynthLibrary <$> dlopen path [RTLD_NOW]

closeFluidSynthLibrary :: FluidSynthLibrary -> IO ()
closeFluidSynthLibrary (FluidSynthLibrary dl) =
    dlclose dl

lookupProcedure :: FluidSynthLibrary -> String -> IO (FunPtr a)
lookupProcedure (FluidSynthLibrary dl) name =
    dlsym dl name