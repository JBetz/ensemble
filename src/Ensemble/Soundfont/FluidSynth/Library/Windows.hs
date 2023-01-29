module Ensemble.Soundfont.FluidSynth.Library.Windows where

import Foreign.Ptr
import System.Environment
import System.Win32.DLL as DLL
import System.Win32.Types

newtype FluidSynthLibrary = FluidSynthLibrary { unFluidSynthLibrary :: HMODULE }
    deriving (Show)

openFluidSynthLibrary :: FilePath -> IO FluidSynthLibrary
openFluidSynthLibrary filePath = do
    library <- DLL.loadLibrary filePath
    pure $ FluidSynthLibrary library

closeFluidSynthLibrary :: FluidSynthLibrary -> IO ()
closeFluidSynthLibrary (FluidSynthLibrary hmodule) =
    freeLibrary hmodule

lookupProcedure :: FluidSynthLibrary -> String -> IO (FunPtr a)
lookupProcedure (FluidSynthLibrary hmodule) name = do
    addr <- DLL.getProcAddress hmodule name
    pure $ castPtr addr
