{-# LANGUAGE DataKinds #-}

module Ensemble.API where

import Clap.Host (ClapId (..))
import qualified Clap.Library as CLAP
import Clap.Interface.Plugin
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import qualified Ensemble.Engine as Engine
import Ensemble.Server
import Ensemble.Soundfont (SoundfontId)


data Ok = Ok

newtype PluginLocations = PluginLocations { filePaths :: [FilePath] }

newtype PluginDescriptors = PluginDescriptors { pluginDescriptors :: [PluginDescriptor] }

type FilePaths = [FilePath]
type PluginIndex = Int

type Ensemble = Eff '[Reader Server, Error String, IO]

-- CLAP
getClapPluginLocations :: Ensemble PluginLocations
getClapPluginLocations = 
    sendM $ PluginLocations <$> CLAP.pluginLibraryPaths 

scanForClapPlugins :: FilePaths -> Ensemble PluginDescriptors
scanForClapPlugins filePaths = 
    sendM $ PluginDescriptors <$> CLAP.scanForPluginsIn filePaths

loadClapPlugin :: FilePath -> PluginIndex -> Ensemble Ok
loadClapPlugin filePath index = do
    engine <- asks server_engine
    sendM $ Engine.loadPlugin engine $ ClapId (filePath, index)
    pure Ok

-- Soundfont
initializeSoundfontPlayer :: FilePath -> Ensemble Ok
initializeSoundfontPlayer filePath = do
   engine <- asks server_engine
   sendM $ Engine.initializeSoundfontPlayer engine filePath
   pure Ok

loadSoundfont :: FilePath -> Ensemble SoundfontId
loadSoundfont filePath = do
    engine <- asks server_engine
    sendM $ Engine.loadSoundfont engine filePath