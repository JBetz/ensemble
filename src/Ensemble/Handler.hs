{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Ensemble.Handler where

import Clap.Host
import Clap.Library
import Data.Aeson
import qualified Data.ByteString as BS
import Ensemble.API
import Ensemble.Engine
import Ensemble.Sequencer
import Ensemble.Server

handle :: Server -> InMessage -> IO OutMessage
handle (Server sequencer engine) (InMessage clientId inContent extra) = do 
    outContent <- case inContent of
        In_ClapPluginPaths ->
            Out_ClapPluginPaths <$> pluginLibraryPaths
        In_ScanForClapPlugins filePaths -> 
            Out_ScanForClapPlugins <$> scanForPluginsIn filePaths
        In_LoadClapPlugin filePath index -> do
            loadPlugin engine $ ClapId (filePath, index)
            pure Out_LoadClapPlugin
    pure $ OutMessage clientId outContent extra

getCommand :: IO (Either String InMessage)
getCommand = do
    rawCommand <- BS.getLine
    pure $ eitherDecodeStrict rawCommand