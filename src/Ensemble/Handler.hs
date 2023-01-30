{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Ensemble.Handler where

import Clap.Host
import Clap.Library
import Data.Aeson
import qualified Data.ByteString as BS
import Ensemble.API
import Ensemble.Engine
import Ensemble.Schema ()
import Ensemble.Server

handle :: Server -> InMessage -> IO OutMessage
handle (Server _sequencer engine) (InMessage inContent extra) = do 
    outContent <- case inContent of
        In_ClapPluginPaths ->
            Out_ClapPluginPathsResponse <$> pluginLibraryPaths
        In_ScanForClapPlugins filePaths -> 
            Out_ScanForClapPluginsResponse <$> scanForPluginsIn filePaths
        In_LoadClapPlugin filePath index -> do
            loadPlugin engine $ ClapId (filePath, index)
            pure Out_LoadClapPluginResponse
        In_InitializeSoundfontPlayer filePath -> do
            initializeSoundfontPlayer engine filePath
            pure Out_InitializeSoundfontPlayerResponse
        In_LoadSoundfont filePath -> do
            soundfontId <- loadSoundfont engine filePath
            pure $ Out_LoadSoundfontResponse soundfontId
    pure $ OutMessage outContent extra

getCommand :: IO (Either String InMessage)
getCommand = do
    rawCommand <- BS.getLine
    pure $ eitherDecodeStrict rawCommand