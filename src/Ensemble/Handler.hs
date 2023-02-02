{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Ensemble.Handler where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import qualified Data.Aeson as A
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.Text (unpack)
import Ensemble.Schema ()
import Ensemble.Schema.TaggedJSON (ToTaggedJSON(..))
import qualified Ensemble.API as API
import Ensemble.Server

receiveMessage :: Server -> IO (Either String A.Value)
receiveMessage server = do
    jsonMessage <- A.eitherDecodeStrict <$> BS.getLine
    case jsonMessage of
        Right (A.Object object) -> do
            result <- handler server object
            case result of
                Right (A.Object outMessage) ->
                    pure $ Right $ A.Object $
                        case KeyMap.lookup "@extra" object of
                            Just extraValue -> KeyMap.insert "@extra" extraValue outMessage
                            Nothing -> outMessage
                Right _ ->
                    pure $ Left "Invalid JSON output, needs to be object"
                Left errorMessage ->
                    pure $ Left errorMessage
        Right _ -> pure $ Left "Invalid JSON input, needs to be object"
        Left parseError -> pure $ Left $ "Parse error: " <> parseError

handler :: Server -> KeyMap A.Value -> IO (Either String A.Value)
handler server object = runM $ runError $ runReader server $
    case KeyMap.lookup "@type" object of
        Just (A.String messageType) ->
            case messageType of
                -- CLAP 
                "getClapPluginLocations" -> do 
                    result <- API.getClapPluginLocations
                    pure $ toTaggedJSON result
                "scanForClapPlugins" -> do 
                    filePaths <- lookupField "filePaths"
                    result <- API.scanForClapPlugins filePaths
                    pure $ toTaggedJSON result
                "loadClapPlugin" -> do
                    filePath <- lookupField "filePath"
                    index <- lookupField "index"
                    result <- API.loadClapPlugin filePath index
                    pure $ toTaggedJSON result

                -- Soundfont
                "initializeSoundfontPlayer" -> do
                    filePath <- lookupField "filePath"
                    result <- API.initializeSoundfontPlayer filePath
                    pure $ toTaggedJSON result

                "loadSoundfont" -> do
                    filePath <- lookupField "filePath"
                    result <- API.loadSoundfont filePath
                    pure $ toTaggedJSON result
                        
                "getSoundfontPresets" -> do
                    soundfontId <- lookupField "soundfontId"
                    result <- API.getSoundfontPresets soundfontId
                    pure $ toTaggedJSON result

                -- Sequencer
                "scheduleEvent" -> do
                    tick <- lookupField "tick"
                    event <- lookupField "event"
                    result <- API.scheduleEvent tick event
                    pure $ toTaggedJSON result

                "play" -> do
                    startTick <- lookupField "startTick"
                    result <- API.play startTick
                    pure $ toTaggedJSON result
                other ->
                    throwError @String $ "Invalid message type: " <> unpack other
        Just _ -> 
            throwError @String "Invalid '@type' field"
        Nothing -> 
            throwError @String "Message is missing '@type' field"

    where
        lookupField key = 
            case KeyMap.lookup key object of
                Just value -> 
                    case A.fromJSON value of
                        A.Success a -> pure a
                        A.Error parseError -> throwError $ "Parse error on '" <> show key <> "': "  <> parseError
                Nothing -> 
                    throwError $ "Missing argument: " <> show key