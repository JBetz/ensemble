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
import Data.Maybe
import Data.Text (unpack)
import Ensemble.Error
import Ensemble.Schema ()
import Ensemble.Schema.TaggedJSON (ToTaggedJSON(..))
import qualified Ensemble.API as API
import Ensemble.Server

receiveMessage :: Server -> A.Value -> IO A.Value
receiveMessage server jsonMessage = 
    case jsonMessage of
        A.Object object -> do
            let extraValue = KeyMap.lookup "@extra" object
            result <- handler server object
            pure $ case result of
                Right (A.Object outMessage) ->
                    A.Object $ KeyMap.insert "@extra" (fromMaybe A.Null extraValue) outMessage
                Right _ ->
                    makeError extraValue $ APIError "Invalid JSON output, needs to be object"
                Left errorMessage ->
                    makeError extraValue errorMessage
        _ -> pure $ makeError Nothing $ APIError "Invalid JSON input, needs to be object"
    where
        makeError extraValue apiError = 
            let A.Object errorJson = toTaggedJSON apiError
            in A.Object $ KeyMap.insert "@extra" (fromMaybe A.Null extraValue) errorJson


-- TODO: Use template haskell to generate this function
handler :: Server -> KeyMap A.Value -> IO (Either APIError A.Value)
handler server object = runM $ runError $ runReader server $
    case KeyMap.lookup "@type" object of
        Just (A.String messageType) ->
            case messageType of
                -- Audio
                "getAudioDevices" ->
                    toTaggedJSON <$> API.getAudioDevices

                "startEngine" ->
                    toTaggedJSON <$> API.startEngine

                "stopEngine" -> 
                    toTaggedJSON <$> API.stopEngine

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

                "createSoundfontInstrument" -> do
                    filePath <- lookupField "filePath"
                    result <- API.createSoundfontInstrument filePath
                    pure $ toTaggedJSON result

                -- Sequencer
                "scheduleEvent" -> do
                    tick <- lookupField "tick"
                    event <- lookupField "sequencerEvent"
                    result <- API.scheduleEvent tick event
                    pure $ toTaggedJSON result

                "playSequence" -> do
                    startTick <- lookupField "startTick"
                    result <- API.playSequence startTick
                    pure $ toTaggedJSON result
                    
                "renderSequence" -> do
                    startTick <- lookupField "startTick"
                    endTick <- lookupField "endTick"
                    result <- API.renderSequence startTick endTick
                    pure $ toTaggedJSON result

                "clearSequence" -> do
                    result <- API.clearSequence
                    pure $ toTaggedJSON result

                "playAudio" -> do
                    audioOutput <- lookupField "audioOutput"
                    result <- API.playAudio audioOutput
                    pure $ toTaggedJSON result

                other ->
                    throwError $ APIError $ "Invalid message type: " <> unpack other
        Just _ -> 
            throwError $ APIError "Invalid '@type' field"
        Nothing -> 
            throwError $ APIError "Message is missing '@type' field"

    where
        lookupField key = 
            case KeyMap.lookup key object of
                Just value -> 
                    case A.fromJSON value of
                        A.Success a -> pure a
                        A.Error parseError -> throwError $ APIError $ "Parse error on '" <> show key <> "': "  <> parseError
                Nothing -> 
                    throwError $ APIError $ "Missing argument: " <> show key