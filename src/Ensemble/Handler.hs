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
import Data.Text (unpack)
import Ensemble.Schema ()
import Ensemble.Schema.TaggedJSON (ToTaggedJSON(..))
import qualified Ensemble.API as API
import Ensemble.Server

receiveMessage :: Server -> A.Value -> IO A.Value
receiveMessage server jsonMessage = 
    case jsonMessage of
        A.Object object -> do
            result <- handler server object
            case result of
                Right (A.Object outMessage) ->
                    pure $ A.Object $
                        case KeyMap.lookup "@extra" object of
                            Just extraValue -> KeyMap.insert "@extra" extraValue outMessage
                            Nothing -> outMessage
                Right _ ->
                    pure $ makeError "Invalid JSON output, needs to be object"
                Left errorMessage ->
                    pure $ makeError errorMessage
        _ -> pure $ makeError "Invalid JSON input, needs to be object"
    where
        makeError = toTaggedJSON . API.EnsembleError


-- TODO: Use template haskell to generate this function
handler :: Server -> KeyMap A.Value -> IO (Either String A.Value)
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