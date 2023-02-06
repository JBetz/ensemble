{-# LANGUAGE OverloadedStrings #-}

module Ensemble.Handler where

import Control.Monad.Freer.Error
import qualified Data.Aeson as A
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe
import Ensemble.API
import Ensemble.Config
import Ensemble.Error
import Ensemble.Schema (handleMessage)
import Ensemble.Schema.TaggedJSON (ToTaggedJSON(..))
import Ensemble.Server

handler :: Server -> KeyMap A.Value -> IO (Either APIError A.Value)
handler server object = runEnsemble server $
    case KeyMap.lookup "@type" object of
        Just (A.String messageType) -> 
            handleMessage messageType object
        Just _ -> 
            throwError $ APIError "Invalid '@type' field"
        Nothing -> 
            throwError $ APIError "Message is missing '@type' field"

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
