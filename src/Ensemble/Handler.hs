{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Ensemble.Handler where

import Control.Monad.Freer.Error
import qualified Data.Aeson as A
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe
import Ensemble.Effects
import Ensemble.Error
import Ensemble.Schema (handleMessage)
import Ensemble.Schema.TH
import Ensemble.Schema.TaggedJSON (toTaggedJSON)
import Ensemble.Server

deriveJSON ''ApiError

handler :: Server -> KeyMap A.Value -> IO (Either ApiError A.Value)
handler server object = runEnsemble server $
    case KeyMap.lookup "@type" object of
        Just (A.String messageType) -> 
            handleMessage messageType object
        Just _ -> 
            throwError $ ApiError "Invalid '@type' field"
        Nothing -> 
            throwError $ ApiError "Message is missing '@type' field"

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
                    makeError extraValue $ ApiError "Invalid JSON output, needs to be object"
                Left errorMessage ->
                    makeError extraValue errorMessage
        _ -> pure $ makeError Nothing $ ApiError "Invalid JSON input, needs to be object"
    where
        makeError extraValue apiError = 
            let A.Object errorJson = toTaggedJSON apiError
            in A.Object $ KeyMap.insert "@extra" (fromMaybe A.Null extraValue) errorJson
