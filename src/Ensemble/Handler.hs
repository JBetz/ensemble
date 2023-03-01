{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Ensemble.Handler where

import Control.Exception
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
import GHC.Stack

deriveJSON ''ApiError

handler :: HasCallStack => Server -> KeyMap A.Value -> IO (Either ApiError (KeyMap A.Value))
handler server object = runEnsemble server $
    case KeyMap.lookup "@type" object of
        Just (A.String messageType) -> 
            handleMessage messageType object
        Just _ -> 
            throwError $ ApiError "Invalid '@type' field"
        Nothing -> 
            throwError $ ApiError "Message is missing '@type' field"

receiveMessage :: HasCallStack => Server -> A.Value -> IO A.Value
receiveMessage server = \case
    A.Object object -> do
        let extraValue = KeyMap.lookup "@extra" object
        result <- handler server object `catch` (\(exception :: SomeException) -> 
            pure $ Left $ ApiError $ displayException exception)
        pure $ case result of
            Right outMessage ->
                A.Object $ KeyMap.insert "@extra" (fromMaybe A.Null extraValue) outMessage
            Left errorMessage ->
                makeError extraValue errorMessage
    _ -> pure $ makeError Nothing $ ApiError "Invalid JSON input, needs to be object"
    where
        makeError extraValue apiError = 
            A.Object $ KeyMap.insert "@extra" (fromMaybe A.Null extraValue) (toTaggedJSON apiError)