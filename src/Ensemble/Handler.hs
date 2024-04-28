{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Ensemble.Handler where

import Control.Exception
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

handler :: HasCallStack => Server -> KeyMap A.Value -> IO (KeyMap A.Value)
handler server object = runEnsemble server $
    case KeyMap.lookup "@type" object of
        Just (A.String messageType) -> 
            handleMessage messageType object
        Just _ -> 
            throw $ ApiError "Invalid '@type' field"
        Nothing -> 
            throw $ ApiError "Message is missing '@type' field"

receiveMessage :: HasCallStack => Server -> A.Value -> IO A.Value
receiveMessage server = \case
    A.Object object -> do
        let extraValue = KeyMap.lookup "@extra" object
        outMessage <- handler server object
        pure $ A.Object $ KeyMap.insert "@extra" (fromMaybe A.Null extraValue) outMessage
    _ -> pure $ makeError Nothing $ ApiError "Invalid JSON input, needs to be object"
    where
        makeError extraValue apiError = 
            A.Object $ KeyMap.insert "@extra" (fromMaybe A.Null extraValue) (toTaggedJSON apiError)
