{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Schema where

import Clap.Interface.Plugin
import Clap.Interface.Version (ClapVersion (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy as LBS
import Data.JSON.Schema.Generator
import Data.JSON.Schema.Generator.Types (scInteger)
import Data.Proxy
import Data.Word
import Ensemble.API
import Ensemble.Soundfont
import Ensemble.Schema.TH
import Language.Haskell.TH

instance JSONSchemaPrim Word32 where
    toSchemaPrim _ _ = scInteger

deriveSchema ''ClapVersion
deriveSchema ''PluginDescriptor
deriveSchema ''SoundfontId
deriveSchema ''InMessageContent
deriveSchema ''OutMessageContent
deriveSchema ''InMessage
deriveSchema ''OutMessage


generateSchema :: JSONSchemaGen a => Name -> Proxy a -> IO ()
generateSchema name proxy = do
    let json = A.encodePretty $ generateJson proxy
    LBS.writeFile ("schema/" <> nameBase name <> ".json") json
    where
        generateJson :: JSONSchemaGen a => Proxy a -> A.Value
        generateJson = convert encodingOptions . toSchema (defaultOptions 
            { baseUri = "/schema/"
            , schemaIdSuffix = ".json" 
            })

generateSchemas :: IO ()
generateSchemas = do
    generateSchema ''InMessage (Proxy :: Proxy InMessage)
    generateSchema ''OutMessage (Proxy :: Proxy OutMessage) 
    generateSchema ''InMessageContent (Proxy :: Proxy InMessageContent)
    generateSchema ''OutMessageContent (Proxy :: Proxy OutMessageContent) 