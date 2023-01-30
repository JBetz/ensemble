{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Schema where

import Clap.Interface.Plugin
import Clap.Interface.Version (ClapVersion (..))
import Data.JSON.Schema.Generator
import Data.JSON.Schema.Generator.Types (scInteger)
import Data.Word
import Ensemble.API
import Ensemble.Soundfont
import Ensemble.Schema.TH

instance JSONSchemaPrim Word32 where
    toSchemaPrim _ _ = scInteger

deriveSchemas 
    [ ''ClapVersion
    , ''PluginDescriptor
    , ''SoundfontId
    , ''InMessageContent
    , ''OutMessageContent
    , ''InMessage
    , ''OutMessage
    ]
