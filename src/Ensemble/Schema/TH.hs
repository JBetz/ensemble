{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Schema.TH where

import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.JSON.Schema.Generator
import GHC.Generics
import Language.Haskell.TH

encodingOptions :: A.Options
encodingOptions = 
    let modifier field = case split '_' field of 
            [name] -> name
            _prefix:[name] -> name
            _ -> field
    in A.defaultOptions 
        { A.fieldLabelModifier = modifier
        , A.sumEncoding = A.ObjectWithSingleField 
        }

split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest' -> chunk : split c rest'
  where (chunk, rest) = break (==c) s

deriveSchema :: Name -> DecsQ
deriveSchema name = do
    let generic = StandaloneDerivD Nothing [] (ConT ''Generic `AppT` ConT name)
    fromJson <- A.deriveToJSON encodingOptions name
    toJson <- A.deriveFromJSON encodingOptions name
    let schemaGen = StandaloneDerivD Nothing [] (ConT ''JSONSchemaGen `AppT` ConT name)
    pure $ [generic, schemaGen] <> fromJson <> toJson