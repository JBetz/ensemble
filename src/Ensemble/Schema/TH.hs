{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Schema.TH where

import Control.Monad (join)
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy as LBS
import Data.JSON.Schema.Generator
import Data.Proxy
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
        , A.constructorTagModifier = modifier
        , A.sumEncoding = A.ObjectWithSingleField
        , A.unwrapUnaryRecords = True
        }

split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest' -> chunk : split c rest'
  where (chunk, rest) = break (==c) s

deriveSchemas :: [Name] -> DecsQ
deriveSchemas names = do
    deriveSchemaDecs <- traverse deriveSchema names
    generateSchemasDecs <- makeGenerateSchemas names
    pure $ join deriveSchemaDecs <> generateSchemasDecs

deriveSchema :: Name -> DecsQ
deriveSchema name = do
    let generic = StandaloneDerivD Nothing [] (ConT ''Generic `AppT` ConT name)
    fromJson <- A.deriveToJSON encodingOptions name
    toJson <- A.deriveFromJSON encodingOptions name
    let schemaGen = StandaloneDerivD Nothing [] (ConT ''JSONSchemaGen `AppT` ConT name)
    pure $ [generic, schemaGen] <> fromJson <> toJson

generateSchema :: JSONSchemaGen a => String -> Proxy a -> IO ()
generateSchema name proxy = do
    let json = A.encodePretty $ generateJson proxy
    LBS.writeFile ("schema/" <> name <> ".json") json
    where
        generateJson :: JSONSchemaGen a => Proxy a -> A.Value
        generateJson = convert encodingOptions . toSchema (defaultOptions 
            { baseUri = "/schema/"
            , schemaIdSuffix = ".json" 
            })

makeGenerateSchemas :: [Name] -> DecsQ
makeGenerateSchemas schemaNames = do
    let generateSchemasName = mkName "generateSchemas"
    statements <- traverse makeGenerateSchema schemaNames
    let body = NormalB $ DoE Nothing statements
    let signature = SigD generateSchemasName (AppT (ConT ''IO) (TupleT 0))
    let function = FunD generateSchemasName [Clause [] body []]
    pure [signature, function]
    where
        makeGenerateSchema schemaName = do
            let generateSchemaName = mkName "generateSchema"
            let schemaType = ConT schemaName
            pure $ NoBindS $ AppE (AppE (VarE generateSchemaName) (LitE $ StringL (nameBase schemaName))) (proxyFor schemaType)

proxyFor :: Type -> Exp
proxyFor = SigE (ConE 'Proxy) . AppT (ConT ''Proxy)