{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Schema.TH where

import Prelude hiding (break)

import Control.Monad (join)
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Char (toLower)
import Data.Foldable (traverse_, foldlM)
import Data.Traversable (for)
import GHC.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Ensemble.Schema.TaggedJSON

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
        , A.unwrapUnaryRecords = False
        }

break                   :: (a -> Bool) -> [a] -> ([a],[a])
break _ xs@[]           =  (xs, xs)
break p xs@(x:xs')
           | p x        =  ([],xs)
           | otherwise  =  let (ys,zs) = break p xs' in (x:ys,zs)

split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest' -> chunk : split c rest'
  where (chunk, rest) = break (==c) s

deriveToTaggedJSON :: Name -> DecQ
deriveToTaggedJSON name = do
    let classType = ConT ''ToTaggedJSON
    let forType = ConT name
    let functionName = 'toTaggedJSON
    objectVariable <- newName "object"
    let body = NormalB $ AppE (AppE (VarE 'defaultToTaggedJSON) (LitE $ StringL $ nameBase name)) (VarE objectVariable)
    pure $ InstanceD Nothing [] (AppT classType forType)
        [FunD functionName 
            [Clause [VarP objectVariable] body []]]

deriveJSON :: Name -> DecsQ
deriveJSON name = do
    let generic = StandaloneDerivD Nothing [] (ConT ''Generic `AppT` ConT name)
    fromJson <- A.deriveToJSON encodingOptions name
    toJson <- A.deriveFromJSON encodingOptions name
    toTaggedJson <- deriveToTaggedJSON name
    pure $ [generic] <> fromJson <> toJson <> [toTaggedJson]

deriveJSONs :: [Name] -> DecsQ
deriveJSONs names = do
    deriveSchemaDecs <- traverse deriveJSON names
    pure $ join deriveSchemaDecs

schemaHeader :: String
schemaHeader = "vector {t:Type} # [ t ] = Vector t;"

writeSchema :: [String] -> [String] -> IO ()
writeSchema types functions = do
    let fileName = "ensemble.tl"
    writeFile fileName schemaHeader
    traverse_ (appendFile fileName . (<>) "\n\n") types
    appendFile fileName "\n\n---functions---"
    traverse_ (appendFile fileName . (<>) "\n\n") functions 

showType :: Type -> String
showType = \case 
    ConT name -> nameBase name
    AppT ListT itemType -> 
        if itemType == ConT ''Char
            then "String"
            else "vector<" <> showType itemType <> ">" 
    AppT (ConT _) returnType -> showType returnType
    other -> error $ "showType: " <> show other

showField :: (Name, Type) -> String
showField (fieldName, fieldType) =
    let nameString =  last (split '_' (last (split '.' $ show fieldName)))
        typeString = showType fieldType
   in nameString <> ":" <> typeString

getArgumentTypes :: Type -> [Type]
getArgumentTypes = \case
    AppT (AppT ArrowT argumentType) rest -> argumentType:getArgumentTypes rest
    other -> [other]

showFunctionType :: Type -> Q String
showFunctionType functionType = 
    case getArgumentTypes functionType of
        [returnType] -> pure $  "= " <> showType returnType
        types -> do
            let returnType = showType (last types)
            arguments <- foldlM (\acc currentType -> do
                resolvedType <- resolveTypeSynonyms currentType
                pure $ acc <> uncapitalise (showType currentType) <> ":" <> showType resolvedType <> " ") 
                "" 
                (init types) 
            pure $ arguments <> "= " <> returnType

makeGenerateSchema :: [Name] -> [Name] -> DecsQ
makeGenerateSchema typeNames functionNames = do
    jsonDecs <- deriveJSONs typeNames
    typeDefinitions <- for typeNames $ \typeName -> do
        datatypeInfo <- reifyDatatype typeName
        for (datatypeCons datatypeInfo) $ \constructorInfo -> do
            let name = uncapitalise $ nameBase $ constructorName constructorInfo
            case constructorVariant constructorInfo of
                RecordConstructor fieldNames -> do
                    let fields = zip fieldNames (constructorFields constructorInfo)
                    pure $ foldl (\acc field -> acc <> showField field <> " ") (name <> " ") fields <> "= " <> nameBase typeName <> ";"                    
                _ -> pure $ name <> " = " <> nameBase typeName <> ";"                    
    functionDefinitions <- for functionNames $ \functionName -> do
        info <- reify functionName
        case info of
            VarI _ functionType _ -> do
                let name = nameBase functionName
                functionTypeString <- showFunctionType functionType
                pure $ name <> " " <> functionTypeString <> ";"
            _ -> error $ show info
    let generateSchemaName = mkName "generateSchema"
    let body = NormalB $ DoE Nothing [ NoBindS $ AppE (AppE (VarE 'writeSchema) (ListE (LitE . StringL <$> join typeDefinitions))) (ListE (LitE . StringL <$> functionDefinitions)) ]
    let signature = SigD generateSchemaName (AppT (ConT ''IO) (TupleT 0))
    let function = FunD generateSchemaName [Clause [] body []]
    pure $ jsonDecs <> [signature, function]


uncapitalise :: String -> String
uncapitalise [] = []
uncapitalise (c:cs) = toLower c : cs