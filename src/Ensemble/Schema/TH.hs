{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Schema.TH where

import Prelude hiding (break)

import Control.Monad (join)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.TH as A
import qualified Data.Aeson.Types as A
import Data.Char (toLower)
import Data.Foldable (traverse_, foldlM)
import Data.Traversable (for)
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Ensemble.API
import Ensemble.Schema.TaggedJSON
import Foreign.Ptr
import GHC.Generics (Generic)

encodingOptions :: A.Options
encodingOptions = 
    let modifier field = case split '_' field of 
            [name] -> name
            _prefix:[name] -> name
            _ -> field
    in A.defaultOptions 
        { A.fieldLabelModifier = modifier
        , A.constructorTagModifier = modifier
        , A.sumEncoding = A.UntaggedValue
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

deriveCustomJSONs :: [Name] -> DecsQ
deriveCustomJSONs names = do
    decs <- for names (\name -> do
        fromJson <- deriveCustomFromJSON name
        toJson <- deriveCustomToJSON name
        pure [fromJson, toJson])
    pure $ join decs

deriveCustomToJSON :: Name -> DecQ
deriveCustomToJSON name = do
    constructors <- datatypeCons <$> reifyDatatype name
    let objectName = mkName "object"
    let body = NormalB $ CaseE (VarE objectName) $  
            (\constructor -> 
                let valueName = mkName "value"
                    pattern = ConP (constructorName constructor) [] [VarP valueName]
                    tag = toSubclassName $ constructorName constructor
                    caseBody = NormalB $ AppE (AppE (VarE 'defaultToTaggedJSON) (LitE $ StringL tag)) (VarE valueName)
                in Match pattern caseBody []
                ) <$> constructors
    pure $ InstanceD Nothing [] (AppT (ConT ''A.ToJSON) (ConT name))
        [FunD 'A.toJSON [Clause [VarP objectName] body []]]

deriveCustomFromJSON :: Name -> DecQ
deriveCustomFromJSON name = do
    constructors <- datatypeCons <$> reifyDatatype name
    let objectName = mkName "object"
    let eventTypeName = mkName "eventType"
    let otherName = mkName "other"
    let failureMessage = AppE (AppE (VarE '(<>)) (LitE $ StringL $ "Invalid " <> nameBase name <> " type: ")) (VarE otherName)
    let failureCase = Match (VarP otherName) (NormalB $ AppE (VarE 'A.parseFail) failureMessage) []
    let matches = fmap (\constructor -> 
            let pattern = LitP $ StringL $ toSubclassName (constructorName constructor)  
                parsed = AppE (VarE 'A.parseJSON) (AppE (ConE 'A.Object) (VarE objectName))
                caseBody = NormalB $ AppE (AppE (VarE 'fmap) (ConE $ constructorName constructor)) parsed
            in Match pattern caseBody []
            ) constructors
    let body = NormalB $ AppE (AppE (VarE 'A.withObject) (LitE $ StringL $ nameBase name)) (LamE [VarP objectName] $ 
                    DoE Nothing 
                        [ BindS (VarP eventTypeName ) (AppE (AppE (VarE 'A.parseField) (VarE objectName)) (LitE $ StringL "@type")) 
                        , NoBindS $ CaseE (AppE (VarE 'A.toString) (VarE eventTypeName)) (matches <> [failureCase])
                        ])
    pure $ InstanceD Nothing [] (AppT (ConT ''A.FromJSON) (ConT name))
        [FunD 'A.parseJSON [Clause [] body []]]

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
    TupleT 0 -> "Void"
    AppT ListT itemType -> if
        | itemType == ConT ''Char -> "String"
        | otherwise -> "vector<" <> showType itemType <> ">" 
    AppT (ConT name) innerType -> if
        | name == ''Ensemble -> showType innerType
        | name == ''Ptr -> "Void" 
        | otherwise -> error $ "Unrepresentable higher order type: " <> show name
    other -> error $ "Unrepresentable type: " <> show other

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

generateTypeDefinition :: Name -> Q [String]
generateTypeDefinition typeName = do
    datatypeInfo <- reifyDatatype typeName
    for (datatypeCons datatypeInfo) $ \constructorInfo -> do
        let name = toSubclassName $ constructorName constructorInfo
        case constructorVariant constructorInfo of
            RecordConstructor fieldNames -> do
                let fields = zip fieldNames (constructorFields constructorInfo)
                pure $ foldl (\acc field -> acc <> showField field <> " ") (name <> " ") fields <> "= " <> nameBase typeName <> ";"                    
            _ ->
                case constructorFields constructorInfo of
                    [] -> pure $ name <> " = " <> nameBase typeName <> ";"                    
                    [ConT singleField] -> do
                        fieldInfo <- reifyDatatype singleField
                        pure $ case datatypeCons fieldInfo of
                            [subConstructorInfo] -> 
                                case constructorVariant subConstructorInfo of
                                    RecordConstructor fieldNames ->
                                        let fields = zip fieldNames (constructorFields subConstructorInfo)
                                        in foldl (\acc field -> acc <> showField field <> " ") (name <> " ") fields <> " = " <> nameBase typeName <> ";"
                                    _ ->
                                        name <> " value:" <> showType (ConT singleField) <> " = " <> nameBase typeName <> ";" 
                            _ -> error $ "Invalid constructor field: " <> show constructorInfo
                    _ -> 
                        error $ "Invalid constructor fields: " <> show constructorInfo

generateFunctionDefinition :: Name -> Q String
generateFunctionDefinition functionName = do
    info <- reify functionName
    case info of
        VarI _ functionType _ -> do
            let name = nameBase functionName
            functionTypeString <- showFunctionType functionType
            pure $ name <> " " <> functionTypeString <> ";"
        _ -> error $ "Invalid function: " <> show functionName

makeGenerateSchema :: [Name] -> [Name] -> DecsQ
makeGenerateSchema typeNames functionNames = do
    typeDefinitions <- traverse generateTypeDefinition typeNames                  
    functionDefinitions <- traverse generateFunctionDefinition functionNames
    let generateSchemaName = mkName "generateSchema"
    let body = NormalB $ DoE Nothing [ NoBindS $ AppE (AppE (VarE 'writeSchema) (ListE (LitE . StringL <$> join typeDefinitions))) (ListE (LitE . StringL <$> functionDefinitions)) ]
    let signature = SigD generateSchemaName (AppT (ConT ''IO) (TupleT 0))
    let function = FunD generateSchemaName [Clause [] body []]
    pure [signature, function]


toSubclassName :: Name -> String
toSubclassName = uncapitalise . filter (/= '_') . nameBase

uncapitalise :: String -> String
uncapitalise [] = []
uncapitalise (c:cs) = toLower c : cs