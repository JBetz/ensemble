{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Schema.TH where

import Prelude hiding (break)

import Control.Monad (join)
import Control.Monad.Freer.Error
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.TH as A
import qualified Data.Aeson.Types as A
import Data.Foldable (traverse_, foldlM, foldl')
import Control.Monad.Freer
import Data.Text (Text)
import Data.Traversable (for)
import Ensemble.Util
import Ensemble.API
import Ensemble.Error
import Ensemble.Schema.TaggedJSON
import Foreign.Ptr
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

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
        , A.unwrapUnaryRecords = True
        }

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

showResolvedType :: Type -> Q String
showResolvedType type' = resolveTypeSynonyms type' >>= showType True

showUnresolvedType :: Type -> Q String
showUnresolvedType = showType False

showType :: Bool -> Type -> Q String
showType resolve = \case
    ConT name -> if
        | nameBase name == "CFloat" -> pure "Float"
        | nameBase name == "String" -> pure "String"
        | not resolve -> pure $ nameBase name
        | otherwise -> do
            typeInfo <- reifyDatatype name
            if | datatypeVariant typeInfo == Newtype -> showType resolve $ head (constructorFields (head (datatypeCons typeInfo)))
               | otherwise -> pure $ nameBase name
    TupleT 0 -> pure "Void"
    AppT ListT itemType -> if
        | itemType == ConT ''Char -> pure "String"
        | otherwise ->  do
            itemTypeString <- showType resolve itemType
            pure $ "vector<" <> itemTypeString <> ">" 
    AppT (ConT name) innerType -> if
        | name == ''Ensemble -> showType resolve innerType
        | name == ''Ptr -> pure "Void" 
        | name == ''Maybe -> showType resolve innerType
        | otherwise -> error $ "Unrepresentable higher order type: " <> show name
    other -> error $ "Unrepresentable type: " <> show other

showField :: (Name, Type) -> Q String
showField (fieldName, fieldType) = do
    let nameString =  last (split '_' (last (split '.' $ show fieldName)))
    typeString <- showResolvedType fieldType
    pure $ nameString <> ":" <> typeString

getArgumentTypes :: Type -> [Type]
getArgumentTypes = \case
    AppT (AppT ArrowT argumentType) rest -> argumentType:getArgumentTypes rest
    other -> [other]

showFunctionType :: Type -> Q String
showFunctionType functionType = 
    case getArgumentTypes functionType of
        [returnType] -> do
            returnTypeString <- showUnresolvedType returnType
            pure $ "= " <> returnTypeString
        types -> do
            returnTypeString <- showUnresolvedType (last types)
            arguments <- foldlM (\acc currentType -> do
                currentTypeString <- showUnresolvedType currentType
                resolvedTypeString <- showResolvedType currentType
                pure $ acc <> uncapitalise currentTypeString <> ":" <> resolvedTypeString <> " ") 
                "" 
                (init types) 
            pure $ arguments <> "= " <> returnTypeString

generateTypeDefinition :: Name -> Q [String]
generateTypeDefinition typeName = do
    datatypeInfo <- reifyDatatype typeName
    for (datatypeCons datatypeInfo) $ \constructorInfo -> do
        let name = toSubclassName $ constructorName constructorInfo
        case constructorVariant constructorInfo of
            RecordConstructor fieldNames -> do
                let fields = zip fieldNames (constructorFields constructorInfo)
                fieldsString <- foldlM (\acc field -> do
                    fieldString <- showField field
                    pure $ acc <> fieldString <> " ") (name <> " ") fields
                pure $ fieldsString <> "= " <> nameBase typeName <> ";"                    
            _ ->
                case constructorFields constructorInfo of
                    [] -> pure $ name <> " = " <> nameBase typeName <> ";"                    
                    [ConT singleField] -> do
                        fieldInfo <- reifyDatatype singleField
                        case datatypeCons fieldInfo of
                            [subConstructorInfo] -> 
                                case constructorVariant subConstructorInfo of
                                    RecordConstructor fieldNames -> do
                                        let fields = zip fieldNames (constructorFields subConstructorInfo)
                                        fieldsString <- foldlM (\acc field -> do
                                            fieldString <- showField field
                                            pure $ acc <> fieldString <> " ") (name <> " ") fields
                                        pure $ fieldsString <> " = " <> nameBase typeName <> ";"
                                    _ -> do
                                        typeString <- showResolvedType (ConT singleField)
                                        pure $ name <> " value:" <> typeString  <> " = " <> nameBase typeName <> ";" 
                            _ -> error $ "Invalid constructor field: " <> show fieldInfo
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

makeAPI :: [Name] -> [Name] -> DecsQ
makeAPI typeNames functionNames = do
    generateSchemaDecs <- makeGenerateSchema typeNames functionNames
    handleMessageDecs <- makeHandleMessage functionNames
    pure $ generateSchemaDecs <> handleMessageDecs

makeGenerateSchema :: [Name] -> [Name] -> DecsQ
makeGenerateSchema typeNames functionNames = do
    typeDefinitions <- traverse generateTypeDefinition typeNames                  
    functionDefinitions <- traverse generateFunctionDefinition functionNames
    let generateSchemaName = mkName "generateSchema"
    let body = NormalB $ DoE Nothing [ NoBindS $ AppE (AppE (VarE 'writeSchema) (ListE (LitE . StringL <$> join typeDefinitions))) (ListE (LitE . StringL <$> functionDefinitions)) ]
    let signature = SigD generateSchemaName (AppT (ConT ''IO) (TupleT 0))
    let function = FunD generateSchemaName [Clause [] body []]
    pure [signature, function]

makeHandleMessage :: [Name] -> DecsQ
makeHandleMessage functionNames = do
    let functionName = mkName "handleMessage"
    let messageTypeName = mkName "messageType"
    let objectName = mkName "object"
    cases <- traverse (makeCase objectName) functionNames
    let body = NormalB $ CaseE (VarE messageTypeName) cases
    let signature = SigD functionName $ AppT (AppT ArrowT (ConT ''Text)) (AppT (AppT ArrowT (AppT (ConT ''KeyMap) (ConT ''A.Value))) (AppT (ConT ''Ensemble) (ConT ''A.Value)))
    let function = FunD functionName [Clause [VarP messageTypeName, VarP objectName] body []]
    pure [signature, function]

makeCase :: Name -> Name -> Q Match 
makeCase objectName functionName = do
    info <- reify functionName
    case info of
        VarI _ functionType _ -> do
            let argumentTypes = init $ getArgumentTypes functionType
                makeLookupStatement argumentType = do
                    argumentTypeString <- showUnresolvedType argumentType
                    let argumentName = uncapitalise argumentTypeString
                        lookupExpression = multiAppE (VarE 'lookupField) [LitE (StringL argumentName), VarE objectName]
                    pure $ BindS (VarP $ mkName argumentName) lookupExpression
            lookupStatements <- traverse makeLookupStatement argumentTypes
            let resultName = mkName "result"
            argumentTypeStrings <- traverse showUnresolvedType argumentTypes 
            let callStatement = BindS (VarP resultName) (multiAppE (VarE functionName) $ VarE . mkName . uncapitalise <$> argumentTypeStrings)
                returnStatement = NoBindS $ AppE (VarE 'pure) (AppE (VarE 'toTaggedJSON) (VarE resultName))
                statements = lookupStatements <> [callStatement, returnStatement]
                body = NormalB $ DoE Nothing statements
            pure $ Match (LitP $ StringL $ nameBase functionName) body []
        _ -> error $ "Invalid function: " <> show functionName

lookupField :: (A.FromJSON a, Member (Error APIError) effs) => A.Key -> KeyMap A.Value -> Eff effs a
lookupField key object = 
    case KeyMap.lookup key object of
        Just value -> 
            case A.fromJSON value of
                A.Success a -> pure a
                A.Error parseError -> throwError $ APIError ("Parse error on '" <> show key <> "': "  <> parseError)
        Nothing -> 
            throwError $ APIError ("Missing argument: " <> show key)

toSubclassName :: Name -> String
toSubclassName = uncapitalise . filter (/= '_') . nameBase

multiAppE :: Exp -> [Exp] -> Exp
multiAppE base args =
  foldl' AppE base args