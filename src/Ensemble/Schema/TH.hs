{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Ensemble.Schema.TH where

import Prelude hiding (break)

import Control.Monad (join)
import Control.Monad.Freer
import Control.Monad.Freer.Error
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.TH as A
import qualified Data.Aeson.Types as A
import Data.Foldable (traverse_, foldlM, foldl')
import Data.Text (Text)
import Data.Traversable (for)
import Ensemble.Error
import Ensemble.Util
import Ensemble.Schema.TaggedJSON
import Foreign.Ptr
import GHC.Generics (Generic)
import GHC.Stack
import GHC.TypeLits
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

data Argument (name :: Symbol) t = Argument t

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

deriveHasTypeTag :: HasCallStack => Name -> DecQ
deriveHasTypeTag name = do
    let classType = ConT ''HasTypeTag
    let forType = ConT name
    let functionName = 'typeTag
    datatype <- reifyDatatype name
    case datatypeCons datatype of 
        [single] ->
            case datatypeVariant datatype of
                Newtype -> do 
                    resolvedTypeName <- showResolvedType $ head $ constructorFields single
                    let body = NormalB $ LitE $ StringL $ uncapitalise resolvedTypeName
                    pure $ InstanceD Nothing [] (AppT classType forType)
                        [FunD functionName [Clause [VarP (mkName "_")] body []]]
                _ -> do
                    let body = NormalB $ LitE $ StringL $ toSubclassName (constructorName single)
                    pure $ InstanceD Nothing [] (AppT classType forType)
                        [FunD functionName [Clause [VarP (mkName "_")] body []]]
        multiple -> do
            let objectName = mkName "object"
            let matches =
                    (\constructor -> 
                        let pattern = ConP (constructorName constructor) [] $ VarP (mkName "_") <$ constructorFields constructor 
                            caseBody = NormalB $ LitE $ StringL $ toSubclassName (constructorName constructor)
                        in Match pattern caseBody []
                    ) <$> multiple
            let body = NormalB $ CaseE (VarE objectName) (matches <> [failPattern])
            pure $ InstanceD Nothing [] (AppT classType forType)
                [FunD functionName 
                    [Clause [VarP objectName] body []]]


deriveCustomJSONs :: [Name] -> DecsQ
deriveCustomJSONs names = do
    decs <- for names $ \name -> do
        fromJson <- deriveCustomFromJSON name
        toJson <- deriveCustomToJSON name
        toTaggedJson <- deriveHasTypeTag name
        pure [fromJson, toJson, toTaggedJson]
    pure $ join decs

failPattern :: Match
failPattern = 
    let otherName = mkName "other" 
    in Match (VarP otherName) (NormalB $ AppE (VarE 'error) (AppE (VarE 'show) (VarE otherName))) []

deriveCustomToJSON :: HasCallStack => Name -> DecQ
deriveCustomToJSON name = do
    constructors <- datatypeCons <$> reifyDatatype name
    let objectName = mkName "object"
    let matches = (\constructor -> 
            let valueName = mkName "value"
                pattern = ConP (constructorName constructor) [] [VarP valueName]
                caseBody = NormalB $ multiAppE (VarE 'toTaggedCustomJSON) 
                    [ LitE $ StringL $ toSubclassName $ constructorName constructor
                    , VarE valueName
                    ]
            in Match pattern caseBody []
            ) <$> constructors
    let body = NormalB $ CaseE (VarE objectName) (matches <> [failPattern])
    pure $ InstanceD Nothing [] (AppT (ConT ''A.ToJSON) (ConT name))
        [FunD 'A.toJSON [Clause [VarP objectName] body []]]

deriveCustomFromJSON :: HasCallStack => Name -> DecQ
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

deriveJSON :: HasCallStack => Name -> DecsQ
deriveJSON name = do
    let generic = StandaloneDerivD Nothing [] (ConT ''Generic `AppT` ConT name)
    fromJson <- A.deriveToJSON encodingOptions name
    toJson <- A.deriveFromJSON encodingOptions name
    toTaggedJson <- deriveHasTypeTag name
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

showResolvedType :: HasCallStack => Type -> Q String
showResolvedType type' = resolveTypeSynonyms type' >>= showType True

showUnresolvedType :: HasCallStack => Type -> Q String
showUnresolvedType = showType False

showReturnType :: HasCallStack => Type -> Q String
showReturnType = \case
    AppT _ returnType -> showResolvedType returnType
    other -> error $ "Invalid return type: " <> show other

showType :: HasCallStack => Bool -> Type -> Q String
showType resolve = \case
    ConT name -> if
        | nameBase name == "CFloat" -> pure "Float"
        | nameBase name == "Text" -> pure "String"
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
        | name == ''Ptr -> pure "Void" 
        | name == ''Maybe -> showType resolve innerType
        | otherwise -> error $ "Unrepresentable higher order type: " <> show name
    other -> error $ "Unrepresentable type: " <> show other

showField :: HasCallStack => (Name, Type) -> Q String
showField (fieldName, fieldType) = do
    let nameString =  last (split '_' (last (split '.' $ show fieldName)))
    typeString <- showResolvedType fieldType
    pure $ nameString <> ":" <> typeString

getArgumentTypes :: Type -> [Type]
getArgumentTypes = \case
    AppT (AppT ArrowT argumentType) rest -> argumentType:getArgumentTypes rest
    other -> [other]

showFunctionType :: HasCallStack => Type -> Q String
showFunctionType functionType = 
    case getArgumentTypes functionType of
        [returnType] -> do
            returnTypeString <- showReturnType returnType
            pure $ "= " <> returnTypeString
        types -> do
            returnTypeString <- showReturnType (last types)
            arguments <- foldlM (\acc -> \case 
                AppT (AppT (ConT _) (LitT (StrTyLit argumentName))) argumentType -> do
                    resolvedTypeString <- showResolvedType argumentType
                    pure $ acc <> argumentName <> ":" <> resolvedTypeString <> " "
                other -> error $ "Invalid argument type: " <> show other) 
                "" 
                (init types) 
            pure $ arguments <> "= " <> returnTypeString

generateTypeDefinition :: HasCallStack => Name -> Q [String]
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

generateFunctionDefinition :: HasCallStack => Name -> Q String
generateFunctionDefinition functionName = do
    info <- reify functionName
    case info of
        VarI _ functionType _ -> do
            let name = nameBase functionName
            functionTypeString <- showFunctionType functionType
            pure $ name <> " " <> functionTypeString <> ";"
        _ -> error $ "Invalid function: " <> show functionName

makeAPI :: HasCallStack => [Name] -> [Name] -> DecsQ
makeAPI typeNames functionNames = do
    generateSchemaDecs <- makeGenerateSchema typeNames functionNames
    handleMessageDecs <- makeHandleMessage functionNames
    pure $ generateSchemaDecs <> handleMessageDecs

makeGenerateSchema :: HasCallStack => [Name] -> [Name] -> DecsQ
makeGenerateSchema typeNames functionNames = do
    typeDefinitions <- traverse generateTypeDefinition typeNames                  
    functionDefinitions <- traverse generateFunctionDefinition functionNames
    let generateSchemaName = mkName "generateSchema"
    let body = NormalB $ DoE Nothing [ NoBindS $ AppE (AppE (VarE 'writeSchema) (ListE (LitE . StringL <$> join typeDefinitions))) (ListE (LitE . StringL <$> functionDefinitions)) ]
    let signature = SigD generateSchemaName (AppT (ConT ''IO) (TupleT 0))
    let function = FunD generateSchemaName [Clause [] body []]
    pure [signature, function]

makeHandleMessage :: HasCallStack => [Name] -> DecsQ
makeHandleMessage functionNames = do
    let functionName = mkName "handleMessage"
    let messageTypeName = mkName "messageType"
    let objectName = mkName "object"
    cases <- traverse (makeCase objectName) functionNames
    let body = NormalB $ CaseE (VarE messageTypeName) (cases <> [failPattern])
    let signature = SigD functionName $ AppT (AppT ArrowT (ConT ''Text)) (AppT (AppT ArrowT (AppT (ConT ''KeyMap) (ConT ''A.Value))) (AppT (ConT $ mkName "Ensemble") (AppT (ConT ''KeyMap) (ConT ''A.Value))))
    let function = FunD functionName [Clause [VarP messageTypeName, VarP objectName] body []]
    pure [signature, function]

getArgumentInfo :: Type -> (String, Type)
getArgumentInfo = \case
    AppT (AppT (ConT _) (LitT (StrTyLit argumentName))) argumentType ->  (argumentName, argumentType)
    other -> error $ "Invalid argument type: " <> show other

makeCase :: HasCallStack => Name -> Name -> Q Match 
makeCase objectName functionName = do
    info <- reify functionName
    case info of
        VarI _ functionType _ -> do
            let argumentInfos = getArgumentInfo <$> init (getArgumentTypes functionType)
                makeLookupStatement = \(argumentName, _argumentType) -> do
                    let lookupExpression = multiAppE (VarE 'lookupField) [LitE (StringL argumentName), VarE objectName]
                    pure $ BindS (VarP $ mkName argumentName) lookupExpression
            lookupStatements <- traverse makeLookupStatement argumentInfos
            let resultName = mkName "result"
            let callStatement = BindS (VarP resultName) (multiAppE (VarE functionName) $ AppE (ConE 'Argument) . VarE . mkName <$> (fst <$> argumentInfos))
                returnStatement = NoBindS $ AppE (VarE 'pure) (AppE (VarE 'toTaggedJSON) (VarE resultName))
                statements = lookupStatements <> [callStatement, returnStatement]
                body = NormalB $ DoE Nothing statements
            pure $ Match (LitP $ StringL $ nameBase functionName) body []
        _ -> error $ "Invalid function: " <> show functionName

lookupField :: (A.FromJSON a, Member (Error ApiError) effs) => A.Key -> KeyMap A.Value -> Eff effs a
lookupField key object = 
    case KeyMap.lookup key object of
        Just value -> 
            case A.fromJSON value of
                A.Success a -> pure a
                A.Error parseError -> throwError $ ApiError $ "Parse error on '" <> show key <> "': "  <> parseError
        Nothing -> 
            throwError $ ApiError $ "Missing argument: " <> show key

toSubclassName :: Name -> String
toSubclassName = uncapitalise . filter (/= '_') . nameBase

multiAppE :: Exp -> [Exp] -> Exp
multiAppE base args =
  foldl' AppE base args