{-# LANGUAGE OverloadedStrings #-}

module Ensemble.Schema.TaggedJSON where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text, pack)

class HasTypeTag a where
    typeTag :: a -> String

instance HasTypeTag Text where
    typeTag _ = "String"

instance HasTypeTag a => HasTypeTag [a] where
    typeTag (first:_) = "vector<" <> typeTag first <> ">"
    typeTag [] = "vector<Void>"

toTaggedJSON :: (HasTypeTag a, A.ToJSON a) => a -> A.Value
toTaggedJSON object =
    A.Object $ case A.toJSON object of
        A.Object jsonObject -> KeyMap.insert "@type" typeNameValue jsonObject 
        other -> KeyMap.fromList [("@type", typeNameValue), ("value", other)] 
    where 
        typeNameValue = A.String $ pack (typeTag object)

toTaggedCustomJSON :: A.ToJSON a => String -> a -> A.Value
toTaggedCustomJSON typeTag object =
    A.Object $ case A.toJSON object of
        A.Object jsonObject -> KeyMap.insert "@type" typeNameValue jsonObject 
        other -> KeyMap.fromList [("@type", typeNameValue), ("value", other)] 
    where 
        typeNameValue = A.String $ pack typeTag