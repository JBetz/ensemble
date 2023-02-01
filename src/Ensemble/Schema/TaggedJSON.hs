{-# LANGUAGE OverloadedStrings #-}

module Ensemble.Schema.TaggedJSON where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (pack)

class A.ToJSON a => ToTaggedJSON a where
    toTaggedJSON :: a -> A.Object

defaultToTaggedJSON :: A.ToJSON a => String -> a -> A.Object
defaultToTaggedJSON typeName object =
    case A.toJSON object of
        A.Object jsonObject -> KeyMap.insert "@type" typeNameValue jsonObject 
        other -> KeyMap.fromList [("@type", typeNameValue), ("value", other)] 
    where 
        typeNameValue = A.String $ pack typeName
