{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Types representing the domain model
module DomainModel where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import DerivingVia
import GHC.Generics
import qualified Generics.SOP as SOP
import Language.Haskell.To.Elm

data Book = Book
  { bookId :: Int,
    title :: Text,
    imageUrl :: Text,
    author :: Author
  }
  deriving (Eq, Show, Read, Generic, Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasElmType, HasElmDecoder Aeson.Value, HasElmEncoder Aeson.Value) via ElmType "Api.Book.Book" Book

data Author = Author
  { authorId :: Int,
    name :: Text
  }
  deriving (Eq, Show, Read, Generic, Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasElmType, HasElmDecoder Aeson.Value, HasElmEncoder Aeson.Value) via ElmType "Api.Author.Author" Author

-- Experiments with JSON encoding and decoding

data Examples = Examples
  { -- A custom sum-type
    example1 :: [Example1],
    -- Maybe Maybe as a value in a list
    -- example2 :: [Maybe (Maybe ())],
    example3 :: [Maybe (Maybe Int)],
    -- Maybe Maybe  as an object field
    example4 :: Maybe (Maybe Int),
    example5 :: Maybe (Maybe Int),
    example6 :: Maybe (Maybe Int)
  }
  deriving (Eq, Show, Read, Generic, Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasElmType, HasElmDecoder Aeson.Value, HasElmEncoder Aeson.Value) via ElmType "Api.Examples.Examples" Examples

data Example1
  = Example1A {field1 :: Int, field2 :: Text}
  | Example1B {field3 :: Bool {- , field4 :: () -}}
  deriving (Eq, Show, Read, Generic, Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasElmType, HasElmDecoder Aeson.Value, HasElmEncoder Aeson.Value) via ElmType "Api.Example1.Example1" Example1
