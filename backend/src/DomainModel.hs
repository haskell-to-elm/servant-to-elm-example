{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Types representing the domain model
module DomainModel where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import DerivingVia
import GHC.Generics
import qualified Generics.SOP as SOP
import Language.Elm.Definition (Definition)
import Language.Haskell.To.Elm

-- |
-- List of type definitions to be written to .elm files
-- Each new type from domain model should be added there,
-- otherwise the root Elm module will fail to import some missing module,
-- or will refer to the type which definition was not written to file:
typeDefinitions :: [Definition]
typeDefinitions =
  jsonDefinitions @Book
    <> jsonDefinitions @Author
    <> jsonDefinitions @Examples
    -- <> jsonDefinitions @Adt1 -- Error in elm decoder
    <> jsonDefinitions @Adt2

data Book = Book
  { bookId :: Int,
    title :: Text,
    imageUrl :: Text,
    author :: Author
  }
  deriving (Eq, Show, Read, Generic, Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasElmType, HasElmDecoder Aeson.Value, HasElmEncoder Aeson.Value) via ElmType "Api.Book.Book" Book

data NewBook = NewBook
  { title :: String,
    author :: NewBookAuthor,
    imageUrl :: String
  }
  deriving (Eq, Show, Read, Generic, Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasElmType, HasElmDecoder Aeson.Value, HasElmEncoder Aeson.Value) via ElmType "Api.Book.NewBook" NewBook

data NewBookAuthor
  = CreateNewAuthor NewAuthor
  | ExistingAuthorId Int
  deriving (Eq, Show, Read, Generic, Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasElmType, HasElmDecoder Aeson.Value, HasElmEncoder Aeson.Value) via ElmType "Api.Book.NewBookAuthor" NewBookAuthor

data Author = Author
  { authorId :: Int,
    name :: Text
  }
  deriving (Eq, Show, Read, Generic, Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasElmType, HasElmDecoder Aeson.Value, HasElmEncoder Aeson.Value) via ElmType "Api.Author.Author" Author

newtype NewAuthor = NewAuthor {name :: String}
  deriving (Eq, Show, Read, Generic, Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasElmType, HasElmDecoder Aeson.Value, HasElmEncoder Aeson.Value) via ElmType "Api.Author.NewAuthor" NewAuthor

-- Experiments with JSON encoding and decoding

data Examples = Examples
  { -- A custom sum-type
    -- adt1 :: [Adt1],
    adt2 :: [Adt2],
    -- Maybe Maybe as a value in a list
    example3 :: [Maybe (Maybe Int)],
    -- Maybe Maybe  as an object field
    example4 :: Maybe (Maybe Int),
    example5 :: Maybe (Maybe Int),
    example6 :: Maybe (Maybe Int)
    -- unit :: () -- Error: no instance for HasElmType ()
  }
  deriving (Eq, Show, Read, Generic, Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasElmType, HasElmDecoder Aeson.Value, HasElmEncoder Aeson.Value) via ElmType "Api.Examples.Examples" Examples

data Adt1 -- Error in elm decoder, so not saved to .elm file
  = Adt1A {field1 :: Int, field2 :: Text}
  | Adt1B {field3 :: Bool}
  deriving (Eq, Show, Read, Generic, Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasElmType, HasElmDecoder Aeson.Value, HasElmEncoder Aeson.Value) via ElmType "Api.Examples.Adt1" Adt1

data Adt2
  = Adt2A Int Text
  | Adt2B Bool
  deriving (Eq, Show, Read, Generic, Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasElmType, HasElmDecoder Aeson.Value, HasElmEncoder Aeson.Value) via ElmType "Api.Examples.Adt2" Adt2
