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
    authorName :: Text
  }
  deriving (Eq, Show, Read, Generic, Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasElmType, HasElmDecoder Aeson.Value, HasElmEncoder Aeson.Value) via ElmType "Api.Book.Book" Book

data Author = Author
  { authorId :: Int,
    name :: Text
  }
  deriving (Eq, Show, Read, Generic, Aeson.ToJSON, Aeson.FromJSON, SOP.Generic, SOP.HasDatatypeInfo)
  deriving (HasElmType, HasElmDecoder Aeson.Value, HasElmEncoder Aeson.Value) via ElmType "Api.Author.Author" Author