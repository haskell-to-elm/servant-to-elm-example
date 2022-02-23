{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- This module is a Special Boilerplate to define ElmType and reduce boilerplate
module DerivingVia where

import qualified Data.Aeson as Aeson
import Data.Proxy
import Data.String (fromString)
import qualified Data.Text as T
import GHC.Generics (Generic, Rep)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Generics.SOP as SOP
import qualified Language.Elm.Name as Name
import Language.Haskell.To.Elm

newtype ElmType (name :: Symbol) a
  = ElmType a

instance
  (Generic a, Aeson.GToJSON Aeson.Zero (Rep a)) =>
  Aeson.ToJSON (ElmType name a)
  where
  toJSON (ElmType a) =
    Aeson.genericToJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = dropWhile (== '_')} a

instance
  (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)) =>
  Aeson.FromJSON (ElmType name a)
  where
  parseJSON =
    fmap ElmType . Aeson.genericParseJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = dropWhile (== '_')}

instance
  (SOP.HasDatatypeInfo a, SOP.All2 HasElmType (SOP.Code a), KnownSymbol name) =>
  HasElmType (ElmType name a)
  where
  elmDefinition =
    Just $
      deriveElmTypeDefinition @a defaultOptions {fieldLabelModifier = dropWhile (== '_')} $
        fromString $ symbolVal $ Proxy @name

instance
  (SOP.HasDatatypeInfo a, HasElmType a, SOP.All2 (HasElmDecoder Aeson.Value) (SOP.Code a), HasElmType (ElmType name a), KnownSymbol name) =>
  HasElmDecoder Aeson.Value (ElmType name a)
  where
  elmDecoderDefinition =
    Just $
      deriveElmJSONDecoder
        @a
        defaultOptions {fieldLabelModifier = dropWhile (== '_')}
        Aeson.defaultOptions {Aeson.fieldLabelModifier = dropWhile (== '_')}
        $ Name.Qualified moduleName $ lowerName <> "Decoder"
    where
      Name.Qualified moduleName name = fromString $ symbolVal $ Proxy @name
      lowerName = T.toLower (T.take 1 name) <> T.drop 1 name

instance
  (SOP.HasDatatypeInfo a, HasElmType a, SOP.All2 (HasElmEncoder Aeson.Value) (SOP.Code a), HasElmType (ElmType name a), KnownSymbol name) =>
  HasElmEncoder Aeson.Value (ElmType name a)
  where
  elmEncoderDefinition =
    Just $
      deriveElmJSONEncoder
        @a
        defaultOptions {fieldLabelModifier = dropWhile (== '_')}
        Aeson.defaultOptions {Aeson.fieldLabelModifier = dropWhile (== '_')}
        $ Name.Qualified moduleName $ lowerName <> "Encoder"
    where
      Name.Qualified moduleName name = fromString $ symbolVal $ Proxy @name
      lowerName = T.toLower (T.take 1 name) <> T.drop 1 name
