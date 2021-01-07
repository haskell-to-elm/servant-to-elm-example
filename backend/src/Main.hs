{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Data.Aeson as Aeson
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Text (hPutDoc)
import DerivingVia
import GHC.Generics
import qualified Generics.SOP as SOP
import Language.Elm.Name (Module)
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import Language.Haskell.To.Elm
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Servant.Options
import Servant
import Servant.API
import Servant.To.Elm
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)
import System.IO (IOMode (..), withFile)

main :: IO ()
main = do
  codegenMain
  serverMain

-- Types (Domain Model)
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

-- Server
type BookAPI = "book" :> Get '[JSON] Book

bookApi :: Proxy BookAPI
bookApi = Proxy

bookExample :: Book
bookExample = Book 1 "Haskell in Depth" "Vitaly Bragilevsky"

server :: Server BookAPI
server = pure bookExample

app :: Application
app =
  logStdoutDev
    . cors (const $ Just policy)
    . provideOptions bookApi
    $ serve bookApi server
  where
    policy = simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}

serverMain :: IO ()
serverMain = do
  let port = 8080
  putStrLn $ "starting server at " <> show port
  run port app

-- Code generation
writeContentsToFile :: forall ann. Module -> Doc ann -> IO ()
writeContentsToFile moduleName contents = do
  let path = T.unpack $ "../frontend/src/" <> T.intercalate "/" moduleName <> ".elm"
  createDirectoryIfMissing True $ takeDirectory path
  print $ "writing " <> path <> " ..."
  withFile path WriteMode (`hPutDoc` contents)

codegenMain :: IO ()
codegenMain = do
  let definitions =
        map (elmEndpointDefinition "Config.urlBase" ["Api", "Api"]) (elmEndpoints @BookAPI)
          <> jsonDefinitions @Book

      modules =
        Pretty.modules $
          Simplification.simplifyDefinition <$> definitions

  forM_ (HashMap.toList modules) $ uncurry writeContentsToFile
