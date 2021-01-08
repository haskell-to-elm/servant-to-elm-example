{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- This module is responsible for defining server API type (the entire API surface)
--  and olso functions that run the server
module Server where

import Data.Function
import Data.Text (Text)
import qualified Data.Text as T
import DomainModel
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Servant.Options
import Servant
import StubData

runServer :: IO ()
runServer = do
  let port = 8080
  putStrLn $ "Starting server at port " <> show port
  run port app

app :: Application
app =
  logStdoutDev
    . cors (const $ Just policy) -- apply CORS things
    . provideOptions libraryApi -- options are necessary for POST requests
    $ serve libraryApi server
  where
    policy = simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]} -- also for POST requests, AFAIK

libraryApi :: Proxy LibraryAPI
libraryApi = Proxy

-- This API is not intended to look like ideomatic REST.
-- Instead, it is shaped to match the usage scenarios.
type LibraryAPI =
  "books" :> QueryParam "query" Text :> Get '[JSON] [Book]
    :<|> "book" :> ReqBody '[JSON] NewBook :> PostNoContent '[JSON] NoContent -- Missing implementation: can't use PostCreated (which is Verb 'POST 201)
    :<|> "authors" :> QueryParam "query" Text :> Get '[JSON] [Author]
    :<|> "search" :> QueryParam "query" Text :> Get '[JSON] UniversalSearchResults -- combined search for data of all types
    :<|> "experiment" :> Get '[JSON] CodegenExperiment

server :: Server LibraryAPI
server =
  booksHandler
    :<|> (\_ -> pure NoContent)
    :<|> authorsHandler
    :<|> searchHandler
    :<|> pure codegenExperiment

booksHandler :: Maybe Text -> Handler [Book]
booksHandler (Just query) =
  pure $ filter (\Book {title = title} -> (T.isInfixOf `on` T.toLower) query title) stubBooks
booksHandler _ = pure stubBooks

authorsHandler :: Maybe Text -> Handler [Author]
authorsHandler (Just query) =
  pure $ filter (\Author {name = name} -> (T.isInfixOf `on` T.toLower) query name) stubAuthors
authorsHandler _ = pure stubAuthors

searchHandler :: Maybe Text -> Handler UniversalSearchResults
searchHandler mQuery = UniversalSearchResults <$> authorsHandler mQuery <*> booksHandler mQuery