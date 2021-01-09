{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- This module is responsible for defining server API type (the entire API surface)
--  and olso functions that run the server
module Server where

import Control.Monad.IO.Class
import Data.Text (Text)
import Database
import Database.SQLite.Simple
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
  let dbfile = ":memory:"

  withConnection dbfile $ \conn -> do
    initDb conn
    putStrLn $ "Starting server at port " <> show port
    run port (app conn)

app :: Connection -> Application
app conn =
  logStdoutDev
    . cors (const $ Just policy) -- apply CORS things
    . provideOptions libraryApi -- options are necessary for POST requests
    $ serve libraryApi (server conn)
  where
    policy = simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]} -- for POST requests, AFAIK

libraryApi :: Proxy LibraryAPI
libraryApi = Proxy

-- This API is not intended to look like ideomatic REST.
-- Instead, it is shaped to match the usage scenarios.
type LibraryAPI =
  "books" :> QueryParam "query" Text :> Get '[JSON] [Book]
    :<|> "book" :> ReqBody '[JSON] NewBook :> PostNoContent '[JSON] NoContent -- Missing instance: can't use PostCreated (which is Verb 'POST 201)
    :<|> "authors" :> QueryParam "query" Text :> Get '[JSON] [Author]
    :<|> "search" :> QueryParam "query" Text :> Get '[JSON] UniversalSearchResults
    :<|> "experiment" :> Get '[JSON] CodegenExperiment

server :: Connection -> Server LibraryAPI
server conn =
  getBooks
    :<|> postBook
    :<|> getAuthors
    :<|> search
    :<|> pure codegenExperiment
  where
    {- handlers are named similar to functions, generated by servant-to-elm -}
    getBooks = liftIO . queryBooks conn
    getAuthors = liftIO . queryAuthors conn
    postBook = liftIO . fmap (const NoContent) . insertBook conn

    -- combined search for data of all types
    search :: Maybe Text -> Handler UniversalSearchResults
    search maybeQuery = UniversalSearchResults <$> getAuthors maybeQuery <*> getBooks maybeQuery
