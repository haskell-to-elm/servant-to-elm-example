{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- This module is responsible for defining server API type (the entire API surface)
--  and olso functions that run the server
module Server where

import DomainModel
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Servant.Options
import Servant
import StubData

type LibraryAPI =
  "books" :> Get '[JSON] [Book]
    :<|> "authors" :> Get '[JSON] [Author]
    :<|> "examples" :> Get '[JSON] Examples

libraryApi :: Proxy LibraryAPI
libraryApi = Proxy

server :: Server LibraryAPI
server =
  pure stubBooks
    :<|> pure stubAuthors
    :<|> pure stubExamples

app :: Application
app =
  logStdoutDev
    . cors (const $ Just policy) -- appy CORS things
    . provideOptions libraryApi -- options are necessary for POST requests
    $ serve libraryApi server -- actually run the server
  where
    policy = simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]} -- also for POST requests, AFAIK

serverMain :: IO ()
serverMain = do
  let port = 8080
  putStrLn $ "Starting server at port " <> show port
  run port app