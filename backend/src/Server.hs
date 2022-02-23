-- |
-- This module is responsible for defining server API type (the entire API surface)
-- and functions that run the server
module Server where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import Database
import Database.SQLite.Simple
import DomainModel
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant

runServer :: IO ()
runServer = do
  let port = (8080 :: Int)
  let dbfile = ":memory:"

  withConnection dbfile $ \conn -> do
    initDb conn
    putStrLn $ "Starting server at port " <> show port
    run port (app conn)

app :: Connection -> Application
app conn =
  logStdoutDev
    . cors (const $ Just corsPolicy)
    $ serve libraryApi (server conn)
  where
    -- Content-Type header is necessary for POST requests
    corsPolicy = simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}

libraryApi :: Proxy LibraryAPI
libraryApi = Proxy

-- This API is not intended to look like idiomatic REST.
-- Instead, it is shaped to match the usage scenarios.
type LibraryAPI =
  "books" :> QueryParam "query" Text :> Get '[JSON] [Book]
    -- TODO: Missing instance of HasElmType: can't just use PostCreated (which is Verb 'POST 201)
    :<|> "book" :> ReqBody '[JSON] NewBook :> PostNoContent
    :<|> "authors" :> QueryParam "query" Text :> Get '[JSON] [Author]
    :<|> "search" :> QueryParam "query" Text :> Get '[JSON] UniversalSearchResults

server :: Connection -> Server LibraryAPI
server conn =
  getBooks
    :<|> postBook
    :<|> getAuthors
    :<|> search
  where
    {- Handlers are named similar to functions, generated by servant-to-elm -}
    getBooks = liftIO . queryBooks conn

    getAuthors = liftIO . queryAuthors conn

    postBook :: NewBook -> Handler NoContent
    postBook newBook = do
      insertionResult <- liftIO . runExceptT $ insertBook conn newBook
      case insertionResult of
        Right _ -> pure NoContent
        Left (UserReadableError e) -> throwError $ err400 {errBody = LTE.encodeUtf8 $ LT.fromStrict e}
        Left InternalError -> throwError err500

    -- Combined search for data of all types
    search :: Maybe Text -> Handler UniversalSearchResults
    search maybeQuery = UniversalSearchResults <$> getAuthors maybeQuery <*> getBooks maybeQuery
