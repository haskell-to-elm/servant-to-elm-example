{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API

data Book = Book
  { title :: Text,
    authorName :: Text
  }
  deriving (Eq, Show, Read, Generic)

instance FromJSON Book

instance ToJSON Book

type HelloAPI = "book" :> Get '[JSON] Book

helloApi :: Proxy HelloAPI
helloApi = Proxy

server :: Server HelloAPI
server = pure $ Book "Haskell in Depth" "Vitaly Bragilevsky"

app :: Application
app = serve helloApi server

main :: IO ()
main = run 8080 app
