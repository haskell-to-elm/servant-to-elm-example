{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- This module is responsible for dealing with the database
module Database where

import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.Text (Text)
import Database.SQLite.Simple
import DomainModel
import StubData

{- Instances for Database interaction -}
instance FromRow Author where
  fromRow = Author <$> field <*> field

instance FromRow Book where
  fromRow =
    ( \bookId title imageUrl authorId authorName ->
        Book bookId title imageUrl (Author authorId authorName)
    )
      <$> field <*> field <*> field <*> field <*> field

-- |
-- Create tables and populate them with demo data
initDb :: Connection -> IO ()
initDb conn = do
  putStrLn "Populationg Database with stub data ..."
  createTables
  traverse_ (uncurry insertAuthorAndBooks) stubBooks
  (query_ conn "SELECT id, name FROM authors" :: IO [Author]) >>= print
    >>= print
  where
    createTables :: IO ()
    createTables = do
      execute_
        conn
        "CREATE TABLE authors ( id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL UNIQUE )"
      execute_
        conn
        "CREATE TABLE books ( \
        \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
        \ title TEXT NOT NULL, \
        \ author_id INTEGER NOT NULL REFERENCES authors(id), \
        \ image_url TEXT NOT NULL, \
        \ CONSTRAINT unique_book_title_per_author UNIQUE (title, author_id) \
        \ )"

    insertAuthorAndBooks :: NewAuthor -> [NewBook] -> IO ()
    insertAuthorAndBooks newAuthor books = do
      authorId <- insertAuthor conn newAuthor
      traverse_ (insertBook conn) $
        (\b -> b {author = ExistingAuthorId $ fromIntegral authorId} :: NewBook) <$> books

{- Functions for querying the database.
The reason for separating them is that they are used
not only in the context of handling requests (which is Server),
but also for seeding the database (which is IO) -}

-- |
-- Returns id because it's useful for seeding the database
insertAuthor :: Connection -> NewAuthor -> IO Int64
insertAuthor conn NewAuthor {name = name} = do
  execute conn "INSERT INTO authors (name) VALUES (?)" (Only name)
  lastInsertRowId conn

-- |
-- Returns id just to preserve style, never used :D
insertBook :: Connection -> NewBook -> IO Int64
insertBook conn NewBook {title = title, author = author, imageUrl = imageUrl} =
  case author of
    ExistingAuthorId existingId -> do
      execute conn "INSERT INTO books (title, author_id, image_url) VALUES (?, ?, ?)" (title, existingId, imageUrl)
      lastInsertRowId conn
    -- "Exclusive" transaction provides highest isolation level in SQLite, AFAIK.
    -- Any exception, be it database or IO, leads to rollback.
    -- So, an Author will be written only if a Book can be written.
    -- This can happen, for example, if we put one more uniqueness constraint on the books table.
    CreateNewAuthor newAuthor -> withExclusiveTransaction conn $ do
      authorId <- insertAuthor conn newAuthor
      execute conn "INSERT INTO books (title, author_id, image_url) VALUES (?, ?, ?)" (title, authorId, imageUrl)
      lastInsertRowId conn

-- |
-- SQL has special syntax for regular expressions
-- % before and after means that the substring can appear anywhere in the text
-- (which is not an ideal approach when causes database not to use index - because a starting substring can be anything)
toPattern :: Text -> Text
toPattern t = "%" <> t <> "%"

queryBooks :: Connection -> Maybe Text -> IO [Book]
queryBooks conn (Just queryString) =
  query
    conn
    "SELECT books.id, books.title, books.image_url, authors.id, authors.name \
    \ FROM books \
    \ JOIN authors \
    \ ON books.author_id=authors.id \
    \ WHERE books.title LIKE ? \
    \ ORDER BY books.title"
    (Only $ toPattern queryString)
-- in case of in-memory database omiting regex filter really won't improve anything,
-- but in filesystem-based dbs like Postges a different story.
queryBooks conn Nothing =
  query_
    conn
    "SELECT books.id, books.title, books.image_url, authors.id, authors.name \
    \ FROM books \
    \ JOIN authors \
    \ ON books.author_id=authors.id \
    \ ORDER BY books.title"

queryAuthors :: Connection -> Maybe Text -> IO [Author]
queryAuthors conn (Just queryString) =
  query
    conn
    "SELECT id, name FROM authors WHERE name LIKE ? ORDER BY name"
    (Only $ toPattern queryString)
queryAuthors conn Nothing = query_ conn "SELECT authors.id, authors.name FROM authors"
