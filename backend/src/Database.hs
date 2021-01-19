{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- This module is responsible for dealing with the database
module Database where

import Control.Exception (throw, try)
import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.Text (Text)
import Database.SQLite.Simple
import DomainModel
import StubData

-- |
-- Create tables and populate them with demo data
initDb :: Connection -> IO ()
initDb conn = do
  putStrLn "Populationg Database with stub data ..."
  createTables
  res <- runExceptT $ traverse_ (uncurry insertAuthorAndBooks) stubBooks
  putStrLn $ either show (const "Done") res
  where
    createTables :: IO ()
    createTables = do
      execute_ conn "PRAGMA foreign_keys = ON" -- Foreign keys don't work in SQLite without this
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

    insertAuthorAndBooks :: NewAuthor -> [NewBook] -> ExceptT ApplicationError IO ()
    insertAuthorAndBooks newAuthor books = do
      authorId <- insertAuthor conn newAuthor
      traverse_ (insertBook conn) $
        (\b -> b {author = ExistingAuthorId $ fromIntegral authorId} :: NewBook) <$> books

{- Functions for querying the database.
The reason for separating them is that they are used
not only in the context of handling requests (which is Server),
but also for seeding the database (which is IO) -}

-- |
-- Provides user-readable error about domain model constraints and other expected situations
data ApplicationError
  = UserReadableError Text
  | InternalError
  deriving stock (Eq, Show)

-- |
-- Returns id because it's useful for seeding the database
-- catches only specific kind of error, which are normal and expected
insertAuthor :: Connection -> NewAuthor -> ExceptT ApplicationError IO Int64
insertAuthor conn NewAuthor {name} =
  withExceptT transformError . ExceptT . try $ do
    execute conn "INSERT INTO authors (name) VALUES (?)" (Only name)
    lastInsertRowId conn
  where
    transformError (SQLError ErrorConstraint "UNIQUE constraint failed: authors.name" _) =
      UserReadableError "Author with such name already exists. The author's name must be unique."
    transformError _ = InternalError

insertBook :: Connection -> NewBook -> ExceptT ApplicationError IO Int64
insertBook conn NewBook {title, author, imageUrl} =
  do
    begin
    authorId <-
      ( case author of
          ExistingAuthorId x -> pure $ fromIntegral x
          CreateNewAuthor newAuthor -> insertAuthor conn newAuthor
        )
    bookId <- ExceptT . fmap (first transformError) . try $ do
      execute conn "INSERT INTO books (title, author_id, image_url) VALUES (?, ?, ?)" (title, authorId, imageUrl)
      lastInsertRowId conn
    commit
    pure bookId
    -- Errors lead to rollback.
    -- So, an Author will be written only if a Book can be written.
    -- This can happen, for example, if we put one more uniqueness constraint on the books table
    `catchError` (\e -> rollback >> throwError e)
  where
    -- "Exclusive" transaction provides highest isolation level in SQLite.
    begin = lift $ execute_ conn "BEGIN EXCLUSIVE TRANSACTION"
    commit = lift $ execute_ conn "COMMIT TRANSACTION"
    rollback = lift $ execute_ conn "ROLLBACK TRANSACTION"
    transformError (SQLError ErrorConstraint "UNIQUE constraint failed: books.title, books.author_id" _) =
      UserReadableError "This author already has a book with such a title. Book title must be unique per author."
    transformError (SQLError ErrorConstraint "FOREIGN KEY constraint failed" _) =
      UserReadableError "An author with such an id does not exist."
    transformError e = throw e

-- |
-- SQL has special syntax for regular expressions
-- % before and after means that the substring can appear anywhere in the text
-- (which is not an ideal approach when causes database not to use index - because a starting substring can be anything)
toPattern :: Text -> Text
toPattern t = "%" <> t <> "%"

queryBooks :: Connection -> Maybe Text -> IO [Book]
queryBooks conn maybeQuery =
  fmap fromSqlRow
    <$> query
      conn
      "SELECT books.id, books.title, books.image_url, authors.id, authors.name \
      \ FROM books \
      \ JOIN authors \
      \ ON books.author_id=authors.id \
      \ WHERE books.title LIKE ? \
      \ ORDER BY books.title"
      (Only $ maybe "%" toPattern maybeQuery)
  where
    fromSqlRow (bookId, title, imageUrl, authorId, authorName) =
      Book bookId title imageUrl (Author authorId authorName)

queryAuthors :: Connection -> Maybe Text -> IO [Author]
queryAuthors conn maybeQuery =
  fmap (uncurry Author)
    <$> query
      conn
      "SELECT id, name FROM authors WHERE name LIKE ? ORDER BY name"
      (Only $ maybe "%" toPattern maybeQuery)
