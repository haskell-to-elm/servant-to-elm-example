{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- This module only stores stub data
module StubData where

import Data.List
import DomainModel

stubBooks :: [Book]
stubBooks =
  [ Book 1 "Treasure Island" "https://m.media-amazon.com/images/I/51C6NXR94gL.jpg" (Author 1 "Robert Louis Stevenson"),
    Book 2 "Strange Case of Dr Jekyll and Mr Hyde" "https://m.media-amazon.com/images/I/51e8pkDxjfL.jpg" (Author 1 "Robert Louis Stevenson"),
    Book 3 "Critique of Pure Reason" "https://m.media-amazon.com/images/I/51h+rBXrYeL.jpg" (Author 2 "Immanuel Kant"),
    Book 4 "The Neverending Story" "https://m.media-amazon.com/images/I/51AnD2Fki3L.jpg" (Author 3 "Michael Ende"),
    Book 5 "Momo" "https://m.media-amazon.com/images/I/61AuiRa4nmL.jpg" (Author 3 "Michael Ende"),
    Book 6 "Practical Haskell" "https://m.media-amazon.com/images/I/61j3IHmnqvL.jpg" (Author 4 "Alejandro Serrano"),
    Book 7 "Book of Monads" "https://m.media-amazon.com/images/I/51x-SNjPKjL.jpg" (Author 4 "Alejandro Serrano"),
    Book 8 "Programming in Haskell" "https://m.media-amazon.com/images/I/61Fo+7epgQL.jpg" (Author 5 "Graham Hutton"),
    Book 9 "Haskell Programming from First Principles" "https://haskellbook.com/img/book-cover-front.png" (Author 6 "Chris Allen & Julie Moronuki"),
    Book 10 "Haskell in Depth" "https://m.media-amazon.com/images/I/41t4PD4AwOL.jpg" (Author 7 "Vitaly Bragilevsky")
  ]

stubAuthors :: [Author]
stubAuthors = nub $ (\Book {author = author} -> author) <$> stubBooks

codegenExperiment :: CodegenExperiment
codegenExperiment =
  CodegenExperiment
    { -- adt1 = [Adt1A 42 "Hi", Adt1B True],
      adt2 = [Adt2A 42 "Hi", Adt2B True],
      listOfMaybeMaybeInt = [Nothing, Just Nothing, Just (Just 42)],
      fieldMaybeMaybeInt1 = Nothing,
      fieldMaybeMaybeInt2 = Just Nothing,
      fieldMaybeMaybeInt3 = Just (Just 42),
      text = "some Data.Text value",
      string = "some String value"
      -- unit = ()
    }
