{-# LANGUAGE OverloadedStrings #-}

-- |
-- This module only stores stub data
module StubData where

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
stubAuthors =
  [ Author 1 "Robert Louis Stevenson",
    Author 2 "Immanuel Kant",
    Author 3 "Michael Ende",
    Author 4 "Alejandro Serrano",
    Author 5 "Graham Hutton",
    Author 6 "Chris Allen & Julie Moronuki",
    Author 7 "Vitaly Bragilevsky"
  ]

-- Experiments with JSON encoding and decoding

stubExamples :: Examples
stubExamples =
  Examples
    { example1 = [Example1A 42 "Hi", Example1B True ()],
      example2 = [Nothing, Just Nothing, Just (Just ())],
      example3 = [Nothing, Just Nothing, Just (Just 42)],
      example4 = Nothing,
      example5 = Just Nothing,
      example6 = Just (Just 42)
    }

{-
Encoded by Aeson as:
  {
    "example1": [
      { "tag": "Example1A", "field1": 42, "field2": "Hi" },
      { "tag": "Example1B", "field3": true, "field4": [] }
    ],
    "example2": [null, null, []],
    "example3": [null, null, 42],
    "example4": null,
    "example5": null,
    "example6": 42
  }
-}