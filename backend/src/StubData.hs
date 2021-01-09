{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- This module only stores stub data
module StubData where

import DomainModel

stubBooks :: [(NewAuthor, [NewBook])]
stubBooks =
  [ ( NewAuthor "Robert Louis Stevenson",
      [ NewBook "Treasure Island" (ExistingAuthorId 0) "https://m.media-amazon.com/images/I/51C6NXR94gL.jpg",
        NewBook "Strange Case of Dr Jekyll and Mr Hyde" (ExistingAuthorId 0) "https://m.media-amazon.com/images/I/51e8pkDxjfL.jpg"
      ]
    ),
    ( NewAuthor "Immanuel Kant",
      [NewBook "Critique of Pure Reason" (ExistingAuthorId 0) "https://m.media-amazon.com/images/I/51h+rBXrYeL.jpg"]
    ),
    ( NewAuthor "Michael Ende",
      [ NewBook "The Neverending Story" (ExistingAuthorId 0) "https://m.media-amazon.com/images/I/51AnD2Fki3L.jpg",
        NewBook "Momo" (ExistingAuthorId 0) "https://m.media-amazon.com/images/I/61AuiRa4nmL.jpg"
      ]
    ),
    ( NewAuthor "Alejandro Serrano",
      [ NewBook "Practical Haskell" (ExistingAuthorId 0) "https://m.media-amazon.com/images/I/61j3IHmnqvL.jpg",
        NewBook "Book of Monads" (ExistingAuthorId 0) "https://m.media-amazon.com/images/I/51x-SNjPKjL.jpg"
      ]
    ),
    ( NewAuthor "Graham Hutton",
      [ NewBook "Programming in Haskell" (ExistingAuthorId 0) "https://m.media-amazon.com/images/I/61Fo+7epgQL.jpg"
      ]
    ),
    ( NewAuthor "Chris Allen & Julie Moronuki",
      [ NewBook "Haskell Programming from First Principles" (ExistingAuthorId 0) "https://haskellbook.com/img/book-cover-front.png"
      ]
    ),
    ( NewAuthor "Vitaly Bragilevsky",
      [ NewBook "Haskell in Depth" (ExistingAuthorId 0) "https://m.media-amazon.com/images/I/41t4PD4AwOL.jpg"
      ]
    )
  ]

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
      string = "some String value",
      int = [],
      -- int16 = [],
      -- int32 = [],
      -- int64 = [],
      -- integer = [],
      -- float = [],
      double = []
      -- unit = ()
    }
