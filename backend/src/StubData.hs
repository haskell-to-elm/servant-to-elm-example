{-# LANGUAGE OverloadedStrings #-}

-- |
-- This module only stores stub data
module StubData where

import DomainModel

stubBook :: Book
stubBook = Book 1 "Haskell in Depth" "Vitaly Bragilevsky"

stubAuthor :: Author
stubAuthor = Author 1 "Vitaly Bragilevsky"
