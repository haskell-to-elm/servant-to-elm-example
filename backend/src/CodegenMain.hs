{-# LANGUAGE RankNTypes #-}

-- |
-- This module is responsible for generating Elm files, but not for defining types
module Main where

import Codegen (runCodegen)

main :: IO ()
main = runCodegen