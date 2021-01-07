module Main where

import Codegen
import Server

main :: IO ()
main = do
  runCodegen
  serverMain