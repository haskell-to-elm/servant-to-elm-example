{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- This module is responsible for generating Elm files, but not for defining types
module Codegen where

import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Text (hPutDoc)
import DomainModel
import Language.Elm.Name (Module)
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import Language.Haskell.To.Elm
import Servant.To.Elm
import Server
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)
import System.IO (IOMode (..), withFile)

writeContentsToFile :: forall ann. Module -> Doc ann -> IO ()
writeContentsToFile moduleName contents = do
  let path = T.unpack $ "../frontend/src/" <> T.intercalate "/" moduleName <> ".elm"
  createDirectoryIfMissing True $ takeDirectory path
  putStrLn $ "Writing file: " <> path
  withFile path WriteMode (`hPutDoc` contents)

runCodegen :: IO ()
runCodegen = do
  putStrLn "Generating Elm types and codecs..."
  -- Collect all definitions
  let definitions =
        map (elmEndpointDefinition "Config.urlBase" ["Api", "Api"]) (elmEndpoints @LibraryAPI)
          -- Each new type from domain model should be added there
          -- (otherwise the root Elm module will fail to import some missing module):
          <> jsonDefinitions @Book
          <> jsonDefinitions @Author
      -- Combine definitions into modules
      modules = Pretty.modules $ Simplification.simplifyDefinition <$> definitions
  -- For each module write contents to file
  forM_ (HashMap.toList modules) $ uncurry writeContentsToFile
