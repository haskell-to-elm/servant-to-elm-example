{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- This module is responsible for generating Elm files, but not for defining types
module Codegen where

import Control.Monad
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Text (hPutDoc)
import DomainModel
import Language.Elm.Definition (Definition)
import Language.Elm.Name (Module)
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import Servant.To.Elm
import Server
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, findExecutable, removeDirectoryRecursive)
import System.Exit
import System.FilePath.Posix (takeDirectory)
import System.IO (IOMode (..), withFile)
import System.Process (readProcessWithExitCode)

elmSrcPath :: FilePath
elmSrcPath = "../frontend/src/"

writeContentsToFile :: forall ann. Module -> Doc ann -> IO ()
writeContentsToFile moduleName contents = do
  let path = elmSrcPath <> T.unpack (T.intercalate "/" moduleName) <> ".elm"
  -- It's important to check and create missing directories individually, because there may several directories
  createDirectoryIfMissing True $ takeDirectory path
  putStrLn $ "Writing file: " <> path
  withFile path WriteMode (`hPutDoc` contents)

endpointDefinitions :: [Definition]
endpointDefinitions = map (elmEndpointDefinition "Config.urlBase" ["Api", "Api"]) (elmEndpoints @LibraryAPI)

recursivelyDeleteDirectories :: [Text] -> IO ()
recursivelyDeleteDirectories subdirectories = do
  let directories = (elmSrcPath <>) . T.unpack <$> subdirectories
  forM_ directories $ \d -> do
    doesExist <- doesDirectoryExist d
    when doesExist $ do
      putStrLn ""
      putStrLn $ "Recursively deleting a directory: " <> d
      removeDirectoryRecursive d

formatGeneratedCode :: [Text] -> IO ExitCode
formatGeneratedCode directories = do
  putStrLn ""
  mElmFormat <- findExecutable "elm-format"
  case mElmFormat of
    Just elmFormatPath -> do
      putStrLn $ "Found elm-format: " <> elmFormatPath
      putStrLn ""
      codes <- forM directories $ \dirName -> do
        let dir = elmSrcPath <> T.unpack dirName
        putStrLn $ "Formatting .elm files in a directory (recursively): " <> dir
        (code, stdout, stderr) <- readProcessWithExitCode elmFormatPath [dir, "--yes"] ""
        unless (null stdout) $ putStrLn stdout
        unless (null stderr) $ putStrLn stderr
        return code
      return . fromMaybe ExitSuccess $ find (/= ExitSuccess) codes
    Nothing -> do
      TIO.putStrLn "elm-format was not found in PATH, leaving output unformatted."
      return ExitSuccess

runCodegen :: IO ()
runCodegen = do
  -- Since there isn't any restriction on writing the generated code to different directories,
  -- we can't say for sure which the previous directories were,
  -- therefore we explicitly specify a list of directories we want to be deleted recursively before writing new files.
  recursivelyDeleteDirectories ["Api"]
  TIO.putStrLn ""
  TIO.putStrLn "Generating Elm types and codecs..."
  -- Combine endpoint definitions with type definitions and divide them into modules
  let modules = Pretty.modules $ Simplification.simplifyDefinition <$> (endpointDefinitions <> typeDefinitions)
  -- For each module write contents to file
  forM_ (HashMap.toList modules) $ uncurry writeContentsToFile
  formatGeneratedCode ["Api"] >>= exitWith
