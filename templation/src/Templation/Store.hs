{-# LANGUAGE OverloadedStrings #-}

-- | Create a Stack Template
--
-- The format is not really explained anywhere,
-- but it's basically a list of mustache templates
-- wrapped in School of Haskell Markdown Multi-File syntax.
--
-- https://www.schoolofhaskell.com/school/how-to-use-the-school-of-haskell/soh-markdown#multi-file-snippets
module Templation.Store
  ( Config (..),
    makeTemplate,
  )
where

import Control.Monad (foldM)
import Data.ByteString.Lazy qualified as LB
import Data.Foldable ()
import Data.Function ((&))
import Data.List (sort)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Data.Text.Lazy.IO qualified as LT
import System.Directory (listDirectory)
import System.FilePath (isExtensionOf)
import System.PosixCompat (getFileStatus, isDirectory)

-- | Recursively list all relevant files
--
-- Directories like @.git@, @.stack-work@ or @dist-newstyle@ are skipped.
-- Ideally, we would parse .gitignore Files as well
listFiles :: FilePath -> IO [FilePath]
listFiles = go []
  where
    go :: [FilePath] -> FilePath -> IO [FilePath]
    go acc p = do
      entries <- listDirectory p
      ( foldM prependPath acc
          . fmap (combinePath p)
          . filter (not . (`elem` [".git", ".stack-work", "dist-newstyle", "stack.yaml.lock"]))
          . filter (not . ("hsfiles" `isExtensionOf`))
        )
        entries
    prependPath :: [FilePath] -> FilePath -> IO [FilePath]
    prependPath acc p = do
      st <- getFileStatus p
      if isDirectory st
        then go acc p
        else pure (p : acc)

-- | Combine 2 filepaths
--
-- >>> combinePath "a" "b"
-- "a/bc"
-- >>> combinePath "" "b"
-- "b"
-- >>> combinePath "." "b"
-- "b"
combinePath :: FilePath -> FilePath -> FilePath
combinePath "" p2 = p2
combinePath "." p2 = p2
combinePath p1 p2 = p1 <> "/" <> p2

readSourceFile :: Config -> FilePath -> IO LT.Builder
readSourceFile cfg p = startFile . LT.decodeUtf8 <$> LB.readFile p
  where
    startFile bs =
      LT.fromLazyText "{-# START_FILE "
        <> processContent cfg (LT.pack p)
        <> " #-}\n"
        <> processContent cfg bs

combineSourceFiles :: Config -> LT.Builder -> FilePath -> IO LT.Builder
combineSourceFiles cfg b p = (b <>) <$> readSourceFile cfg p

makeHSFile :: Config -> IO LT.Builder
makeHSFile cfg =
  listFiles (project cfg)
    >>= foldM (combineSourceFiles cfg) (LT.fromLazyText LT.empty) . sort

processContent :: Config -> LT.Text -> LT.Builder
processContent Config {name, username, email} txt =
  txt
    & LT.splitOn name
    & LT.intercalate "{{name}}"
    & LT.splitOn username
    & LT.intercalate "{{author}}"
    & LT.splitOn email
    & LT.intercalate "{{email}}"
    & LT.fromLazyText

data Config = Config
  { project :: FilePath,
    name :: LT.Text,
    username :: LT.Text,
    email :: LT.Text,
    output :: FilePath,
    verbose :: Bool
  }
  deriving (Show)

makeTemplate :: Config -> IO ()
makeTemplate cfg =
  makeHSFile cfg
    >>= writeOutput (output cfg)
  where
    writeOutput "" = LT.putStr . LT.toLazyText
    writeOutput fn = LT.writeFile fn . LT.toLazyText
