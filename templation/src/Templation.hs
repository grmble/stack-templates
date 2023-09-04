{-# LANGUAGE OverloadedStrings #-}

-- | Create and modify Stack Templates
--
-- The format is not really explained anywhere,
-- but it's basically a list of mustache templates
-- wrapped in School of Haskell Markdown Multi-File syntax.
--
-- https://www.schoolofhaskell.com/school/how-to-use-the-school-of-haskell/soh-markdown#multi-file-snippets
module Templation where

import Control.Monad (foldM)
import Data.ByteString.Lazy qualified as LB
import Data.Foldable ()
import Data.Function ((&))
import Data.List (sort)
import Data.Text.Conversions qualified as TC
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Data.Text.Lazy.IO qualified as LT
import System.Directory (getCurrentDirectory, listDirectory)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (isExtensionOf, takeFileName)
import System.PosixCompat (getFileStatus, isDirectory)
import Templation.Git (gitConfig)

data Config = Config
  { name :: LT.Text,
    author :: LT.Text,
    email :: LT.Text,
    path :: FilePath,
    replace :: Bool
  }
  deriving (Show)

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
        <> processContent cfg (TC.convertText p)
        <> " #-}\n"
        <> processContent cfg bs

combineSourceFiles :: Config -> LT.Builder -> FilePath -> IO LT.Builder
combineSourceFiles cfg b p = (b <>) <$> readSourceFile cfg p

makeHSFile :: Config -> IO LT.Builder
makeHSFile cfg =
  listFiles (path cfg)
    >>= foldM (combineSourceFiles cfg) (LT.fromLazyText LT.empty) . sort

processContent :: Config -> LT.Text -> LT.Builder
processContent cfg txt =
  if replace cfg
    then
      txt
        & LT.splitOn (name cfg)
        & LT.intercalate "{{name}}"
        & LT.splitOn (author cfg)
        & LT.intercalate "{{author}}"
        & LT.splitOn (email cfg)
        & LT.intercalate "{{email}}"
        & LT.fromLazyText
    else txt & LT.fromLazyText

parseOptions :: Config -> [String] -> Either String Config
parseOptions = foldM handleOpt
  where
    handleOpt :: Config -> String -> Either String Config
    handleOpt cfg "-n" = Right cfg {replace = False}
    handleOpt _ _ = usage
    usage =
      Left $
        "Usage: templation [-n]\n"
          <> "\n"
          <> "Creates a stack template from project in current directory.\n"
          <> "\n"
          <> "-n\tDo not replace project name, author and email..\n"

main :: IO ()
main = do
  p <- getCurrentDirectory
  let n = takeFileName p
  args <- getArgs
  author <- gitConfig "user.name"
  email <- gitConfig "user.email"
  case parseOptions
    ( Config
        { path = ".",
          name = TC.convertText n,
          author = author,
          email = email,
          replace = True
        }
    )
    args of
    Left msg -> die msg
    Right cfg -> do
      out <- makeHSFile cfg
      LT.putStr $ LT.toLazyText out
