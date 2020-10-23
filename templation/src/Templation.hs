{-# LANGUAGE NamedFieldPuns #-}
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
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Foldable ()
import Data.List (intersperse, sort, unfoldr)
import Data.String.Conversions (cs)
import System.Directory (getCurrentDirectory, listDirectory)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (isExtensionOf, takeFileName)
import System.PosixCompat (getFileStatus, isDirectory)

data Config = Config
  { name :: B.ByteString,
    path :: FilePath,
    replaceName :: Bool
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
          . filter (not . ("cabal" `isExtensionOf`))
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

readSourceFile :: Config -> FilePath -> IO Builder
readSourceFile cfg p = startFile <$> B.readFile p
  where
    startFile bs =
      byteString "{-# START_FILE " <> processContent cfg (cs p) <> " #-}\n"
        <> processContent cfg bs

combineSourceFiles :: Config -> Builder -> FilePath -> IO Builder
combineSourceFiles cfg b p = (b <>) <$> readSourceFile cfg p

makeHSFile :: Config -> IO Builder
makeHSFile cfg =
  listFiles (path cfg)
    >>= foldM (combineSourceFiles cfg) (byteString B.empty) . sort

findPrePost ::
  B.ByteString -> B.ByteString -> Maybe (B.ByteString, B.ByteString)
findPrePost sub bs =
  case B.breakSubstring sub bs of
    ("", "") -> Nothing
    (pre, "") -> Just (pre, "")
    (pre, subpost) -> Just (pre, B.drop (B.length sub) subpost)

processContent :: Config -> B.ByteString -> Builder
processContent cfg bs =
  if replaceName cfg
    then
      foldMap byteString $
        intersperse "{{name}}" (unfoldr (findPrePost (name cfg)) bs)
          >>= unfoldr (findPrePost "\r")
    else byteString bs

parseOptions :: Config -> [String] -> Either String Config
parseOptions = foldM handleOpt
  where
    handleOpt :: Config -> String -> Either String Config
    handleOpt cfg "-n" = Right cfg {replaceName = False}
    handleOpt _ _ = usage
    usage =
      Left $
        "Usage: templation [-n]\n" <> "\n"
          <> "Creates a stack template from project in current directory.\n"
          <> "\n"
          <> "-n\tDo not replace project name.\n"

main :: IO ()
main = do
  p <- getCurrentDirectory
  let n = takeFileName p
  args <- getArgs
  case parseOptions (Config {path = ".", name = cs n, replaceName = True}) args of
    Left msg -> die msg
    Right cfg -> do
      out <- makeHSFile cfg
      LB.putStr $ toLazyByteString out
