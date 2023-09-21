{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Templation.Init where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.ST (RealWorld, stToIO)
import Control.Monad.Trans.Resource (MonadResource, allocate, runResourceT)
import Data.Attoparsec.Text qualified as A
import Data.ByteString.Char8 qualified as B
import Data.Char (isSpace)
import Data.Conduit (runConduit, (.|))
import Data.Conduit.Attoparsec (conduitParser)
import Data.Conduit.Combinators qualified as C
import Data.Function ((&))
import Data.HashMap.Strict qualified as H
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO (Handle, IOMode (..), hClose, openBinaryFile)

data Token
  = StartFile {fn :: T.Text}
  | Param {name :: T.Text}
  | Text {text :: T.Text}
  deriving (Show, Eq)

data TokenB
  = StartFileB {fn :: String}
  | Bytes {bs :: B.ByteString}
  deriving (Show, Eq)

-- | Returns 'False' for any white space character or 'noch', otherwise True
isNotSpaceOr :: Char -> Char -> Bool
isNotSpaceOr noch ch = not $ (ch == noch || isSpace ch)

-- | Parser for START_FILE Stanza
--
-- >>> parseOnly startFile "{-# START_FILE foo.txt #-}\n"
-- Right "foo.txt"
startFile :: A.Parser T.Text
startFile = do
  _ <- A.string "{-#"
  A.skipSpace
  _ <- A.string "START_FILE"
  A.skipSpace
  fn <- A.takeWhile1 (isNotSpaceOr '#')
  A.skipSpace
  _ <- A.string "#-}"
  _ <- A.endOfLine
  return fn

-- | Parser for parameters
--
-- >>> parseOnly parameter "{{ test }}"
-- Right "test"
parameter :: A.Parser T.Text
parameter = do
  _ <- A.string "{{"
  A.skipSpace
  p <- A.takeWhile1 (isNotSpaceOr '}')
  A.skipSpace
  _ <- A.string "}}"
  return p

-- | Text chunk until next possible start for START_FILE or parameter
--
-- >>> parseOnly otherText "foo{}"
-- Right "foo"
otherText :: A.Parser T.Text
otherText = A.scan 256 notOpenBracket
  where
    notOpenBracket :: Int -> Char -> Maybe Int
    notOpenBracket _ '{' = Nothing
    notOpenBracket n _ = if n <= 0 then Nothing else Just (n - 1)

token :: A.Parser Token
token = A.choice [StartFile <$> startFile, Param <$> parameter, Text <$> A.string "{", Text <$> otherText]

toTokenB :: H.HashMap T.Text T.Text -> Token -> TokenB
toTokenB dict Param {name} =
  H.lookupDefault "" name dict
    & T.encodeUtf8
    & Bytes
toTokenB _ Text {text} = Bytes $ T.encodeUtf8 text
toTokenB _ StartFile {fn} = StartFileB $ T.unpack fn

-- writeFiles _ Bytes {bs} = _
writeFiles :: (MonadResource m) => FilePath -> STRef RealWorld (Maybe Handle) -> TokenB -> m ()
writeFiles dir ref StartFileB {fn} = do
  let path = dir </> fn
  liftIO $ putStrLn ("writing to " <> path)
  let parent = takeDirectory path
  liftIO $ createDirectoryIfMissing True parent
  (_, handle) <- allocate (openBinaryFile path WriteMode) hClose
  _ <- liftIO $ stToIO $ modifySTRef ref (const (Just handle))
  return ()
writeFiles _ ref Bytes {bs} = do
  handle <- liftIO $ stToIO $ readSTRef ref
  case handle of
    Nothing -> liftIO $ ioError (userError "No output handle - Text before START_FILE")
    Just h -> liftIO $ B.hPutStr h bs
  return ()

main :: IO ()
main = do
  let dict = H.fromList [("name", "xxx"), ("author", "Juergen Gmeiner"), ("email", "spamless.juergen@gmail.com")] :: H.HashMap T.Text T.Text
  ref <- stToIO $ newSTRef (Nothing :: Maybe Handle)
  runResourceT $
    runConduit $
      C.sourceFile "../default.hsfiles"
        .| C.decodeUtf8
        .| conduitParser token
        .| C.map (toTokenB dict . snd)
        .| C.iterM (writeFiles "/tmp" ref)
        .| C.sinkNull

-- TODO
-- params in START_FILE
