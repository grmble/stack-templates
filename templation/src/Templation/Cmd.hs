{-# LANGUAGE DuplicateRecordFields #-}
-- disable cse because it breaks implict cmdargs
{-# OPTIONS_GHC -fno-cse #-}

-- | Cmd Line Parser
--
-- Implicit CmdArgs is much shorter than explicit, but it has a few quirks.
--
-- First, disable CSE.  Second, no nested records with shared sub-records -
-- they get called twice and the help texts get doubled.
module Templation.Cmd
  ( Op (..),
    parseArgs,
  )
where

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import System.Console.CmdArgs.Implicit
  ( CmdArgs,
    Data,
    Mode,
    Typeable,
    cmdArgsMode,
    cmdArgsRun,
    help,
    modes,
    opt,
    program,
    summary,
    typ,
    (&=),
  )
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)
import Templation.Git (gitConfig)

-- implicit cmdargs is much shorter
-- but it does not like deep hierarchical records

-- would have to use gibhub api (with api key)
-- to list *.hsfiles in a repo

-- also getting github raw files sucks,
-- for now i'll just use stack for my
-- cabal templates
data Op =
  {-
      Init
        { template :: String,
          repo :: String,
          username :: String,
          email :: String,
          project :: FilePath,
          verbose :: Bool
        }
    | -}
  Store
  { output :: FilePath,
    username :: String,
    email :: String,
    project :: FilePath,
    verbose :: Bool
  }
  deriving (Show, Eq, Data, Typeable)

data Defaults = Defaults
  { repo :: String,
    username :: String,
    email :: String,
    project :: FilePath
  }
  deriving (Show, Eq)

defaults :: IO Defaults
defaults = do
  repo <- lookupEnv "REPO" <&> fromMaybe "grmble/stack-templates"
  username <- gitConfig "user.name"
  email <- gitConfig "user.email"
  project <- getCurrentDirectory
  return $ Defaults {repo, username, email, project}

{-
initOp :: Defaults -> Op
initOp Defaults {repo, username, email, project} =
  Init
    { template = "default" &= typ "FILE" &= opt "default" &= help "Template .hsfile in repo",
      repo =
        repo
          &= typ "REPO"
          &= opt "$REPO"
          &= help "Repo as in grmble/stack_templates or full github url, default: $ROOT",
      username = username &= typ "USER" &= opt "git config --get user.name" &= help "Username for author fields, default from git config",
      email = email &= typ "EMAIL" &= opt "git config --get user.email" &= help "Email for email fields, default from git config",
      project = project &= typ "DIR" &= opt "current directory" &= help "project directory",
      verbose = False &= help "Verbose output"
    }
    &= help "Initialize a project from the given template"
-}

storeOp :: Defaults -> Op
storeOp Defaults {username, email, project} =
  Store
    { output = "" &= typ "FILE" &= opt "xxx" &= help "Output .hsfile (default: standard out)",
      username = username &= typ "USER" &= opt "git config --get user.name" &= help "Username for author fields, default from git config",
      email = email &= typ "EMAIL" &= opt "git config --get user.email" &= help "Email for email fields, default from git config",
      project = project &= typ "DIR" &= opt "current directory" &= help "project directory",
      verbose = False &= help "Verbose output"
    }
    &= help "Store a project as a template"

mode :: IO (Mode (CmdArgs Op))
mode = do
  d <- defaults
  return $
    cmdArgsMode $
      modes [storeOp d]
        &= help "Store templates"
        &= program "templation"
        &= summary "templation v0.2"

parseArgs :: IO Op
parseArgs =
  mode
    >>= cmdArgsRun
