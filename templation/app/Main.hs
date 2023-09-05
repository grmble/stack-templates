{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Text.Lazy qualified as LT
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName)
import Templation.Cmd
import Templation.Store qualified as Store

main :: IO ()
main =
  parseArgs >>= \case
    Init {template, repo, username, email, project} ->
      print "init"
    List {repo} ->
      print "list"
    Store {username, email, project, output, verbose} -> do
      p <-
        if project == "."
          then getCurrentDirectory
          else return project
      let name = takeFileName p

      Store.makeTemplate
        Store.Config
          { Store.username = LT.pack username,
            Store.email = LT.pack email,
            Store.project = p,
            Store.output = output,
            Store.name = LT.pack name,
            Store.verbose
          }
