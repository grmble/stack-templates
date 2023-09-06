{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Text.Lazy qualified as LT
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName)
import Templation.Cmd
import Templation.Hsfiles qualified as Hsfiles

main :: IO ()
main =
  parseArgs >>= \case
    Hsfiles {username, email, project, output, verbose} -> do
      p <-
        if project == "."
          then getCurrentDirectory
          else return project
      let name = takeFileName p

      Hsfiles.makeTemplate
        Hsfiles.Config
          { Hsfiles.username = LT.pack username,
            Hsfiles.email = LT.pack email,
            Hsfiles.project = p,
            Hsfiles.output = output,
            Hsfiles.name = LT.pack name,
            Hsfiles.verbose
          }
    i@Init {} ->
      -- will not be parsed, the mode is deactivated
      print i
