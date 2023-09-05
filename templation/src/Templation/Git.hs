-- | Read Git Config
-- |
-- | Needs its own module without OverloadedStrings
module Templation.Git
  ( gitConfig,
  )
where

import Data.Functor ((<&>))
import Development.Shake.Command
  ( StdoutTrim (fromStdoutTrim),
    cmd,
  )

gitConfig :: String -> IO String
gitConfig cn =
  cmd "git" ["config", "--get", cn]
    <&> fromStdoutTrim
