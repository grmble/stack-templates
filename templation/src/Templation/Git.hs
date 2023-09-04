-- | Read Git Config
-- |
-- | Needs its own module without OverloadedStrings
module Templation.Git where

import Data.Functor ((<&>))
import Data.Text.Conversions qualified as TC
import Data.Text.Lazy qualified as LT
import Development.Shake.Command
  ( StdoutTrim (fromStdoutTrim),
    cmd,
  )

gitConfig :: LT.Text -> IO LT.Text
gitConfig cn =
  cmd "git" ["config", "--get", TC.convertText cn :: String]
    <&> TC.convertText @String @LT.Text . fromStdoutTrim
