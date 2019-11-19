module DocTestSpec where

import Test.DocTest
import Test.Hspec

-- not needed, but nice for ghci
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  runIO $ doctest ["-isrc", "src/Templation.hs"]
