module DemoSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Int" $
    it "==" $
      1 `shouldBe` (1::Int)
