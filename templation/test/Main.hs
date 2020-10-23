module Main where

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.DocTest (doctest)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = do
  doctest ["-isrc", "src/Templation.hs"]
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [testProperty "reverse" prop_reverse]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "List comparison (different length)" $
        ([1, 2, 3] :: [Int]) `compare` [1, 2] @?= GT,
      -- the following test does not hold
      testCase "List comparison (same length)" $
        ([1, 2, 3] :: [Int]) `compare` [1, 2, 2] @?= GT
    ]

prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs