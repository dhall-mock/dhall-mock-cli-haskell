module Main where

import DhallMockCommand (MockState(..), registerExpectationCmd, sendRandomRequestCmd)

import Hedgehog (forAll, property, executeSequential)
import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range

import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = defaultMain propRegExpectation

propRegExpectation :: TestTree
propRegExpectation =
  testProperty "register-expectation" . property $ do
  let
    commands = [registerExpectationCmd, sendRandomRequestCmd]
    initialState = MockState []
    testRange = Range.linear 1 1000
  actions <- forAll $ Gen.sequential testRange initialState commands
  executeSequential initialState actions
