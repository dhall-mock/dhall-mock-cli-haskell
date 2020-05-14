module Main where

import DhallMockCommand (MockState(..), registerExpectationCmd, sendRandomRequestCmd)

import Hedgehog (TestLimit, forAll, property, executeParallel, withTests)
import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range

import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = defaultMain propRegExpectation

propRegExpectation :: TestTree
propRegExpectation =
  testProperty "register-expectation" . withTests (20 :: TestLimit) . property $ do
  let
    commands = [registerExpectationCmd, sendRandomRequestCmd]
    initialState = MockState []
    parr = Gen.parallel (Range.linear 10 100) (Range.linear 1 10)
  actions <- forAll $ parr initialState commands
  executeParallel initialState actions
