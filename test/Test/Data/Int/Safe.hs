{-# LANGUAGE OverloadedStrings #-}
module Test.Data.Int.Safe (tests) where

import Test.Gen qualified as Gen

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree(), testGroup)
import Test.Tasty.Hedgehog (testProperty, testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Data.Int.Safe"
    [testPropertyNamed "Addition" "prop_add" prop_add]

prop_add :: Property
prop_add = property $ do
  -- Generate two operands
  x <- forAll Gen.safeInt
  y <- forAll Gen.safeInt

  -- Add the 2 numbers and compare it to an arbitrary precision integer. If there's an
  -- overflow, it should manifest here
  toInteger (x + y) === toInteger x + toInteger y
