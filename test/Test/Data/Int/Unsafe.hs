{-# LANGUAGE OverloadedStrings #-}
module Test.Data.Int.Unsafe (tests) where

import Test.Gen qualified as Gen

import Hedgehog
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree(), testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)
import Test.Tasty.ExpectedFailure (expectFailBecause)

tests :: TestTree
tests =
  testGroup
    "Data.Int.Unsafe"
    [ expectFailBecause "UnsafeInt should create overflows" $
        testPropertyNamed "Addition" "prop_add" prop_add,

      testPropertyNamed "Safe Addition" "prop_add_safe" prop_add_safe,

      testPropertyNamed
        "Unsafe Addition Overflow"
        "prop_add_unsafe_positive"
        prop_add_unsafe_positive,

      testPropertyNamed
        "Unsafe Addition Underflow"
        "prop_add_unsafe_negative"
        prop_add_unsafe_negative
    ]

prop_add :: Property
prop_add = property $ do
  -- Generate two operands of UnsafeInt
  int1 <- forAll $ Gen.unsafeInt (Range.linear minBound maxBound)
  int2 <- forAll $ Gen.unsafeInt (Range.linear minBound maxBound)

  -- Add the 2 numbers and compare it to an arbitrary precision integer
  -- If there's an overflow, it should manifest here
  toInteger (int1 + int2) === toInteger int1 + toInteger int2

prop_add_safe :: Property
prop_add_safe = property $ do
  -- Addition is safe only if int1 + int2 is less than 128. We will generate two integers
  -- less than 64, which guarantees the sum is in range
  let min' = minBound `div` 2
      max' = maxBound `div` 2

  -- Generate two operands of UnsafeInt between -63 and 63
  int1 <- forAll $ Gen.unsafeInt (Range.linear min' max')
  int2 <- forAll $ Gen.unsafeInt (Range.linear min' max')

  -- Add the 2 numbers and compare it to an arbitrary precision integer. If there's an
  -- overflow, it should manifest here
  toInteger (int1 + int2) === toInteger int1 + toInteger int2

prop_add_unsafe_positive :: Property
prop_add_unsafe_positive = property $ do
  -- Addition is unsafe if int1 + int2 is greater than 128. We will generate two integers
  -- greater than 64, which guarantees the sum is out of range
  let min' = (maxBound `div` 2) + 1

  int1 <- forAll $ Gen.unsafeInt (Range.linear min' maxBound)
  int2 <- forAll $ Gen.unsafeInt (Range.linear min' maxBound)

  -- Add the 2 numbers and compare it to an arbitrary precision integer. This should
  -- cause an overflow.
  toInteger (int1 + int2) /== toInteger int1 + toInteger int2

prop_add_unsafe_negative :: Property
prop_add_unsafe_negative = property $ do
  -- Addition is safe if int1 + int2 is less than -128. We will generate two integers
  -- less than -63, which guarantees the sum is out of range
  let max' = ((maxBound `div` 2) + 1) * (-1)

  int1 <- forAll $ Gen.unsafeInt (Range.linear minBound max')
  int2 <- forAll $ Gen.unsafeInt (Range.linear minBound max')

  -- Add the 2 numbers and compare it to an arbitrary precision integer. This should
  -- cause an underflow.
  toInteger (int1 + int2) /== toInteger int1 + toInteger int2
