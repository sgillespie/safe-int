module Test.Data.Int.Unsafe (tests) where

import Test.Gen qualified as Gen

import Hedgehog
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree(), testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.ExpectedFailure (expectFailBecause)

tests :: TestTree
tests =
  testGroup
    "Data.Int.Unsafe"
    [ expectFailBecause "UnsafeInt should create overflows" $
        testProperty "Addition" prop_add
    ]

prop_add :: Property
prop_add = property $ do
  -- Generate two operands of UnsafeInt
  int1 <- forAll $ Gen.unsafeInt (Range.linear minBound maxBound)
  int2 <- forAll $ Gen.unsafeInt (Range.linear minBound maxBound)

  -- Add the 2 numbers and compare it to an arbitrary precision integer
  -- If there's an overflow, it should manifest here
  toInteger (int1 + int2) === toInteger int1 + toInteger int2
