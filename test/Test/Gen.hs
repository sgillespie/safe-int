module Test.Gen (safeInt, unsafeInt) where

import Data.Int.Safe (SafeInt(..))
import Data.Int.Unsafe (UnsafeInt(..))

import Data.Int (Int8())
import Hedgehog (Gen(), Range())
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

unsafeInt :: Range Int8 -> Gen UnsafeInt
unsafeInt range = UnsafeInt <$> Gen.int8 range

safeInt :: Gen SafeInt
safeInt = Gen.choice
  [ Int8 <$> Gen.int8 (Range.linear minBound maxBound),
    Int16 <$> Gen.int16 (Range.linear minBound maxBound)
  ]
