module Test.Gen (unsafeInt) where

import Data.Int.Unsafe (UnsafeInt(..))

import Data.Int (Int8())
import Hedgehog (Gen(), Range())
import Hedgehog.Gen qualified as Gen

unsafeInt :: Range Int8 -> Gen UnsafeInt
unsafeInt range = UnsafeInt <$> Gen.int8 range
