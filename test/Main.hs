module Main (main) where

import Test.Data.Int.Safe qualified as SafeInt
import Test.Data.Int.Unsafe qualified as UnsafeInt

import Test.Tasty (TestTree(), defaultMain, testGroup)
import Test.Tasty.Hedgehog ()

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "tests" [UnsafeInt.tests, SafeInt.tests]
