{-# LANGUAGE DerivingStrategies #-}
module Data.Int.Unsafe
  (UnsafeInt (..),
  ) where

import Data.Int (Int8())

newtype UnsafeInt = UnsafeInt {unInt :: Int8}
    deriving stock (Eq, Show)
    deriving newtype (Enum, Integral, Num, Ord, Real)
