module Data.Int.Safe (SafeInt(..)) where

import Data.Int

-- | A wrapper for all Integer types that will widen when necessary
data SafeInt
  = Int8 Int8
  | Int16 Int16
  | Int32 Int32
  | Int64 Int64
  | Integer Integer
  deriving (Eq, Show)

instance Num SafeInt where
  (+) = add
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate = undefined

instance Integral SafeInt where
  quotRem = undefined

  toInteger (Int8 x) = toInteger x
  toInteger (Int16 x) = toInteger x
  toInteger (Int32 x) = toInteger x
  toInteger (Int64 x) = toInteger x
  toInteger (Integer x) = toInteger x

instance Real SafeInt where
  toRational (Int8 x) = toRational x
  toRational (Int16 x) = toRational x
  toRational (Int32 x) = toRational x
  toRational (Int64 x) = toRational x
  toRational (Integer x) = toRational x

instance Enum SafeInt where
  toEnum = undefined
  fromEnum = undefined

instance Ord SafeInt where
  compare = undefined

add :: SafeInt -> SafeInt -> SafeInt
add x y =
  -- We always widen to the next biggest int size. For example, when we add Int8 + Int8 we
  -- return Int16. We do this for the largest operand, ex, Int8 + Int16 = Int32
  case (x, y) of
     -- Int8 -> Int16
    (Int8 x', Int8 y') -> Int16 (fromIntegral x' + fromIntegral y')
    -- Int16 -> Int32
    (Int8 x', Int16 y') -> Int32 (fromIntegral x' + fromIntegral y')
    (Int16 x', Int8 y') -> Int32 (fromIntegral x' + fromIntegral y')
    (Int16 x', Int16 y') -> Int32 (fromIntegral x' + fromIntegral y')
    -- Int32 -> Int64
    (Int32 x', Int8 y') -> Int64 (fromIntegral x' + fromIntegral y')
    (Int32 x', Int16 y') -> Int64 (fromIntegral x' + fromIntegral y')
    (Int8 x', Int32 y') -> Int64 (fromIntegral x' + fromIntegral y')
    (Int16 x', Int32 y') -> Int64 (fromIntegral x' + fromIntegral y')
    (Int32 x', Int32 y') -> Int64 (fromIntegral x' + fromIntegral y')
    -- Int64 -> Integer
    (Int64 x', Int8 y') -> Int64 (fromIntegral x' + fromIntegral y')
    (Int64 x', Int16 y') -> Int64 (fromIntegral x' + fromIntegral y')
    (Int64 x', Int32 y') -> Int64 (fromIntegral x' + fromIntegral y')
    (Int8 x', Int64 y') -> Int64 (fromIntegral x' + fromIntegral y')
    (Int16 x', Int64 y') -> Int64 (fromIntegral x' + fromIntegral y')
    (Int32 x', Int64 y') -> Int64 (fromIntegral x' + fromIntegral y')
    (Int64 x', Int64 y') -> Int64 (fromIntegral x' + fromIntegral y')
    -- Integer -> Integer
    (Integer x', Int8 y') -> Integer (x' + fromIntegral y')
    (Integer x', Int16 y') -> Integer (x' + fromIntegral y')
    (Integer x', Int32 y') -> Integer (x' + fromIntegral y')
    (Integer x', Int64 y') -> Integer (x' + fromIntegral y')
    (Int8 x', Integer y') -> Integer (fromIntegral x' + y')
    (Int16 x', Integer y') -> Integer (fromIntegral x' + y')
    (Int32 x', Integer y') -> Integer (fromIntegral x' + y')
    (Int64 x', Integer y') -> Integer (fromIntegral x' + y')
    (Integer x', Integer y') -> Integer (x' + y')
