{-# LANGUAGE InstanceSigs #-}
{-|
Module      : Syntax.Coins
Description : Types for coins on the Bitcoin or Dogecoin blockchain.

This is mostly a lot of boiler-plate code to define BCoins and DCoins,
which wrap Integer, so that we can't mix the 2. We also define a general
class Coins that allows as to write generic code that work on either.
-}
module Coins where

-- | A Bitcoin-denominated value
newtype BCoins = BCoins Int deriving (Eq, Show)

-- | A Dogecoin-denominated valuenewtype BCoins = BCoins Integer deriving (Eq, Show)
newtype DCoins = DCoins Int deriving (Eq, Show)

instance Num BCoins where
  (BCoins x) + (BCoins y) = BCoins (x + y)
  (BCoins x) * (BCoins y) = BCoins (x * y)
  negate (BCoins x) = BCoins (negate x)
  abs (BCoins x) = BCoins (abs x)
  signum (BCoins x) = BCoins (signum x)
  fromInteger x = BCoins (fromInteger x)

instance Enum BCoins where
  toEnum = BCoins . toEnum
  fromEnum (BCoins x) = fromEnum x

instance Ord BCoins where
  compare (BCoins x) (BCoins y) = compare x y

instance Real BCoins where
  toRational (BCoins x) = toRational x

instance Integral BCoins where
  toInteger (BCoins x) = toInteger x
  quotRem (BCoins x) (BCoins y) = (BCoins q, BCoins r)
    where (q, r) = x `quotRem` y
  divMod (BCoins x) (BCoins y) = (BCoins q, BCoins r)
    where (q, r) = x `divMod` y

instance Num DCoins where
  (DCoins x) + (DCoins y) = DCoins (x + y)
  (DCoins x) * (DCoins y) = DCoins (x * y)
  negate (DCoins x) = DCoins (negate x)
  abs (DCoins x) = DCoins (abs x)
  signum (DCoins x) = DCoins (signum x)
  fromInteger x = DCoins (fromInteger x)
  
instance Enum DCoins where
  toEnum = DCoins . toEnum
  fromEnum (DCoins x) = fromEnum x

instance Ord DCoins where
  compare (DCoins x) (DCoins y) = compare x y

instance Real DCoins where
  toRational (DCoins x) = toRational x

instance Integral DCoins where
  toInteger :: DCoins -> Integer
  toInteger (DCoins x) = toInteger x
  quotRem (DCoins x) (DCoins y) = (DCoins q, DCoins r)
    where (q, r) = x `quotRem` y
  divMod (DCoins x) (DCoins y) = (DCoins q, DCoins r)
    where (q, r) = x `divMod` y

-- | The Coins class contains both BCoins and DCoins and allows
-- us to write generic code that works on either type.
class Integral c => Coins c
instance Coins BCoins
instance Coins DCoins