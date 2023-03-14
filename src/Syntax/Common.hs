{-|
Module      : Syntax.Common
Description : Common types for BitMLx and BitML syntax.

Notice that we define separate types for coins and transactions
that live on different blockchains.
-}
module Syntax.Common where

-- | Participant names like "A", "B", etc
data P = P {
    pname :: String,
    pk :: String
} deriving (Eq, Ord, Show)

-- | A Bitcoin-denominated value
type BCoins = Int

-- | A Dogecoin-denominated value
type DCoins = Int

-- | A name for a deposit representing a transaction in some blockchain
type Deposit = String

-- | A name for a secret
type SName = String

-- | A hash for a secret
type SHash = String

-- | Time passing on the contract execution
type Time = Int

-- | Logical predicates
data Pred =
    PTrue
    | PAnd Pred Pred
    | POr Pred Pred
    | PNot Pred
    | PEq E E
    | PNeq E E
    | PBtwn E E E
    | PLt E E
    deriving (Eq, Ord, Show)

-- Arithmetic expressions for logical predicates
data E  = EInt Integer
    -- | Length of a secret
    | ELength SName
    | EAdd E E
    | ESub E E
    deriving (Eq, Ord, Show)

