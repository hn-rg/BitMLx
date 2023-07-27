{-|
Module      : Syntax.Common
Description : Common types for BitMLx and BitML syntax.
-}
module Syntax.Common where

-- A participant in the contract
data P = P {
    -- | The participant's name, like "A", "B", etc.
    pname :: String,
    -- | The participant's public key.
    pk :: String
} deriving (Eq, Ord, Show)

-- | A unique identifier for a participant's deposit.
type DepositId = String

-- | A name for a secret.
type SName = String

-- | A hash for a secret.
type SHash = String

-- | Time passing on the contract execution.
type Time = Int

-- | A unique identifier for a node on the syntax tree of a BitMLx contract.
type NodeLabel = (String, String)

emptyLabel :: NodeLabel
emptyLabel = ("", "")

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

-- Arithmetic expressions for logical predicates.
data E  = EInt Integer
    -- | Length of a secret.
    | ELength SName
    | EAdd E E
    | ESub E E
    deriving (Eq, Ord, Show)

