{-|
Module      : Syntax.BitML
Description : BitML syntax definition.

Both Bitcoin and Dogecoin BitML contracts share the same datatypes
and we use parameters to differenciate the type of coins and
deposits they'll work on.

Use import qualified to avoid ambiguity with BitMLx syntax.
-}
module Syntax.BitML where

import Syntax.Common ( DepositId, P, Pred, SHash, SName, Time )
import Coins (Coins)

-- | BitML contract preconditions
-- The arguments are for the coins and deposit types respectively.
data Precondition c =
    Deposit P c DepositId
    | Volatile P c DepositId DepositId
    | Secret P SName SHash
    deriving (Eq, Show)

-- | BitML contract
-- The arguments are for the coins and deposit types respectively.
type Contract c = [GuardedContract c]

-- | BitML guarded contract
-- The arguments are for the coins and deposit types respectively.
data GuardedContract c =
    Put [DepositId] (Contract c)
    | Reveal [SName] (Contract c)
    | RevealIf [SName] Pred (Contract c)
    | PutReveal [DepositId] [SName] (Contract c)
    | PutRevealIf [DepositId] [SName] Pred (Contract c)
    | Withdraw P
    | Auth [P] (GuardedContract c)
    | Split [(c, Contract c)]
    | After Time (GuardedContract c)
    deriving (Eq, Show)


-- | Shorthand operator for deposits.
(!) :: Coins c => P -> c -> DepositId -> Precondition c
(p ! v) x = Deposit p v x

newtype ContractAdvertisement c = ContractAdvertisement ([Precondition c], Contract c)