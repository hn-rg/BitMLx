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
data G c =
    Deposit P c DepositId
    | Volatile P c DepositId DepositId
    | Secret P SName SHash
    deriving (Eq, Show)

-- | BitML contract
-- The arguments are for the coins and deposit types respectively.
type C c = [D c]

-- | BitML guarded contract
-- The arguments are for the coins and deposit types respectively.
data D c =
    Put [DepositId] (C c)
    | Reveal [SName] (C c)
    | RevealIf [SName] Pred (C c)
    | PutReveal [DepositId] [SName] (C c)
    | PutRevealIf [DepositId] [SName] Pred (C c)
    | Withdraw P
    | Auth [P] (D c)
    | Split [(c, C c)]
    | After Time (D c)
    deriving (Eq, Show)


-- | Shorthand operator for deposits.
(!) :: Coins c => P -> c -> DepositId -> G c
(p ! v) x = Deposit p v x

newtype ContractAdvertisement c = ContractAdvertisement ([G c], C c)