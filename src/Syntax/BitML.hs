{-|
Module      : Syntax.BitML
Description : BitML syntax definition.

Both Bitcoin and Dogecoin BitML contracts share the same datatypes
and we use parameters to differenciate the type of coins and
deposits they'll work on.

Use import qualified to avoid ambiguity with BitMLx syntax.
-}
module Syntax.BitML where

import Syntax.Common ( Deposit, P, Pred, SHash, SName, Time )
import Coins (Coins)

-- | BitML contract preconditions
-- The arguments are for the coins and deposit types respectively.
data G c =
    Deposit P c Deposit
    | Volatile P c Deposit Deposit
    | Secret P SName SHash
    deriving (Eq, Show)

-- | BitML contract
-- The arguments are for the coins and deposit types respectively.
type C c = [D c]

-- | BitML guarded contract
-- The arguments are for the coins and deposit types respectively.
data D c =
    Put [Deposit] (C c)
    | Reveal [SName] (C c)
    | RevealIf [SName] Pred (C c)
    | PutReveal [Deposit] [SName] (C c)
    | PutRevealIf [Deposit] [SName] Pred (C c)
    | Withdraw P
    | Auth [P] (D c)
    | Split [(c, C c)]
    | After Time (D c)
    deriving (Eq, Show)


-- | Shorthand operator for deposits.
(!) :: Coins c => P -> c -> Deposit -> G c
(p ! v) x = Deposit p v x