{-# LANGUAGE GADTs #-}
{-|
Module      : Syntax.BitMLx
Description : BitMLx syntax definition.

We also define here some short-hand operators that will help
us write examples that look a bit more similar to the code
you'd see on the paper.

Use import qualified to avoid ambiguity with BitML syntax.
-}
module Syntax.BitMLx where

import Coins ( DCoins, BCoins )
import Syntax.Common ( Pred, Time, SHash, SName, DepositId, P, NodeLabel )

-- | BitMLx contract preconditions
data Precondition =
    -- | Participant deposits that will fund the contract balance.
    -- 
    -- Note that this doesn't count collaterals, which are specific to the compiler
    -- implementation and will be added up on each blockchain as part of the compialtion. 
    Deposit P (BCoins,DCoins) DepositId
    -- | A secret that can be revealed as a condition for execution of a contract.
    | Secret P SName SHash
    deriving (Eq,Show)

-- | BitMLx cotnract preconditions with starting time and time elapse.
-- | In the paper, these are called t and delta.
data TimedPreconditions =
    TimedPreconditions Time Time [Precondition]
    deriving (Eq, Show)


-- | BitMLx contract
data Contract where
    -- | A Priority Choice between contracts GuardedContract and Contract.
    PriorityChoice :: GuardedContract -> Contract -> Contract
    -- | End the current contract, distributing the funds among participants 
    -- following the given proportionals.
    -- 
    -- The proportions should be numbers between 0 and 1, and
    -- they should add up to 1 on each blockchain. 
    Withdraw :: [((BCoins, DCoins), P)] -> Contract
    WithdrawAll :: P -> Contract
    deriving (Eq, Ord, Show)

-- | Blocking BitMLx contract
data GuardedContract where
    -- | Reveal all secrets on a list as a condition to execute Contract.
    Reveal :: [SName] -> Contract -> GuardedContract
    -- | Reveal secrets and evaluate a logical predicate over the values. Contract will only be executed if both:
    -- All secrets are revealed and match the hashes in the preconditions.
    -- The predicate evaluates to True when instanciated with the values revealed.
    RevealIf :: [SName] -> Pred -> Contract -> GuardedContract
    -- | A list of participants are required to authorize the execution of contract GuardedContract
    -- with a digital signature.
    Auth :: [P] -> GuardedContract -> GuardedContract
    -- | Splits the contract into many subcontracts.
    -- The numeric Rational are the proportions of the balance each
    -- subcontract takes on each Bitcoin and Dogecoin respectively.
    --
    -- The proportions should be numbers between 0 and 1, and
    -- they should add up to 1 on each blockchain. 
    Split :: [((BCoins, DCoins), Contract)] -> GuardedContract 
    -- | Idem to `Withdraw` but as a guarded contract.
    WithdrawD :: [((BCoins, DCoins), P)] -> GuardedContract
    WithdrawAllD :: P -> GuardedContract
    deriving (Eq, Ord, Show)

-- | Shorthand operator for deposits.
(!) :: P -> (BCoins, DCoins) -> DepositId -> Precondition
(p ! (bv, dv)) z = Deposit p (bv, dv) z

-- | Shorthand operator for Authorizations.
infix 3 #:
(#:) :: [P] -> GuardedContract -> GuardedContract
(#:) = Auth 

-- | Shorthand operator for priority choices.
infixr 2 +>
(+>) :: GuardedContract -> Contract -> Contract
d +> c = PriorityChoice d c