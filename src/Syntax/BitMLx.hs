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
data G =
    -- | Participant deposits that will fund the contract balance.
    -- 
    -- Note that this doesn't count collaterals, which are specific to the compiler
    -- implementation and will be added up on each blockchain as part of the compialtion. 
    Deposit P (BCoins,DCoins) DepositId
    -- | A secret that can be revealed as a condition for execution of a contract.
    | Secret P SName SHash 
    deriving (Eq,Show)

-- | BitMLx contract
data C where
    -- | A Priority Choice between contracts D and C.
    PriorityChoice :: D -> C -> C
    -- | A Priority Choice with an added time offset.
    TimedPriorityChoice :: Time -> D -> C -> C
    -- | End the current contract, distributing the funds among participants 
    -- following the given proportionals.
    -- 
    -- The proportions should be numbers between 0 and 1, and
    -- they should add up to 1 on each blockchain. 
    Withdraw :: [(P, (Rational, Rational))] -> C
    deriving (Eq, Ord, Show)

-- | Blocking BitMLx contract
data D where
    -- | Reveal all secrets on a list as a condition to execute C.
    Reveal :: [SName] -> C -> D
    -- | Reveal secrets and evaluate a logical predicate over the values. C will only be executed if both:
    -- All secrets are revealed and match the hashes in the preconditions.
    -- The predicate evaluates to True when instanciated with the values revealed.
    RevealIf :: [SName] -> Pred -> C -> D
    -- | A list of participants are required to authorize the execution of contract D
    -- with a digital signature.
    Auth :: [P] -> D -> D
    -- | Splits the contract into many subcontracts.
    -- The numeric Rational are the proportions of the balance each
    -- subcontract takes on each Bitcoin and Dogecoin respectively.
    --
    -- The proportions should be numbers between 0 and 1, and
    -- they should add up to 1 on each blockchain. 
    Split :: [((Rational, Rational), C)] -> D 
    -- | Idem to `Withdraw` but as a guarded contract.
    WithdrawD :: [(P, (Rational, Rational))] -> D
    deriving (Eq, Ord, Show)

-- | Shorthand operator for deposits.
(!) :: P -> (BCoins, DCoins) -> DepositId -> G
(p ! (bv, dv)) z = Deposit p (bv, dv) z

-- | Shorthand operator for Authorizations.
infix 3 #:
(#:) :: [P] -> D -> D
(#:) = Auth 

-- | Shorthand operator for priority choices.
infixr 2 +>
(+>) :: D -> C -> C
d +> c = PriorityChoice d c

-- | Short-hand unary operator for the particular case where a single participant
-- withdraws the whole balance. Ideally we would use some syntactic
-- sugar/overloading/notation abuse to just write this as `Withdraw A`
-- but that would give us typing problems here.
withdrawAll :: P -> C
withdrawAll p = Withdraw [(p, (1, 1))]

-- | Guarded contract version of `withdrawAll
withdrawAllD :: P -> D
withdrawAllD p = WithdrawD [(p, (1, 1))]

($@) :: () -> P -> C
() $@ p = Withdraw [(p, (1, 1))]

($$@) :: () -> P -> D
() $$@ p = WithdrawD [(p, (1, 1))]

