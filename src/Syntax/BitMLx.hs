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
import Syntax.Common ( Pred, Time, SHash, SName, Deposit, P, NodeLabel )

-- | BitMLx contract preconditions
data G =
    -- | Persistent deposits that will fund the contract's initial balance
    Deposit P (BCoins,DCoins) (Deposit, Deposit)
    -- | Volatile deposits that can be later added to the contract's balance during execution
    | Volatile P (BCoins,DCoins) (Deposit, Deposit) (Deposit, Deposit)
    -- | Collateral deposits that won't be part of the contract balance but,
    -- will serve to enforce synchronous execution on both blockchains
    -- using punishment threats to disuade any participants from breaking
    -- synchronicity.
    | Collateral P (BCoins,DCoins) (Deposit, Deposit)
    -- | A secret that can be revealed as a condition for execution of
    -- a contract
    | Secret P SName SHash
    -- | Special secrets that are part of the synchronicity mechanism.
    -- Notice that even though in our paper step secrets are regular secrets,
    -- in this PoC implementation, it's more convenient to consider them a special
    -- kind of secret.
    -- In a production-oriented environment, the list of step secrets to provide should
    -- actually be an extra output of the compiler, after analysing the contract's
    -- structure and keeping track of all priority choices that need them.
    -- A BitMLx contract runner client would then provide hashes for all the step secrets
    -- and reveal them as part of the execution (or not, dependig on the implemented strategy)
    -- without the user ever knowing or caring about them.
    | StepSecret P NodeLabel (SName, SHash) (SName, SHash) 
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
-- data C =
--     PriorityChoice D C
--     -- | A Priority Choice with an added time offset.
--     | TimedPriorityChoice Time D C
--     -- | End the current contract, distributing the funds among participants 
--     -- following the given proportionals.
--     -- 
--     -- The proportions should be numbers between 0 and 1, and
--     -- they should add up to 1 on each blockchain. 
--     | Withdraw [(P, (Rational, Rational))]
--     deriving (Eq, Ord, Show)

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
(!) :: P -> (BCoins, DCoins) -> (Deposit, Deposit) -> G
(p ! (bv, dv)) (bx, dx) = Deposit p (bv, dv) (bx, dx)

-- | Shorthand operator for collaterals
(!!) :: P -> (BCoins, DCoins) -> (Deposit, Deposit) -> G
(p !! (bv, dv)) (bx, dx) = Collateral p (bv, dv) (bx, dx)

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

