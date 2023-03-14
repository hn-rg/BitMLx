{-|
Module      : Syntax.BitMLx
Description : BitMLx syntax definition.

We also define here some short-hand operators that will help
us write examples that look a bit more similar to the code
you'd see on the paper.

Use import qualified to avoid ambiguity with BitML syntax.
-}
module Syntax.BitMLx where

import Syntax.Common
    ( Pred, Time, SHash, SName, Deposit, DCoins, BCoins, P )

-- | BitMLx contract preconditions
data G = Deposit P (BCoins,DCoins) (Deposit, Deposit)
    | Volatile P (BCoins,DCoins) (Deposit, Deposit) (Deposit, Deposit)
    | Secret P SName SHash
    | Collateral P (BCoins,DCoins) (Deposit, Deposit)
    deriving (Eq,Show)

-- | BitMLx contract
data C = D 
    | PriorityChoice D C
    | TimedPriorityChoice Time D C
    | NullContract
    deriving (Eq, Ord, Show)

-- | Blocking BitMLx contract
data D = Put [(Deposit, Deposit)] C
    | Reveal [SName] C
    | PutReveal [(Deposit, Deposit)] [SName] C
    | RevealIf [SName] Pred C
    | PutRevealIf [(Deposit, Deposit)] [SName] Pred C
    | Auth [P] D
    | Withdraw P
    -- | Splits the contract into many subcontracts.
    -- The numeric parameters are the percentages of the balance each
    -- subcontract takes on each blockchain. The percentages on each
    -- blockchain should add up to 1.
    | Split [((Rational, Rational), C)]
    deriving (Eq, Ord, Show)

-- | Shorthand operator for deposits
(!) :: P -> (BCoins, DCoins) -> (Deposit, Deposit) -> G
(p ! (bv, dv)) (bx, dx) = Deposit p (bv, dv) (bx, dx)

-- | Shorthand operator for collaterals
(!!) :: P -> (BCoins, DCoins) -> (Deposit, Deposit) -> G
(p !! (bv, dv)) (bx, dx) = Collateral p (bv, dv) (bx, dx)

-- | Shorthand operator for Authorizations.
infixl 3 ##
(##) :: [P] -> D -> D
(##) = Auth 

-- | Shorthand operator for priority choices
infixl 2 +>
(+>) :: D -> C -> C
d +> c = PriorityChoice d c
