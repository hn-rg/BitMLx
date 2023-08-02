module Examples.SimpleExchange where

import Syntax.Common ( P(..), Pred (PEq, PAnd, PBtwn, PNeq), E (ELength, EInt) )
import Syntax.BitMLx
import Data.Ratio ((%))

exampleName :: [Char]
exampleName = "SimpleExchange"

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

participants :: [P]
participants = [pA, pB]

preconditions :: TimedPreconditions
preconditions = TimedPreconditions 1 10 [
    pA ! (1, 0) $ "A_deposit"
    , pB ! (0, 1) $ "B_deposit"
    ]


contract :: Contract
contract =
    WithdrawD [
        ((0, 1), pA),
        ((1, 0), pB)
    ]
    +> Withdraw [
        ((1, 0), pA),
        ((0, 1), pB)
    ]