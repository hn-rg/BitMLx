module Examples.MutualTC where

import Syntax.Common ( P(..) )
import Syntax.BitMLx
import Data.Ratio ((%))

import ExampleRunner (BitMLxExample(BitMLxExample))

exampleName :: [Char]
exampleName = "2PMutualTC"

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

participants :: [P]
participants = [pA, pB]

preconditions :: TimedPreconditions
preconditions = TimedPreconditions 1 10 [
    pA ! (1, 1) $ "A_deposit"
    , pB ! (1, 1) $ "B_deposit"
    , Secret pA "a" "__SOME_HASH__"
    , Secret pB "b" "__SOME_HASH__"
    ]

contract :: Contract
contract =
    Reveal ["a", "b"] (
        Withdraw [
            ((1, 1), pA),
            ((1, 1), pB)
        ]
    ) 
    +> Reveal ["a"] (WithdrawAll pA)
    +> Reveal ["b"] (WithdrawAll pB)
    +> WithdrawAll pA


sourceAdvertisement :: ContractAdvertisement
sourceAdvertisement = ContractAdvertisement preconditions contract

example = BitMLxExample exampleName participants sourceAdvertisement