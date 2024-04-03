module Examples.ReceiverChosenDenomination where

import Syntax.Common ( P(..) )
import Syntax.BitMLx
import Data.Ratio ((%))

import ExampleRunner (BitMLxExample(BitMLxExample))

exampleName :: [Char]
exampleName = "ReceiverChosenDenomination"

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}
participants :: [P]
participants = [pA, pB]

preconditions :: TimedPreconditions
preconditions = TimedPreconditions 1 10 [
    pA ! (1, 1) $ "A_deposit",
    pB ! (0, 0) $ "B_deposit"
    ]

contract :: Contract
contract = 
    [pB] &: WithdrawD [
        ((1, 0), pB),
        ((0, 1), pA)
    ]
    +> WithdrawD [
        ((0, 1), pB),
        ((1, 0), pA)
    ]
    +> WithdrawAll pA 


sourceAdvertisement :: ContractAdvertisement
sourceAdvertisement = ContractAdvertisement preconditions contract

example = BitMLxExample exampleName participants sourceAdvertisement