module Examples.MultichainPaymentExchange where

import Syntax.Common ( P(..) )
import Syntax.BitMLx
import Data.Ratio ((%))

import ExampleRunner (BitMLxExample(BitMLxExample))

exampleName :: [Char]
exampleName = "MultichainPaymentExchange"

pC = P {pname = "C", pk = "pkC"}
pR = P {pname = "R", pk = "pkR"}
pX = P {pname = "X", pk = "pkX"}

participants :: [P]
participants = [pC, pR, pX]

preconditions :: TimedPreconditions
preconditions = TimedPreconditions 1 10 [
    pC ! (10, 0) $ "C_deposit", 
    pR ! (0, 0) $ "R_deposit",
    pX ! (0, 100) $ "X_deposit"
    ]


contract :: Contract
contract = 
    [pC] &: WithdrawD [
        ((10, 0), pR),
        ((0, 100), pX)
    ]
    +> WithdrawD [
        ((10, 0), pX),
        ((0, 100), pR)
    ]
    +> WithdrawAll pC


sourceAdvertisement :: ContractAdvertisement
sourceAdvertisement = ContractAdvertisement preconditions contract

example = BitMLxExample exampleName participants sourceAdvertisement