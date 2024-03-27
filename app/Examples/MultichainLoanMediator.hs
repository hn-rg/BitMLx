module Examples.MultichainLoanMediator where

import Syntax.Common ( P(..) )
import Syntax.BitMLx
import Data.Ratio ((%))

import ExampleRunner (BitMLxExample(BitMLxExample))

exampleName :: [Char]
exampleName = "MultichainLoanMediator"

pB = P {pname = "B", pk = "pkB"}
pL = P {pname = "L", pk = "pkL"}
pM = P {pname = "M", pk = "pkM"}

participants :: [P]
participants = [pB, pL, pM]

preconditions :: TimedPreconditions
preconditions = TimedPreconditions 1 10 [
    pB ! (3, 0) $ "B_deposit",
    pL ! (0, 30) $ "L_deposit",
    pM ! (0, 0) $ "M_deposit"
    ]


contract :: Contract
contract = 
    Split [
        ((0, 30), WithdrawAll pB),
        ((3, 0), installment)
    ]
    +> Withdraw [
        ((3, 0), pB),
        ((0, 30), pL)
    ]

installment :: Contract
installment = 
    [pM] &: Split [
        ((1, 0), WithdrawAll pB),
        ((2, 0), (
            [pM] &: Split [
                ((1, 0), WithdrawAll pB),
                ((1, 0), (([pM] &: WithdrawAllD pB) +> WithdrawAll pL))
            ]
            +> WithdrawAll pL
            )
        )
    ]
    +> WithdrawAll pL


sourceAdvertisement :: ContractAdvertisement
sourceAdvertisement = ContractAdvertisement preconditions contract

example = BitMLxExample exampleName participants sourceAdvertisement