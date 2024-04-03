module Examples.TwoPartyAgreement where

import Syntax.Common ( P(..), Pred (PEq, PAnd, PBtwn, PNeq), E (ELength, EInt) )
import Syntax.BitMLx
import Data.Ratio ((%))

import ExampleRunner (BitMLxExample(BitMLxExample))

exampleName :: [Char]
exampleName = "TwoPartyAgreement"

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

participants :: [P]
participants = [pA, pB]

preconditions :: TimedPreconditions
preconditions = TimedPreconditions 1 10 [
    pA ! (1, 1) $ "A_deposit", 
    pB ! (0, 0) $ "B_deposit", 
    Secret pA "a" "__SOME_HASH__", 
    Secret pB "b" "__SOME_HASH__"
    ]


contract :: Contract
contract =
    RevealIf ["a", "b"]
        (PAnd
            (PBtwn (ELength "b") (EInt 0) (EInt 1))
            (PEq (ELength "a") (ELength "b")))
        (Withdraw [
            ((1, 0), pB),
            ((0, 1), pA)
        ])
    +> RevealIf ["a", "b"]
        (PAnd
            (PBtwn (ELength "b") (EInt 0) (EInt 1))
            (PNeq (ELength "a") (ELength "b")))
        (Withdraw [
            ((0, 1), pB),
            ((1, 0), pA)
        ])
    +> WithdrawAll pA


sourceAdvertisement :: ContractAdvertisement
sourceAdvertisement = ContractAdvertisement preconditions contract

example = BitMLxExample exampleName participants sourceAdvertisement