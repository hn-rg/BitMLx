module Examples.Escrow where

import Syntax.Common ( P(..), Pred (PEq, PAnd, PBtwn, PNeq), E (ELength, EInt) )
import Syntax.BitMLx
import Coins

exampleName :: [Char]
exampleName = "Escrow"

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}
pM = P {pname = "M", pk = "pkM"}

participants :: [P]
participants = [pA, pB]

preconditions :: TimedPreconditions
preconditions = TimedPreconditions 1 10 [
    pA ! (10, 10) $ "A_deposit"
    , pB ! (0, 0) $ "B_deposit"
    ]

contract :: Contract
contract =
    [pA] &: WithdrawAllD pB
    +> [pB] &: WithdrawAllD pA
    +> [pA] &: resolve (1, 1) (9, 9)
    +> [pB] &: resolve (1, 1) (9, 9)
    +> WithdrawAll pB

resolve :: (BCoins, DCoins) -> (BCoins, DCoins) -> GuardedContract
resolve v v' =
    Split [
        (v, WithdrawAll pM),
        (v', ([pM] &: WithdrawAllD pA) +> WithdrawAll pB) 
    ]


sourceAdvertisement :: ContractAdvertisement
sourceAdvertisement = ContractAdvertisement preconditions contract