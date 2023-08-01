module Examples.Escrow where

import Syntax.Common ( P(..), Pred (PEq, PAnd, PBtwn, PNeq), E (ELength, EInt) )
import Syntax.BitMLx
import Data.Ratio ((%))

exampleName :: [Char]
exampleName = "Escrow"

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}
pM = P {pname = "M", pk = "pkM"}

participants :: [P]
participants = [pA, pB]

preconditions :: TimedPreconditions
preconditions = TimedPreconditions 1 10 [
    pA ! (1, 1) $ "A_deposit"
    , pB ! (1, 1) $ "B_deposit"
    ]

contract :: Contract
contract =
    [pA] #: withdrawAllD pB
    +> [pB] #: withdrawAllD pA
    +> [pA] #: resolve 0.1 0.9
    +> [pB] #: resolve 0.1 0.9
    +> withdrawAll pB

resolve :: Rational -> Rational -> GuardedContract
resolve v v' =
    Split [
        ((v, v), withdrawAll pM),
        ((v', v'), ([pM] #: withdrawAllD pA) +> withdrawAll pB) 
    ]