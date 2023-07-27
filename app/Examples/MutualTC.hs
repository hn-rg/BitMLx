module Examples.MutualTC where

import Syntax.Common ( P(..) )
import Syntax.BitMLx
import Data.Ratio ((%))

exampleName :: [Char]
exampleName = "2PMutualTC"

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

participants :: [P]
participants = [pA, pB]

preconditions :: [G]
preconditions = [
    pA ! (1, 1) $ "A_deposit"
    , pB ! (1, 1) $ "B_deposit"
    , Secret pA "a" "__SOME_HASH__"
    , Secret pB "b" "__SOME_HASH__"
    ]

contract :: C
contract =
    Reveal ["a", "b"] (
        Withdraw [
            (pA, (1%2, 1%2)),
            (pB, (1%2, 1%2))
        ]
    ) 
    +> Reveal ["a"] (withdrawAll pA)
    +> Reveal ["b"] (withdrawAll pB)
    +> withdrawAll pA