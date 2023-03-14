module Examples.Withdraw where

import Prelude hiding ((!!))
import Syntax.Common ( P(..) )
import Syntax.BitMLx ( (!), (!!), D(Withdraw), G )

exampleName :: [Char]
exampleName = "withdraw"

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

participants :: [P]
participants = [pA, pB]

preconditions :: [G]
preconditions = [
    pA ! (1, 1) $ ("bd_a", "dd_a")
    , pA !! (2, 2) $ ("bc_a", "dc_a")
    , pB !! (2, 2) $ ("bc_b", "dc_b")
    ]

contract :: D
contract = Withdraw pB
