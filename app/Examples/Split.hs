module Examples.Split where

import Prelude hiding ((!!))
import Syntax.Common ( P(..) )
import Syntax.BitMLx
import Data.Ratio ((%))

exampleName :: [Char]
exampleName = "Split"

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

participants :: [P]
participants = [pA, pB]

preconditions :: [G]
preconditions = [
    pA ! (2, 2) $ ("bd_a", "dd_a")
    , pB ! (2, 2) $ ("bd_b", "dd_b")
    , pA !! (0, 0) $ ("bc_a", "dc_a")
    , pB !! (0, 0) $ ("bc_b", "dc_b")
    , StepSecret pA ("L", "") ("A_Bitcoin_S_Name_L_", "A_Bitcoin_S_Hash_L_") ("A_Dogecoin_S_Name_L_", "A_Dogecoin_S_Hash_L_")
    , StepSecret pB ("L", "") ("B_Bitcoin_S_Name_L_", "B_Bitcoin_S_Hash_L_") ("B_Dogecoin_S_Name_L_", "B_Dogecoin_S_Hash_L_")
    ]

contract :: C
contract =
    Split [
        ((1%2, 1%2), Withdraw pA),
        ((1%2, 1%2), Withdraw pB)
    ] +> Refund