module Examples.PriorityChoice where

import Prelude hiding ((!!))
import Syntax.Common ( P(..) )
import Syntax.BitMLx

exampleName :: [Char]
exampleName = "PriorityChoice"

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

participants :: [P]
participants = [pA, pB]

preconditions :: [G]
preconditions = [
    pA ! (1, 1) $ ("bd_a", "dd_a")
    , pB ! (1, 1) $ ("bd_b", "dd_b")
    , pA !! (2, 2) $ ("bc_a", "dc_a")
    , pB !! (2, 2) $ ("bc_b", "dc_b")
    , StepSecret pA ("L", "") ("A_Bitcoin_S_Name_L_", "A_Bitcoin_S_Hash_L_") ("A_Dogecoin_S_Name_L_", "A_Dogecoin_S_Hash_L_")
    , StepSecret pB ("L", "") ("B_Bitcoin_S_Name_L_", "B_Bitcoin_S_Hash_L_") ("B_Dogecoin_S_Name_L_", "B_Dogecoin_S_Hash_L_")
    ]

contract :: C
contract = WithdrawD pA +> Refund