module Examples.Split where

import Prelude hiding ((!!))
import Syntax.Common ( P(..) )
import Syntax.BitMLx
import Data.Ratio ((%))

exampleName :: [Char]
exampleName = "split"

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
    , StepSecret pA ("", "") ("A_Bitcoin_S_Name__", "A_Bitcoin_S_Hash__") ("A_Dogecoin_S_Name__", "A_Dogecoin_S_Hash__")
    , StepSecret pB ("", "") ("B_Bitcoin_S_Name__", "B_Bitcoin_S_Hash__") ("B_Dogecoin_S_Name__", "B_Dogecoin_S_Hash__")
    , StepSecret pA ("L", "0") ("A_Bitcoin_S_Name_L_0", "A_Bitcoin_S_Hash_L_0") ("A_Dogecoin_S_Name_L_0", "A_Dogecoin_S_Hash_L_0")
    , StepSecret pB ("L", "0") ("B_Bitcoin_S_Name_L_0", "B_Bitcoin_S_Hash_L_0") ("B_Dogecoin_S_Name_L_0", "B_Dogecoin_S_Hash_L_0")
    , StepSecret pA ("L", "1") ("A_Bitcoin_S_Name_L_1", "A_Bitcoin_S_Hash_L_1") ("A_Dogecoin_S_Name_L_1", "A_Dogecoin_S_Hash_L_1")
    , StepSecret pB ("L", "1") ("B_Bitcoin_S_Name_L_1", "B_Bitcoin_S_Hash_L_1") ("B_Dogecoin_S_Name_L_1", "B_Dogecoin_S_Hash_L_1")
    ]

contract :: C
contract =
    Split [
        ((1%2, 1%2), Withdraw pA +> NullContract),
        ((1%2, 1%2), Withdraw pB +> NullContract)
    ] +> NullContract