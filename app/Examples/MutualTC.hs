module Examples.MutualTC where

import Prelude hiding ((!!))
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
    pA ! (1, 1) $ ("bd_a", "dd_a")
    , pB ! (1, 1) $ ("bd_b", "dd_b")
    , pA !! (0, 0) $ ("bc_a", "dc_a")
    , pB !! (0, 0) $ ("bc_b", "dc_b")
    , StepSecret pA ("L", "") ("A_Bitcoin_S_Name_L_", "A_Bitcoin_S_Hash_L_") ("A_Dogecoin_S_Name_L_", "A_Dogecoin_S_Hash_L_")
    , StepSecret pB ("L", "") ("B_Bitcoin_S_Name_L_", "B_Bitcoin_S_Hash_L_") ("B_Dogecoin_S_Name_L_", "B_Dogecoin_S_Hash_L_")
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