module Examples.Escrow where

import Prelude hiding ((!!))
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

preconditions :: [G]
preconditions = [
    pA ! (1, 1) $ ("bd_a", "dd_a")
    , pB ! (1, 1) $ ("bd_b", "dd_b")
    , pA !! (0, 0) $ ("bc_a", "dc_a")
    , pB !! (0, 0) $ ("bc_b", "dc_b")
    , StepSecret pA ("L", "") ("A_Bitcoin_S_Name_L_", "__SOME_HASH__") ("A_Dogecoin_S_Name_L_", "__SOME_HASH__")
    , StepSecret pB ("L", "") ("B_Bitcoin_S_Name_L_", "__SOME_HASH__") ("B_Dogecoin_S_Name_L_", "__SOME_HASH__")
    , StepSecret pA ("RL", "") ("A_Bitcoin_S_Name_RL_", "__SOME_HASH__") ("A_Dogecoin_S_Name_RL_", "__SOME_HASH__")
    , StepSecret pB ("RL", "") ("B_Bitcoin_S_Name_RL_", "__SOME_HASH__") ("B_Dogecoin_S_Name_RL_", "__SOME_HASH__")
    , StepSecret pA ("RRL", "") ("A_Bitcoin_S_Name_RRL_", "__SOME_HASH__") ("A_Dogecoin_S_Name_RRL_", "__SOME_HASH__")
    , StepSecret pB ("RRL", "") ("B_Bitcoin_S_Name_RRL_", "__SOME_HASH__") ("B_Dogecoin_S_Name_RRL_", "__SOME_HASH__")
    , StepSecret pA ("RRRL", "") ("A_Bitcoin_S_Name_RRRL_", "__SOME_HASH__") ("A_Dogecoin_S_Name_RRRL_", "__SOME_HASH__")
    , StepSecret pB ("RRRL", "") ("B_Bitcoin_S_Name_RRRL_", "__SOME_HASH__") ("B_Dogecoin_S_Name_RRRL_", "__SOME_HASH__")
    ]

contract :: C
contract =
    [pA] #: WithdrawD pB
    +> [pB] #: WithdrawD pA
    +> [pA] #: resolve 0.1 0.9
    +> [pB] #: resolve 0.1 0.9
    +> Refund

resolve :: Rational -> Rational -> D
resolve v v' =
    Split [
        ((v, v), Withdraw pM),
        ((v', v'), [pM] #: WithdrawD pA +> Withdraw pB) 
    ]