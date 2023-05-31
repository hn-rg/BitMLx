module Examples.SimpleExchange where

import Prelude hiding ((!!))
import Syntax.Common ( P(..), Pred (PEq, PAnd, PBtwn, PNeq), E (ELength, EInt) )
import Syntax.BitMLx
    ( (!),
      (!!),
      (#:),
      (+>),
      C(Withdraw, Refund),
      D(WithdrawD, Split),
      G(StepSecret) )
import Data.Ratio ((%))

exampleName :: [Char]
exampleName = "SimpleExchange"

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

participants :: [P]
participants = [pA, pB]

preconditions :: [G]
preconditions = [
    pA ! (1, 0) $ ("bd_a", "dd_a")
    , pB ! (0, 1) $ ("bd_b", "dd_b")
    , pA !! (0, 0) $ ("bc_a", "dc_a")
    , pB !! (0, 0) $ ("bc_b", "dc_b")
    , StepSecret pA ("L", "") ("A_Bitcoin_S_Name_L_", "__SOME_HASH__") ("A_Dogecoin_S_Name_L_", "__SOME_HASH__")
    , StepSecret pB ("L", "") ("B_Bitcoin_S_Name_L_", "__SOME_HASH__") ("B_Dogecoin_S_Name_L_", "__SOME_HASH__")
    ]


contract :: C
contract =
    Split [
        ((0, 1), Withdraw pA),
        ((1, 0), Withdraw pB)
        ]
    +> Refund