module Examples.NaiveExchangeLottery where

import Syntax.Common ( P(..), Pred (PEq, PAnd, PBtwn, PNeq), E (ELength, EInt) )
import Syntax.BitMLx
import Data.Ratio ((%))

exampleName :: [Char]
exampleName = "NaiveExchangeLottery"

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
    RevealIf ["a", "b"]
        (PAnd
            (PBtwn (ELength "b") (EInt 0) (EInt 1))
            (PEq (ELength "a") (ELength "b")))
        (withdrawAll pA) 
    +> RevealIf ["a", "b"]
        (PAnd
            (PBtwn (ELength "b") (EInt 0) (EInt 1))
            (PNeq (ELength "a") (ELength "b")))
        (withdrawAll pB) 
    +> RevealIf ["a", "b"]
        (PBtwn (ELength "b")
            (EInt 0) (EInt 1))
        (withdrawAll pB) 
    +> withdrawAll pA