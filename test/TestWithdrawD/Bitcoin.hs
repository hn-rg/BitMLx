module TestWithdrawD.Bitcoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), )
import Syntax.Common (P(..))
import Syntax.BitML ( (!), D(Withdraw, Split, Reveal), G (Secret), C )


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}


preconditions :: [G BCoins]
preconditions = [
    pA ! 1 $ "bd_A"
    , pB ! 1 $ "bd_B"
    , pA ! 0 $ "bc_A"
    , pB ! 0 $ "bc_B"
    , Secret (P {pname = "A", pk = "pkA"}) "A_Bitcoin_S_Name__" "A_Bitcoin_S_Hash__"
    , Secret (P {pname = "B", pk = "pkB"}) "B_Bitcoin_S_Name__" "B_Bitcoin_S_Hash__"
    ]

contract :: C BCoins
contract = [
    Reveal ["A_Bitcoin_S_Name__"] [Withdraw pA],
    Reveal ["B_Bitcoin_S_Name__"] [Withdraw pA]
    ]
