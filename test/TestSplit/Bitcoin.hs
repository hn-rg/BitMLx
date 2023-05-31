module TestSplit.Bitcoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), )
import Syntax.Common (P(..))
import Syntax.BitML ( (!), D(Withdraw, Split, Reveal, After), G (Secret), C )


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}


preconditions :: [G BCoins]
preconditions = [
    pA ! 2 $ "bd_A"
    , pB ! 2 $ "bd_B"
    , pA ! 0 $ "bc_A"
    , pB ! 0 $ "bc_B"
    , Secret pA "A_Bitcoin_S_Name__" "A_Bitcoin_S_Hash__"
    , Secret pB "B_Bitcoin_S_Name__" "B_Bitcoin_S_Hash__"
    ]

contract :: C BCoins
contract = [
  Reveal ["A_Bitcoin_S_Name__"] [doSplit],
  Reveal ["B_Bitcoin_S_Name__"] [doSplit] 
  ]

doSplit = Split [
  (BCoins 3, [
    Withdraw pA
    ]),
  (BCoins 1, [
    Withdraw pB
    ])
  ]