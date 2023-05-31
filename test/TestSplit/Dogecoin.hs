module TestSplit.Dogecoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitML ( (!), D(Withdraw, Split, Reveal, After), G (Secret), C )


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}


preconditions :: [G DCoins]
preconditions = [
    pA ! 2 $ "dd_A"
    , pB ! 2 $ "dd_B"
    , pA ! 0 $ "dc_A"
    , pB ! 0 $ "dc_B"
    , Secret pA "A_Dogecoin_S_Name__" "A_Dogecoin_S_Hash__"
    , Secret pB "B_Dogecoin_S_Name__" "B_Dogecoin_S_Hash__"
    ]

contract :: C DCoins
contract = [
  Reveal ["A_Dogecoin_S_Name__"] [doSplit],
  Reveal ["B_Dogecoin_S_Name__"] [doSplit] 
  ]

doSplit = Split [
  (DCoins 1, [
    Withdraw pA
    ]),
  (DCoins 3, [
    Withdraw pB
    ])
  ]