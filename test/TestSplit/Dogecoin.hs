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
    pA ! 2 $ "A_deposit_Dogecoin"
    , pB ! 2 $ "B_deposit_Dogecoin"
    , Secret pA "StepSecret_A___" "__HASH__PLACEHOLDER__"
    , Secret pB "StepSecret_B___" "__HASH__PLACEHOLDER__"
    ]

contract :: C DCoins
contract = [
  Reveal ["StepSecret_A___"] [doSplit],
  Reveal ["StepSecret_B___"] [doSplit] 
  ]

doSplit = Split [
  (DCoins 1, [
    Withdraw pA
    ]),
  (DCoins 3, [
    Withdraw pB
    ])
  ]