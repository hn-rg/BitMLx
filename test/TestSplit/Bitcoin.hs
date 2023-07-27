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
    pA ! 2 $ "A_deposit_Bitcoin"
    , pB ! 2 $ "B_deposit_Bitcoin"
    , Secret pA "StepSecret_A___" "__HASH__PLACEHOLDER__"
    , Secret pB "StepSecret_B___" "__HASH__PLACEHOLDER__"
    ]

contract :: C BCoins
contract = [
  Reveal ["StepSecret_A___"] [doSplit],
  Reveal ["StepSecret_B___"] [doSplit] 
  ]

doSplit = Split [
  (BCoins 3, [
    Withdraw pA
    ]),
  (BCoins 1, [
    Withdraw pB
    ])
  ]