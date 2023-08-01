module TestSplit.Dogecoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitML ( (!), GuardedContract(Withdraw, Split, Reveal, After), Precondition (Secret), Contract )


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}


preconditions :: [Precondition DCoins]
preconditions = [
    pA ! 2 $ "A_deposit_Dogecoin"
    , pB ! 2 $ "B_deposit_Dogecoin"
    , Secret pA "StepSecret_A___" "__HASH__PLACEHOLDER__"
    , Secret pB "StepSecret_B___" "__HASH__PLACEHOLDER__"
    ]

contract :: Contract DCoins
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