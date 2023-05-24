module TestSplit.Bitcoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), )
import Syntax.Common (P(..))
import Syntax.BitML ( (!), D(Withdraw, Split, Reveal, After), G (Secret) )


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
    , Secret pA "A_Bitcoin_S_Name__0" "A_Bitcoin_S_Hash__0"
    , Secret pB "B_Bitcoin_S_Name__0" "B_Bitcoin_S_Hash__0"
    , Secret pA "A_Bitcoin_S_Name__1" "A_Bitcoin_S_Hash__1"
    , Secret pB "B_Bitcoin_S_Name__1" "B_Bitcoin_S_Hash__1"
    ]

contract :: D BCoins
contract = Split [
  (BCoins 3, [
    Reveal ["A_Bitcoin_S_Name__0"] [Withdraw pA],
    Reveal ["B_Bitcoin_S_Name__0"] [Withdraw pA],
    After 11 (Reveal [] [
      Reveal ["A_Dogecoin_S_Name__0"] [Withdraw pB],
      Reveal ["B_Dogecoin_S_Name__0"] [Withdraw pA]
      ])
    ]),
  (BCoins 1, [
    Reveal ["A_Bitcoin_S_Name__1"] [Withdraw pB],
    Reveal ["B_Bitcoin_S_Name__1"] [Withdraw pB],
    After 11 (Reveal [] [
      Reveal ["A_Dogecoin_S_Name__1"] [Withdraw pB],
      Reveal ["B_Dogecoin_S_Name__1"] [Withdraw pA]
      ])
    ])
  ]