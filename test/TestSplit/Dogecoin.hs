module TestSplit.Dogecoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitML ( (!), D(Withdraw, Split, Reveal, After), G (Secret) )


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
    , Secret pA "A_Dogecoin_S_Name__0" "A_Dogecoin_S_Hash__0"
    , Secret pB "B_Dogecoin_S_Name__0" "B_Dogecoin_S_Hash__0"
    , Secret pA "A_Dogecoin_S_Name__1" "A_Dogecoin_S_Hash__1"
    , Secret pB "B_Dogecoin_S_Name__1" "B_Dogecoin_S_Hash__1"
    ]

contract :: D DCoins
contract = Split [
  (DCoins 1, [
    Reveal ["A_Dogecoin_S_Name__0"] [Withdraw pA],
    Reveal ["B_Dogecoin_S_Name__0"] [Withdraw pA],
    After 11 (Reveal [] [
      Reveal ["A_Bitcoin_S_Name__0"] [Withdraw pB],
      Reveal ["B_Bitcoin_S_Name__0"] [Withdraw pA]
      ])
    ]),
  (DCoins 3, [
    Reveal ["A_Dogecoin_S_Name__1"] [Withdraw pB],
    Reveal ["B_Dogecoin_S_Name__1"] [Withdraw pB],
    After 11 (Reveal [] [
      Reveal ["A_Bitcoin_S_Name__1"] [Withdraw pB],
      Reveal ["B_Bitcoin_S_Name__1"] [Withdraw pA]
      ])
    ])
  ]