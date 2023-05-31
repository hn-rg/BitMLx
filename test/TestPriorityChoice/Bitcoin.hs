module TestPriorityChoice.Bitcoin where

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
    pA ! 1 $ "bd_A"
    , pB ! 1 $ "bd_B"
    , pA ! 0 $ "bc_A"
    , pB ! 0 $ "bc_B"
    , Secret pA "A_Bitcoin_S_Name_L_" "__SOME_HASH__"
    , Secret pB "B_Bitcoin_S_Name_L_" "__SOME_HASH__"
    ]

contract :: C BCoins
contract = [
    Reveal ["A_Bitcoin_S_Name_L_"] [Withdraw pA]
    , Reveal ["B_Bitcoin_S_Name_L_"] [Withdraw pA],
    After 11 (
        Reveal [] [Reveal ["A_Dogecoin_S_Name_L_"] [Withdraw pB],
        Reveal ["B_Dogecoin_S_Name_L_"] [Withdraw pA],
        After 21 (Reveal [] [Withdraw pB])]
    )]
