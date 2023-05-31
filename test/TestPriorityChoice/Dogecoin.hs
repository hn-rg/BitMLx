module TestPriorityChoice.Dogecoin where

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
    pA ! 1 $ "dd_A"
    , pB ! 1 $ "dd_B"
    , pA ! 0 $ "dc_A"
    , pB ! 0 $ "dc_B"
    , Secret pA "A_Dogecoin_S_Name_L_" "__SOME_HASH__"
    , Secret pB "B_Dogecoin_S_Name_L_" "__SOME_HASH__"
    ]

contract :: C DCoins
contract = [
    Reveal ["A_Dogecoin_S_Name_L_"] [Withdraw pA]
    , Reveal ["B_Dogecoin_S_Name_L_"] [Withdraw pA],
    After 11 (
        Reveal [] [Reveal ["A_Bitcoin_S_Name_L_"] [Withdraw pB],
        Reveal ["B_Bitcoin_S_Name_L_"] [Withdraw pA],
        After 21 (Reveal [] [Withdraw pB])]
    )]