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
    pA ! 1 $ "A_deposit_Dogecoin"
    , pB ! 1 $ "B_deposit_Dogecoin"
    , Secret pA "StepSecret_A__L_" "__HASH__PLACEHOLDER__"
    , Secret pB "StepSecret_B__L_" "__HASH__PLACEHOLDER__"
    ]

contract :: C DCoins
contract = [
    Reveal ["StepSecret_A__L_"] [Withdraw pA]
    , Reveal ["StepSecret_B__L_"] [Withdraw pA],
    After 11 (Reveal [] [
        Reveal ["StepSecret_A__L_"] [Withdraw pB],
        Reveal ["StepSecret_B__L_"] [Withdraw pA],
        After 21 (Reveal [] [Withdraw pB])]
    )]