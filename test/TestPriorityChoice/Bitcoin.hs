module TestPriorityChoice.Bitcoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), )
import Syntax.Common (P(..))
import Syntax.BitML ( (!), GuardedContract(Withdraw, Split, Reveal, After), Precondition (Secret), Contract )


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}


preconditions :: [Precondition BCoins]
preconditions = [
    pA ! 1 $ "A_deposit_Bitcoin"
    , pB ! 1 $ "B_deposit_Bitcoin"
    , Secret pA "StepSecret_A__L_" "__HASH__PLACEHOLDER__"
    , Secret pB "StepSecret_B__L_" "__HASH__PLACEHOLDER__"
    , Secret pA "StartSecret_A" "__HASH__PLACEHOLDER__"
    , Secret pB "StartSecret_B" "__HASH__PLACEHOLDER__"
    ]

contract :: Contract BCoins
contract = [
    Reveal ["StepSecret_A__L_"] [Withdraw pA]
    , Reveal ["StepSecret_B__L_"] [Withdraw pA],
    After 11 (
        Reveal [] [Reveal ["StepSecret_A__L_"] [Withdraw pB],
        Reveal ["StepSecret_B__L_"] [Withdraw pA],
        After 21 (Reveal [] [Withdraw pB])]
    )]

