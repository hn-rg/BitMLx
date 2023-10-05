module TestAdvertisement.Dogecoin where

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
    pA ! 1 $ "A_deposit_Dogecoin"
    , pB ! 1 $ "B_deposit_Dogecoin"
    , Secret pA "StepSecret_A__L_" "__HASH__PLACEHOLDER__"
    , Secret pB "StepSecret_B__L_" "__HASH__PLACEHOLDER__"
    , Secret pA "StepSecret_A__LL_" "__HASH__PLACEHOLDER__"
    , Secret pB "StepSecret_B__LL_" "__HASH__PLACEHOLDER__"
    , Secret pA "StartSecret_A" "__HASH__PLACEHOLDER__"
    , Secret pB "StartSecret_B" "__HASH__PLACEHOLDER__"
    ]

contract :: Contract DCoins
contract = 
    [
        Reveal ["StepSecret_A__L_", "StartSecret_A", "StartSecret_B"] [
            Reveal ["StepSecret_A__LL_"] [Withdraw pA],
            Reveal ["StepSecret_B__LL_"] [Withdraw pA],
            After 21 (Reveal [] [
                Reveal ["StepSecret_A__LL_"] [Withdraw pB],
                Reveal ["StepSecret_B__LL_"] [Withdraw pA],
                After 31 (Reveal [] [Withdraw pB])
            ]
        )],
        Reveal ["StepSecret_B__L_", "StartSecret_A", "StartSecret_B"] [
            Reveal ["StepSecret_A__LL_"] [Withdraw pA],
            Reveal ["StepSecret_B__LL_"] [Withdraw pA],
            After 21 (Reveal [] [
                Reveal ["StepSecret_A__LL_"] [Withdraw pB],
                Reveal ["StepSecret_B__LL_"] [Withdraw pA],
                After 31 (Reveal [] [Withdraw pB])
            ])
        ],
        After 11 (Reveal [] [
            Reveal ["StepSecret_A__L_"] [Withdraw pB],
            Reveal ["StepSecret_B__L_"] [Withdraw pA],
            After 21 (Reveal [] [
                Split [
                    (DCoins 1,[Withdraw pA]),
                    (DCoins 1,[Withdraw pB])
                ]
            ])
        ])
    ]