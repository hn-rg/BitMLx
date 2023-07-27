module TestWithdrawD.Dogecoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitML ( (!), D(Withdraw, Split, Reveal), G (Secret), C )

participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}


preconditions :: [G DCoins]
preconditions = [
    pA ! 1 $ "A_deposit_Dogecoin"
    , pB ! 1 $ "B_deposit_Dogecoin"
    , Secret pA "StepSecret_A___" "__HASH__PLACEHOLDER__"
    , Secret pB "StepSecret_B___" "__HASH__PLACEHOLDER__"
    ]

contract :: C DCoins
contract = [
    Reveal ["StepSecret_A___"] [
        Split [
            (DCoins 1, [Withdraw pA]),
            (DCoins 1, [Withdraw pB])
        ]
    ],
    Reveal ["StepSecret_B___"] [
        Split [
            (DCoins 1, [Withdraw pA]),
            (DCoins 1,[Withdraw pB])
        ]]
    ]
