module TestAuthorize.Dogecoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitML
    ( (!),
      GuardedContract(Withdraw, Split, Reveal),
      Precondition(Secret),
      Contract,
      (&:) )

participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}


preconditions :: [Precondition DCoins]
preconditions = [
    pA ! 1 $ "A_deposit_Dogecoin"
    , pB ! 1 $ "B_deposit_Dogecoin"
    , Secret pA "StepSecret_A___" "__HASH__PLACEHOLDER__"
    , Secret pB "StepSecret_B___" "__HASH__PLACEHOLDER__"
    , Secret pA "StartSecret_A" "__HASH__PLACEHOLDER__"
    , Secret pB "StartSecret_B" "__HASH__PLACEHOLDER__"
    ]

contract :: Contract DCoins
contract = [
    [pA, pB] &: Reveal ["StepSecret_A___"] [
        Split [
            (DCoins 1, [Withdraw pA]),
            (DCoins 1, [Withdraw pB])
        ]
    ],
    [pA, pB] &: Reveal ["StepSecret_B___"] [
        Split [
            (DCoins 1, [Withdraw pA]),
            (DCoins 1,[Withdraw pB])
        ]]
    ]
