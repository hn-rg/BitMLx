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
    pA ! 1 $ "dd_A"
    , pB ! 1 $ "dd_B"
    , pA ! 0 $ "dc_A"
    , pB ! 0 $ "dc_B"
    , Secret (P {pname = "A", pk = "pkA"}) "A_Dogecoin_S_Name__" "A_Dogecoin_S_Hash__"
    , Secret (P {pname = "B", pk = "pkB"}) "B_Dogecoin_S_Name__" "B_Dogecoin_S_Hash__"
    ]

contract :: C DCoins
contract = [
    Reveal ["A_Dogecoin_S_Name__"] [
        Split [
            (DCoins 1, [Withdraw pA]),
            (DCoins 1, [Withdraw pB])
        ]
    ],
    Reveal ["B_Dogecoin_S_Name__"] [
        Split [
            (DCoins 1, [Withdraw pA]),
            (DCoins 1,[Withdraw pB])
        ]]
    ]
