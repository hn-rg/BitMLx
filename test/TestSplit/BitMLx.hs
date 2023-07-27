
module TestSplit.BitMLx where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitMLx ( G, (!), D(WithdrawD, Split), C, (+>), withdrawAll)
import Data.Ratio ((%))


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

preconditions :: [G]
preconditions = [
    pA ! (2, 2) $ "A_deposit"
    , pB ! (2, 2) $ "B_deposit"
    ]

contract :: D
contract = Split [
        ((3%4, 1%4), withdrawAll pA),
        ((1%4, 3%4), withdrawAll pB)
    ]
