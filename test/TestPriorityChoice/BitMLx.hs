
module TestPriorityChoice.BitMLx where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitMLx ( G , (!), D(WithdrawD), (+>), C (Withdraw), withdrawAllD, withdrawAll)


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

preconditions :: [G]
preconditions = [
    pA ! (1, 1) $ "A_deposit"
    , pB ! (1, 1) $ "B_deposit"
    ]

contract :: C
contract = withdrawAllD pA +> withdrawAll pB