
module TestWithdrawD.BitMLx where
    
import Data.Ratio ((%))

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitMLx ( G, (!), D(WithdrawD), withdrawAllD)


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

preconditions :: [G]
preconditions = [
    pA ! (1, 1) $ "A_deposit"
    , pB ! (1, 1) $ "B_deposit"
    ]

contract :: D
contract = WithdrawD [
    (pA, (1, 1%2)),
    (pB, (0, 1%2))
    ]