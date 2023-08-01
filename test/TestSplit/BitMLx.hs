
module TestSplit.BitMLx where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitMLx ( Precondition, (!), GuardedContract(WithdrawD, Split), Contract, (+>), withdrawAll, TimedPreconditions(..))
import Data.Ratio ((%))


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

preconditions :: TimedPreconditions
preconditions = TimedPreconditions 1 10 [
    pA ! (2, 2) $ "A_deposit"
    , pB ! (2, 2) $ "B_deposit"
    ]

contract :: GuardedContract
contract = Split [
        ((3%4, 1%4), withdrawAll pA),
        ((1%4, 3%4), withdrawAll pB)
    ]
