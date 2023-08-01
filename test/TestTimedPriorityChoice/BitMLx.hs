
module TestTimedPriorityChoice.BitMLx where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitMLx (
    Precondition , (!), GuardedContract(WithdrawD), (+|), (|>), (+|>), Contract (Withdraw, TimedPriorityChoice),
    withdrawAllD, withdrawAll, TimedPreconditions(..)
    )


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

preconditions :: TimedPreconditions
preconditions = TimedPreconditions 1 10 [
    pA ! (1, 1) $ "A_deposit"
    , pB ! (1, 1) $ "B_deposit"
    ]

contract :: Contract
contract =  withdrawAllD pA +|42|> withdrawAll pB 