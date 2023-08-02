
module TestManyParticipantsPriorityChoice.BitMLx where
    
import Data.Ratio ((%))

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitMLx ( Precondition, (!), GuardedContract(WithdrawAllD), (+>), Contract (WithdrawAll), TimedPreconditions(..))


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}
pC = P {pname = "C", pk = "pkC"}

preconditions :: TimedPreconditions
preconditions = TimedPreconditions 1 10 [
    pA ! (1, 2) $ "A_deposit"
    , pB ! (2, 1) $ "B_deposit"
    , pC ! (3, 3) $ "C_deposit"
    ]

contract :: Contract
contract = WithdrawAllD pA +> WithdrawAllD pB +> WithdrawAll pB