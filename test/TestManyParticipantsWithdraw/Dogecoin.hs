module TestManyParticipantsWithdraw.Dogecoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitML ( (!), GuardedContract(Withdraw, Split, Reveal, After), Precondition (Secret), Contract )

participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}
pC = P {pname = "C", pk = "pkC"}


preconditions :: [Precondition DCoins]
preconditions = [
    pA ! 8 $ "A_deposit_Dogecoin"
    , pB ! 7 $ "B_deposit_Dogecoin"
    , pC ! 9 $ "C_deposit_Dogecoin"
    ]

contract :: Contract DCoins
contract = [
    Split [
        (DCoins 9,[Withdraw pA]),
        (DCoins 9,[Withdraw pB]),
        (DCoins 6,[Withdraw pC])
    ]]
