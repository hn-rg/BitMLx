module TestManyParticipantsWithdraw.Bitcoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), )
import Syntax.Common (P(..))
import Syntax.BitML ( (!), GuardedContract(Withdraw, Split, Reveal, After), Precondition (Secret), Contract )


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}
pC = P {pname = "C", pk = "pkC"}


preconditions :: [Precondition BCoins]
preconditions = [
    pA ! 7 $ "A_deposit_Bitcoin"
    , pB ! 8 $ "B_deposit_Bitcoin"
    , pC ! 9 $ "C_deposit_Bitcoin"
    ]

contract :: Contract BCoins
contract = [
    Split [
        (BCoins 6,[Withdraw pA]),
        (BCoins 6,[Withdraw pB]),
        (BCoins 12,[Withdraw pC])
    ]]
