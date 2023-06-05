module TestManyParticipantsWithdraw.Bitcoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), )
import Syntax.Common (P(..))
import Syntax.BitML ( (!), D(Withdraw, Split, Reveal, After), G (Secret), C )


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}
pC = P {pname = "C", pk = "pkC"}


preconditions :: [G BCoins]
preconditions = [
    pA ! 1 $ "bd_A"
    , pB ! 2 $ "bd_B"
    , pC ! 3 $ "bd_C"
    , pA ! 6 $ "bc_A"
    , pB ! 6 $ "bc_B"
    , pC ! 6 $ "bc_C"
    ]

contract :: C BCoins
contract = [
    Split [
        (BCoins 6,[Withdraw pA]),
        (BCoins 6,[Withdraw pB]),
        (BCoins 12,[Withdraw pC])
    ]]
