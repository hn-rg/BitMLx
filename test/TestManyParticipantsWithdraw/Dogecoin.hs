module TestManyParticipantsWithdraw.Dogecoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitML ( (!), D(Withdraw, Split, Reveal, After), G (Secret), C )

participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}
pC = P {pname = "C", pk = "pkC"}


preconditions :: [G DCoins]
preconditions = [
    pA ! 2 $ "dd_A"
    , pB ! 1 $ "dd_B"
    , pC ! 3 $ "dd_C"
    , pA ! 6 $ "dc_A"
    , pB ! 6 $ "dc_B"
    , pC ! 6 $ "dc_C"
    ]

contract :: C DCoins
contract = [
    Split [
        (DCoins 9,[Withdraw pA]),
        (DCoins 9,[Withdraw pB]),
        (DCoins 6,[Withdraw pC])
    ]]
