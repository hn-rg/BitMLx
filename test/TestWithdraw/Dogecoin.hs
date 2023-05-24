module TestWithdraw.Dogecoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitML ( (!), D(Withdraw, Split), G )

participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}


preconditions :: [G DCoins]
preconditions = [
    pA ! 1 $ "dd_A"
    , pB ! 1 $ "dd_B"
    , pA ! 0 $ "dc_A"
    , pB ! 0 $ "dc_B"
    ]

contract :: D DCoins
contract = Withdraw pA