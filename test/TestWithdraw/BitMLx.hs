
module TestWithdraw.BitMLx where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitMLx ( G, (!), (!!), D(Withdraw) )
import Prelude hiding ((!!))


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

preconditions :: [G]
preconditions = [
    pA ! (1, 1) $ ("bd_A", "dd_A")
    , pB ! (1, 1) $ ("bd_B", "dd_B")
    , pA !! (0, 0) $ ("bc_A", "dc_A")
    , pB !! (0, 0) $ ("bc_B", "dc_B")
    ]

contract :: D
contract = Withdraw pA