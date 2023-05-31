module TestRefund.Bitcoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), )
import Syntax.Common (P(..))
import Syntax.BitML ( (!), D(Withdraw, Split, Reveal), G (Secret), C )


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}


preconditions :: [G BCoins]
preconditions = [
    pA ! 1 $ "bd_A"
    , pB ! 1 $ "bd_B"
    , pA ! 0 $ "bc_A"
    , pB ! 0 $ "bc_B"
    ]

contract :: C BCoins
contract = [Split [
    (1, [Withdraw pA]),
    (1, [Withdraw pB])
    ]]