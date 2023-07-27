module TestWithdraw.Dogecoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitML ( (!), D(Withdraw, Split, Reveal), G (Secret), C )

participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}


preconditions :: [G DCoins]
preconditions = [
    pA ! 1 $ "A_deposit_Dogecoin"
    , pB ! 1 $ "B_deposit_Dogecoin"
    ]

contract :: C DCoins
contract = [
    Split [
        (DCoins 1,[Withdraw pA]),
        (DCoins 1,[Withdraw pB])
    ]]
