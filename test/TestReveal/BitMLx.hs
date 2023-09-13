
module TestReveal.BitMLx where
    
import Data.Ratio ((%))

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitMLx ( Precondition (Secret), (!), GuardedContract(Reveal), TimedPreconditions(..), (&:), Contract (Withdraw))


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

preconditions :: TimedPreconditions
preconditions = TimedPreconditions 1 10 [
    pA ! (1, 1) $ "A_deposit"
    , pB ! (1, 1) $ "B_deposit"
    , Secret pA "s" "__SOME_HASH__" 
    ]

contract :: GuardedContract
contract = Reveal ["s"] $ Withdraw [
    ((2, 1), pA),
    ((0, 1), pB)
    ]