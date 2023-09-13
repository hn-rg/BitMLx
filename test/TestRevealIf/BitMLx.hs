
module TestRevealIf.BitMLx where
    
import Data.Ratio ((%))

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..), Pred (PEq), E (ELength, EInt))
import Syntax.BitMLx ( Precondition (Secret), (!), GuardedContract(RevealIf), TimedPreconditions(..), (&:), Contract (Withdraw))


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

predicate :: Pred
predicate = PEq (ELength "s") (EInt 42) 

contract :: GuardedContract
contract = RevealIf ["s"] predicate $ Withdraw [
    ((2, 1), pA),
    ((0, 1), pB)
    ]