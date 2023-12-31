module TestReveal.Bitcoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), )
import Syntax.Common (P(..))
import Syntax.BitML
    ( (!),
      GuardedContract(Withdraw, Split, Reveal),
      Precondition(Secret),
      Contract,
      (&:) )


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}


preconditions :: [Precondition BCoins]
preconditions = [
    pA ! 1 $ "A_deposit_Bitcoin"
    , pB ! 1 $ "B_deposit_Bitcoin"
    , Secret pA "s" "__SOME_HASH__" 
    , Secret pA "StepSecret_A___" "__HASH__PLACEHOLDER__"
    , Secret pB "StepSecret_B___" "__HASH__PLACEHOLDER__"
    , Secret pA "StartSecret_A" "__HASH__PLACEHOLDER__"
    , Secret pB "StartSecret_B" "__HASH__PLACEHOLDER__"
    ]

contract :: Contract BCoins
contract = [
    Reveal ["StepSecret_A___", "s"] [Withdraw pA],
    Reveal ["StepSecret_B___", "s"] [Withdraw pA]
    ]
