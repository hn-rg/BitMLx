module TestRevealIf.Dogecoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (DCoins(..))
import Syntax.Common (P(..), Pred (PEq), E (ELength, EInt))
import Syntax.BitML
    ( (!),
      GuardedContract(Withdraw, Split, RevealIf),
      Precondition(Secret),
      Contract,
      (&:) )

participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}


preconditions :: [Precondition DCoins]
preconditions = [
    pA ! 1 $ "A_deposit_Dogecoin"
    , pB ! 1 $ "B_deposit_Dogecoin"
    , Secret pA "s" "__SOME_HASH__" 
    , Secret pA "StepSecret_A___" "__HASH__PLACEHOLDER__"
    , Secret pB "StepSecret_B___" "__HASH__PLACEHOLDER__"
    , Secret pA "StartSecret_A" "__HASH__PLACEHOLDER__"
    , Secret pB "StartSecret_B" "__HASH__PLACEHOLDER__"
    ]
    
predicate :: Pred
predicate = PEq (ELength "s") (EInt 42)

contract :: Contract DCoins
contract = [
    RevealIf ["StepSecret_A___", "s"] predicate [
        Split [
            (DCoins 1, [Withdraw pA]),
            (DCoins 1, [Withdraw pB])
        ]
    ],
    RevealIf ["StepSecret_B___", "s"] predicate [
        Split [
            (DCoins 1, [Withdraw pA]),
            (DCoins 1,[Withdraw pB])
        ]]
    ]
