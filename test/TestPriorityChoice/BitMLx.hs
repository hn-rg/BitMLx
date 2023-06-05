
module TestPriorityChoice.BitMLx where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitMLx ( G (StepSecret), (!), (!!), D(WithdrawD), (+>), C (Withdraw), withdrawAllD, withdrawAll)
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
    , StepSecret pA ("L", "") ("A_Bitcoin_S_Name_L_", "__SOME_HASH__") ("A_Dogecoin_S_Name_L_", "__SOME_HASH__")
    , StepSecret pB ("L", "") ("B_Bitcoin_S_Name_L_", "__SOME_HASH__") ("B_Dogecoin_S_Name_L_", "__SOME_HASH__")
    ]

contract :: C
contract = withdrawAllD pA +> withdrawAll pB