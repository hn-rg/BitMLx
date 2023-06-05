
module TestSplit.BitMLx where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitMLx ( G (StepSecret), (!), (!!), D(WithdrawD, Split), C, (+>), withdrawAll)
import Prelude hiding ((!!))
import Data.Ratio ((%))


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

preconditions :: [G]
preconditions = [
    pA ! (2, 2) $ ("bd_A", "dd_A")
    , pB ! (2, 2) $ ("bd_B", "dd_B")
    , pA !! (0, 0) $ ("bc_A", "dc_A")
    , pB !! (0, 0) $ ("bc_B", "dc_B")
    , StepSecret pA ("", "") ("A_Bitcoin_S_Name__", "A_Bitcoin_S_Hash__") ("A_Dogecoin_S_Name__", "A_Dogecoin_S_Hash__")
    , StepSecret pB ("", "") ("B_Bitcoin_S_Name__", "B_Bitcoin_S_Hash__") ("B_Dogecoin_S_Name__", "B_Dogecoin_S_Hash__")
    ]

contract :: D
contract = Split [
        ((3%4, 1%4), withdrawAll pA),
        ((1%4, 3%4), withdrawAll pB)
    ]
