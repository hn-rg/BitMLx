
module TestWithdrawD.BitMLx where
    
import Prelude hiding ((!!))
import Data.Ratio ((%))

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitMLx ( G (StepSecret), (!), (!!), D(WithdrawD), withdrawAllD)


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
    , StepSecret pA ("", "") ("A_Bitcoin_S_Name__", "A_Bitcoin_S_Hash__") ("A_Dogecoin_S_Name__", "A_Dogecoin_S_Hash__")
    , StepSecret pB ("", "") ("B_Bitcoin_S_Name__", "B_Bitcoin_S_Hash__") ("B_Dogecoin_S_Name__", "B_Dogecoin_S_Hash__")
    ]

contract :: D
contract = WithdrawD [
    (pA, (1, 1%2)),
    (pB, (0, 1%2))
    ]