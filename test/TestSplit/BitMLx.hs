
module TestSplit.BitMLx where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitMLx ( G (StepSecret), (!), (!!), D(Withdraw, Split), C (NullContract), (+>) )
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
    , StepSecret pA ("", "0") ("A_Bitcoin_S_Name__0", "A_Bitcoin_S_Hash__0") ("A_Dogecoin_S_Name__0", "A_Dogecoin_S_Hash__0")
    , StepSecret pB ("", "0") ("B_Bitcoin_S_Name__0", "B_Bitcoin_S_Hash__0") ("B_Dogecoin_S_Name__0", "B_Dogecoin_S_Hash__0")
    , StepSecret pA ("", "1") ("A_Bitcoin_S_Name__1", "A_Bitcoin_S_Hash__1") ("A_Dogecoin_S_Name__1", "A_Dogecoin_S_Hash__1")
    , StepSecret pB ("", "1") ("B_Bitcoin_S_Name__1", "B_Bitcoin_S_Hash__1") ("B_Dogecoin_S_Name__1", "B_Dogecoin_S_Hash__1")
    ]

contract :: D
contract = Split [
        ((3%4, 1%4), Withdraw pA +> NullContract),
        ((1%4, 3%4), Withdraw pB +> NullContract)
    ]
