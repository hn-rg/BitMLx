
module TestManyParticipantsPriorityChoice.BitMLx where
    
import Data.Ratio ((%))

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitMLx ( G (StepSecret), (!), (!!), D(WithdrawD, Split), (+>), C (Withdraw), withdrawAllD, withdrawAll)
import Prelude hiding ((!!))


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}
pC = P {pname = "C", pk = "pkC"}

preconditions :: [G]
preconditions = [
    pA ! (1, 2) $ ("bd_A", "dd_A")
    , pB ! (2, 1) $ ("bd_B", "dd_B")
    , pC ! (3, 3) $ ("bd_C", "dd_C")
    , pA !! (6, 6) $ ("bc_A", "dc_A")
    , pB !! (6, 6) $ ("bc_B", "dc_B")
    , pC !! (6, 6) $ ("bc_C", "dc_C")
    , StepSecret pA ("L", "") ("A_Bitcoin_S_Name_L_", "__SOME_HASH__") ("A_Dogecoin_S_Name_L_", "__SOME_HASH__")
    , StepSecret pB ("L", "") ("B_Bitcoin_S_Name_L_", "__SOME_HASH__") ("B_Dogecoin_S_Name_L_", "__SOME_HASH__")
    , StepSecret pC ("L", "") ("C_Bitcoin_S_Name_L_", "__SOME_HASH__") ("C_Dogecoin_S_Name_L_", "__SOME_HASH__")
    , StepSecret pA ("RL", "") ("A_Bitcoin_S_Name_RL_", "__SOME_HASH__") ("A_Dogecoin_S_Name_RL_", "__SOME_HASH__")
    , StepSecret pB ("RL", "") ("B_Bitcoin_S_Name_RL_", "__SOME_HASH__") ("B_Dogecoin_S_Name_RL_", "__SOME_HASH__")
    , StepSecret pC ("RL", "") ("C_Bitcoin_S_Name_RL_", "__SOME_HASH__") ("C_Dogecoin_S_Name_RL_", "__SOME_HASH__")
    ]

contract :: C
contract = withdrawAllD pA +> withdrawAllD pB +> withdrawAll pB