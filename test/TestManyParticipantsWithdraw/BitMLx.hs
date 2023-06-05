
module TestManyParticipantsWithdraw.BitMLx where
    
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
    ]

contract :: C
contract = Withdraw [
        (pA, (0, 1%2)),
        (pB, (0, 1%2)),
        (pC, (1, 0))
    ]