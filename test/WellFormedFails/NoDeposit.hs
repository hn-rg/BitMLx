
module WellFormedFails.NoDeposit where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), testCaseSteps, assertFailure )

import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitMLx (ContractAdvertisement(ContractAdvertisement), TimedPreconditions(TimedPreconditions), Contract(Withdraw), (!), GuardedContract (WithdrawD), (+>), (#:))
import Compiler.WellFormed (assertWellFormed)
import Compiler.Error (CompilationError(InconsistentWithdraw, NoDeposit))
import qualified Compiler.Error as Test.WellFormedFails

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}
pC = P {pname = "C", pk = "pkC"}

-- | Single option, inconsistent Bitcoin.
noDeposit1 :: ContractAdvertisement
noDeposit1 = ContractAdvertisement
    (
        TimedPreconditions 1 10 [
            pA ! (1, 1) $ "A_deposit",
            pB ! (1, 1) $ "B_deposit"
        ]
    )
    (
        Withdraw [
            ((2, 2), pC)
        ]
    )

-- | Single option, inconsistent Bitcoin.
noDeposit2 :: ContractAdvertisement
noDeposit2 = ContractAdvertisement
    (
        TimedPreconditions 1 10 [
            pA ! (1, 1) $ "A_deposit",
            pB ! (1, 1) $ "B_deposit"
        ]
    )
    (
        [pC] #: WithdrawD [
            ((2, 1), pA),
            ((0, 1), pB)
        ] 
        +> Withdraw [
            ((1, 0), pA),
            ((1, 2), pB)
        ]
    )


noDepositTest1 :: TestTree
noDepositTest1 = testCaseSteps "No deposit Withdraw" $ \step -> do
    step "Withdraw by participant with no deposit in the preconditions is not well formed."
    assertWellFormed noDeposit1 @?= Left (NoDeposit pC)

noDepositTest2 :: TestTree
noDepositTest2 = testCaseSteps "No deposit Authorization" $ \step -> do
    step "Authorization by participant with no deposit in the preconditions is not well formed."
    assertWellFormed noDeposit2 @?= Left (NoDeposit pC)


noDepositTests = testGroup "No Deposits"
  [ noDepositTest1
  , noDepositTest2
  ]
