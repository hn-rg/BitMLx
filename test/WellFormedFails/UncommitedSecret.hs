
module WellFormedFails.UncommitedSecret where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), testCaseSteps, assertFailure )

import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitMLx (ContractAdvertisement(ContractAdvertisement), TimedPreconditions(TimedPreconditions), Contract(Withdraw, WithdrawAll), (!), GuardedContract (WithdrawD, Split, Reveal), (+>), Precondition (Secret))
import Compiler.WellFormed (assertWellFormed)
import Compiler.Error (CompilationError(InconsistentWithdraw, InconsistentSplit))
import qualified Compiler.Error as Test.WellFormedFails
import Compiler.Error (CompilationError(UncommitedSecret))

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

-- | Single option, inconsistent Dogecoin.
uncommitedSecret1 :: ContractAdvertisement
uncommitedSecret1 = ContractAdvertisement
    (
        TimedPreconditions 1 10 [
            pA ! (1, 1) $ "A_deposit",
            pB ! (1, 1) $ "B_deposit",
            Secret pB "b" "__HASH_PLACEHOLDER__"
        ]
    )
    (
        Reveal ["a"]
            (WithdrawAll pA)
        +> WithdrawAll pB
    )

uncommitedSecretTest :: TestTree
uncommitedSecretTest = testCaseSteps "Uncommited Secret" $ \step -> do
    step "Reveal statement refering to a secret not commited in preconditions is not well formed."
    assertWellFormed uncommitedSecret1 @?= Left (UncommitedSecret "a")
