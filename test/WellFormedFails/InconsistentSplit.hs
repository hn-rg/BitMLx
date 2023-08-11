
module WellFormedFails.InconsistentSplit where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), testCaseSteps, assertFailure )

import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitMLx (ContractAdvertisement(ContractAdvertisement), TimedPreconditions(TimedPreconditions), Contract(Withdraw, WithdrawAll), (!), GuardedContract (WithdrawD, Split), (+>))
import Compiler.WellFormed (assertWellFormed)
import Compiler.Error (CompilationError(InconsistentWithdraw, InconsistentSplit))
import qualified Compiler.Error as Test.WellFormedFails

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

-- | Single option, inconsistent Bitcoin.
inconsistentSplit1 :: ContractAdvertisement
inconsistentSplit1 = ContractAdvertisement
    (
        TimedPreconditions 1 10 [
            pA ! (1, 1) $ "A_deposit",
            pB ! (1, 1) $ "B_deposit"
        ]
    )
    (
        Split [
            ((42, 1), WithdrawAll pA),
            ((0, 1), WithdrawAll pB)
        ] +> WithdrawAll pA
    )


-- | Single option, inconsistent Dogecoin.
inconsistentSplit2 :: ContractAdvertisement
inconsistentSplit2 = ContractAdvertisement
    (
        TimedPreconditions 1 10 [
            pA ! (1, 1) $ "A_deposit",
            pB ! (1, 1) $ "B_deposit"
        ]
    )
    (
        Split [
            ((2, 1), WithdrawAll pA),
            ((0, 42), WithdrawAll pB)
        ] +> WithdrawAll pA
    )


-- | Null balance branch.
inconsistentSplit3 :: ContractAdvertisement
inconsistentSplit3 = ContractAdvertisement
    (
        TimedPreconditions 1 10 [
            pA ! (1, 1) $ "A_deposit",
            pB ! (1, 1) $ "B_deposit"
        ]
    )
    (
        Split [
            ((2, 1), WithdrawAll pA),
            ((0, 0), WithdrawAll pB)
        ] +> WithdrawAll pA
    )

inconsistentSplitTest1 :: TestTree
inconsistentSplitTest1 = testCaseSteps "Inconsistent Bitcoin Split" $ \step -> do
    step "Split with inconsistent Bitcoin division is not well formed."
    assertWellFormed inconsistentSplit1 @?= Left InconsistentSplit

inconsistentSplitTest2 :: TestTree
inconsistentSplitTest2 = testCaseSteps "Inconsistent Dogecoin Split" $ \step -> do
    step "Split with inconsistent Dogecoin division is not well formed."
    assertWellFormed inconsistentSplit2 @?= Left InconsistentSplit

inconsistentSplitTest3 :: TestTree
inconsistentSplitTest3 = testCaseSteps "Null Branch Split" $ \step -> do
    step "Split with a null balance branch is not well formed."
    assertWellFormed inconsistentSplit3 @?= Left InconsistentSplit


inconsistentSplitTests = testGroup "Inconsistent Splits"
  [ inconsistentSplitTest1
  , inconsistentSplitTest2
  , inconsistentSplitTest3
  ]
