
module WellFormedFails.InconsistentWithdraw where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), testCaseSteps, assertFailure )

import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitMLx (ContractAdvertisement(ContractAdvertisement), TimedPreconditions(TimedPreconditions), Contract(Withdraw), (!), GuardedContract (WithdrawD), (+>))
import Compiler.WellFormed (assertWellFormed)
import Compiler.Error (CompilationError(InconsistentWithdraw))
import qualified Compiler.Error as Test.WellFormedFails

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

-- | Single option, inconsistent Bitcoin.
inconsistentWithdraw1 :: ContractAdvertisement
inconsistentWithdraw1 = ContractAdvertisement
    (
        TimedPreconditions 1 10 [
            pA ! (1, 1) $ "A_deposit",
            pB ! (1, 1) $ "B_deposit"
        ]
    )
    (
        Withdraw [
            ((42, 1), pA),
            ((0, 1), pB)
        ]
    )


-- | Single option, inconsistent Dogecoin.
inconsistentWithdraw2 :: ContractAdvertisement
inconsistentWithdraw2 = ContractAdvertisement
    (
        TimedPreconditions 1 10 [
            pA ! (1, 1) $ "A_deposit",
            pB ! (1, 1) $ "B_deposit"
        ]
    )
    (
        Withdraw [
            ((2, 1), pA),
            ((0, 42), pB)
        ]
    )


-- | Many options, inconsistent D
inconsistentWithdraw3 :: ContractAdvertisement
inconsistentWithdraw3 = ContractAdvertisement
    (
        TimedPreconditions 1 10 [
            pA ! (1, 1) $ "A_deposit",
            pB ! (1, 1) $ "B_deposit"
        ]
    )
    (
        WithdrawD [
            ((42, 1), pA),
            ((0, 1), pB)
        ] 
        +> Withdraw [
            ((2, 1), pA),
            ((0, 1), pB)
        ]
    )

-- | Many options, inconsistent C
inconsistentWithdraw4 :: ContractAdvertisement
inconsistentWithdraw4 = ContractAdvertisement
    (
        TimedPreconditions 1 10 [
            pA ! (1, 1) $ "A_deposit",
            pB ! (1, 1) $ "B_deposit"
        ]
    )
    (
        WithdrawD [
            ((2, 1), pA),
            ((0, 1), pB)
        ] 
        +> Withdraw [
            ((2, 1), pA),
            ((0, 42), pB)
        ]
    )

-- | Funds are fine, but participants are repeated.
inconsistentWithdraw5 :: ContractAdvertisement
inconsistentWithdraw5 = ContractAdvertisement
    (
        TimedPreconditions 1 10 [
            pA ! (1, 1) $ "A_deposit",
            pB ! (1, 1) $ "B_deposit"
        ]
    )
    (
        Withdraw [
            ((2, 1), pA),
            ((0, 42), pA)
        ]
    )

-- | Empty participants list.
inconsistentWithdraw6 :: ContractAdvertisement
inconsistentWithdraw6 = ContractAdvertisement
    (
        TimedPreconditions 1 10 [
            pA ! (1, 1) $ "A_deposit",
            pB ! (1, 1) $ "B_deposit"
        ]
    )
    (
        Withdraw []
    )

inconsistentWithdrawTest1 :: TestTree
inconsistentWithdrawTest1 = testCaseSteps "Inconsistent Bitcoin Withdraw" $ \step -> do
    step "Withdraw with inconsistent Bitcoin division is not Well Formed."
    assertWellFormed inconsistentWithdraw1 @?= Left InconsistentWithdraw

inconsistentWithdrawTest2 :: TestTree
inconsistentWithdrawTest2 = testCaseSteps "Inconsistent Dogecoin Withdraw" $ \step -> do
    step "Withdraw with inconsistent Dogecoin division is not Well Formed."
    assertWellFormed inconsistentWithdraw2 @?= Left InconsistentWithdraw

inconsistentWithdrawTest3 :: TestTree
inconsistentWithdrawTest3 = testCaseSteps "Inconsistent WithdrawD" $ \step -> do
    step "WithdrawD with inconsistent Bitcoin division is not Well Formed."
    assertWellFormed inconsistentWithdraw3 @?= Left InconsistentWithdraw

inconsistentWithdrawTest4 :: TestTree
inconsistentWithdrawTest4 = testCaseSteps "Nested inconsistent Withdraw" $ \step -> do
    step "Nested Withdraw with inconsistent Bitcoin division is not Well Formed."
    assertWellFormed inconsistentWithdraw4 @?= Left InconsistentWithdraw

inconsistentWithdrawTest5 :: TestTree
inconsistentWithdrawTest5 = testCaseSteps "Repeated participants Withdraw" $ \step -> do
    step "Withdraw with repeated participants is not Well Formed."
    assertWellFormed inconsistentWithdraw5 @?= Left InconsistentWithdraw

inconsistentWithdrawTest6 :: TestTree
inconsistentWithdrawTest6 = testCaseSteps "Empty withdraw" $ \step -> do
    step "Withdraw with empty participants list is not Well Formed."
    assertWellFormed inconsistentWithdraw6 @?= Left InconsistentWithdraw

inconsistentWithdrawTests = testGroup "Inconsistent Withdraws"
  [ inconsistentWithdrawTest1
  , inconsistentWithdrawTest2
  , inconsistentWithdrawTest3
  , inconsistentWithdrawTest4
  , inconsistentWithdrawTest5
  , inconsistentWithdrawTest6
  ]
