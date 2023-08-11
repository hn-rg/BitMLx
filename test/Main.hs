module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import TestWithdraw.Test (testWithdraw)
import TestSplit.Test (testSplit)
import TestPriorityChoice.Test (testPriorityChoice)
import TestWithdrawD.Test (testWithdrawD)
import TestManyParticipantsWithdraw.Test (testManyParticipantsWithdraw)
import TestManyParticipantsPriorityChoice.Test (testManyParticipantsPriorityChoice)
import WellFormedFails.InconsistentWithdraw (inconsistentWithdrawTests)
import WellFormedFails.InconsistentSplit (inconsistentSplitTests)
import WellFormedFails.UncommitedSecret (uncommitedSecretTest)
import WellFormedFails.NoDeposit (noDepositTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests"
  [ testWithdrawD
  , testSplit
  , testPriorityChoice
  , testWithdraw
  , testManyParticipantsWithdraw
  , testManyParticipantsPriorityChoice
  -- , inconsistentWithdrawTests
  -- , inconsistentSplitTests
  -- , uncommitedSecretTest
  -- , noDepositTests
  ]
