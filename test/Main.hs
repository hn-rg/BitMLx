module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import TestWithdraw.Test (testWithdraw)
import TestSplit.Test (testSplit)
import TestPriorityChoice.Test (testPriorityChoice)
import TestWithdrawD.Test (testWithdrawD)
import TestManyParticipantsWithdraw.Test (testManyParticipantsWithdraw)
import TestManyParticipantsPriorityChoice.Test (testManyParticipantsPriorityChoice)
import TestAuthorize.Test (testAuthorize)
import TestReveal.Test (testReveal)
import TestRevealIf.Test (testRevealIf)
import WellFormedFails.InconsistentWithdraw (inconsistentWithdrawTests)
import WellFormedFails.InconsistentSplit (inconsistentSplitTests)
import WellFormedFails.UncommitedSecret (uncommitedSecretTest)
import WellFormedFails.NoDeposit (noDepositTests)
import TestStipulation (testContractStipulation)
import TestAdvertisement.Test (testAdvertisement)

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
  , testAuthorize
  , testReveal
  , testRevealIf
  , testContractStipulation
  , testAdvertisement
  , inconsistentWithdrawTests
  , inconsistentSplitTests
  , uncommitedSecretTest
  , noDepositTests
  ]
