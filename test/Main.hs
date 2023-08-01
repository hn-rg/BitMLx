module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import TestWithdraw.Test (testWithdraw)
import TestSplit.Test (testSplit)
import TestPriorityChoice.Test (testPriorityChoice)
import TestTimedPriorityChoice.Test (testTimedPriorityChoice)
import TestWithdrawD.Test (testWithdrawD)
import TestManyParticipantsWithdraw.Test (testManyParticipantsWithdraw)
import TestManyParticipantsPriorityChoice.Test (testManyParticipantsPriorityChoice)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests"
  [ testWithdrawD
  , testSplit
  , testPriorityChoice
  , testTimedPriorityChoice
  , testWithdraw
  , testManyParticipantsWithdraw
  , testManyParticipantsPriorityChoice
  ]
