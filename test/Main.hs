module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import TestWithdraw.Test (testWithdraw)
import TestSplit.Test (testSplit)
import TestPriorityChoice.Test (testPriorityChoice)
import TestWithdrawD.Test (testWithdrawD)
import TestRefund.Test (testRefund)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests"
  [ testWithdrawD
  , testSplit
  , testPriorityChoice
  , testWithdraw
  , testRefund
  ]
