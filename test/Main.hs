module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import TestWithdraw.Test (testWithdraw)
import TestSplit.Test (testSplit)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests"
  [ testWithdraw
  , testSplit
  ]
