module TestWithdraw.Test where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), testCaseSteps, assertFailure )

import qualified TestWithdraw.BitMLx as BitMLx
import qualified TestWithdraw.Bitcoin as Bitcoin
import qualified TestWithdraw.Dogecoin as Dogecoin
import Compiler.Preconditions (compilePreconditions)
import Compiler.Contract (compileD)
import Compiler.Settings ( bitcoinSettings, dogecoinSettings )


testWithdraw :: TestTree
testWithdraw = testCaseSteps "Compile a Withdraw contract" $ \step -> do
    step "Compiling preconditions..."
    let (bitcoinResult, dogecoinResult) = compilePreconditions BitMLx.preconditions
    bitcoinResult @?= Bitcoin.preconditions
    dogecoinResult @?= Dogecoin.preconditions

    step "Building settings..."
    let bitcoinSettings' = bitcoinSettings BitMLx.preconditions
    let dogecoinSettings' = dogecoinSettings BitMLx.preconditions

    step "Compiling to Bitcoin BitML..."
    let bitcoinResult = compileD bitcoinSettings' BitMLx.contract
    compiledBitcoin <- case bitcoinResult of
        Right result -> return result
        Left compilationError -> assertFailure ("Compilation Error: " ++ show compilationError )
    compiledBitcoin @?= Bitcoin.contract

    step "Compiling to Dogecoin BitML..."
    let dogecoinResult = compileD dogecoinSettings' BitMLx.contract
    compiledDogecoin <- case dogecoinResult of
        Right result -> return result
        Left compilationError -> assertFailure ("Compilation Error: " ++ show compilationError )
    compiledDogecoin @?= Dogecoin.contract