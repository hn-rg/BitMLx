module TestWithdrawD.Test where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), testCaseSteps, assertFailure )

import qualified TestWithdrawD.BitMLx as BitMLx
import qualified TestWithdrawD.Bitcoin as Bitcoin
import qualified TestWithdrawD.Dogecoin as Dogecoin
import Compiler.Preconditions (compilePreconditions)
import Compiler.Contract (compileD)
import TestSettings ( bitcoinTestCompilerSettings, dogecoinTestCompilerSettings )


testWithdrawD :: TestTree
testWithdrawD = testCaseSteps "Compile a Withdraw guarded contract" $ \step -> do
    step "Building settings..."
    let bitcoinSettings' = bitcoinTestCompilerSettings BitMLx.preconditions (Right BitMLx.contract)
    let dogecoinSettings' = dogecoinTestCompilerSettings BitMLx.preconditions (Right BitMLx.contract)

    step "Compiling preconditions..."
    let bitcoinPreconditions = compilePreconditions bitcoinSettings' BitMLx.preconditions
    let dogecoinPreconditions = compilePreconditions dogecoinSettings' BitMLx.preconditions
    bitcoinPreconditions @?= Bitcoin.preconditions
    dogecoinPreconditions @?= Dogecoin.preconditions

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