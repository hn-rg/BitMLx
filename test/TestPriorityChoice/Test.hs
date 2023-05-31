module TestPriorityChoice.Test where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), testCaseSteps, assertFailure )

import qualified TestPriorityChoice.BitMLx as BitMLx
import qualified TestPriorityChoice.Bitcoin as Bitcoin
import qualified TestPriorityChoice.Dogecoin as Dogecoin
import Compiler.Preconditions (compilePreconditions)
import Compiler.Contract (compileC)
import Compiler.Settings ( bitcoinSettings, dogecoinSettings )


testPriorityChoice :: TestTree
testPriorityChoice = testCaseSteps "Compile a Priority Choice contract" $ \step -> do
    step "Compiling preconditions..."
    let (bitcoinResult, dogecoinResult) = compilePreconditions BitMLx.preconditions
    bitcoinResult @?= Bitcoin.preconditions
    dogecoinResult @?= Dogecoin.preconditions

    step "Building settings..."
    let bitcoinSettings' = bitcoinSettings BitMLx.preconditions
    let dogecoinSettings' = dogecoinSettings BitMLx.preconditions

    step "Compiling to Bitcoin BitML..."
    let bitcoinResult = compileC bitcoinSettings' BitMLx.contract
    compiledBitcoin <- case bitcoinResult of
        Right result -> return result
        Left compilationError -> assertFailure ("Compilation Error: " ++ show compilationError )
    compiledBitcoin @?= Bitcoin.contract

    step "Compiling to Dogecoin BitML..."
    let dogecoinResult = compileC dogecoinSettings' BitMLx.contract
    compiledDogecoin <- case dogecoinResult of
        Right result -> return result
        Left compilationError -> assertFailure ("Compilation Error: " ++ show compilationError )
    compiledDogecoin @?= Dogecoin.contract