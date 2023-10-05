module TestPriorityChoice.Test where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), testCaseSteps, assertFailure )

import qualified TestPriorityChoice.BitMLx as BitMLx
import qualified TestPriorityChoice.Bitcoin as Bitcoin
import qualified TestPriorityChoice.Dogecoin as Dogecoin
import Compiler.Preconditions (compilePreconditions)
import Compiler.Contract (compileC)
import TestSettings ( bitcoinTestCompilerSettings, dogecoinTestCompilerSettings )


testPriorityChoice :: TestTree
testPriorityChoice = testCaseSteps "Compile a Priority Choice contract" $ \step -> do
    step "Building settings..."
    let bitcoinSettings' = bitcoinTestCompilerSettings BitMLx.preconditions (Left BitMLx.contract)
    let dogecoinSettings' = dogecoinTestCompilerSettings BitMLx.preconditions (Left BitMLx.contract)


    step "Compiling preconditions..."
    let bitcoinPreconditions = compilePreconditions bitcoinSettings' BitMLx.preconditions
    let dogecoinPreconditions = compilePreconditions dogecoinSettings' BitMLx.preconditions
    bitcoinPreconditions @?= Bitcoin.preconditions
    dogecoinPreconditions @?= Dogecoin.preconditions

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