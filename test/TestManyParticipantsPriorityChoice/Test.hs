module TestManyParticipantsPriorityChoice.Test where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), testCaseSteps, assertFailure )

import qualified TestManyParticipantsPriorityChoice.BitMLx as BitMLx
import qualified TestManyParticipantsPriorityChoice.Bitcoin as Bitcoin
import qualified TestManyParticipantsPriorityChoice.Dogecoin as Dogecoin
import Compiler.Preconditions (compilePreconditions)
import Compiler.Contract (compileC)
import Compiler.Settings ( bitcoinSettings, dogecoinSettings )


testManyParticipantsPriorityChoice :: TestTree
testManyParticipantsPriorityChoice = testCaseSteps "Compile a Priority Choice with many participants and many choices" $ \step -> do
    step "Building settings..."
    let bitcoinSettings' = bitcoinSettings BitMLx.preconditions (Left BitMLx.contract)
    let dogecoinSettings' = dogecoinSettings BitMLx.preconditions (Left BitMLx.contract)

    step "Compiling preconditions..."
    let (bitcoinResult, dogecoinResult) = compilePreconditions bitcoinSettings' dogecoinSettings' BitMLx.preconditions
    bitcoinResult @?= Bitcoin.preconditions
    dogecoinResult @?= Dogecoin.preconditions

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