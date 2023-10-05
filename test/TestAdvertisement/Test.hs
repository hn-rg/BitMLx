module TestAdvertisement.Test where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), testCaseSteps, assertFailure )

import qualified TestAdvertisement.BitMLx as BitMLx
import qualified TestAdvertisement.Bitcoin as Bitcoin
import qualified TestAdvertisement.Dogecoin as Dogecoin
import Compiler.Preconditions (compilePreconditions)
import Compiler.Contract (compileC)
import qualified Syntax.BitML as BitML
import Compiler.Advertisement (compileAdvertisement)
import Compiler.Settings ( bitcoinSettings, dogecoinSettings )


testAdvertisement :: TestTree
testAdvertisement = testCaseSteps "Compile a contract advertisement" $ \step -> do
    step "Building settings..."
    let bitcoinSettings' = bitcoinSettings BitMLx.advertisement
    let dogecoinSettings' = dogecoinSettings BitMLx.advertisement

    step "Compiling Bitcoin advertisement..."
    BitML.ContractAdvertisement bitcoinPreconditions bitcoinContract <- case compileAdvertisement bitcoinSettings' BitMLx.advertisement of
        Right result -> return result
        Left compilationError -> assertFailure ("Compilation Error: " ++ show compilationError )
    bitcoinPreconditions @?= Bitcoin.preconditions
    bitcoinContract @?= Bitcoin.contract

    step "Compiling Dogecoin advertisement..."
    BitML.ContractAdvertisement dogecoinPreconditions dogecoinContract <- case compileAdvertisement dogecoinSettings' BitMLx.advertisement of
        Right result -> return result
        Left compilationError -> assertFailure ("Compilation Error: " ++ show compilationError )
    dogecoinPreconditions @?= Dogecoin.preconditions
    dogecoinContract @?= Dogecoin.contract