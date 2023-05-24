
{-# LANGUAGE OverloadedStrings #-}

module TestSplit where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Data.Ratio ((%))

import Compiler.Settings
    ( CompilerSettings, bitcoinSettings, dogecoinSettings )
import Coins (BCoins(..), DCoins(..))
import Syntax.Common (P(..))
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Syntax.BitML (D(Withdraw))
import Compiler.Contract (compileD)
import Compiler.Preconditions (compilePreconditions)

participants :: [P]
participants = [pA, pB]
pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}


bitmlxContract :: BitMLx.D
bitmlxContract =
    BitMLx.Split [
        ((1%2, 1%2), BitMLx.Withdraw pA BitMLx.+> BitMLx.NullContract),
        ((1%2, 1%2), BitMLx.Withdraw pB BitMLx.+> BitMLx.NullContract)
    ]


bitmlxPreconditions :: [BitMLx.G]
bitmlxPreconditions = [
    pA BitMLx.! (2, 2) $ ("bd_a", "dd_a")
    , pB BitMLx.! (2, 2) $ ("bd_b", "dd_b")
    , pA BitMLx.!! (0, 0) $ ("bc_a", "dc_a")
    , pB BitMLx.!! (0, 0) $ ("bc_b", "dc_b")
    ]


expectedBitcoinPreconditions :: [BitML.G BCoins]
expectedBitcoinPreconditions = [
    pA BitML.! 1 $ "bd_A"
    , pB BitML.! 1 $ "bd_B"
    , pA BitML.! 0 $ "bc_A"
    , pB BitML.! 0 $ "bc_B"
    ]

expectedBitcoinContract :: BitML.D BCoins
expectedBitcoinContract = BitML.Split [
  (BCoins 2, [BitML.Withdraw pA])
  , (BCoins 0, [BitML.Withdraw pB])
  ]

expectedDogecoinPreconditions :: [BitML.G DCoins]
expectedDogecoinPreconditions = [
    pA BitML.! 1 $ "dd_A"
    , pB BitML.! 1 $ "dd_B"
    , pA BitML.! 0 $ "dc_A"
    , pB BitML.! 0 $ "dc_B"
    ]

expectedDogecoinContract :: BitML.D DCoins
expectedDogecoinContract = BitML.Split [
  (DCoins 2, [BitML.Withdraw pA])
  , (DCoins 0, [BitML.Withdraw pB])
  ]

testPreconditions :: TestTree
testPreconditions = testCase "Test compiling the preconditions" $ do
  let (bitcoinResult, dogecoinResult) = compilePreconditions bitmlxPreconditions
  bitcoinResult @?= expectedBitcoinPreconditions
  dogecoinResult @?= expectedDogecoinPreconditions 

testBitcoin :: TestTree
testBitcoin = testCase "Test compiling a Withdraw contract to Bitcoin BitML" $ do
  let settings = bitcoinSettings bitmlxPreconditions
  let compilationResult = compileD settings bitmlxContract
  compilationResult @?= Right expectedBitcoinContract

testDogecoin :: TestTree
testDogecoin = testCase "Test compiling a Withdraw contract to Dogecoin BitML" $ do
  let settings = dogecoinSettings bitmlxPreconditions
  let compilationResult = compileD settings bitmlxContract
  compilationResult @?= Right expectedDogecoinContract


testWithdrawSuite :: TestTree
testWithdrawSuite = testGroup "Withdraw Tests"
  [ testPreconditions
  , testBitcoin
  , testDogecoin
  ]