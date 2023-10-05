module TestStipulation where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), testCaseSteps, assertFailure )

import TestSettings ( bitcoinTestCompilerSettings )
import Syntax.BitMLx (Contract (WithdrawAll, Withdraw), TimedPreconditions (TimedPreconditions), GuardedContract (WithdrawAllD, Reveal), (+>), (!), Precondition)
import Syntax.Common (P(..))
import Compiler.Stipulation (makeStipulationContract)


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}

preconditions :: [Precondition]
preconditions =  [
    pA ! (1, 1) $ "A_deposit"
    , pB ! (2, 2) $ "B_deposit"
    ]

timedPreconditions :: TimedPreconditions
timedPreconditions = TimedPreconditions 1 10 preconditions

contract :: Contract
contract = WithdrawAllD pA +> WithdrawAll pB

expectedStipulationContract :: Contract
expectedStipulationContract =
    Reveal ["StartSecret_A", "StartSecret_B"] (WithdrawAllD pA +> WithdrawAll pB)
    +> Withdraw [((1, 1), pA), ((2, 2), pB)]

testContractStipulation :: TestTree
testContractStipulation = testCaseSteps "Build a stipulation contract" $ \step -> do
    let settings' = bitcoinTestCompilerSettings timedPreconditions (Left contract)
    makeStipulationContract settings' preconditions contract @?= expectedStipulationContract