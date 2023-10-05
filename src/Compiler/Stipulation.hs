{-# LANGUAGE NamedFieldPuns #-}
module Compiler.Stipulation where

import Coins (DCoins, BCoins)
import Syntax.BitMLx (TimedPreconditions (TimedPreconditions), Contract (Withdraw), Precondition (Secret, Deposit), GuardedContract (Reveal), (+>), ContractAdvertisement(ContractAdvertisement))
import Syntax.Common (P)
import Compiler.Settings (CompilerSettings (CompilerSettings, startSecrets))
import Data.Map (elems)


-- | The refund contract just refunds every participant with their original deposit.
-- When compiled, this will include their collaterals. 
refundContract :: [Precondition] -> Contract
refundContract preconditions = let
    userDeposits = deposits preconditions
    in Withdraw userDeposits
    where
        deposits :: [Precondition] -> [((BCoins, DCoins), P)]
        deposits [] = []
        deposits (Secret {}:ps) = deposits ps
        deposits (Deposit p (bcoins, dcoins) _:ps) = ((bcoins, dcoins), p):deposits ps

-- | The entry-point and initial synchronization between the target blockchains.
--
-- The idea here is we want to either run the contract on both blockchains or
-- refund on both. We can express this as a priority choice between starting the BitMLx
-- contract or refunding the deposits.
-- - If both contracts are published in their respective blockchains, honest users will reveal
-- their start secrets, enabling the execution of start.
-- - Once all start secrets are revealed, one honest user will take the init branch by revealing
-- their first step secret.
-- - If only one of them is published, then honest users will wait and go for the refund option.
-- - If a malicious user tries to initialize one contract but refund the other, the honest users
--   can punish him/her on the refund side with the step secret.
makeStipulationContract :: CompilerSettings c -> [Precondition] -> Contract -> Contract
makeStipulationContract CompilerSettings{startSecrets} preconditions contract =
    let start = Reveal (elems startSecrets) contract
        refund = refundContract preconditions
    in start +> refund
