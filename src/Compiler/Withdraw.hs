module Compiler.Withdraw where

import Syntax.Common (P, BCoins, DCoins, Deposit)
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx

-- | A withdraw statement in BitMLx is compiled in BitML as a split between:
-- - The corresponding withdraw statement for the specified participant.
-- - For each participant, a withdraw of their collateral.
compileWithdraw :: [BitMLx.G] -> P -> (BitML.C BCoins, BitML.C DCoins)
compileWithdraw preconditions p =
    let
        bitcoinCollateralWithdraws = map (\ (p, (bitcoins, _)) -> (bitcoins, [BitML.Withdraw p])) (collateralsByParticipant preconditions)
        dogecoinCollateralWithdraws = map (\ (p, (_, dogecoins)) -> (dogecoins, [BitML.Withdraw p])) (collateralsByParticipant preconditions)
        (bitcoinsBalance, dogecoinsBalance) = contractBalance preconditions
        bitcoinContract = [
            BitML.Split (
                (bitcoinsBalance, [BitML.Withdraw p]) : bitcoinCollateralWithdraws
                )
            ]
        dogecoinContract = [
            BitML.Split (
                (dogecoinsBalance, [BitML.Withdraw p]) : dogecoinCollateralWithdraws
                )
            ]
    in (bitcoinContract, dogecoinContract)


-- | Sum of the balances of all persistent deposits.
contractBalance :: [BitMLx.G] -> (BCoins, DCoins)
contractBalance preconditions =
    let
        getBitcoins (BitMLx.Deposit _ (bitcoins, _) _) = [bitcoins]
        getBitcoins _notDeposit = []
        getDogecoins (BitMLx.Deposit _ (_, dogecoins) _) = [dogecoins]
        getDogecoins _notDeposit = []
        bitcoinValues = concatMap getBitcoins preconditions
        dogecoinValues = concatMap getBitcoins preconditions
    in (sum bitcoinValues, sum dogecoinValues)

-- | Extracts the values of the collaterals and groups them by participant. 
collateralsByParticipant :: [BitMLx.G] -> [(P, (BCoins, DCoins))]
collateralsByParticipant preconditions =
    let
        getParticipantAndCoins (BitMLx.Collateral p (bitcoins, dogecoins) _) = [(p, (bitcoins, dogecoins))]
        getParticipantAndCoins _notCollateral = []
    in concatMap getParticipantAndCoins preconditions
