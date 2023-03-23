module Compiler.Settings (CompilerSettings(..), initialSettings) where

import Data.Map.Strict (Map, fromList, empty)

import Coins (BCoins (BCoins), DCoins (DCoins))
import Syntax.Common (Time, SName, P, NodeLabel)
import qualified Syntax.BitMLx as BitMLx
import qualified Data.Map as Map

-- | Compiler settings.
data CompilerSettings = CompilerSettings {
    -- | The participants of the contract,
    -- defined as everyone who has locked collateral into
    -- the contract.
    participants :: [P]
    -- | The sum of all bitcoin deposits.
    , bitcoinBalance :: BCoins
    -- | The sum of dogecoin deposits. 
    , dogecoinBalance :: DCoins
    -- | How much bitcoin each participant locked as collateral.
    , bitcoinCollateral :: BCoins
    -- | How much dogecoin each participant locked as collateral.
    , dogecoinCollateral :: DCoins
    -- | Step secrets used in bitcoin when compiling a priority choice.
    , bitcoinStepSecretsMap :: Map NodeLabel (Map P SName)
    -- | Step secrets used in dogecoin when compiling a priority choice.
    , dogecoinStepSecretsMap :: Map NodeLabel (Map P SName)
    -- | A security parameter that establishes how much exclusivity time we
    -- give to fases of the stipulation protocol and to actions before skipping them.
    , elapseTime :: Time
    -- | Basically a time counter that increments every time we need to give time exclusivity
    -- to some step. We represent this by increasing this parameter on recursive compiler calls.
    , currentTime :: Time
    -- The label of the current node we are compiling. Labels uniquely determine the step secret
    -- we'll use on a priority choice and are built by appending "L" or "R" depending on whether
    -- we took the left or right action.
    , currentLabel :: NodeLabel
}

-- | Initialize the compiler settings given the contract preconditions.
initialSettings :: [BitMLx.G] -> CompilerSettings
initialSettings preconditions = CompilerSettings {
    participants = participantsFromPreconditions preconditions
    , bitcoinBalance = bitcoinBalanceFromPreconditions preconditions
    , dogecoinBalance = dogecoinBalanceFromPreconditions preconditions
    , bitcoinCollateral = bitcoinCollateralFromPreconditions preconditions
    , dogecoinCollateral = dogecoinCollateralFromPreconditions preconditions
    , bitcoinStepSecretsMap = bitcoinStepSecretsFromPreconditions preconditions
    , dogecoinStepSecretsMap = dogecoinStepSecretsFromPreconditions preconditions
    , currentTime = 1
    , elapseTime = 10
    , currentLabel = ""
}

-- | Extract a list of the contract's participants given it's preconditions.
participantsFromPreconditions :: [BitMLx.G] -> [P]
participantsFromPreconditions preconditions = 
    let
        getParticipant (BitMLx.Collateral p _ _) = [p]
        getParticipant _notCollateral = []
    in concatMap getParticipant preconditions

-- | Calculate the contract's bitcoin balance given it's preconditions.
bitcoinBalanceFromPreconditions :: [BitMLx.G] -> BCoins
bitcoinBalanceFromPreconditions preconditions = 
    let
        getBitcoins (BitMLx.Deposit _ (bitcoins, _) _) = bitcoins
        getBitcoins _notDeposit = 0
        bitcoinValues = map getBitcoins preconditions
    in sum bitcoinValues

-- | Calculate the contract's dogecoin balance given it's preconditions.
dogecoinBalanceFromPreconditions :: [BitMLx.G] -> DCoins
dogecoinBalanceFromPreconditions preconditions = 
    let
        getDogecoins (BitMLx.Deposit _ (_, dogecoins) _) = dogecoins
        getDogecoins _notDeposit = 0
        dogecoinValues = map getDogecoins preconditions
    in sum dogecoinValues

-- | Calculate how much bitcoin participant have to lock as collateral.
bitcoinCollateralFromPreconditions :: [BitMLx.G] -> BCoins
bitcoinCollateralFromPreconditions preconditions = BCoins (n - 2) * balance
    where
        n = length (participantsFromPreconditions preconditions)
        balance = bitcoinBalanceFromPreconditions preconditions

-- | Calculate how much dogecoin participant have to lock as collateral.
dogecoinCollateralFromPreconditions :: [BitMLx.G] -> DCoins
dogecoinCollateralFromPreconditions preconditions = DCoins (n - 2) * balance
    where
        n = length (participantsFromPreconditions preconditions)
        balance = dogecoinBalanceFromPreconditions preconditions

-- | Map step secrets by label and participant so that we can easily query for them later.
bitcoinStepSecretsFromPreconditions :: [BitMLx.G] -> Map NodeLabel (Map P SName)
bitcoinStepSecretsFromPreconditions preconditions =
    let 
        aux (BitMLx.StepSecret p l (bn, _) _) = [(l, p, bn)]
        aux _notStepSecret = []
        stepSecretTuples = concatMap aux preconditions
    in Map.fromListWith (Map.unionWith (++)) [(a, Map.singleton b c) | (a, b, c) <- stepSecretTuples]

-- | Map step secrets by label and participant so that we can easily query for them later.
dogecoinStepSecretsFromPreconditions :: [BitMLx.G] -> Map NodeLabel (Map P SName)
dogecoinStepSecretsFromPreconditions preconditions = 
    let 
        aux (BitMLx.StepSecret p l _ (dn,_)) = [(l, p, dn)]
        aux _notStepSecret = []
        stepSecretTuples = concatMap aux preconditions
    in Map.fromListWith (Map.unionWith (++)) [(a, Map.singleton b c) | (a, b, c) <- stepSecretTuples]
