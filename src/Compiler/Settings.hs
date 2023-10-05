{-# LANGUAGE RankNTypes #-}
module Compiler.Settings where

import Data.Map.Strict (Map, fromList, empty, insert)

import Coins (BCoins (BCoins), DCoins (DCoins), Coins)
import Syntax.Common (Time, SName, P (pname), NodeLabel, emptyLabel)
import qualified Syntax.BitMLx as BitMLx
import qualified Data.Map as Map
import Compiler.StepSecrets (generateStepSecretsMap)
import Syntax.BitMLx (TimedPreconditions(..), ContractAdvertisement (ContractAdvertisement))

-- | Compiler settings for a target blockchain.
data CompilerSettings c = CompilerSettings {
    -- | Target blockchain name. Useful when generating deposit identifiers.
    targetBlockchain :: String
    -- | The participants of the contract,
    -- defined as everyone who has locked collateral into
    -- the contract.
    , participants :: [P]
    -- | The sum of all deposits.
    , balance :: c
    -- | How much money each participant locked as collateral.
    , collateral :: c
    -- | Secrets that all participants will reveal to authorize
    -- the start of a contract.
    , startSecrets :: Map P SName
    -- | Step secrets used synchronous execution of priority choices.
    , stepSecretsByLabel :: Map NodeLabel (Map P SName)
    -- | A security parameter that establishes how much exclusivity time we
    -- give to fases of the stipulation protocol and to actions before skipping them.
    , elapseTime :: Time
    -- | Basically a time counter that increments every time we need to give time exclusivity
    -- to some step. We represent this by increasing this parameter on recursive compiler calls.
    , currentTime :: Time
    -- | The label of the current node we are compiling. Labels uniquely determine the step secret
    -- we'll use on a priority choice and are built by appending "L" or "R" depending on whether
    -- we took the left or right action.
    , currentLabel :: NodeLabel
    -- | A hack to configure the compiler to choose the correct argument on 2-tuples
    -- of the form (bitcoin, dogecoin). Useful when compiling cases like Split.
    , coinChooser :: (BCoins, DCoins) -> c
}

-- | Compiler settings to produce the Bitcoin BitML contract, given the contract preconditions.
bitcoinSettings :: ContractAdvertisement -> CompilerSettings BCoins
bitcoinSettings advertisement = CompilerSettings {
    targetBlockchain = "Bitcoin"
    , participants = participantsFromPreconditions preconditions
    , balance = balanceFromPreconditions coinChooser preconditions
    , collateral = collateralFromPreconditions coinChooser preconditions
    , stepSecretsByLabel = generateStepSecretsMap participants contract
    , startSecrets = generateStartSecrets participants
    , currentTime = startingTime
    , elapseTime = elapseTime
    , currentLabel = emptyLabel
    , coinChooser = coinChooser
} where
    ContractAdvertisement timedPreconditions contract = advertisement
    (TimedPreconditions startingTime elapseTime preconditions) = timedPreconditions
    participants = participantsFromPreconditions preconditions
    coinChooser = fst


-- | Compiler settings to produce the Dogecoin BitML contract, given the contract preconditions.
dogecoinSettings :: ContractAdvertisement -> CompilerSettings DCoins
dogecoinSettings advertisement = CompilerSettings {
    targetBlockchain = "Dogecoin"
    , participants = participantsFromPreconditions preconditions
    , balance = balanceFromPreconditions coinChooser preconditions
    , collateral = collateralFromPreconditions coinChooser preconditions
    , stepSecretsByLabel = generateStepSecretsMap participants contract
    , startSecrets = generateStartSecrets participants
    , currentTime = startingTime
    , elapseTime = elapseTime
    , currentLabel = emptyLabel
    , coinChooser = coinChooser
} where
    ContractAdvertisement timedPreconditions contract = advertisement
    (TimedPreconditions startingTime elapseTime preconditions) = timedPreconditions
    participants = participantsFromPreconditions preconditions
    coinChooser = snd

-- | Extract a list of the contract's participants given it's preconditions.
participantsFromPreconditions :: [BitMLx.Precondition] -> [P]
participantsFromPreconditions preconditions =
    let
        getParticipant (BitMLx.Deposit p _ _) = [p]
        getParticipant _notCollateral = []
    in concatMap getParticipant preconditions

-- | Extract a list of the contract's secrets given it's preconditions.
secretsFromPreconditions :: [BitMLx.Precondition] -> [SName]
secretsFromPreconditions preconditions =
    let
        getSecret (BitMLx.Secret _ s _) = [s]
        getSecret _notSecret = []
    in concatMap getSecret preconditions

-- | Calculate the contract's balance given it's preconditions and a target blockchain.
balanceFromPreconditions :: Coins a => ((BCoins, DCoins) -> a) -> [BitMLx.Precondition] -> a
balanceFromPreconditions coinChooser preconditions =
    let
        getCoins (BitMLx.Deposit _ coins _) = coinChooser coins
        getCoins _notDeposit = 0
        coinValues = map getCoins preconditions
    in sum coinValues

-- | Calculate how much coins participants have to lock as collateral.
collateralFromPreconditions :: Coins a => ((BCoins, DCoins) -> a) -> [BitMLx.Precondition] -> a
collateralFromPreconditions coinChooser preconditions = fromInteger (toInteger (n - 2)) * balance
    where
        n = length (participantsFromPreconditions preconditions)
        balance = balanceFromPreconditions coinChooser preconditions

generateStartSecrets :: [P] -> Map P SName
generateStartSecrets participants = fromList [(p, "StartSecret" ++ "_" ++ pname p) | p <- participants]