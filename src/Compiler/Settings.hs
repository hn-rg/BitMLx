{-# LANGUAGE RankNTypes #-}
module Compiler.Settings (CompilerSettings(..), bitcoinSettings, dogecoinSettings, participantsFromPreconditions, secretsFromPreconditions, balanceFromPreconditions) where

import Data.Map.Strict (Map, fromList, empty, insert)

import Coins (BCoins (BCoins), DCoins (DCoins), Coins)
import Syntax.Common (Time, SName, P, NodeLabel, emptyLabel)
import qualified Syntax.BitMLx as BitMLx
import qualified Data.Map as Map
import Compiler.StepSecrets (generateStepSecretsMap)
import Syntax.BitMLx (TimedPreconditions(..))

-- | Compiler settings for a target blockchain.
data CompilerSettings c = CompilerSettings {
    -- | The participants of the contract,
    -- defined as everyone who has locked collateral into
    -- the contract.
    participants :: [P]
    -- | The sum of all deposits.
    , balance :: c
    -- | How much money each participant locked as collateral.
    , collateral :: c
    -- | Sum of balance and all collaterals. Because of the general Coins type,
    -- making this sum requires many ugly type conversions (see below). Precalculating
    -- this here increases code readability in the implementation of statements like
    -- Split, which have a high enough cognitive load already.
    , totalFunds :: c
    -- | Step secrets used in this blockchain (the one we are compiling now)
    -- when compiling a priority choice.
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
bitcoinSettings :: BitMLx.TimedPreconditions -> Either BitMLx.Contract BitMLx.GuardedContract -> CompilerSettings BCoins
bitcoinSettings (TimedPreconditions startingTime elapseTime preconditions) contract = CompilerSettings {
    participants = participantsFromPreconditions preconditions
    , balance = balanceFromPreconditions coinChooser preconditions
    , collateral = collateralFromPreconditions coinChooser preconditions
    , totalFunds = totalFundsFromPreconditions coinChooser preconditions
    , stepSecretsByLabel = generateStepSecretsMap participants contract
    , currentTime = startingTime
    , elapseTime = elapseTime
    , currentLabel = emptyLabel
    , coinChooser = coinChooser
} where
    participants = participantsFromPreconditions preconditions
    coinChooser = fst


-- | Compiler settings to produce the Dogecoin BitML contract, given the contract preconditions.
dogecoinSettings :: BitMLx.TimedPreconditions -> Either BitMLx.Contract BitMLx.GuardedContract -> CompilerSettings DCoins
dogecoinSettings (TimedPreconditions startingTime elapseTime preconditions) contract = CompilerSettings {
    participants = participantsFromPreconditions preconditions
    , balance = balanceFromPreconditions coinChooser preconditions
    , collateral = collateralFromPreconditions coinChooser preconditions
    , totalFunds = totalFundsFromPreconditions coinChooser preconditions
    , stepSecretsByLabel = generateStepSecretsMap participants contract
    , currentTime = startingTime
    , elapseTime = elapseTime
    , currentLabel = emptyLabel
    , coinChooser = coinChooser
} where
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

-- | The total funds (balance + all collaterals) of a contract on a given blockchain.
totalFundsFromPreconditions :: Coins a => ((BCoins, DCoins) -> a) -> [BitMLx.Precondition] -> a
totalFundsFromPreconditions coinChooser preconditions = balance + fromInteger (toInteger (n * fromInteger (toInteger collateral)))
    where
        n = length (participantsFromPreconditions preconditions)
        balance = balanceFromPreconditions coinChooser preconditions
        collateral = collateralFromPreconditions coinChooser preconditions