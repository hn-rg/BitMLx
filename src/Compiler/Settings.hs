{-# LANGUAGE RankNTypes #-}
module Compiler.Settings (CompilerSettings(..), bitcoinSettings, dogecoinSettings) where

import Data.Map.Strict (Map, fromList, empty)

import Coins (BCoins (BCoins), DCoins (DCoins), Coins)
import Syntax.Common (Time, SName, P, NodeLabel)
import qualified Syntax.BitMLx as BitMLx
import qualified Data.Map as Map

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
    , thisChainStepSecretsByLabel :: Map NodeLabel (Map P SName)
    -- | Step secrets used in the other blockchain (the one we are keeping sync with)
    -- when compiling a priority choice.
    , otherChainStepSecretsByLabel :: Map NodeLabel (Map P SName)
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
    , coinChooser :: (Rational, Rational) -> Rational
}

-- | Initialize the compiler settings given the contract preconditions.
bitcoinSettings :: [BitMLx.G] -> CompilerSettings BCoins
bitcoinSettings preconditions = CompilerSettings {
    participants = participantsFromPreconditions preconditions
    , balance = balanceFromPreconditions coinChooser preconditions
    , collateral = collateralFromPreconditions coinChooser preconditions
    , totalFunds = totalFundsFromPreconditions coinChooser preconditions
    , thisChainStepSecretsByLabel = bitcoinStepSecretsFromPreconditions preconditions
    , otherChainStepSecretsByLabel = dogecoinStepSecretsFromPreconditions preconditions
    , currentTime = 1
    , elapseTime = 10
    , currentLabel = ("", "")
    , coinChooser = coinChooser
} where coinChooser = fst

dogecoinSettings :: [BitMLx.G] -> CompilerSettings DCoins
dogecoinSettings preconditions = CompilerSettings {
    participants = participantsFromPreconditions preconditions
    , balance = balanceFromPreconditions coinChooser preconditions
    , collateral = collateralFromPreconditions coinChooser preconditions
    , totalFunds = totalFundsFromPreconditions coinChooser preconditions
    , thisChainStepSecretsByLabel = dogecoinStepSecretsFromPreconditions preconditions
    , otherChainStepSecretsByLabel = bitcoinStepSecretsFromPreconditions preconditions
    , currentTime = 1
    , elapseTime = 10
    , currentLabel = ("", "")
    , coinChooser = snd
} where coinChooser = snd

-- | Extract a list of the contract's participants given it's preconditions.
participantsFromPreconditions :: [BitMLx.G] -> [P]
participantsFromPreconditions preconditions =
    let
        getParticipant (BitMLx.Collateral p _ _) = [p]
        getParticipant _notCollateral = []
    in concatMap getParticipant preconditions

-- | Calculate the contract's balance given it's preconditions and a target blockchain.
balanceFromPreconditions :: Coins a => ((BCoins, DCoins) -> a) -> [BitMLx.G] -> a
balanceFromPreconditions coinChooser preconditions =
    let
        getCoins (BitMLx.Deposit _ coins _) = coinChooser coins
        getCoins _notDeposit = 0
        coinValues = map getCoins preconditions
    in sum coinValues

-- | Calculate how much coins participants have to lock as collateral.
collateralFromPreconditions :: Coins a => ((BCoins, DCoins) -> a) -> [BitMLx.G] -> a
collateralFromPreconditions coinChooser preconditions = fromInteger (toInteger (n - 2)) * balance
    where
        n = length (participantsFromPreconditions preconditions)
        balance = balanceFromPreconditions coinChooser preconditions

-- | The total funds (balance + all collaterals) of a contract on a given blockchain.
totalFundsFromPreconditions :: Coins a => ((BCoins, DCoins) -> a) -> [BitMLx.G] -> a
totalFundsFromPreconditions coinChooser preconditions = balance + fromInteger (toInteger (n * fromInteger (toInteger collateral)))
    where
        n = length (participantsFromPreconditions preconditions)
        balance = balanceFromPreconditions coinChooser preconditions
        collateral = collateralFromPreconditions coinChooser preconditions

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
