module TestSettings where


import qualified Data.Map.Strict as Map

import Coins (BCoins, DCoins)
import Syntax.Common (NodeLabel, P (pname), SName, emptyLabel)
import Syntax.BitMLx (TimedPreconditions (TimedPreconditions), Contract, GuardedContract)
import Compiler.Settings (CompilerSettings (..), participantsFromPreconditions, balanceFromPreconditions, collateralFromPreconditions, generateStartSecrets)
import Compiler.StepSecrets (stepSecretsC, stepSecretsD, stepSecretName)

bitcoinTestCompilerSettings :: TimedPreconditions -> Either Contract GuardedContract -> CompilerSettings BCoins
bitcoinTestCompilerSettings (TimedPreconditions startingTime elapseTime preconditions) contract = CompilerSettings {
    targetBlockchain = "Bitcoin"
    , participants = participantsFromPreconditions preconditions
    , balance = balanceFromPreconditions coinChooser preconditions
    , collateral = collateralFromPreconditions coinChooser preconditions
    , stepSecretsByLabel = stepSecretsForTests participants contract
    , startSecrets = generateStartSecrets participants
    , currentTime = startingTime
    , elapseTime = elapseTime
    , currentLabel = emptyLabel
    , coinChooser = coinChooser
} where
    participants = participantsFromPreconditions preconditions
    coinChooser = fst

dogecoinTestCompilerSettings :: TimedPreconditions -> Either Contract GuardedContract -> CompilerSettings DCoins
dogecoinTestCompilerSettings (TimedPreconditions startingTime elapseTime preconditions) contract = CompilerSettings {
    targetBlockchain = "Dogecoin"
    , participants = participantsFromPreconditions preconditions
    , balance = balanceFromPreconditions coinChooser preconditions
    , collateral = collateralFromPreconditions coinChooser preconditions
    , stepSecretsByLabel = stepSecretsForTests participants contract
    , startSecrets = generateStartSecrets participants
    , currentTime = startingTime
    , elapseTime = elapseTime
    , currentLabel = emptyLabel
    , coinChooser = coinChooser
} where
    participants = participantsFromPreconditions preconditions
    coinChooser = snd


stepSecretsForTests :: [P] -> Either Contract GuardedContract -> Map.Map NodeLabel (Map.Map P SName)
stepSecretsForTests participants contract =
    let onContract = case contract of
            Left c -> stepSecretsC participants emptyLabel c
            Right d -> stepSecretsD participants emptyLabel d
    in Map.fromListWith
        (Map.unionWith (++))
        [(l, Map.singleton p s) | (l, byParticipant) <- onContract, (p, s) <- byParticipant]