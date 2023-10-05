{-# LANGUAGE NamedFieldPuns #-}
module Compiler.Preconditions where

import Coins ( Coins, DCoins, BCoins)
import Syntax.Common ( DepositId, P, SName )
import Compiler.Settings (CompilerSettings (..))
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Data.Map.Strict (toList)
import Syntax.BitMLx ( TimedPreconditions(..) )

-- | Compiles a BitMLx contract precondition into a
-- BitML preconditions for Bitcoin and one for Dogecoin.
compileG :: Coins c => CompilerSettings c -> BitMLx.Precondition -> BitML.Precondition c
-- | BitML deposits add the needed collaterals on each chain.
compileG CompilerSettings{targetBlockchain, collateral, coinChooser} (BitMLx.Deposit p coins z) =
    BitML.Deposit p (coinChooser coins + collateral) (z ++ "_" ++ targetBlockchain)
-- | BitMLx secrets translate to identical secrets on both chains.
compileG _ (BitMLx.Secret p n h) = BitML.Secret p n h

-- | Extra secrets added to synchronize priority choices. 
stepSecretPreconditionsFromSettings :: Coins c => CompilerSettings c -> [BitML.Precondition c]
stepSecretPreconditionsFromSettings CompilerSettings{stepSecretsByLabel} =
    concat [
        [
            BitML.Secret participant secretName stepSecretHashPlaceholder
            | (participant, secretName) <- toList stepSecrets
        ] | (_nodeLabel, stepSecrets) <- toList stepSecretsByLabel
    ]

-- | Extra secrets added for the stipulation protocol. 
startSecretPreconditionsFromSettings :: Coins c => CompilerSettings c -> [BitML.Precondition c]
startSecretPreconditionsFromSettings CompilerSettings{startSecrets} =
    [
        BitML.Secret participant secretName stepSecretHashPlaceholder
        | (participant, secretName) <- toList startSecrets
    ] 

-- | Compile all preconditions for a BitMLx contract
compilePreconditions ::Coins c => CompilerSettings c -> TimedPreconditions -> [BitML.Precondition c]
compilePreconditions settings (TimedPreconditions _ _ preconditions) =
    map (compileG settings) preconditions
    ++ stepSecretPreconditionsFromSettings settings
    ++ startSecretPreconditionsFromSettings settings

-- | A BitMLx contract runner client would then provide hashes for all the step secrets
-- and reveal them as part of the execution (or not, dependig on the implemented strategy)
-- without the user ever knowing or caring about them.
-- Here, we just put a placeholder.
stepSecretHashPlaceholder :: String
stepSecretHashPlaceholder = "__HASH__PLACEHOLDER__"
