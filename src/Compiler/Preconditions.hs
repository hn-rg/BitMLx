{-# LANGUAGE NamedFieldPuns #-}
module Compiler.Preconditions where

import Coins ( Coins, DCoins, BCoins)
import Syntax.Common ( DepositId, P, SName )
import Compiler.Settings (CompilerSettings (..))
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Data.Map.Strict (toList)
import Syntax.BitMLx

-- | Compiles a BitMLx contract precondition into a
-- BitML preconditions for Bitcoin and one for Dogecoin.
compileG :: CompilerSettings BCoins -> CompilerSettings DCoins -> BitMLx.Precondition -> (BitML.Precondition BCoins, BitML.Precondition DCoins)
-- | BitML deposits add the needed collaterals on each chain.
compileG bitcoinSettings dogecoinSettings (BitMLx.Deposit p (bv,dv) z) =
    (
        BitML.Deposit p (bv + collateral bitcoinSettings) (z ++ "_Bitcoin"),
        BitML.Deposit p (dv + collateral dogecoinSettings) (z ++ "_Dogecoin")
    )
-- | BitMLx secrets translate to identical secrets on both chains.
compileG _ _ (BitMLx.Secret p n h) = (BitML.Secret p n h, BitML.Secret p n h)


-- | Extra secrets added to synchronize priority choices. 
stepSecretPreconditionsFromSettings :: Coins c => CompilerSettings c -> [BitML.Precondition c]
stepSecretPreconditionsFromSettings CompilerSettings{stepSecretsByLabel} =
    concat [
        [
            BitML.Secret participant secretName stepSecretHashPlaceholder
            | (participant, secretName) <- toList stepSecrets
        ] | (_nodeLabel, stepSecrets) <- toList stepSecretsByLabel
    ]

-- | Compile all preconditions for a BitMLx contract
compilePreconditions :: CompilerSettings BCoins -> CompilerSettings DCoins -> TimedPreconditions -> ([BitML.Precondition BCoins], [BitML.Precondition DCoins])
compilePreconditions  bitcoinSettings dogecoinSettings (TimedPreconditions _ _ preconditions) =
    unzip (
        map (compileG bitcoinSettings dogecoinSettings) preconditions
        ++ zip 
            (stepSecretPreconditionsFromSettings bitcoinSettings)
            (stepSecretPreconditionsFromSettings dogecoinSettings)
    )

-- | A BitMLx contract runner client would then provide hashes for all the step secrets
-- and reveal them as part of the execution (or not, dependig on the implemented strategy)
-- without the user ever knowing or caring about them.
-- Here, we just put a placeholder.
stepSecretHashPlaceholder :: String
stepSecretHashPlaceholder = "__HASH__PLACEHOLDER__"