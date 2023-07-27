{-# LANGUAGE RecordWildCards #-}
module Compiler.PriorityChoice ( compilePriorityChoice ) where

import Data.Map.Strict (Map, elems)
import Data.List (delete)

import Coins ( Coins, BCoins, DCoins)
import Syntax.Common ( P (pname), Time(..), SName(..), NodeLabel )
import Syntax.BitML ( D(Reveal, Withdraw, Split, After) )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import {-# SOURCE #-} Compiler.Contract (compileC, compileD)
import Compiler.Error ( CompilationError(..) )
import Compiler.Settings ( CompilerSettings (..) )
import Compiler.Auxiliary (eitherLookup, tau, revealAny, listEither)


-- | A priority choice between a guarded contract D and a contract C is compiled to a choice between:
-- - Anyone reveals their step secrets on both chains before time t and we execute D synchronously.
-- - After t passes without anyone revealing a step secret on one chain, we "skip", meaning:
--    - If anyone reveals a step secret on the other chain (meaning they didn't skip there)
--      we punish them by splitting their collateral on this chain among everyone else. The honest participants
--      then also receive a refund for their deposits and their collaterals.
--    - If time t' passes and everyone is skipping synchronously, then we execute C.
compilePriorityChoice :: Coins c => CompilerSettings c -> BitMLx.D -> BitMLx.C -> Maybe Time -> Either CompilationError (BitML.C c)
compilePriorityChoice settings@CompilerSettings{currentLabel = (choiceLabel, splitLabel), ..} d c overrideTimeout = do
        d' <- compileD settings{currentLabel = (choiceLabel ++ "L", splitLabel), currentTime = currentTime + elapseTime} d
        c' <- compileC settings{currentLabel = (choiceLabel ++ "R", splitLabel), currentTime = currentTime + 2 * elapseTime} c
        punish <- punishAnyone settings
        let t = currentTime + elapseTime
            t' = currentTime + 2 * elapseTime
            skip = tau (punish ++ [After t' (tau c')])
        Right (d' ++ [After t skip])

-- | Builds the sum contract where we punish any participant here for revealing
-- their step secret on the other blockchain.
punishAnyone :: Coins c => CompilerSettings c -> Either CompilationError (BitML.C c)
punishAnyone settings@CompilerSettings{currentLabel = (choiceLabel, splitLabel), ..} = do
    stepSecrets <- eitherLookup
        (choiceLabel ++ "L", splitLabel) stepSecretsByLabel
        (StepSecretsNotFoundForNode (choiceLabel, splitLabel))
    listEither [ punish p participants balance collateral stepSecrets | p <- participants]

-- | Punish a participant that reveals their step secret on the other blockchain
-- by splitting it's collateral among all other participants on this blockchain.
-- Because of how we define the collaterals ((n-2) * balance), the total funds are just enough for
-- each honest participant to withdraw an amount equal to the contract balance + their own collateral.
punish :: Coins c => P -> [P] -> c -> c -> Map P SName -> Either CompilationError (BitML.D c)
punish p allParticipants balance collateral stepSecrets = do
    secret <- eitherLookup p stepSecrets (StepSecretsNotFoundForParticipant p)
    let honestParticipants = delete p allParticipants
        executePunishment = case honestParticipants of
            [h] -> Withdraw h
            hs -> Split [(balance + collateral, [Withdraw h]) | h <- hs]
    Right $ Reveal [secret] [executePunishment]