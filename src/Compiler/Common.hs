{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Compiler.Common where

import Data.Map (elems)

import Coins (Coins)
import Syntax.Common (SName, P)
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Compiler.Auxiliary (eitherLookup)
import Compiler.Error (CompilationError (StepSecretsNotFoundForNode))
import Compiler.Settings (CompilerSettings(CompilerSettings, currentLabel, stepSecretsByLabel))


-- | Small cheat to convert a guarded contract into a contract.
-- Notice that the price of using tau is that it introduces an
-- extra transaction, with it's associated transaction fees.
tau :: BitML.Contract c -> BitML.GuardedContract c
tau = BitML.Reveal []

-- | An alternative reveal construction that works by requiring any of a list of secrets
-- to be revealed instead of all of them.
revealAny :: Coins c =>  [SName] -> BitML.Contract c -> BitML.Contract c
revealAny secrets subcontract =  [BitML.Reveal [s] subcontract | s <- secrets]

-- | This function is a wrapper around a compiled guarded contract. It adds the prerequisite of revealing 
--   a step secret from any participant for this node label before executing.
--
--   Step secrets are a key part of our synchronicity mechanism. Revealing a step secret is proof of
--   execution intent. When a malicious participant executes a branch on one blockchain but doesn't
--   replicate the action on the other, their step secret can be used to punish them on the second
--   blockchain.
syncStepWrapper :: Coins c => CompilerSettings c -> BitML.Contract c -> Either CompilationError (BitML.Contract c)
syncStepWrapper CompilerSettings{currentLabel, stepSecretsByLabel, ..} step = do
    stepSecrets <- eitherLookup currentLabel stepSecretsByLabel (StepSecretsNotFoundForNode currentLabel)
    Right $ revealAny (elems stepSecrets) step
