{-# LANGUAGE RecordWildCards #-}
module Compiler.Withdraw where

import Data.Map (elems)
import Data.List (delete)

import Coins ( Coins )
import Syntax.Common ( P )
import Syntax.BitML (D(Withdraw, Split), C)
import Compiler.Auxiliary (eitherLookup, revealAny)
import Compiler.Settings ( CompilerSettings(..) )
import Compiler.Error (CompilationError (StepSecretsNotFoundForNode))


-- | A top-level withdraw statement in BitMLx is compiled in BitML as a split between:
-- - The corresponding withdraw statement for the specified participant,
--   where they get the contract's balance + their collateral.
-- - For each other participant, a withdraw of their collateral.
compileWithdrawC :: Coins c => CompilerSettings c -> P -> Either CompilationError (C c)
compileWithdrawC settings@CompilerSettings{..} p = do
    let others = delete p participants
        executeWithdraw = case others of
            [] -> error "Single participant contract."
            -- Contracts with only 2 parties require no collaterals,
            -- so the other participant doesn't withdraw anything.
            [_other] -> Withdraw p
            others'-> Split $ (balance + collateral, [Withdraw p]) : [(collateral, [Withdraw p']) | p' <- others']
    Right [executeWithdraw]

-- | A guarded Withdraw compiles quite similarly to a top-level Withdraw but,
-- additionally requires a participant to reveal a step secret before executing.
-- This step secret can be used on the containing priority choice to punish them
-- if they attempt an asynchronous execution. 
compileWithdrawD :: Coins c => CompilerSettings c -> P -> Either CompilationError (C c)
compileWithdrawD settings@CompilerSettings{currentLabel = (choiceLabel, splitLabel), ..} p = do
    thisChainStepSecrets <- eitherLookup (choiceLabel, splitLabel) thisChainStepSecretsByLabel (StepSecretsNotFoundForNode (choiceLabel, splitLabel))
    executeWithdraw <- compileWithdrawC settings p
    Right $ revealAny (elems thisChainStepSecrets) executeWithdraw