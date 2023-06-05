{-# LANGUAGE RecordWildCards #-}
module Compiler.Withdraw where

import Data.Map (elems)
import Data.List (delete)

import Coins ( Coins )
import Syntax.Common ( P )
import Syntax.BitML (D(Withdraw, Split), C)
import Compiler.Auxiliary (eitherLookup, revealAny, scaleCoins, listEither, tupleEither)
import Compiler.Settings ( CompilerSettings(..) )
import Compiler.Error (CompilationError (StepSecretsNotFoundForNode))


-- | A top-level withdraw statement in BitMLx is compiled in BitML as a split between:
-- - The corresponding withdraw statement for the specified participant,
--   where they get the contract's balance + their collateral.
-- - For each other participant, a withdraw of their collateral.
compileWithdrawC :: Coins c => CompilerSettings c -> [(P, (Rational, Rational))] -> Either CompilationError (C c)
compileWithdrawC settings@CompilerSettings{..} fundsMapping =
    if length fundsMapping' == 1 && length participants == 2 then
        Right [Withdraw (fst $ head fundsMapping')]
    else do
        splitBranches <- listEither [tupleEither (getWithdrawAmount p , [Withdraw p]) | p <- participants]
        Right [Split splitBranches]
    where
        fundsMapping' = [
            (p, coins)
            | (p, (btc, doge)) <- fundsMapping,
            let coins = coinChooser (btc, doge),
            coins > 0
            ]
        getWithdrawAmount p = case lookup p fundsMapping' of
            Just coins -> do
                fromBalance <- balance `scaleCoins` coins
                Right $ fromBalance + collateral
            Nothing -> Right collateral

-- | A guarded Withdraw compiles quite similarly to a top-level Withdraw but,
-- additionally requires a participant to reveal a step secret before executing.
-- This step secret can be used on the containing priority choice to punish them
-- if they attempt an asynchronous execution. 
compileWithdrawD :: Coins c => CompilerSettings c -> [(P, (Rational, Rational))] -> Either CompilationError (C c)
compileWithdrawD settings@CompilerSettings{currentLabel = (choiceLabel, splitLabel), ..} fundMapping = do
    thisChainStepSecrets <- eitherLookup (choiceLabel, splitLabel) thisChainStepSecretsByLabel (StepSecretsNotFoundForNode (choiceLabel, splitLabel))
    executeWithdraw <- compileWithdrawC settings fundMapping
    Right $ revealAny (elems thisChainStepSecrets) executeWithdraw