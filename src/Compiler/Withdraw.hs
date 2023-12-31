{-# LANGUAGE NamedFieldPuns #-}
module Compiler.Withdraw where


import Coins ( Coins, BCoins, DCoins )
import Syntax.Common ( P )
import Syntax.BitML (GuardedContract(Withdraw, Split), Contract)
import Compiler.Auxiliary (eitherLookup, listEither, tupleEither)
import Compiler.Common (syncStepWrapper)
import Compiler.Settings ( CompilerSettings(..) )
import Compiler.Error (CompilationError (StepSecretsNotFoundForNode))


-- | A top-level withdraw statement in BitMLx is compiled in BitML as a split between:
-- - The corresponding withdraw statement for the specified participant,
--   where they get the contract's balance + their collateral.
-- - For each other participant, a withdraw of their collateral.
compileWithdrawC :: Coins c => CompilerSettings c -> [((BCoins, DCoins), P)] -> Either CompilationError (Contract c)
compileWithdrawC settings@CompilerSettings{participants, collateral, coinChooser} fundsMapping =
    if length fundsMapping' == 1
    then compileWithdrawAllC settings ((fst . head) fundsMapping')
    else let splitBranches = [(getWithdrawAmount p , [Withdraw p]) | p <- participants]
        in Right [Split splitBranches]
    where
        fundsMapping' = [
            (p, coins)
            | ((btc, doge), p) <- fundsMapping,
            let coins = coinChooser (btc, doge),
            coins > 0
            ]
        getWithdrawAmount p = case lookup p fundsMapping' of
            Just coins -> coins + collateral
            Nothing -> collateral

-- | A guarded Withdraw compiles quite similarly to a top-level Withdraw but,
-- additionally requires a participant to reveal a step secret before executing.
-- This step secret can be used on the containing priority choice to punish them
-- if they attempt an asynchronous execution.
compileWithdrawD :: Coins c => CompilerSettings c -> [((BCoins, DCoins), P)] -> Either CompilationError (Contract c)
compileWithdrawD settings@CompilerSettings{currentLabel, stepSecretsByLabel} fundsMapping = do
    executeWithdraw <- compileWithdrawC settings fundsMapping
    syncStepWrapper settings executeWithdraw

compileWithdrawAllC :: Coins c => CompilerSettings c -> P -> Either CompilationError (Contract c)
compileWithdrawAllC settings@CompilerSettings{participants, balance, collateral} p = -- compileWithdrawC settings [(balance settings, p)]
    if length participants == 2 then
        Right [Withdraw p]
    else
        let splitBranches = [(getWithdrawAmount p, [Withdraw p]) | p <- participants]
        in Right [Split splitBranches]
    where
        getWithdrawAmount p' = if p == p'
            then balance + collateral
            else collateral

compileWithdrawAllD :: Coins c => CompilerSettings c -> P -> Either CompilationError (Contract c)
compileWithdrawAllD settings p = do
    executeWithdraw <- compileWithdrawAllC settings p
    syncStepWrapper settings executeWithdraw