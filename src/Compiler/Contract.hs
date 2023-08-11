{-|
Module      : Compiler.Contract
Description : Function to compile Contract and GuardedContract

Functions here are the entry point for the compiler but,
they work more like routers, delegating each specific case
to a dedicated module.
-}
module Compiler.Contract ( compileC, compileD ) where

import Coins ( Coins, BCoins, DCoins)
import Syntax.Common ( Time, SName, P )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Compiler.Error ( CompilationError(..) )
import Compiler.Withdraw ( compileWithdrawC, compileWithdrawD, compileWithdrawAllC, compileWithdrawAllD )
import Compiler.Preconditions ( compileG )
import Compiler.Settings (CompilerSettings)
import {-# SOURCE #-} Compiler.PriorityChoice (compilePriorityChoice)
import {-# SOURCE #-} Compiler.Split (compileSplit)
import Syntax.BitMLx (Contract(PriorityChoice, Withdraw, WithdrawAll), GuardedContract (WithdrawD, Split, WithdrawAllD))


-- | Given a BitMLx contract, compile it to a contract on the target blockchain,
-- according to some settings previously compiled from the contract preconditions.
compileC :: Coins c => CompilerSettings c -> BitMLx.Contract -> Either CompilationError (BitML.Contract c)
compileC settings (PriorityChoice d c) = compilePriorityChoice settings d c
compileC settings (Withdraw fundsMapping) = compileWithdrawC settings fundsMapping
compileC settings (WithdrawAll p) = compileWithdrawAllC settings p

-- | Given a BitMLx guarded contract, compile it to a contract on the target blockchain,
-- according to some settings previously compiled from the contract preconditions.
-- We delegate each specific case to a dedicated module.
compileD :: Coins c => CompilerSettings c -> BitMLx.GuardedContract -> Either CompilationError (BitML.Contract c)
compileD settings d = case d of
    WithdrawD fundsMapping -> compileWithdrawD settings fundsMapping
    WithdrawAllD p -> compileWithdrawAllD settings p
    Split subcontracts -> compileSplit settings subcontracts
    _notImplementedYet -> Left NotImplemented
