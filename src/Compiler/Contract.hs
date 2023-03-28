{-|
Module      : Compiler.Contract
Description : Function to compile C and D

Functions here are the entry point for the compiler but,
they work more like routers, delegating each specific case
to a dedicated module.
-}
module Compiler.Contract ( compileC, compileD ) where

import Data.Map.Strict ( empty )

import Coins ( Coins, BCoins, DCoins)
import Syntax.Common ( Deposit, Time, SName, P )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Compiler.Error ( CompilationError(..) )
import Compiler.Withdraw ( compileWithdraw )
import Compiler.Preconditions ( compileG )
import Compiler.Settings ( CompilerSettings, )
import {-# SOURCE #-} Compiler.PriorityChoice (compilePriorityChoice)
import {-# SOURCE #-} Compiler.Split (compileSplit)

-- | Given a BitMLx contract, compile it to a contract on the target blockchain,
-- according to some settings previously compiled from the contract preconditions.
compileC :: Coins c => CompilerSettings c -> BitMLx.C -> Either CompilationError (BitML.C c)
compileC settings (BitMLx.PriorityChoice d c) = compilePriorityChoice settings d c Nothing
compileC settings (BitMLx.TimedPriorityChoice t d c) = compilePriorityChoice settings d c (Just t)
compileC settings BitMLx.NullContract = Right []


-- | Given a BitMLx guarded contract, compile it to a contract on the target blockchain,
-- according to some settings previously compiled from the contract preconditions.
-- We delegate each specific case to a dedicated module.
compileD :: Coins c => CompilerSettings c -> BitMLx.D -> Either CompilationError (BitML.D c)
compileD settings d = case d of
    BitMLx.Withdraw p -> Right (compileWithdraw settings p)
    BitMLx.Split subcontracts -> compileSplit settings subcontracts
    _notImplementedYet -> Left NotImplemented
