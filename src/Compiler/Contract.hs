{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
import Compiler.Common ( CompilationError, CompilationResult, ErrorType (..))
import Compiler.Withdraw ( compileWithdraw )
import Compiler.Preconditions ( compileG )
import Compiler.Settings ( CompilerSettings, initialSettings )
import {-# SOURCE #-} Compiler.PriorityChoice (compilePriorityChoice)

-- | Given a BitMLx contract, compile it to a Bitcoin and a Dogecoin BitML contract,
-- according to some settings previously compiled from the contract preconditions.
compileC :: CompilerSettings -> BitMLx.C -> Either CompilationError CompilationResult
compileC settings (BitMLx.PriorityChoice d c) = compilePriorityChoice settings d c Nothing
compileC settings (BitMLx.TimedPriorityChoice t d c) = compilePriorityChoice settings d c (Just t)
compileC settings BitMLx.NullContract = Right ([], [])


-- | Given a BitMLx guarded contract, compile it to a Bitcoin and a Dogecoin BitML contract,
-- according to some settings previously compiled from the contract preconditions.
-- We delegate each specific case to a dedicated module.
compileD :: CompilerSettings -> BitMLx.D -> Either CompilationError CompilationResult
compileD settings d = case d of
    BitMLx.Withdraw p -> Right (compileWithdraw settings p)
    _notImplementedYet -> Left (NotImplemented, "Branch not implemented yet")
