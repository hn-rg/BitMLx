module Compiler (compileG, compileD) where

import Syntax.Common ( DCoins, BCoins, Deposit )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Compiler.Withdraw ( compileWithdraw )
import Compiler.Preconditions ( compileG )


type CompilationResult = (BitML.C BCoins, BitML.C DCoins)

-- | Given a BitMLx contract and it's preconditions, compile to a a Bitcoin BitML and a Dogecoin BitML contract.
-- This is the entry point for the compiler, but we delegate each compilation case to auxiliary modules.
-- TODO: This signature should later be extended to include settings and current compiler state.
-- TODO: Better compilation errors using Either CompilationResult CompilationError 
compileD :: [BitMLx.G] -> BitMLx.D -> Maybe CompilationResult
compileD gs d = case d of
    BitMLx.Withdraw p -> Just (compileWithdraw gs p)
    _notImplementedYet -> Nothing
