module Compiler.PriorityChoice where

import Coins ( Coins, BCoins, DCoins )
import Syntax.Common ( P, Deposit, Time )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Compiler.Common ( CompilationError, CompilationResult )
import Compiler.Settings ( CompilerSettings )

compilePriorityChoice :: CompilerSettings -> BitMLx.D -> BitMLx.C -> Maybe Time -> Either CompilationError CompilationResult
