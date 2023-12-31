module Compiler.PriorityChoice where

import Coins ( Coins )
import Syntax.Common ( P, Time )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Compiler.Error ( CompilationError )
import Compiler.Settings ( CompilerSettings )

compilePriorityChoice :: Coins c => CompilerSettings c -> BitMLx.GuardedContract -> BitMLx.Contract -> Either CompilationError (BitML.Contract c)
