module Compiler.Contract ( compileC, compileD) where

import Coins ( Coins )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Compiler.Error (CompilationError)
import Compiler.Settings ( CompilerSettings )

compileC :: Coins c => CompilerSettings c -> BitMLx.Contract -> Either CompilationError (BitML.Contract c) 
compileD :: Coins c => CompilerSettings c -> BitMLx.GuardedContract -> Either CompilationError (BitML.Contract c)