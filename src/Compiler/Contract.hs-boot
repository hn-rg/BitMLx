module Compiler.Contract ( compileC, compileD) where

import Coins ( Coins, BCoins, DCoins)
import Syntax.Common ( Deposit, Time, SName, P )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Compiler.Common ( CompilationError, CompilationResult, ErrorType )
import Compiler.Settings ( CompilerSettings )

initialSettings :: CompilerSettings
compileC :: CompilerSettings -> BitMLx.C -> Either CompilationError CompilationResult 
compileD :: CompilerSettings -> BitMLx.D -> Either CompilationError CompilationResult 