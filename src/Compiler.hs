{-|
Module      : Compiler
Description : BitMLx to BitML compiler.

-}
module Compiler (compilePreconditions, compileC, compileD, CompilerSettings(..), initialSettings) where

import Data.Map.Strict (empty)

import Coins ( Coins, BCoins, DCoins)
import Syntax.Common ( Deposit, Time, SName, P )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Compiler.Common ( CompilationError, CompilationResult, ErrorType (..) )
import Compiler.Settings ( CompilerSettings(..), initialSettings )
import Compiler.Contract( compileC, compileD )
import Compiler.Preconditions ( compilePreconditions )
