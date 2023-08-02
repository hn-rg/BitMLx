module Compiler.Split where

import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import {-# SOURCE #-} Compiler.Contract (compileC)
import qualified Data.Bifunctor
import Coins (BCoins, DCoins, Coins)
import Compiler.Error (CompilationError)
import Compiler.Settings (CompilerSettings)

type SplitBranch = ((BCoins, DCoins), BitMLx.Contract)
compileSplit :: Coins c => CompilerSettings c -> [SplitBranch] -> Either CompilationError (BitML.Contract c)
