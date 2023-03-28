module Compiler.Split where

import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import {-# SOURCE #-} Compiler.Contract (compileC)
import qualified Data.Bifunctor
import Coins (BCoins, DCoins, Coins)
import Compiler.Error (CompilationError)
import Compiler.Settings (CompilerSettings)

compileSplit :: Coins c => CompilerSettings c -> [((Rational, Rational), BitMLx.C)] -> Either CompilationError (BitML.D c)
