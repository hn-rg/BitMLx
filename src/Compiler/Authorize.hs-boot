{-# LANGUAGE NamedFieldPuns #-}
module Compiler.Authorize where


import Data.Map (elems)

import qualified Syntax.BitMLx as BitMLx
import Syntax.BitML
import Syntax.Common (P)
import Coins (Coins)
import Compiler.Settings (CompilerSettings (CompilerSettings))
import Compiler.Error (CompilationError (StepSecretsNotFoundForNode))


compileAuthorize :: Coins c => CompilerSettings c -> [P] -> BitMLx.GuardedContract -> Either CompilationError (Contract c)
