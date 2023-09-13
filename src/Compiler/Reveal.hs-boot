{-# LANGUAGE NamedFieldPuns #-}
module Compiler.Reveal where


import qualified Syntax.BitMLx as BitMLx
import Syntax.BitML ( Contract )
import Syntax.Common (SName, Pred)
import Coins (Coins)
import Compiler.Settings (CompilerSettings (CompilerSettings))
import Compiler.Error (CompilationError (StepSecretsNotFoundForNode))


compileReveal :: Coins c => CompilerSettings c -> [SName] -> BitMLx.Contract -> Either CompilationError (Contract c)
compileRevealIf :: Coins c => CompilerSettings c -> [SName] -> Pred -> BitMLx.Contract -> Either CompilationError (Contract c)
