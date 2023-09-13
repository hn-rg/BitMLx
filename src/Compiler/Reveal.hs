{-# LANGUAGE NamedFieldPuns #-}
module Compiler.Reveal where


import Data.Map (elems)
import Data.Maybe (fromMaybe)

import qualified Syntax.BitMLx as BitMLx
import Syntax.BitML
import Syntax.Common (SName, Pred (PTrue))
import Coins (Coins)
import Compiler.Settings (CompilerSettings (CompilerSettings, currentLabel, stepSecretsByLabel))
import Compiler.Error (CompilationError (StepSecretsNotFoundForNode))
import Compiler.Auxiliary (eitherLookup)
import Compiler.Contract (compileC)

-- | When we compile a Reveal statement, we need to merge the user secrets with the step secret reveal.
--   This has two reasons. The first is that we save an execution step, since each reveal is later on
--   a Bit/Dogecoin transaction. But the second is that, if they were two separate steps, an adversary
--   would be able to block the execution by revealing only the user secret or the step secret. 
compileReveal :: Coins c => CompilerSettings c -> [SName] -> BitMLx.Contract -> Either CompilationError (Contract c)
compileReveal settings@CompilerSettings{currentLabel, stepSecretsByLabel} secrets followUp = do
    stepSecrets <- eitherLookup currentLabel stepSecretsByLabel (StepSecretsNotFoundForNode currentLabel)
    followUp' <- compileC settings followUp
    Right [Reveal (s : secrets) followUp' | s <- elems stepSecrets]

-- | RevealIf exists mostly for syntactic sugar (many cases have Reveal, without conditions) but
--   compilation is basically the same as Reveal but adding the predicate.
compileRevealIf :: Coins c => CompilerSettings c -> [SName] -> Pred -> BitMLx.Contract -> Either CompilationError (Contract c)
compileRevealIf settings@CompilerSettings{currentLabel, stepSecretsByLabel} secrets predicate followUp = do
    stepSecrets <- eitherLookup currentLabel stepSecretsByLabel (StepSecretsNotFoundForNode currentLabel)
    followUp' <- compileC settings followUp
    Right [RevealIf (s : secrets) predicate followUp' | s <- elems stepSecrets]
