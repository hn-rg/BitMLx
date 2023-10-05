{-# LANGUAGE NamedFieldPuns #-}
module Compiler.Authorize where


import Data.Map (elems)

import qualified Syntax.BitMLx as BitMLx
import Syntax.BitML ((&:), Contract)
import Syntax.Common (P)
import Coins (Coins)
import Compiler.Settings (CompilerSettings (CompilerSettings, currentLabel, stepSecretsByLabel))
import Compiler.Error (CompilationError)
import Compiler.Auxiliary (eitherLookup)
import Compiler.Contract (compileD)


-- | To compile a contract guarded by signatures by a set of participants,
--   we first compile the contract. This will return a sum of many choices
--   each containing the code for the contract, but guarded by the reveal
--   of a different step secret (one for each contractr participant).
--
--  Then, we just guard of the choices with all of the signatures. The order is
--  important here, because if we were to put the signatures after the reveal,
--  execution could get stuck after the reveal and waiting for the signatures.
compileAuthorize :: Coins c => CompilerSettings c -> [P] -> BitMLx.GuardedContract -> Either CompilationError (Contract c)
compileAuthorize settings signers contract = do
    contract' <- compileD settings contract
    Right [signers &: choice | choice <- contract']
