{-|
Module      : Compiler
Description : BitMLx to BitML compiler.

-}
module Compiler (compile) where

import Data.Map.Strict (empty)

import Coins ( Coins, BCoins, DCoins)
import Syntax.Common ( Deposit, Time, SName, P )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Compiler.Error ( CompilationError )
import Compiler.Settings ( CompilerSettings(..), bitcoinSettings, dogecoinSettings )
import Compiler.Contract( compileC, compileD )
import Compiler.Preconditions ( compilePreconditions )


-- | Given a BitMLx contract advertisement, compiles it to a Bitcoin BitML contract
-- advertisement and a Dogecoin BitML contract advertisement.
compile :: [BitMLx.G] -> BitMLx.C -> Either CompilationError (([BitML.G BCoins], BitML.C BCoins), ([BitML.G DCoins], BitML.C DCoins))
compile preconditions contract = do
    bitcoinContract <- compileC (bitcoinSettings preconditions) contract
    dogecoinContract <- compileC (dogecoinSettings preconditions) contract
    let (bitcoinPreconditions, dogecoinPreconditions) = compilePreconditions preconditions
    Right ((bitcoinPreconditions, bitcoinContract), (dogecoinPreconditions, dogecoinContract))