{-|
Module      : Compiler
Description : BitMLx to BitML compiler.

-}
module Compiler (compile) where

import Data.Map.Strict (empty)

import Coins ( Coins, BCoins, DCoins)
import Syntax.Common ( DepositId, Time, SName, P )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Syntax.BitML (ContractAdvertisement(ContractAdvertisement))
import Compiler.Error ( CompilationError )
import Compiler.Settings ( CompilerSettings(..), bitcoinSettings, dogecoinSettings )
import Compiler.Contract( compileC, compileD )
import Compiler.Preconditions ( compilePreconditions )
import Compiler.WellFormed ( assertWellFormed )


-- | Given a BitMLx contract advertisement, compiles it to a Bitcoin BitML contract
-- advertisement and a Dogecoin BitML contract advertisement.
compile :: BitMLx.TimedPreconditions -> BitMLx.Contract -> Either CompilationError (BitML.ContractAdvertisement BCoins, BitML.ContractAdvertisement DCoins)
compile timedPreconditions contract = do
    assertWellFormed (BitMLx.ContractAdvertisement timedPreconditions contract)
    let btcSettings = bitcoinSettings timedPreconditions (Left contract)
        dogeSettings = dogecoinSettings timedPreconditions (Left contract)
    bitcoinContract <- compileC btcSettings contract
    dogecoinContract <- compileC dogeSettings contract
    let (bitcoinPreconditions, dogecoinPreconditions) = compilePreconditions btcSettings dogeSettings timedPreconditions
    Right (ContractAdvertisement (bitcoinPreconditions, bitcoinContract), ContractAdvertisement (dogecoinPreconditions, dogecoinContract))