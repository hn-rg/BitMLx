{-|
Module      : Compiler
Description : BitMLx to BitML compiler.

-}
module Compiler (compileBitMLx) where


import Coins ( Coins, BCoins, DCoins)
import Syntax.Common ( DepositId, Time, SName, P )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Compiler.Advertisement (compileAdvertisement)
import Compiler.Error ( CompilationError )
import Compiler.Settings ( CompilerSettings(..), bitcoinSettings, dogecoinSettings )
import Compiler.WellFormed ( assertWellFormed )
import Syntax.BitMLx (TimedPreconditions(TimedPreconditions))


-- | Entry point for the BitMLx to (Bitcoin BitML, Dogecoin BitML) compiler.
--
-- Given a contract advertisement in BitMLx, returns a pair of advertisements, one for
-- each of the target blockchains.
--
-- Design note: we should generalize this to take as input a list of target blockchains
-- and return a list of advertisements, in particular if we want to extend our language
--  to Nblockchains.
compileBitMLx :: BitMLx.ContractAdvertisement -> Either CompilationError (BitML.ContractAdvertisement BCoins, BitML.ContractAdvertisement DCoins)
compileBitMLx bitmlxAdvertisement = do
    let btcSettings = bitcoinSettings bitmlxAdvertisement
        dogeSettings = dogecoinSettings bitmlxAdvertisement
    btcAdvertisement <- compileAdvertisement btcSettings bitmlxAdvertisement
    dogeAdvertisement <- compileAdvertisement dogeSettings bitmlxAdvertisement
    Right (btcAdvertisement, dogeAdvertisement)
