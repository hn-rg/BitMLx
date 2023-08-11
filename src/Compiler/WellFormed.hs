module Compiler.WellFormed where

import Syntax.BitMLx (TimedPreconditions (TimedPreconditions), Contract, ContractAdvertisement(ContractAdvertisement) )
import Syntax.Common (P, Time)
import Coins (BCoins, DCoins)
import Compiler.Error (CompilationError)


assertWellFormed :: ContractAdvertisement -> Either CompilationError ()
assertWellFormed advertisement = let
    ContractAdvertisement timedPreconditions contract = advertisement
    TimedPreconditions startTime timeElapse preconditions = timedPreconditions
    in Right ()
