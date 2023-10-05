{-|
Module      : Compiler.Advertisement
Description : BitMLx to BitML compiler.

-}
module Compiler.Advertisement (compileAdvertisement) where


import Coins ( Coins, BCoins, DCoins)
import Syntax.Common ( DepositId, Time, SName, P )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Compiler.Error ( CompilationError )
import Compiler.Settings ( CompilerSettings(..) )
import Compiler.Contract( compileC, compileD )
import Compiler.Preconditions ( compilePreconditions )
import Compiler.Stipulation ( makeStipulationContract )
import Compiler.WellFormed ( assertWellFormed )
import Syntax.BitMLx (TimedPreconditions(TimedPreconditions))


-- | Given a BitMLx contract advertisement, compiles it to a BitML contract advertisement.
--
--   This includes generating the compilation settings for the target blockchain and
-- wrapping the contract with the stipulation protocol.
compileAdvertisement :: Coins c => CompilerSettings c -> BitMLx.ContractAdvertisement -> Either CompilationError (BitML.ContractAdvertisement c)
compileAdvertisement settings advertisement = do
    assertWellFormed advertisement
    let BitMLx.ContractAdvertisement timedPreconditions contract = advertisement
        TimedPreconditions _ _ preconditions = timedPreconditions
        preconditions' = compilePreconditions settings timedPreconditions
        stipulationContract = makeStipulationContract settings preconditions contract 
    contractResult <- compileC settings stipulationContract 
    Right $ BitML.ContractAdvertisement preconditions' contractResult
