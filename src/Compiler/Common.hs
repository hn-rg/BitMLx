module Compiler.Common where

import Data.Map.Strict (Map, empty)

import Coins ( Coins, BCoins, DCoins)
import Syntax.Common ( Deposit, Time, SName, P )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Compiler.Preconditions ( compileG )


type CompilationResult = (BitML.C BCoins, BitML.C DCoins)

-- | Different kind of errors that the compiler can output 
data ErrorType =
    -- | Tried to reveal a secret not declared in the preconditions.
    MissingSecret
    -- | Split bitcoin values don't add up to 1.
    | InconsistentBitcoinSplit
    -- | Split dogecoin values don't add up to 1.
    | InconsistentDogecoinSplit
    -- | Collateral coins are not a multiple of the number of participants
    | NonDividableCollaterals
    -- | Not implemented yet.
    | NotImplemented
    -- | No deposit for participant
    | DepositNotFound
    -- | No collateral for participant
    | CollateralNotFound
    -- | No step secrets for node or participant
    | StepSecretsNotFound
    deriving Show

-- | Errors with type and details.
type CompilationError = (ErrorType, String)
