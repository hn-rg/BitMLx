module Compiler.Error (CompilationError(..)) where

import Data.Map.Strict (Map, empty)

import Coins ( Coins, BCoins, DCoins)
import Syntax.Common ( Time, SName, P, NodeLabel )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx

-- | Different kind of errors that the compiler can output.
data CompilationError =
    -- Withdraw balances don't add up to total contract Balance.
    InconsistentWithdraw
    -- Split balances don't add up to total contract Balance.
    | InconsistentSplit
    -- | This Participant has no deposit in the preconditions.
    | NoDeposit P
    -- | Secret was not commited in preconditions.
    | UncommitedSecret SName
    -- | No step secrets for node.
    | StepSecretsNotFoundForNode NodeLabel
    -- | No step secrets for participant.
    | StepSecretsNotFoundForParticipant P
    -- | Not implemented yet.
    | NotImplemented
    deriving (Show, Eq)
